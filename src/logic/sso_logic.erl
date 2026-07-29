-module(sso_logic).

%%%
% SSO 外部认证配置业务逻辑模块
% SSO external authentication config business logic
%
% 职责：
%   - get_config/0     组装三 provider 配置响应 {ldap?, saml?, oauth2?}
%   - save_config/1    校验 provider 后按 provider upsert
%   - test_connection/2 字段完整性校验 + 基础连通性探测（MVP）
%
% ⚠️ test_connection MVP 边界（保守）：
%   仅做字段完整性 + 连通性探测——LDAP 用 gen_tcp TCP 可达；SAML/OAuth2 对
%   metadata_url/auth_url 做 HTTP(HEAD) 可达探测。**完整的 LDAP bind /
%   SAML 断言校验 / OAuth2 换 token 的真实认证流程超出本次范围**，留待后续。
%%%

-export([get_config/0, save_config/1, test_connection/2]).
-export([implemented_providers/0, is_implemented/1]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").
-include("common.hrl").

-define(VALID_PROVIDERS, [<<"ldap">>, <<"saml">>, <<"oauth2">>]).
%% 真正实现了登录链路的 provider。oauth2 走 auth_oidc_logic 的
%% authorize→callback→exchange；ldap/saml 只有配置表，没有任何认证端点
%% （imboy_router 里没有 saml/ldap 路由，代码里没有 eldap/SAMLResponse 实现）。
%% 配置可以先存下来，但绝不能报「连接成功」或允许 enabled=true —— 那会让
%% 管理员以为 SSO 已可用，实际用户永远登不进来。
-define(IMPLEMENTED_PROVIDERS, [<<"oauth2">>]).
-define(TCP_TIMEOUT, 3000).
-define(HTTP_TIMEOUT, 5000).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 读取三 provider 配置，缺省 provider 不含该键
%% 敏感字段脱敏为 <<"***">>，并附 has_<field> 布尔标志（前端"留空=不修改"约定）
-spec get_config() -> {ok, map()} | {error, term()}.
get_config() ->
    case sso_config_ds:get_all() of
        {ok, M} ->
            {ok, maps:map(fun(_Provider, Cfg) -> mask_secrets(Cfg) end, M)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 敏感字段脱敏：非空 -> ***；同时给出 has_<field> 供前端判断是否已配置
-spec mask_secrets(map()) -> map().
mask_secrets(Cfg) ->
    lists:foldl(
        fun(F, Acc) ->
            HasKey = <<"has_", F/binary>>,
            case maps:get(F, Acc, <<>>) of
                <<>> ->
                    Acc#{HasKey => false};
                _ ->
                    Acc#{F => <<"***">>, HasKey => true}
            end
        end,
        Cfg,
        sso_config_ds:secret_fields()
    ).

%% @doc 保存单个 provider 配置（body 即配置对象，含 provider 字段）
-spec save_config(map()) -> {ok, map()} | {error, binary()}.
save_config(ConfigMap) when is_map(ConfigMap) ->
    Provider = maps:get(<<"provider">>, ConfigMap, undefined),
    case lists:member(Provider, ?VALID_PROVIDERS) of
        false ->
            {error, <<"无效的 provider（应为 ldap|saml|oauth2）"/utf8>>};
        true ->
            %% fail-closed：未实现登录链路的 provider 不允许启用。
            %% 允许以 enabled=false 预存配置，等实现落地后再打开。
            case {wants_enabled(ConfigMap), is_implemented(Provider)} of
                {true, false} ->
                    {error, unimplemented_message(Provider)};
                _ ->
                    sso_config_ds:upsert(Provider, ConfigMap)
            end
    end;
save_config(_) ->
    {error, <<"参数错误"/utf8>>}.

%% @doc 已实现登录链路的 provider 列表
-spec implemented_providers() -> [binary()].
implemented_providers() -> ?IMPLEMENTED_PROVIDERS.

-spec is_implemented(binary()) -> boolean().
is_implemented(Provider) -> lists:member(Provider, ?IMPLEMENTED_PROVIDERS).

%% enabled 兼容 boolean 与字符串（管理端表单可能传 "true"）
-spec wants_enabled(map()) -> boolean().
wants_enabled(Cfg) ->
    case maps:get(<<"enabled">>, Cfg, false) of
        true -> true;
        <<"true">> -> true;
        1 -> true;
        _ -> false
    end.

-spec unimplemented_message(binary()) -> binary().
unimplemented_message(Provider) ->
    <<Provider/binary, " 登录链路尚未实现，不能启用（可先以 enabled=false 保存配置）。"/utf8>>.

%% @doc 连通性 + 字段校验，返回 {Success, Message}
-spec test_connection(binary(), map()) -> {boolean(), binary()}.
test_connection(<<"ldap">>, Config) ->
    test_ldap(Config);
test_connection(<<"saml">>, Config) ->
    test_saml(Config);
test_connection(<<"oauth2">>, Config) ->
    test_oauth2(Config);
test_connection(_, _) ->
    {false, <<"无效的 provider（应为 ldap|saml|oauth2）"/utf8>>}.

%% ===================================================================
%% Internal: provider 探测
%% ===================================================================

%% ⚠️ LDAP/SAML 未实现登录链路：即使字段合法、网络可达，也必须返回 false。
%% 返回 true 会让管理端显示「连接成功」，管理员据此认为 SSO 可用 —— 但没有任何
%% LDAP bind / SAML ACS 端点，用户永远登不进来。探测结果只作为诊断信息附在消息里。
-spec test_ldap(map()) -> {boolean(), binary()}.
test_ldap(Config) ->
    Host = maps:get(<<"host">>, Config, <<>>),
    Port = maps:get(<<"port">>, Config, 0),
    case {is_nonempty_binary(Host), valid_port(Port)} of
        {false, _} ->
            {false, <<"LDAP host 不能为空"/utf8>>};
        {_, false} ->
            {false, <<"LDAP port 无效（应为 1-65535 整数）"/utf8>>};
        {true, true} ->
            {_Reachable, ProbeMsg} = probe_tcp(Host, Port),
            {false, unimplemented_probe_message(<<"ldap">>, ProbeMsg)}
    end.

-spec test_saml(map()) -> {boolean(), binary()}.
test_saml(Config) ->
    Url = maps:get(<<"metadata_url">>, Config, <<>>),
    case valid_http_url(Url) of
        false ->
            {false, <<"SAML metadata_url 无效（需 http:// 或 https:// 开头）"/utf8>>};
        true ->
            {_Reachable, ProbeMsg} = probe_http(Url),
            {false, unimplemented_probe_message(<<"saml">>, ProbeMsg)}
    end.

-spec unimplemented_probe_message(binary(), binary()) -> binary().
unimplemented_probe_message(Provider, ProbeMsg) ->
    <<"连通性探测："/utf8, ProbeMsg/binary, "；但 ", Provider/binary, " 登录链路尚未实现，不可启用。"/utf8>>.

-spec test_oauth2(map()) -> {boolean(), binary()}.
test_oauth2(Config) ->
    AuthUrl = maps:get(<<"auth_url">>, Config, <<>>),
    TokenUrl = maps:get(<<"token_url">>, Config, <<>>),
    case {valid_http_url(AuthUrl), valid_http_url(TokenUrl)} of
        {false, _} ->
            {false, <<"OAuth2 auth_url 无效（需 http:// 或 https:// 开头）"/utf8>>};
        {_, false} ->
            {false, <<"OAuth2 token_url 无效（需 http:// 或 https:// 开头）"/utf8>>};
        {true, true} ->
            probe_http(AuthUrl)
    end.

%% ===================================================================
%% Internal: 探测原语
%% ===================================================================

%% @doc TCP 可达探测（MVP，不做 LDAP bind）
-spec probe_tcp(binary(), integer()) -> {boolean(), binary()}.
probe_tcp(Host, Port) ->
    HostL = binary_to_list(Host),
    case gen_tcp:connect(HostL, Port, [binary, {active, false}], ?TCP_TIMEOUT) of
        {ok, Sock} ->
            gen_tcp:close(Sock),
            {true, <<"TCP 连通成功（未执行 LDAP bind，MVP）"/utf8>>};
        {error, Reason} ->
            {false, iolist_to_binary([<<"TCP 连接失败："/utf8>>, reason_bin(Reason)])}
    end.

%% @doc HTTP 可达探测（MVP，不做断言/换 token）；任何 HTTP 状态码均视为端点可达
-spec probe_http(binary()) -> {boolean(), binary()}.
probe_http(Url) ->
    UrlL = binary_to_list(Url),
    Opts = [{timeout, ?HTTP_TIMEOUT}, {connect_timeout, ?HTTP_TIMEOUT}, {autoredirect, false}],
    case httpc:request(head, {UrlL, []}, Opts, []) of
        {ok, {{_, Code, _}, _, _}} ->
            {true, iolist_to_binary(io_lib:format("HTTP 可达（状态码 ~p，未执行完整认证流程，MVP）", [Code]))};
        {error, Reason} ->
            {false, iolist_to_binary([<<"HTTP 请求失败："/utf8>>, reason_bin(Reason)])}
    end.

%% ===================================================================
%% Internal: 校验小工具
%% ===================================================================

-spec is_nonempty_binary(term()) -> boolean().
is_nonempty_binary(B) when is_binary(B), byte_size(B) > 0 -> true;
is_nonempty_binary(_) -> false.

-spec valid_port(term()) -> boolean().
valid_port(P) when is_integer(P), P > 0, P =< 65535 -> true;
valid_port(_) -> false.

-spec valid_http_url(term()) -> boolean().
valid_http_url(<<"http://", Rest/binary>>) -> byte_size(Rest) > 0;
valid_http_url(<<"https://", Rest/binary>>) -> byte_size(Rest) > 0;
valid_http_url(_) -> false.

-spec reason_bin(term()) -> binary().
reason_bin(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

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

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").
-include("common.hrl").

-define(VALID_PROVIDERS, [<<"ldap">>, <<"saml">>, <<"oauth2">>]).
-define(TCP_TIMEOUT, 3000).
-define(HTTP_TIMEOUT, 5000).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 读取三 provider 配置，缺省 provider 不含该键
-spec get_config() -> {ok, map()} | {error, term()}.
get_config() ->
    sso_config_ds:get_all().

%% @doc 保存单个 provider 配置（body 即配置对象，含 provider 字段）
-spec save_config(map()) -> {ok, map()} | {error, binary()}.
save_config(ConfigMap) when is_map(ConfigMap) ->
    Provider = maps:get(<<"provider">>, ConfigMap, undefined),
    case lists:member(Provider, ?VALID_PROVIDERS) of
        true ->
            sso_config_ds:upsert(Provider, ConfigMap);
        false ->
            {error, <<"无效的 provider（应为 ldap|saml|oauth2）"/utf8>>}
    end;
save_config(_) ->
    {error, <<"参数错误"/utf8>>}.

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
            probe_tcp(Host, Port)
    end.

-spec test_saml(map()) -> {boolean(), binary()}.
test_saml(Config) ->
    Url = maps:get(<<"metadata_url">>, Config, <<>>),
    case valid_http_url(Url) of
        false ->
            {false, <<"SAML metadata_url 无效（需 http:// 或 https:// 开头）"/utf8>>};
        true ->
            probe_http(Url)
    end.

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

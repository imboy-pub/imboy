-module(sso_config_ds).

%%%
% SSO 外部认证配置数据服务模块
% SSO external authentication config data service module
%
% 职责：jsonb 编解码 + provider -> 配置对象的组装，屏蔽底层 repo 存储细节。
% 安全：bind_password / client_secret 等敏感字段本次明文存于 jsonb（GET 回填表单需要）。
%       后续可加密存储 / 脱敏返回（见报告 MVP 边界）。日志不打印配置内容，避免泄露密码。
%%%

-export([get_all/0, upsert/2]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 读取全部 provider 配置，返回 #{<<"ldap">> => Map, ...}（缺省 provider 不含该键）
-spec get_all() -> {ok, map()} | {error, term()}.
get_all() ->
    case sso_config_repo:select_all() of
        {ok, Rows} ->
            M = lists:foldl(
                fun(#{<<"provider">> := Provider, <<"config">> := Cfg}, Acc) ->
                    maps:put(Provider, decode_config(Cfg), Acc)
                end,
                #{},
                Rows
            ),
            {ok, M};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 按 provider upsert 单个配置对象（ConfigMap 含 provider/enabled/全字段）
-spec upsert(binary(), map()) -> {ok, map()} | {error, binary()}.
upsert(Provider, ConfigMap) when is_binary(Provider), is_map(ConfigMap) ->
    Enabled = to_bool(maps:get(<<"enabled">>, ConfigMap, false)),
    Json = jsone:encode(ConfigMap, [native_utf8]),
    case sso_config_repo:upsert(Provider, Enabled, Json) of
        {ok, _Rows} ->
            {ok, #{}};
        {error, Reason} ->
            ?LOG_ERROR("sso_config_ds:upsert provider=~ts error ~p", [Provider, Reason]),
            {error, <<"保存 SSO 配置失败"/utf8>>}
    end.

%% ===================================================================
%% Internal helpers
%% ===================================================================

-spec decode_config(binary() | map()) -> map().
decode_config(Cfg) when is_map(Cfg) ->
    Cfg;
decode_config(Cfg) when is_binary(Cfg) ->
    try jsone:decode(Cfg, [{object_format, map}]) of
        Map when is_map(Map) -> Map;
        _ -> #{}
    catch
        _:_ -> #{}
    end;
decode_config(_) ->
    #{}.

-spec to_bool(term()) -> boolean().
to_bool(true) -> true;
to_bool(<<"true">>) -> true;
to_bool(1) -> true;
to_bool(_) -> false.

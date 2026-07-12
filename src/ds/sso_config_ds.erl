-module(sso_config_ds).

%%%
% SSO 外部认证配置数据服务模块
% SSO external authentication config data service module
%
% 职责：jsonb 编解码 + provider -> 配置对象的组装，屏蔽底层 repo 存储细节。
% 安全：bind_password / client_secret 等敏感字段落库前 AES-256-CBC 加密
%       （enc:v1:<base64(IV)>:<base64(cipher)>，密钥 postgre_aes_key）；
%       历史明文行按前缀识别兼容读取。解密值仅供服务端内部使用
%       （get_provider/1），不经 HTTP 返回；对外展示由 sso_logic 脱敏。
%       日志不打印配置内容，避免泄露密码。
%%%

-export([get_all/0, get_provider/1, upsert/2]).
-export([secret_fields/0]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").
-include("common.hrl").

%% 敏感字段清单（落库加密 + 对外脱敏的统一依据）
-define(SECRET_FIELDS, [<<"bind_password">>, <<"client_secret">>]).
%% enc:v2: = AES-256-GCM（AEAD，当前写入格式）；enc:v1: = 旧 AES-256-CBC（仅兼容读）
-define(ENC_PREFIX_V2, "enc:v2:").
-define(ENC_PREFIX_V1, "enc:v1:").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 读取全部 provider 配置，返回 #{<<"ldap">> => Map, ...}（缺省 provider 不含该键）
%% 注意：敏感字段保持存储态（密文/历史明文），不在此处解密。
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

%% @doc 单 provider 解密后配置（仅供服务端内部使用，如 OIDC 登录流；不得经 HTTP 返回）
-spec get_provider(binary()) -> {ok, map()} | {error, term()}.
get_provider(Provider) when is_binary(Provider) ->
    case get_all() of
        {ok, M} ->
            case maps:get(Provider, M, undefined) of
                undefined ->
                    {error, not_found};
                Cfg ->
                    {ok, reveal_secrets(Cfg)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 按 provider upsert 单个配置对象（ConfigMap 含 provider/enabled/全字段）
%% 敏感字段落库前加密；入参为 <<>> 或 <<"***">>（脱敏哨兵）时保留库中已有密文不变。
-spec upsert(binary(), map()) -> {ok, map()} | {error, binary()}.
upsert(Provider, ConfigMap) when is_binary(Provider), is_map(ConfigMap) ->
    Enabled = to_bool(maps:get(<<"enabled">>, ConfigMap, false)),
    ConfigMap1 = protect_secrets(Provider, ConfigMap),
    Json = jsone:encode(ConfigMap1, [native_utf8]),
    case sso_config_repo:upsert(Provider, Enabled, Json) of
        {ok, _Rows} ->
            {ok, #{}};
        {error, Reason} ->
            ?LOG_ERROR("sso_config_ds:upsert provider=~ts error ~p", [Provider, Reason]),
            {error, <<"保存 SSO 配置失败"/utf8>>}
    end.

%% @doc 敏感字段清单（供 sso_logic 脱敏复用，避免两处清单漂移）
-spec secret_fields() -> [binary()].
secret_fields() ->
    ?SECRET_FIELDS.

%% ===================================================================
%% Internal helpers
%% ===================================================================

%% @doc 落库前处理敏感字段：哨兵值保留原密文，其余加密
-spec protect_secrets(binary(), map()) -> map().
protect_secrets(Provider, ConfigMap) ->
    Existing = stored_config(Provider),
    lists:foldl(
        fun(F, Acc) ->
            New = maps:get(F, Acc, <<>>),
            case is_sentinel(New) of
                true ->
                    %% 留空 / *** = 不修改：保留库中已存值（密文或历史明文）
                    maps:put(F, maps:get(F, Existing, <<>>), Acc);
                false ->
                    maps:put(F, encrypt_secret(New), Acc)
            end
        end,
        ConfigMap,
        ?SECRET_FIELDS
    ).

%% @doc 读取库中当前存储态配置（不解密），用于哨兵保留
-spec stored_config(binary()) -> map().
stored_config(Provider) ->
    case get_all() of
        {ok, M} ->
            maps:get(Provider, M, #{});
        _ ->
            #{}
    end.

-spec is_sentinel(term()) -> boolean().
is_sentinel(<<>>) -> true;
is_sentinel(<<"***">>) -> true;
is_sentinel(V) when is_binary(V) -> false;
is_sentinel(_) -> true.

%% @doc 加密敏感字段：enc:v2:<base64(GCM 组合)>（AEAD）；已加密值（v1/v2）幂等返回
-spec encrypt_secret(binary()) -> binary().
encrypt_secret(<<?ENC_PREFIX_V2, _/binary>> = V) ->
    V;
encrypt_secret(<<?ENC_PREFIX_V1, _/binary>> = V) ->
    %% 历史 CBC 密文不重复加密（读时兼容解密）
    V;
encrypt_secret(Plain) when is_binary(Plain) ->
    Key = config_ds:env(postgre_aes_key),
    case elib_cipher:encrypt_private_key(Plain, Key) of
        {ok, B64} ->
            <<?ENC_PREFIX_V2, B64/binary>>;
        {error, _} ->
            %% fail-closed：加密失败绝不落明文
            <<?ENC_PREFIX_V2>>
    end.

%% @doc 解密敏感字段；v2=GCM，v1=旧 CBC 兼容，无前缀视为历史明文原样返回
-spec decrypt_secret(binary()) -> binary().
decrypt_secret(<<?ENC_PREFIX_V2, B64/binary>>) ->
    Key = config_ds:env(postgre_aes_key),
    case elib_cipher:decrypt_private_key(B64, Key) of
        {ok, Plain} -> Plain;
        {error, _} -> <<>>
    end;
decrypt_secret(<<?ENC_PREFIX_V1, Rest/binary>>) ->
    case binary:split(Rest, <<":">>) of
        [IvB64, Enc] ->
            Key = config_ds:env(postgre_aes_key),
            elib_cipher:aes_decrypt(Enc, Key, base64:decode(IvB64));
        _ ->
            <<>>
    end;
decrypt_secret(V) when is_binary(V) ->
    V;
decrypt_secret(_) ->
    <<>>.

%% @doc 解密配置中的全部敏感字段
-spec reveal_secrets(map()) -> map().
reveal_secrets(Cfg) ->
    lists:foldl(
        fun(F, Acc) ->
            case maps:get(F, Acc, undefined) of
                undefined ->
                    Acc;
                V ->
                    maps:put(F, decrypt_secret(V), Acc)
            end
        end,
        Cfg,
        ?SECRET_FIELDS
    ).

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

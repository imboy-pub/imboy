-module(imboy_env).

%%%
% 环境变量覆盖模块
%
% 启动时从操作系统环境变量读取敏感配置，覆盖 sys.config 中的值。
% 这样生产环境只需设置环境变量即可，无需修改配置文件。
%
% 环境变量命名规范: IMBOY_<KEY>
%   IMBOY_JWT_KEY          -> {imboy, jwt_key}
%   IMBOY_POSTGRE_AES_KEY  -> {imboy, postgre_aes_key}
%   IMBOY_ADM_COOKIE_SECRET -> {imboy, adm_cookie_secret}
%   IMBOY_BASE_URL         -> {imboy, base_url}  (e.g. https://api.example.com)
%   IMBOY_WS_URL           -> {imboy, ws_url}    (e.g. wss://api.example.com/ws)
%   IMBOY_PG_HOST          -> pg_conf 中的 host
%   IMBOY_PG_PASSWORD      -> pg_conf 中的 password
%   IMBOY_PG_DATABASE / IMBOY_PG_DB -> pg_conf 中的 database（两者均接受）
%   IMBOY_PG_PORT          -> pg_conf 中的 port
%   IMBOY_PG_USERNAME / IMBOY_PG_USER -> pg_conf 中的 username（两者均接受）
%   IMBOY_SMTP_USERNAME    -> smtp_option 中的 username
%   IMBOY_SMTP_PASSWORD    -> smtp_option 中的 password
%   IMBOY_REDIS_PASSWORD   -> redis_options 中的 password
%   IMBOY_REDIS_HOST       -> redis_options 中的 host
%   IMBOY_REDIS_PORT       -> redis_options 中的 port
%   IMBOY_API_AUTH_SWITCH  -> {imboy, api_auth_switch}
%   IMBOY_PASSWORD_SALT    -> {imboy, password_salt}
%   IMBOY_ETURNAL_SECRET   -> {imboy, eturnal_secret}
%   IMBOY_JPUSH_APP_KEY    -> {imboy, jpush_app_key}
%   IMBOY_JPUSH_MASTER_SECRET -> {imboy, jpush_master_secret}
%   IMBOY_YJSMS_ACCOUNT    -> {imboy, yjsms_account}
%   IMBOY_YJSMS_SECRET     -> {imboy, yjsms_secret}
%   IMBOY_SOLIDIFIED_KEY   -> {imboy, solidified_key}      (32 字节)
%   IMBOY_SOLIDIFIED_KEY_IV -> {imboy, solidified_key_iv}  (16 字节)
%   IMBOY_LOGIN_RSA_PUB_KEY_FILE  -> {imboy, login_rsa_pub_key_file}
%   IMBOY_LOGIN_RSA_PRIV_KEY_FILE -> {imboy, login_rsa_priv_key_file}
%   IMBOY_JVERIFICATION_RSA_PRIV_KEY_FILE -> {imboy, jverification_rsa_priv_key_file}
%   IMBOY_GARAGE_ENDPOINT  -> garage.endpoint  (e.g. http://s3.example.com)
%   IMBOY_GARAGE_BUCKET    -> garage.bucket
%   IMBOY_GARAGE_ACCESS_KEY -> garage.access_key
%   IMBOY_GARAGE_SECRET_KEY -> garage.secret_key
%%%

-export([override_from_env/0]).
-export([current/0]).
-export([edition/0]).

-include_lib("kernel/include/logger.hrl").

%% @doc 返回当前运行环境（binary，已 normalize）。
%% 优先 OS env `IMBOYENV`（部署期覆盖），其次 application env `imboy.env`
%% （sys.config 默认值），最终空 binary（按生产环境严格对待）。
%%
%% 不做缓存：调用点都不在热路径（admin/router/passport），os:getenv 自身
%% 是 ~50ns 量级。如未来出现热点，再迁 persistent_term。
-spec current() -> binary().
current() ->
    case normalize(os:getenv("IMBOYENV")) of
        <<>> -> normalize(application:get_env(imboy, env, undefined));
        Bin -> Bin
    end.

-spec normalize(term()) -> binary().
normalize(undefined) -> <<>>;
normalize(false) -> <<>>;
normalize(B) when is_binary(B) -> B;
normalize(A) when is_atom(A) -> atom_to_binary(A, utf8);
normalize(L) when is_list(L) -> unicode:characters_to_binary(L);
normalize(_) -> <<>>.

%% @doc 从环境变量覆盖 application config 中的敏感值。
%% 在 imboy_app:start/2 中 validate_runtime_config() 之前调用。
-spec override_from_env() -> ok.
override_from_env() ->
    %% 简单键值覆盖（binary 类型）
    ok = override_binary_key("IMBOY_JWT_KEY", jwt_key),
    ok = override_binary_key("IMBOY_POSTGRE_AES_KEY", postgre_aes_key),
    ok = override_string_key("IMBOY_ADM_COOKIE_SECRET", adm_cookie_secret),

    %% URL 配置覆盖（生产环境必须通过这两个变量消除 sys.config 中的 dev URL）
    ok = override_binary_key("IMBOY_BASE_URL", base_url),
    ok = override_binary_key("IMBOY_WS_URL", ws_url),

    %% PostgreSQL 连接配置覆盖
    ok = override_pg_conf(),

    %% PostgreSQL 超级账户覆盖
    ok = override_super_account(),

    %% SMTP 配置覆盖
    ok = override_smtp(),

    %% Redis 配置覆盖
    ok = override_redis(),

    %% 百度千帆 API 配置覆盖
    ok = override_qianfan(),

    %% 新增敏感配置环境变量覆盖
    ok = override_binary_key("IMBOY_API_AUTH_SWITCH", api_auth_switch),
    ok = override_binary_key("IMBOY_PASSWORD_SALT", password_salt),
    ok = override_binary_key("IMBOY_ETURNAL_SECRET", eturnal_secret),
    ok = override_binary_key("IMBOY_JPUSH_APP_KEY", jpush_app_key),
    ok = override_binary_key("IMBOY_JPUSH_MASTER_SECRET", jpush_master_secret),
    ok = override_binary_key("IMBOY_YJSMS_ACCOUNT", yjsms_account),
    ok = override_binary_key("IMBOY_YJSMS_SECRET", yjsms_secret),

    %% solidified_key / iv 必须是固定长度二进制（32 / 16）；用 binary 接收
    ok = override_binary_key("IMBOY_SOLIDIFIED_KEY", solidified_key),
    ok = override_binary_key("IMBOY_SOLIDIFIED_KEY_IV", solidified_key_iv),

    %% RSA 密钥文件路径（string，不是 binary）
    ok = override_string_key("IMBOY_LOGIN_RSA_PUB_KEY_FILE", login_rsa_pub_key_file),
    ok = override_string_key("IMBOY_LOGIN_RSA_PRIV_KEY_FILE", login_rsa_priv_key_file),
    ok = override_string_key(
        "IMBOY_JVERIFICATION_RSA_PRIV_KEY_FILE", jverification_rsa_priv_key_file
    ),

    %% 万能验证码（仅开发/测试环境；生产环境不设置此变量）
    ok = override_binary_key("IMBOY_VERIFICATION_MASTER_CODE", verification_master_code),

    %% Garage S3 对象存储凭证
    ok = override_garage(),

    %% 版次标记（community|professional|enterprise）：仅标识 + 启动日志
    ok = override_edition(),

    ok.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc 覆盖 binary 类型的简单 app env key
-spec override_binary_key(string(), atom()) -> ok.
override_binary_key(EnvVar, AppKey) ->
    case os:getenv(EnvVar) of
        false ->
            ok;
        Value when is_list(Value), length(Value) > 0 ->
            BinVal = unicode:characters_to_binary(Value),
            application:set_env(imboy, AppKey, BinVal),
            ok;
        _ ->
            ok
    end.

%% @doc 覆盖 string (list) 类型的 app env key
-spec override_string_key(string(), atom()) -> ok.
override_string_key(EnvVar, AppKey) ->
    case os:getenv(EnvVar) of
        false ->
            ok;
        Value when is_list(Value), length(Value) > 0 ->
            application:set_env(imboy, AppKey, Value),
            ok;
        _ ->
            ok
    end.

%% @doc 覆盖 pg_conf 中的连接参数
-spec override_pg_conf() -> ok.
override_pg_conf() ->
    case application:get_env(imboy, pg_conf) of
        {ok, PgConf} when is_map(PgConf) ->
            #{start_mfa := {Mod, Fun, [ConnOpts]}} = PgConf,
            NewConnOpts = override_pg_conn_opts(ConnOpts),
            NewPgConf = PgConf#{start_mfa := {Mod, Fun, [NewConnOpts]}},
            application:set_env(imboy, pg_conf, NewPgConf),
            ok;
        _ ->
            ok
    end.

%% @doc 覆盖 super_account 中的连接参数
-spec override_super_account() -> ok.
override_super_account() ->
    case application:get_env(imboy, super_account) of
        {ok, Account} when is_map(Account) ->
            NewAccount = override_pg_conn_opts(Account),
            application:set_env(imboy, super_account, NewAccount),
            ok;
        _ ->
            ok
    end.

%% @doc 从环境变量覆盖 PG 连接选项 map
%% 支持两套命名（IMBOY_PG_USERNAME / IMBOY_PG_USER，IMBOY_PG_DATABASE / IMBOY_PG_DB）
%% 以兼容 docker-compose.prod.yml 的短名称风格
-spec override_pg_conn_opts(map()) -> map().
override_pg_conn_opts(Opts) ->
    Opts1 = maybe_override_map(Opts, host, "IMBOY_PG_HOST", fun(V) -> V end),
    Opts2 = maybe_override_map_fallback(Opts1, username, "IMBOY_PG_USERNAME", "IMBOY_PG_USER", fun(
        V
    ) ->
        V
    end),
    Opts3 = maybe_override_map(Opts2, password, "IMBOY_PG_PASSWORD", fun(V) -> V end),
    Opts4 = maybe_override_map_fallback(Opts3, database, "IMBOY_PG_DATABASE", "IMBOY_PG_DB", fun(V) ->
        V
    end),
    maybe_override_map(Opts4, port, "IMBOY_PG_PORT", fun list_to_integer/1).

%% @doc 覆盖 SMTP 配置
-spec override_smtp() -> ok.
override_smtp() ->
    case application:get_env(imboy, smtp_option) of
        {ok, SmtpOpts} when is_list(SmtpOpts) ->
            NewOpts1 = maybe_override_proplist(SmtpOpts, username, "IMBOY_SMTP_USERNAME"),
            NewOpts2 = maybe_override_proplist(NewOpts1, password, "IMBOY_SMTP_PASSWORD"),
            NewOpts3 = maybe_override_proplist(NewOpts2, relay, "IMBOY_SMTP_RELAY"),
            application:set_env(imboy, smtp_option, NewOpts3),
            ok;
        _ ->
            ok
    end.

%% @doc 覆盖 Redis 配置
-spec override_redis() -> ok.
override_redis() ->
    case application:get_env(imboy, redis_options) of
        {ok, RedisOpts} when is_list(RedisOpts) ->
            NewOpts1 = maybe_override_proplist(RedisOpts, password, "IMBOY_REDIS_PASSWORD"),
            NewOpts2 = maybe_override_proplist(NewOpts1, host, "IMBOY_REDIS_HOST"),
            NewOpts3 = maybe_override_proplist_int(NewOpts2, port, "IMBOY_REDIS_PORT"),
            application:set_env(imboy, redis_options, NewOpts3),
            ok;
        _ ->
            ok
    end.

%% @doc 覆盖 Garage S3 配置
-spec override_garage() -> ok.
override_garage() ->
    case application:get_env(imboy, garage) of
        {ok, Cfg} when is_map(Cfg) ->
            Cfg1 = maybe_override_map(Cfg, endpoint, "IMBOY_GARAGE_ENDPOINT", fun(V) ->
                unicode:characters_to_binary(V)
            end),
            Cfg2 = maybe_override_map(Cfg1, bucket, "IMBOY_GARAGE_BUCKET", fun(V) ->
                unicode:characters_to_binary(V)
            end),
            Cfg3 = maybe_override_map(Cfg2, access_key, "IMBOY_GARAGE_ACCESS_KEY", fun(V) ->
                unicode:characters_to_binary(V)
            end),
            Cfg4 = maybe_override_map(Cfg3, secret_key, "IMBOY_GARAGE_SECRET_KEY", fun(V) ->
                unicode:characters_to_binary(V)
            end),
            application:set_env(imboy, garage, Cfg4),
            ok;
        _ ->
            ok
    end.

%% @doc 覆盖百度千帆 API 配置
-spec override_qianfan() -> ok.
override_qianfan() ->
    case application:get_env(imboy, qianfan) of
        {ok, QfConf} when is_map(QfConf) ->
            Qf1 = maybe_override_map(
                QfConf,
                api_key,
                "IMBOY_QIANFAN_API_KEY",
                fun(V) -> unicode:characters_to_binary(V) end
            ),
            Qf2 = maybe_override_map(
                Qf1,
                secret_key,
                "IMBOY_QIANFAN_SECRET_KEY",
                fun(V) -> unicode:characters_to_binary(V) end
            ),
            Qf3 = maybe_override_map(
                Qf2,
                auth_access_key,
                "IMBOY_QIANFAN_AUTH_ACCESS_KEY",
                fun(V) -> unicode:characters_to_binary(V) end
            ),
            Qf4 = maybe_override_map(
                Qf3,
                auth_secret_key,
                "IMBOY_QIANFAN_AUTH_SECRET_KEY",
                fun(V) -> unicode:characters_to_binary(V) end
            ),
            application:set_env(imboy, qianfan, Qf4),
            ok;
        _ ->
            ok
    end.

%% @doc 若环境变量存在则覆盖 map 中的 key
-spec maybe_override_map(map(), atom(), string(), fun((string()) -> term())) -> map().
maybe_override_map(Map, Key, EnvVar, Transform) ->
    case os:getenv(EnvVar) of
        false ->
            Map;
        Value when is_list(Value), length(Value) > 0 ->
            Map#{Key => Transform(Value)};
        _ ->
            Map
    end.

%% @doc 若环境变量存在则覆盖 proplist 中的 key（string 类型）
-spec maybe_override_proplist(list(), atom(), string()) -> list().
maybe_override_proplist(PropList, Key, EnvVar) ->
    case os:getenv(EnvVar) of
        false ->
            PropList;
        Value when is_list(Value), length(Value) > 0 ->
            lists:keystore(Key, 1, PropList, {Key, Value});
        _ ->
            PropList
    end.

%% @doc 若环境变量存在则覆盖 proplist 中的 key（integer 类型）
-spec maybe_override_proplist_int(list(), atom(), string()) -> list().
maybe_override_proplist_int(PropList, Key, EnvVar) ->
    case os:getenv(EnvVar) of
        false ->
            PropList;
        Value when is_list(Value), length(Value) > 0 ->
            lists:keystore(Key, 1, PropList, {Key, list_to_integer(Value)});
        _ ->
            PropList
    end.

%% @doc 优先用 EnvVar1，不存在则回退 EnvVar2，都不存在则不覆盖
-spec maybe_override_map_fallback(map(), atom(), string(), string(), fun((string()) -> term())) ->
    map().
maybe_override_map_fallback(Map, Key, EnvVar1, EnvVar2, Transform) ->
    case os:getenv(EnvVar1) of
        Val when is_list(Val), length(Val) > 0 ->
            Map#{Key => Transform(Val)};
        _ ->
            maybe_override_map(Map, Key, EnvVar2, Transform)
    end.

%% @doc 读取 IMBOY_EDITION 版次标记，缺省 community；非法值回退 community 并告警。
%% 仅写入 application env 作版次标识 + 打印启动日志；不在此做任何按版次的功能
%% 开关——专业版/企业版功能属独立闭源模块，社区版不得被植入残缺收费逻辑。
-spec override_edition() -> ok.
override_edition() ->
    Edition =
        case os:getenv("IMBOY_EDITION") of
            E when is_list(E), length(E) > 0 ->
                Bin = unicode:characters_to_binary(string:lowercase(E)),
                case lists:member(Bin, [<<"community">>, <<"professional">>, <<"enterprise">>]) of
                    true ->
                        Bin;
                    false ->
                        ?LOG_WARNING("invalid IMBOY_EDITION '~ts', fallback to community", [Bin]),
                        <<"community">>
                end;
            _ ->
                <<"community">>
        end,
    application:set_env(imboy, edition, Edition),
    ?LOG_NOTICE("IMBoy edition: ~ts", [Edition]),
    ok.

%% @doc 返回当前版次（binary），缺省 community。
-spec edition() -> binary().
edition() ->
    case application:get_env(imboy, edition) of
        {ok, E} when is_binary(E) -> E;
        _ -> <<"community">>
    end.

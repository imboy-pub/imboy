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
%   IMBOY_WECHAT_MCH_ID / IMBOY_WECHAT_APP_ID / IMBOY_WECHAT_API_V3_KEY
%   IMBOY_WECHAT_CERT_SERIAL / IMBOY_WECHAT_PRIVATE_KEY / IMBOY_WECHAT_PLATFORM_PUBLIC_KEY
%   IMBOY_ALIPAY_APP_ID / IMBOY_ALIPAY_PRIVATE_KEY / IMBOY_ALIPAY_PUBLIC_KEY
%   IMBOY_STRIPE_SECRET_KEY / IMBOY_STRIPE_WEBHOOK_SECRET
%   IMBOY_PAYMENT_MODE     -> {imboy, payment_mode}  (sandbox | live)
%   IMBOY_AUTO_MIGRATE     -> {imboy, auto_migrate}  (true | false)
%   IMBOY_PRODUCT_PROFILE   -> {imboy, product_profile} (community | enterprise)
%   IMBOY_E2EE_MODE         -> {imboy, capabilities.e2ee_mode}
%                              (disabled | optional | required | compliance)
%   IMBOY_FEATURE_E2EE      -> {imboy, features.e2ee.enabled}
%   IMBOY_FEATURE_CHANNEL   -> {imboy, features.channel.enabled}
%   IMBOY_FEATURE_CHANNEL_ORDER -> {imboy, features.channel_order.enabled}
%%%

-export([override_from_env/0]).
-export([current/0]).
-export([edition/0]).

-include_lib("kernel/include/logger.hrl").

%% @doc 返回当前运行环境（binary，已 normalize）。
%% 优先 OS env `IMBOYENV`（部署期覆盖），其次 application env `imboy.env`
%% （sys.config 默认值），最终 <<"prod">>（fail-safe：未设置即视为生产）。
%%
%% 不做缓存：调用点都不在热路径（admin/router/passport），os:getenv 自身
%% 是 ~50ns 量级。如未来出现热点，再迁 persistent_term。
-spec current() -> binary().
current() ->
    case normalize(os:getenv("IMBOYENV")) of
        <<>> ->
            case normalize(application:get_env(imboy, env, undefined)) of
                <<>> -> <<"prod">>;
                Bin -> Bin
            end;
        Bin ->
            Bin
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

    %% 蓝绿部署关闭启动期迁移，切流后由显式 db migrate 统一执行。
    ok = override_boolean_key("IMBOY_AUTO_MIGRATE", auto_migrate),

    %% 产品销售策略覆盖：容器镜像不携带被忽略的 sys.pro.config，生产入口
    %% 必须通过显式环境变量决定 E2EE、频道和付费频道是否开启。
    ok = override_product_policy(),

    %% PostgreSQL 连接配置覆盖
    ok = override_pg_conf(),

    %% PostgreSQL 超级账户覆盖
    ok = override_super_account(),

    %% SMTP 配置覆盖
    ok = override_smtp(),

    %% 限流上限覆盖：本地/CI 自动化测试需要放宽 throttle rates，
    %% 避免每次调整都要重编 release（默认 120/min 会让连续巡检 429）
    ok = override_throttle_rate("IMBOY_THROTTLE_API_PER_USER", api_per_user),
    ok = override_throttle_rate("IMBOY_THROTTLE_API_PER_IP", api_per_ip),

    %% Redis 配置覆盖
    ok = override_redis(),

    %% 百度千帆 API 配置覆盖
    ok = override_qianfan(),

    %% 受信反向代理白名单（决定是否采信 x-forwarded-for）
    ok = override_trusted_proxy_ips(),

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

    %% LiveKit SFU 凭证与信令地址
    ok = override_livekit(),

    %% 支付网关凭据（微信/支付宝/Stripe）+ 运行模式
    ok = override_payment(),

    %% A-28：动态插件生命周期写操作总开关（默认关）
    ok = override_plugin_lifecycle_enabled(),

    %% 版次标记（community|professional|enterprise）：仅标识 + 启动日志
    ok = override_edition(),

    ok.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc 覆盖产品档位、E2EE 强度和销售版核心功能开关。
%% 所有非空非法值都 fail-fast，避免拼写错误静默降级成不安全策略。
-spec override_product_policy() -> ok.
override_product_policy() ->
    ok = override_product_profile(),
    ok = override_e2ee_mode(),
    ok = override_feature_switch("IMBOY_FEATURE_E2EE", e2ee),
    ok = override_feature_switch("IMBOY_FEATURE_CHANNEL", channel),
    ok = override_feature_switch("IMBOY_FEATURE_CHANNEL_ORDER", channel_order),
    ok.

-spec override_product_profile() -> ok.
override_product_profile() ->
    case os:getenv("IMBOY_PRODUCT_PROFILE") of
        false ->
            ok;
        Value when is_list(Value), length(Value) > 0 ->
            case string:trim(string:lowercase(Value)) of
                "community" -> application:set_env(imboy, product_profile, community);
                "enterprise" -> application:set_env(imboy, product_profile, enterprise);
                Other -> erlang:error({invalid_env, "IMBOY_PRODUCT_PROFILE", Other})
            end,
            ok;
        _ ->
            ok
    end.

-spec override_e2ee_mode() -> ok.
override_e2ee_mode() ->
    case os:getenv("IMBOY_E2EE_MODE") of
        false ->
            ok;
        Value when is_list(Value), length(Value) > 0 ->
            Mode =
                case string:trim(string:lowercase(Value)) of
                    "disabled" -> disabled;
                    "optional" -> optional;
                    "required" -> required;
                    "compliance" -> compliance;
                    Other -> erlang:error({invalid_env, "IMBOY_E2EE_MODE", Other})
                end,
            Current =
                case application:get_env(imboy, capabilities, #{}) of
                    M when is_map(M) -> M;
                    _ -> #{}
                end,
            application:set_env(imboy, capabilities, Current#{e2ee_mode => Mode}),
            ok;
        _ ->
            ok
    end.

-spec override_feature_switch(string(), atom()) -> ok.
override_feature_switch(EnvVar, FeatureName) ->
    case os:getenv(EnvVar) of
        false ->
            ok;
        Value when is_list(Value), length(Value) > 0 ->
            Enabled = parse_feature_boolean(EnvVar, Value),
            Current =
                case application:get_env(imboy, features, #{}) of
                    M when is_map(M) -> M;
                    _ -> #{}
                end,
            application:set_env(
                imboy,
                features,
                Current#{FeatureName => #{enabled => Enabled}}
            ),
            ok;
        _ ->
            ok
    end.

-spec override_boolean_key(string(), atom()) -> ok.
override_boolean_key(EnvVar, AppKey) ->
    case os:getenv(EnvVar) of
        false ->
            ok;
        Value when is_list(Value), length(Value) > 0 ->
            application:set_env(imboy, AppKey, parse_feature_boolean(EnvVar, Value)),
            ok;
        _ ->
            ok
    end.

-spec parse_feature_boolean(string(), string()) -> boolean().
parse_feature_boolean(EnvVar, Value) ->
    case string:trim(string:lowercase(Value)) of
        "true" -> true;
        "1" -> true;
        "false" -> false;
        "0" -> false;
        Other -> erlang:error({invalid_env, EnvVar, Other})
    end.

%% @doc 覆盖 binary 类型的简单 app env key
-spec override_binary_key(string(), atom()) -> ok.
%% @doc 覆盖 throttle rates 里的单条限流（整数 + 时间单位保持 per_minute）
override_throttle_rate(EnvVar, RateKey) ->
    case os:getenv(EnvVar) of
        false ->
            ok;
        Value when is_list(Value), length(Value) > 0 ->
            case string:to_integer(Value) of
                {Limit, _} when is_integer(Limit), Limit > 0 ->
                    Rates0 = application:get_env(throttle, rates, []),
                    Rates1 = lists:keyreplace(
                        RateKey, 1, Rates0, {RateKey, Limit, per_minute}
                    ),
                    application:set_env(throttle, rates, Rates1),
                    ok;
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

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

%% @doc 覆盖 LiveKit SFU 配置
%% Override LiveKit SFU config (ws_url / api_key / api_secret)
-spec override_livekit() -> ok.
override_livekit() ->
    case application:get_env(imboy, livekit) of
        {ok, Cfg} when is_map(Cfg) ->
            Cfg1 = maybe_override_map(Cfg, ws_url, "IMBOY_LIVEKIT_WS_URL", fun(V) ->
                unicode:characters_to_binary(V)
            end),
            Cfg2 = maybe_override_map(Cfg1, api_key, "IMBOY_LIVEKIT_API_KEY", fun(V) ->
                unicode:characters_to_binary(V)
            end),
            Cfg3 = maybe_override_map(Cfg2, api_secret, "IMBOY_LIVEKIT_API_SECRET", fun(V) ->
                unicode:characters_to_binary(V)
            end),
            application:set_env(imboy, livekit, Cfg3),
            ok;
        _ ->
            ok
    end.

%% @doc 覆盖支付网关凭据（binary 类型）+ 运行模式
%% 凭据严禁写 sys.config，一律走 IMBOY_* 环境变量注入。
%% 私钥(PEM)可含多行，shell 用引号传入即可。
-spec override_payment() -> ok.
override_payment() ->
    %% 微信支付 APIv3
    ok = override_binary_key("IMBOY_WECHAT_MCH_ID", wechat_mch_id),
    ok = override_binary_key("IMBOY_WECHAT_APP_ID", wechat_app_id),
    ok = override_binary_key("IMBOY_WECHAT_API_V3_KEY", wechat_api_v3_key),
    ok = override_binary_key("IMBOY_WECHAT_CERT_SERIAL", wechat_cert_serial),
    ok = override_binary_key("IMBOY_WECHAT_PRIVATE_KEY", wechat_private_key),
    ok = override_binary_key("IMBOY_WECHAT_PLATFORM_PUBLIC_KEY", wechat_platform_public_key),
    ok = override_binary_key("IMBOY_WECHAT_NOTIFY_URL", wechat_notify_url),
    %% 支付宝
    ok = override_binary_key("IMBOY_ALIPAY_APP_ID", alipay_app_id),
    ok = override_binary_key("IMBOY_ALIPAY_PRIVATE_KEY", alipay_private_key),
    ok = override_binary_key("IMBOY_ALIPAY_PUBLIC_KEY", alipay_public_key),
    ok = override_binary_key("IMBOY_ALIPAY_NOTIFY_URL", alipay_notify_url),
    %% 支付宝证书模式 SN（登录/支付证书加签方式；由证书 PEM 算好后注入，见 alipay_openapi:cert_sn/1）
    ok = override_binary_key("IMBOY_ALIPAY_APP_CERT_SN", alipay_app_cert_sn),
    ok = override_binary_key("IMBOY_ALIPAY_ROOT_CERT_SN", alipay_root_cert_sn),
    %% 支付宝商户 PID（2088 开头，authinfo 签名串用）
    ok = override_binary_key("IMBOY_ALIPAY_PID", alipay_pid),
    %% Stripe
    ok = override_binary_key("IMBOY_STRIPE_SECRET_KEY", stripe_secret_key),
    ok = override_binary_key("IMBOY_STRIPE_WEBHOOK_SECRET", stripe_webhook_secret),
    %% 网关运行模式：sandbox（默认）| live
    ok = override_payment_mode(),
    ok = override_payment_gateway_enabled(),
    ok.

%% @doc 覆盖受信反向代理白名单（逗号分隔，如 "127.0.0.1,10.0.0.5"）。
%%
%% elib_req:get_client_ip/1 只有在**直连对端**命中本名单时才采信
%% x-forwarded-for；该 IP 是 throttle_middleware 两个限流桶的 key。
%%
%% 默认 [127.0.0.1, ::1] 与 deploy/nginx 的 proxy_pass http://127.0.0.1:9800
%% 一致，标准单机 compose 部署无需配置。
%%
%% 什么时候必须配：后端前面还有云 LB / 额外一层 nginx / k8s ingress ——
%% 此时直连对端不是 127.0.0.1，XFF 会被全部忽略，所有客户端在限流器眼里
%% 变成同一个 IP（那一跳的出口 IP），共用一个桶 → 正常用户互相挤掉、
%% 出现莫名其妙的登录频率限制。这是 fail-closed 方向的故障，安全但影响可用性，
%% 需要把各跳出口 IP 显式列进来。
%%
%% 空值/全空白条目会被丢弃；若最终为空列表则保留原配置不覆盖，
%% 避免一个手误的空环境变量把白名单清空（那会让 XFF 永久失效）。
-spec override_trusted_proxy_ips() -> ok.
override_trusted_proxy_ips() ->
    case os:getenv("IMBOY_TRUSTED_PROXY_IPS") of
        Value when is_list(Value), length(Value) > 0 ->
            Ips = [
                list_to_binary(Trimmed)
             || Part <- string:split(Value, ",", all),
                Trimmed <- [string:trim(Part)],
                Trimmed =/= ""
            ],
            case Ips of
                [] ->
                    ok;
                _ ->
                    application:set_env(imboy, trusted_proxy_ips, Ips),
                    ok
            end;
        _ ->
            ok
    end.

%% @doc 覆盖支付网关运行模式（atom：sandbox | live）
%%
%% 只有精确的 "sandbox"（忽略大小写与首尾空白）才进 sandbox，其余一律 live。
%%
%% 此前的规则是反的：非法值回退 sandbox，注释理由写"安全默认，避免误走真实
%% 扣款"。但这句话只覆盖了一半风险 —— payment_sign:sandbox_verify/3 是
%% **完全跳过验签**，而 /api/v1/payment/callback/:gateway 免 JWT。对回调验签
%% 这一侧，sandbox 才是危险方向：`IMBOY_PAYMENT_MODE=production`、`live `
%% （尾空格）、`LIVE-` 之类的误配都会静默落到"任何人都能伪造回调入账"。
%%
%% 反过来，误配落到 live 的后果是拿不到凭据 → {error, no_credential} → 回调
%% 被拒绝：吵闹、可见、可修，且不会造成资金损失。两害相权取可见的那个。
-spec override_payment_mode() -> ok.
override_payment_mode() ->
    case os:getenv("IMBOY_PAYMENT_MODE") of
        Value when is_list(Value), length(Value) > 0 ->
            Mode =
                case string:trim(string:lowercase(Value)) of
                    "sandbox" -> sandbox;
                    _ -> live
                end,
            application:set_env(imboy, payment_mode, Mode),
            ok;
        _ ->
            ok
    end.

%% @doc 覆盖外部支付网关总开关（boolean，默认 false）
%%
%% 方向与 override_payment_mode/0 相反：这里只有精确的 "true"/"1"（忽略大小写
%% 与首尾空白）才开启，其余一律关闭。因为"关闭"是安全方向 —— 关闭时网关端点
%% 直接拒绝，误配最多是功能不可用；而误开启会让一个未配凭据的部署方在
%% strict 环境下 fail-fast，或更糟：以为自己配好了收款其实没有。
-spec override_payment_gateway_enabled() -> ok.
override_payment_gateway_enabled() ->
    case os:getenv("IMBOY_PAYMENT_GATEWAY_ENABLED") of
        Value when is_list(Value), length(Value) > 0 ->
            Enabled =
                case string:trim(string:lowercase(Value)) of
                    "true" -> true;
                    "1" -> true;
                    _ -> false
                end,
            application:set_env(imboy, payment_gateway_enabled, Enabled),
            ok;
        _ ->
            ok
    end.

%% @doc 覆盖动态插件生命周期写操作总开关（boolean，默认 false，A-28）。
%%
%% 镜像 override_payment_gateway_enabled/0：只有精确的 "true"/"1" 才开启，其余一律
%% 关闭。关闭时 adm_plugin_handler 的 7 个写端点返回 ?ERR_FEATURE_DISABLED（审计
%% #43/#44：install 的 Path 无白名单 + 签名 100% 放行，admin 可达即代码加载面）。
-spec override_plugin_lifecycle_enabled() -> ok.
override_plugin_lifecycle_enabled() ->
    case os:getenv("IMBOY_PLUGIN_LIFECYCLE_ENABLED") of
        Value when is_list(Value), length(Value) > 0 ->
            Enabled =
                case string:trim(string:lowercase(Value)) of
                    "true" -> true;
                    "1" -> true;
                    _ -> false
                end,
            application:set_env(imboy, plugin_lifecycle_enabled, Enabled),
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

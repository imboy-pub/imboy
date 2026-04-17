-module(imboy_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

% -include("log.hrl").
-include_lib("public_key/include/public_key.hrl").

%% @doc 启动 application 回调
-spec start(term(), term()) -> {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_Type, _Args) ->
    _ = inets:start(),
    ok = imboy_env:override_from_env(),
    %% prime IMBOYENV 缓存：所有运行时模块统一走 imboy_env:current/0
    _ = imboy_env:current(),
    ok = validate_runtime_config(),
    ok = ensure_solidified_keys(),
    ok = ensure_rsa_keys(),
    ok = imboy_migrate:migrate(),
    _ = imboy_syn:init(),
    % 初始化 TSID 分布式ID生成器
    TsidDcId = application:get_env(imboy, tsid_dc_id, 1),
    TsidNodeId = application:get_env(imboy, tsid_node_id, 1),
    TsidDcBits = application:get_env(imboy, tsid_dc_bits, 3),
    ok = elib_tsid:init(#{
        dc_id => TsidDcId,
        node_id => TsidNodeId,
        dc_bits => TsidDcBits,
        names => tsid_generator_names()
    }),
    % 初始化集群管理
    _ = imboy_cluster:init(),
    % 初始化验证码 ETS 表
    _ = simple_captcha_ets:init(),
    % 显式初始化 throttle 限流规则，防止 sys.config 加载时序问题导致 rate_not_set
    ok = init_throttle_rates(),
    % khepri:start(),
    % begin handler
    Routes = imboy_router:get_routes(),
    % cowboy_router:dispatch_rules()
    Dispatch = cowboy_router:compile(Routes),
    StartMode = config_ds:env(start_mode, http),
    _ = if
        StartMode == quic ->
            start_quic(Dispatch);
        true ->
            ProtoOpts = #{
                env => #{dispatch => Dispatch},
                middlewares => [
                    cowboy_router % 必须是第一个元素
                    , cors_middleware % CORS 中间件，处理跨域请求
                    , security_headers_middleware % 安全响应头中间件
                    , auth_middleware % 认证中间件
                    , throttle_middleware % 限流中间件
                    , cowboy_handler
                ],
                % metrics_callback => do_metrics_callback(),
                stream_handlers => [
                    cowboy_compress_h
                    , cowboy_stream_h
                    % , cowboy_metrics_h
                ],
                tcp_opts => [
                    % 【关键修复】禁用 Nagle 算法，消除小消息延迟
                    {nodelay, true}
                ]
            },
            Port = case os:getenv("HTTP_PORT") of
                P when is_list(P) ->
                    list_to_integer(P);
                false ->
                    config_ds:env(http_port)
            end,
            case StartMode of
                tls ->
                    start_tls(ProtoOpts, Port);
                _ ->
                    start_clear(ProtoOpts, Port)
            end
    end,
    imboy_sup:start_link().


% do_metrics_callback() ->
%    fun(Metrics) ->
%       error_logger:error_msg("@@ metrics~n~p~n", [Metrics]),
%       ok
%    end.

%% @doc 停止 application 回调
-spec stop(term()) -> ok.
stop(_State) ->
    StartMode = config_ds:env(start_mode, http),
    case StartMode of
        http_tls ->
            _ = cowboy:stop_listener(imboy_listener),
            _ = cowboy:stop_listener(imboy_listener_tls);
        tls ->
            _ = cowboy:stop_listener(imboy_listener_tls);
        _ ->
            _ = cowboy:stop_listener(imboy_listener)
    end.


%% ===================================================================
%% TSID 命名生成器配置
%% ===================================================================

%% @doc 返回需要独立号段的 TSID 命名生成器列表
%%
%% 每个名称对应一张实体表，拥有独立的 sequence 计数器。
%% 同一节点同一毫秒内，不同生成器可能产生相同数值的 ID，
%% 但因为它们属于不同的数据库表，主键不冲突。
-spec tsid_generator_names() -> [atom()].
tsid_generator_names() ->
    [
        %% ── 用户相关 ──
        user,
        user_device,
        user_collect,
        user_denylist,
        user_tag,
        user_tag_relation,
        %% ── 好友相关 ──
        friend,
        friend_category,
        %% ── 群组相关 ──
        group_info,
        group_member,
        group_notice,
        group_log,
        group_random_code,
        group_category,
        group_tag,
        group_vote,
        group_vote_option,
        group_schedule,
        group_schedule_reminder,
        group_album,
        group_album_photo,
        group_album_comment,
        group_file,
        group_task,
        group_task_assignment,
        %% ── 消息相关 (msg_id 标识用, 不做排序) ──
        msg_c2c,
        msg_c2g,
        msg_c2s,
        msg_s2c,
        msg_store,
        msg_read,
        msg_mention,
        msg_forward,
        msg_reply,
        msg_reaction,
        msg_pinned,
        %% ── 会话相关 ──
        conversation_pin,
        conversation_delete,
        %% ── 附件 / 媒体 ──
        attachment,
        %% ── 朋友圈 ──
        moment_post,
        moment_comment,
        moment_like,
        moment_timeline,
        moment_post_acl,
        moment_report,
        %% ── 频道 ──
        channel,
        channel_message,
        channel_subscription,
        channel_admin,
        channel_message_view,
        channel_order,
        channel_invitation,
        %% ── 反馈 ──
        feedback,
        feedback_reply,
        %% ── E2EE 恢复 ──
        e2ee_transfer,
        e2ee_social,
        e2ee_local_backup,
        e2ee_shard_transmission_log,
        %% ── 举报 ──
        report_ticket,
        report_action_log,
        %% ── 钱包 ──
        wallet,
        wallet_transaction,
        %% ── 直播 ──
        live_room,
        %% ── 推送 ──
        push_token,
        %% ── 公告 ──
        announcement,
        %% ── 合规 ──
        compliance_key,
        %% ── 管理员 ──
        adm_user,
        %% ── 应用配置 ──
        app_version,
        app_ddl,
        app_upgrade_log,
        app_version_policy,
        %% ── 全文搜索 ──
        fts_user
    ].

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
-spec start_quic(cowboy_router:dispatch_rules()) -> {ok, pid()} | {error, any()}.
start_quic(_Dispatch) ->
    {error, <<"调整中的功能"/utf8>>}.
    % PrivDir = code:priv_dir(imboy),
    % cowboy:start_quic(#{socket_opts => [
    %                                     % {cert, "deps/quicer/test/quicer_SUITE_data/cert.pem"},
    %                                     % {key, "deps/quicer/test/quicer_SUITE_data/key.pem"}
    %                                     {cert, PrivDir ++ config_ds:env(certfile)},
    %                                     {key, PrivDir ++ config_ds:env(keyfile)}]},
    %                   #{env => #{dispatch => Dispatch}}).


-spec start_tls(map(), integer()) -> {ok, pid()} | {error, any()}.
start_tls(ProtoOpts, Port) ->
    PrivDir = code:priv_dir(imboy),
    cowboy:start_tls(imboy_listener_tls,
                     [{port, Port},
                      {cacertfile, PrivDir ++ config_ds:env(cacertfile)},
                      {certfile, PrivDir ++ config_ds:env(certfile)},
                      {keyfile, PrivDir ++ config_ds:env(keyfile)}],
                     ProtoOpts).


-spec start_clear(map(), integer()) -> {ok, pid()} | {error, any()}.
start_clear(ProtoOpts, Port) ->
    cowboy:start_clear(imboy_listener,
                       [{port, Port}],
                       ProtoOpts).

%% @doc 显式初始化 API throttle 限流规则
%% throttle 库通过 sys.config 的 {rates, [...]} 自动 setup，但在某些启动时序下
%% 可能在 Cowboy 中间件就绪前未完成初始化，导致 throttle:check/2 返回 rate_not_set。
%% 此处显式 setup 两个关键规则作为保障。
-spec init_throttle_rates() -> ok.
init_throttle_rates() ->
    %% 每用户每分钟 API 调用上限（与 sys.config 保持一致）
    ok = throttle:setup(api_per_user, 120, per_minute),
    %% 每 IP 每分钟 API 调用上限（与 sys.config 保持一致）
    ok = throttle:setup(api_per_ip, 60, per_minute),
    ok.

-spec validate_runtime_config() -> ok.
validate_runtime_config() ->
    case is_strict_env(runtime_env()) of
        true ->
            %% 生产环境必须配置的敏感项
            ok = ensure_required_secret(jwt_key),
            ok = ensure_required_secret(postgre_aes_key),
            ok = ensure_required_secret(adm_cookie_secret),
            ok = ensure_required_secret(solidified_key),
            ok = ensure_required_secret(solidified_key_iv),
            ok = ensure_required_secret(password_salt),
            ok = ensure_required_file(login_rsa_pub_key_file),
            ok = ensure_required_file(login_rsa_priv_key_file),
            %% API 签名验证开关在生产环境必须显式开启
            ok = ensure_api_auth_switch_on(),
            %% 若配置了 TURN 服务器，则 eturnal_secret 不可为空
            ok = ensure_eturnal_secret_if_turn_configured(),
            %% 若启用了推送通知，则极光凭据不可为空
            ok = ensure_jpush_if_push_enabled(),
            %% 若启用了短信，则平台凭据不可为空
            ok = ensure_sms_if_enabled(),
            %% 验证数据库密码不是默认值（pg_conf + super_account 同步检查）
            ok = ensure_pg_password_not_default(),
            ok = ensure_super_account_password_not_default(),
            ok;
        false ->
            ok
    end.

%% @doc 若开启了 push.enabled = true，则 jpush_app_key/jpush_master_secret 必须配置
-spec ensure_jpush_if_push_enabled() -> ok.
ensure_jpush_if_push_enabled() ->
    case config_ds:env([push, enabled], false) of
        true ->
            ok = ensure_required_secret(jpush_app_key),
            ok = ensure_required_secret(jpush_master_secret),
            ok;
        _ ->
            ok
    end.

%% @doc 若开启了 sms.switch = <<"on">>，则平台凭据必须配置
-spec ensure_sms_if_enabled() -> ok.
ensure_sms_if_enabled() ->
    case config_ds:env([sms, switch], <<"off">>) of
        <<"on">> ->
            Platform = config_ds:env([sms, platform], <<>>),
            ensure_sms_platform_credentials(Platform);
        _ ->
            ok
    end.

ensure_sms_platform_credentials(<<"yjsms">>) ->
    ok = ensure_required_secret(yjsms_account),
    ok = ensure_required_secret(yjsms_secret);
ensure_sms_platform_credentials(<<"aliyun">>) ->
    AliyunCfg = config_ds:env([sms, <<"aliyun">>], []),
    KeyId  = proplists:get_value(key_id, AliyunCfg, <<>>),
    KeySec = proplists:get_value(key_secret, AliyunCfg, <<>>),
    case {normalize_secret(KeyId), normalize_secret(KeySec)} of
        {<<>>, _} -> erlang:error({missing_required_config, {sms, aliyun, key_id}});
        {_, <<>>} -> erlang:error({missing_required_config, {sms, aliyun, key_secret}});
        _         -> ok
    end;
ensure_sms_platform_credentials(_) ->
    %% 未知平台或留空：sms.switch=on 的前提下视为配置错误
    erlang:error({missing_required_config, {sms, platform}}).

%% @doc 同步校验 super_account 密码不是常见弱密码
-spec ensure_super_account_password_not_default() -> ok.
ensure_super_account_password_not_default() ->
    case application:get_env(imboy, super_account) of
        {ok, #{password := Pwd}} ->
            BlockedPasswords = ["123456", "password", "abc54321", ""],
            case lists:member(Pwd, BlockedPasswords) of
                true ->
                    erlang:error({insecure_super_account_password,
                        "Production super_account password is too weak or is a known default"});
                false ->
                    ok
            end;
        _ ->
            ok
    end.

%% @doc 生产环境要求 api_auth_switch 显式设置为 <<"on">>
-spec ensure_api_auth_switch_on() -> ok.
ensure_api_auth_switch_on() ->
    case config_ds:env(api_auth_switch, <<"off">>) of
        <<"on">> ->
            ok;
        _ ->
            erlang:error({insecure_config,
                "Production requires api_auth_switch = <<\"on\">>. "
                "Set {api_auth_switch, <<\"on\">>} in sys.config or "
                "export IMBOY_API_AUTH_SWITCH=on"})
    end.

%% @doc 若配置了 TURN 服务器则要求 eturnal_secret 非空
-spec ensure_eturnal_secret_if_turn_configured() -> ok.
ensure_eturnal_secret_if_turn_configured() ->
    case config_ds:env(eturnal_turn_urls, []) of
        [] ->
            ok;
        [_|_] ->
            case normalize_secret(config_ds:env(eturnal_secret, <<>>)) of
                <<>> ->
                    erlang:error({missing_required_config,
                        "eturnal_turn_urls is configured but eturnal_secret is empty. "
                        "Set {eturnal_secret, <<\"your-turn-secret\">>} in sys.config or "
                        "export IMBOY_ETURNAL_SECRET=<secret>"});
                _ ->
                    ok
            end
    end.

%% @doc 确保 solidified_key / solidified_key_iv 已就绪
%% 生产环境：validate_runtime_config 已 fail-fast，此处无需处理
%% 非生产环境：未配置则自动生成随机值写入 application env（重启后失效，仅供开发/测试）
-spec ensure_solidified_keys() -> ok.
ensure_solidified_keys() ->
    case normalize_secret(config_ds:env(solidified_key, <<>>)) of
        <<>> ->
            Key = crypto:strong_rand_bytes(32),
            IV  = crypto:strong_rand_bytes(16),
            ok = application:set_env(imboy, solidified_key, Key),
            ok = application:set_env(imboy, solidified_key_iv, IV),
            logger:warning("[imboy] solidified_key not configured — "
                           "auto-generated random key/iv for this session. "
                           "HMAC signatures (email binding, group invite) "
                           "will be invalid after restart. "
                           "Set solidified_key / solidified_key_iv in sys.config for production."),
            ok;
        _ ->
            %% key 已配置；若 iv 单独缺失也补全
            case normalize_secret(config_ds:env(solidified_key_iv, <<>>)) of
                <<>> ->
                    IV = crypto:strong_rand_bytes(16),
                    ok = application:set_env(imboy, solidified_key_iv, IV),
                    logger:warning("[imboy] solidified_key_iv not configured — auto-generated."),
                    ok;
                _ ->
                    ok
            end
    end.

%% @doc 确保生产环境数据库密码不是常见弱密码
-spec ensure_pg_password_not_default() -> ok.
ensure_pg_password_not_default() ->
    case application:get_env(imboy, pg_conf) of
        {ok, #{start_mfa := {_, _, [#{password := Pwd}]}}} ->
            BlockedPasswords = ["123456", "password", "abc54321", ""],
            case lists:member(Pwd, BlockedPasswords) of
                true ->
                    erlang:error({insecure_pg_password,
                        "Production database password is too weak or is a known default"});
                false ->
                    ok
            end;
        _ ->
            ok
    end.

%% @doc 确保 RSA 公私钥已就绪（PEM 二进制写入 application env）
%% 生产环境：配置文件路径，启动时读取；validate_runtime_config 已 fail-fast
%% 非生产环境：未配置则自动生成临时 RSA-2048 密钥对（重启后失效，仅供开发/测试）
-spec ensure_rsa_keys() -> ok.
ensure_rsa_keys() ->
    PubFile  = normalize_secret(config_ds:env(login_rsa_pub_key_file, "")),
    PrivFile = normalize_secret(config_ds:env(login_rsa_priv_key_file, "")),
    case {PubFile, PrivFile} of
        {<<>>, _} ->
            %% 未配置文件路径 → 自动生成临时密钥对
            {PubPem, PrivPem} = generate_rsa_keypair(),
            ok = application:set_env(imboy, login_rsa_pub_key,  PubPem),
            ok = application:set_env(imboy, login_rsa_priv_key, PrivPem),
            %% jverification 共用 login 私钥（开发环境）
            ok = application:set_env(imboy, jverification_rsa_priv_key, PrivPem),
            logger:warning("[imboy] login_rsa_pub_key_file not configured — "
                           "auto-generated ephemeral RSA-2048 keypair. "
                           "Set login_rsa_pub_key_file / login_rsa_priv_key_file "
                           "in sys.config for production.");
        {PubF, PrivF} ->
            {ok, PubPem}  = file:read_file(binary_to_list(PubF)),
            {ok, PrivPem} = file:read_file(binary_to_list(PrivF)),
            ok = application:set_env(imboy, login_rsa_pub_key,  PubPem),
            ok = application:set_env(imboy, login_rsa_priv_key, PrivPem),
            %% jverification 独立私钥（可选，未配置则复用 login 私钥）
            JvFile = normalize_secret(config_ds:env(jverification_rsa_priv_key_file, "")),
            JvPem = case JvFile of
                <<>> -> PrivPem;
                JvF  ->
                    {ok, P} = file:read_file(binary_to_list(JvF)),
                    P
            end,
            ok = application:set_env(imboy, jverification_rsa_priv_key, JvPem)
    end,
    ok.

%% @doc 生成临时 RSA-2048 密钥对，返回 {PubPem, PrivPem}
-spec generate_rsa_keypair() -> {binary(), binary()}.
generate_rsa_keypair() ->
    PrivKey = public_key:generate_key({rsa, 2048, 65537}),
    #'RSAPrivateKey'{modulus = Mod, publicExponent = Exp} = PrivKey,
    PubKey  = #'RSAPublicKey'{modulus = Mod, publicExponent = Exp},
    PrivPem = public_key:pem_encode([public_key:pem_entry_encode('RSAPrivateKey', PrivKey)]),
    PubPem  = public_key:pem_encode([public_key:pem_entry_encode('RSAPublicKey', PubKey)]),
    {PubPem, PrivPem}.

%% @doc 生产环境：确保指定的文件路径配置项非空
-spec ensure_required_file(atom()) -> ok.
ensure_required_file(Key) ->
    case normalize_secret(config_ds:env(Key, "")) of
        <<>> ->
            erlang:error({missing_required_config, Key});
        _ ->
            ok
    end.

-spec ensure_required_secret(atom()) -> ok.
ensure_required_secret(Key) ->
    case normalize_secret(config_ds:env(Key, <<>>)) of
        <<>> ->
            erlang:error({missing_required_config, Key});
        _ ->
            ok
    end.

-spec runtime_env() -> binary().
runtime_env() ->
    %% 薄封装到 imboy_env:current/0，保留旧名以避免外部调用方破坏
    imboy_env:current().

-spec is_strict_env(binary()) -> boolean().
is_strict_env(Env) ->
    lists:member(Env, [<<"pro">>, <<"prod">>, <<"production">>]).

-spec normalize_secret(term()) -> binary().
normalize_secret(undefined) ->
    <<>>;
normalize_secret(false) ->
    <<>>;
normalize_secret(Value) when is_binary(Value) ->
    Value;
normalize_secret(Value) when is_list(Value) ->
    unicode:characters_to_binary(Value);
normalize_secret(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
normalize_secret(Value) ->
    ec_cnv:to_binary(Value).

%%% @doc INT-BE-02 31-operation 真实 HTTP conformance 套件的 test-only
%%% harness（不进任何 release）。
%%%
%%% 三件事（复用仓内既有配方，无新发明）：
%%%
%%%   1. **disposable PG**：`inttest_marker_db:provision`（env 前缀
%%%      INTBE02_INTTEST；与 EPGZ-02/04/05、EOV21A2 同款）建一次性 marker
%%%      库并全链迁移；同一 PG 服务器上与并行 worker 的共享库互不可见。
%%%   2. **pooler `pgsql` 池**指向 marker 库（agent_preflight_facts_pg_tests
%%%      同款）：被测 handler 内 `elib_pg:query/with_tx`（池化路径）经此落
%%%      到 marker 库——认证链、业务、幂等行全部真库真事务，不 mock 认证
%%%      结论、不 mock elib_pg。
%%%   3. **真 Cowboy listener**：`imboy_router:get_routes()` 全量 dispatch +
%%%      与 imboy_app 同序的中间件链（enterprise_internal_wiring_http_tests
%%%      同款 ?MIDDLEWARES），HTTP 层零 mock；仅**外部替身**走 meck：
%%%      `inet:getaddrs`（INT-12 SSRF 守卫的 DNS 解析，
%%%      enterprise_msg_asset_webhook_pg_tests 同款 with_public_dns）与
%%%      V3-14 S3-only 直传的 S3 服务端替身（合成 endpoint/bucket，meck
%%%      httpc passthrough 分流 + garage driver head/read 供数；INT-07/08
%%%      presign 签发 s3_put descriptor、PUT 直打存储、confirm 真实读流
%%%      均由替身承载，无受控网关正文路径）。
%%%
%%% 合成租户（995 段 ID，与 987/988/989/991/992/993 段互不冲突）在 setup
%%% 阶段 COMMIT 提交；HTTP 用例各自走 handler 自己的事务（真提交），marker
%%% 库随 release 整库 DROP，不触碰共享库。
-module(intbe02_http_support).

-export([
    setup_all/0,
    teardown_all/1,
    http/3,
    http/4,
    http/5,
    signed_headers/6,
    auth/1,
    idem/1,
    json_header_val/2,
    cover/2,
    covered_ids/0,
    sql_exec/2,
    sql_exec/3,
    one/2,
    one/3,
    upload_file/3,
    direct_group_file/4,
    enable_group_direct/0,
    start_direct_edge/1,
    stop_direct_edge/1,
    follow_direct_edge/1,
    s3_stub_object/1,
    declare_real_garage/1,
    issue_credential/4,
    with_public_dns/1,
    fixture/2
]).

%% ---- 夹具常量（995 段；见模块头） ----

-define(OWNER_A, 995001).
-define(OWNER_B, 995002).
-define(H_A1, 995011).
-define(H_A2, 995012).
-define(H_A3, 995013).
-define(PRIN_A, 995014).
-define(FR_A, 995015).
-define(FR_B, 995016).
-define(ALICE, 995017).
-define(H_B1, 995021).

-define(ORG_A, 995101).
-define(ORG_B, 995102).

-define(WS_A1, 995201).
-define(WS_A2, 995202).
-define(WS_B1, 995211).

-define(GRP_HUMAN, 995301).
-define(GRP_HUMAN_MEMBER, 995601).
-define(GRP_B, 995311).

-define(PRJ_A1, 995401).
-define(CHN_A1, 995501).
-define(CHN_A2, 995502).

-define(APP_KEY_A, <<"intbe02-oa-a">>).
-define(APP_KEY_C, <<"intbe02-oa-c">>).
-define(APP_KEY_W, <<"intbe02-oa-w">>).
-define(APP_KEY_D, <<"intbe02-oa-d">>).
-define(APP_KEY_RO, <<"intbe02-oa-ro">>).
-define(APP_KEY_SSO, <<"intbe02-oa-sso">>).

-define(SECRET_A, <<"intbe02_secret_a_0123456789abcdefghij">>).
-define(SECRET_C, <<"intbe02_secret_c_0123456789abcdefghij">>).
-define(SECRET_W, <<"intbe02_secret_w_0123456789abcdefghij">>).
-define(SECRET_D, <<"intbe02_secret_d_0123456789abcdefghij">>).
-define(SECRET_RO, <<"intbe02_secret_ro_0123456789abcdef">>).
-define(SECRET_SSO, <<"intbe02_secret_sso_0123456789abcdef">>).

%% 窄写应用（App W）：认证与凭证均有效，供「未覆盖 Workspace 的写操作被
%% 资源归属边界拒绝」负例使用（403 归因到边界）。

%% App A：主应用；主链全部正例/幂等用例都用它。

-define(EXT_H1, <<"intbe02-ext-h1">>).
-define(EXT_H2, <<"intbe02-ext-h2">>).
-define(EXT_H3, <<"intbe02-ext-h3">>).
-define(EXT_FR_A, <<"intbe02-ext-fr-a">>).
-define(EXT_FR_B, <<"intbe02-ext-fr-b">>).
-define(EXT_ALICE, <<"intbe02-ext-alice">>).

-define(REDIRECT, <<"https://oa.customer.example.com/sso/cb">>).
-define(NONCE, <<"nonce_intbe02_0123456789abcdef">>).

-define(PUBLIC_IP, {93, 184, 216, 34}).

-define(LISTENER, intbe02_http_listener).
-define(COVERED_TAB, intbe02_covered_ops).
-define(UPLOAD_ORIGIN, <<"http://intbe02.invalid">>).
%% V3-14 S3-only 直传替身：合成 storage endpoint/bucket（DNS 不可达；仅
%% meck httpc S3 替身截获）。对象本体存 persistent_term，PUT/HEAD/GET 全
%% 部离线。
-define(S3_STUB_ENDPOINT, <<"http://intbe02-s3.invalid">>).
-define(S3_STUB_BUCKET, <<"intbe02-synthetic">>).
-define(S3_OBJ_NS, {intbe02_http_support, s3_object}).
%% 真 Garage 模式声明（persistent_term；application:load(imboy) 会以
%% -config 值覆盖此前 set_env，预置 env 不可靠——真链调用方必须在
%% setup_all 之前显式声明本测试拥有的 garage env）。
-define(REAL_GARAGE_KEY, {intbe02_http_support, real_garage}).

-define(MIDDLEWARES, [
    cowboy_router,
    cors_middleware,
    security_headers_middleware,
    auth_middleware,
    feature_gate_middleware,
    throttle_middleware,
    cowboy_handler
]).

%%%===================================================================
%%% Setup / Teardown
%%%===================================================================

-spec setup_all() -> map().
setup_all() ->
    Storage = setup_storage(),
    try
        State = setup_with_limiter(),
        State#{storage_fixture => Storage}
    catch
        Class:Reason:Stack ->
            raise_after_cleanup(
                Class,
                Reason,
                Stack,
                [fun() -> teardown_storage(Storage) end]
            )
    end.

setup_with_limiter() ->
    LimiterOwner = ensure_limiter_owner(),
    try
        setup_all(LimiterOwner)
    catch
        Class:Reason:Stack ->
            raise_after_cleanup(
                Class,
                Reason,
                Stack,
                [fun() -> stop_limiter_owner(LimiterOwner) end]
            )
    end.

setup_all(LimiterOwner) ->
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    %% throttle rates 必须在 ensure_all_started 之前注入（throttle_app:start 读 env 初始化桶）。
    %% 夹具卫生（rv7 全量实证）：注入前保存、teardown 复原——共享 VM 内
    %% 不向后续根泄漏覆写后的 rates（app boot 启动校验按 -config 真值判
    %% 旧/新混跑，残留注入会连环拒启）。
    PreviousRates = application:get_env(throttle, rates),
    application:set_env(throttle, rates, [
        {api_per_user, 100000, per_minute},
        {api_per_user_burst, 100000, per_second}
    ]),
    {ok, _} = application:ensure_all_started(throttle),
    catch throttle:setup(api_per_user, 100000, per_minute),
    catch throttle:setup(api_per_user_burst, 100000, per_second),
    %% WH-05：本链不起 imboy app，config_ds:set/get 依赖的 imboy_cache
    %% 实例须显式拉起（详见 eunit_runner:ensure_cache/0 头注）。
    ok = eunit_runner:ensure_cache(),
    application:set_env(imboy, enterprise_internal_rate_limits, #{
        internal_read => 10000, internal_write => 10000, internal_sso => 10000
    }),
    application:set_env(
        imboy, enterprise_internal_cursor_signing_key, <<"intbe02_cursor_key_0123456789abcdef">>
    ),
    State = inttest_marker_db:provision(#{
        env_prefix => <<"INTBE02_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    setup_stages(
        State#{
            limiter_owner => LimiterOwner,
            previous_throttle_rates => PreviousRates
        },
        [
            fun snapshot_default_pool/1,
            fun suspend_default_pool/1,
            fun(S) ->
                Pid = ensure_pool(S),
                S#{owned_pool_pid => Pid}
            end,
            fun setup_seed/1,
            fun(S) ->
                %% Never adopt or clear a table from a different fixture.
                undefined = ets:whereis(?COVERED_TAB),
                ?COVERED_TAB = ets:new(?COVERED_TAB, [named_table, public, set]),
                S#{owned_covered_tid => ets:whereis(?COVERED_TAB)}
            end,
            fun(S) ->
                {ok, _} = application:ensure_all_started(cowboy),
                {ok, _} = application:ensure_all_started(ranch),
                Dispatch = cowboy_router:compile(imboy_router:get_routes()),
                %% already_started is a refusal, never permission to adopt.
                {ok, Pid} = cowboy:start_clear(
                    ?LISTENER,
                    [{ip, {127, 0, 0, 1}}, {port, 0}],
                    #{env => #{dispatch => Dispatch}, middlewares => ?MIDDLEWARES}
                ),
                S#{owned_listener_pid => Pid}
            end,
            fun setup_result/1
        ]
    ).

setup_stages(State, []) ->
    State;
setup_stages(State, [Step | Rest]) ->
    %% Recurse outside the try: a later failure must not double-release earlier states.
    Result = cleanup_capture(fun() -> Step(State) end),
    case Result of
        {ok, Next} ->
            setup_stages(Next, Rest);
        {error, Class, Reason, Stack} ->
            raise_after_cleanup(Class, Reason, Stack, [
                fun() -> stop_owned_listener(State) end,
                fun() -> delete_owned_covered(State) end,
                fun() -> release_owned_pool(State) end,
                fun() -> inttest_marker_db:release(State) end
            ])
    end.

setup_seed(State) ->
    C = maps:get(conn, State),
    try
        ok = sql_exec(C, <<"BEGIN">>),
        Creds = seed_matrix(C),
        ok = sql_exec(C, <<"COMMIT">>),
        State#{fixture_credentials => Creds}
    catch
        Class:Reason:Stack ->
            raise_after_cleanup(
                Class,
                Reason,
                Stack,
                [fun() -> sql_exec(C, <<"ROLLBACK">>) end]
            )
    end.

setup_result(State) ->
    C = maps:get(conn, State),
    Creds = maps:get(fixture_credentials, State),
    {{127, 0, 0, 1}, Port} = ranch:get_addr(?LISTENER),
    maps:remove(fixture_credentials, State#{
        port => Port,
        app_a => app_id(C, ?APP_KEY_A),
        app_c => app_id(C, ?APP_KEY_C),
        app_w => app_id(C, ?APP_KEY_W),
        app_d => app_id(C, ?APP_KEY_D),
        app_ro => app_id(C, ?APP_KEY_RO),
        app_sso => app_id(C, ?APP_KEY_SSO),
        cred_a => maps:get(cred_a, Creds),
        cred_c => maps:get(cred_c, Creds),
        cred_w => maps:get(cred_w, Creds),
        cred_d => maps:get(cred_d, Creds),
        cred_ro => maps:get(cred_ro, Creds),
        cred_sso => maps:get(cred_sso, Creds)
    }).

cleanup_capture(Fun) ->
    try
        {ok, Fun()}
    catch
        Class:Reason:Stack -> {error, Class, Reason, Stack}
    end.

raise_after_cleanup(Class, Reason, Stack, Operations) ->
    Errors = [E || {error, _, _, _} = E <- [cleanup_capture(F) || F <- Operations]],
    case Errors of
        [] -> erlang:raise(Class, Reason, Stack);
        _ -> erlang:raise(Class, {setup_cleanup_failed, Reason, Errors}, Stack)
    end.

stop_owned_listener(State) ->
    case maps:find(owned_listener_pid, State) of
        error ->
            ok;
        {ok, Pid} ->
            case is_process_alive(Pid) of
                false ->
                    ok;
                true ->
                    #{pid := Pid} = ranch:info(?LISTENER),
                    ok = cowboy:stop_listener(?LISTENER)
            end
    end.

delete_owned_covered(State) ->
    case maps:find(owned_covered_tid, State) of
        error ->
            ok;
        {ok, Tid} ->
            case ets:info(Tid, owner) of
                undefined ->
                    ok;
                Owner when Owner =:= self() ->
                    true = ets:delete(Tid),
                    ok;
                _ ->
                    error(foreign_covered_table_owner)
            end
    end.

snapshot_default_pool(State) ->
    %% Even an absent pool must restore the original configured EUnit default.
    true = os:getenv("IMBOYENV") =:= "test",
    {ok, Conf} = application:get_env(imboy, pg_conf),
    #{name := pgsql, start_mfa := {epgsql, connect, [Opts]}} = Conf,
    true = is_map(Opts),
    case whereis(pgsql) of
        undefined ->
            State#{prior_default_pool => #{absent => true, config => Conf}};
        Pid ->
            Dump = gen_server:call(Pid, dump_pool),
            %% mismatch 必须可诊断：裸 badmatch,false 无法区分第三形态来源
            %% （start_mfa 二次包装 / 异代 init_count / 异库连接参数）。
            case pool_shape_matches(Conf, Dump) of
                true ->
                    ok;
                false ->
                    erlang:error(
                        {pool_shape_mismatch, #{
                            conf => Conf,
                            dump_filtered => maps:with(maps:keys(Conf), Dump),
                            dump_extra_keys => maps:keys(maps:without(maps:keys(Conf), Dump)),
                            pool_pid => Pid
                        }}
                    )
            end,
            Pid = whereis(pgsql),
            Sup = whereis(pooler_pgsql_pool_sup),
            true = lists:member({pooler, Pid, worker, [pooler]}, supervisor:which_children(Sup)),
            State#{prior_default_pool => #{pid => Pid, supervisor => Sup, config => Conf}}
    end.

%% 默认 pgsql 池存在两种合法创建形态（全量并发形态修复，见 full-
%% 20261030T175015Z 七根 snapshot_default_pool L318 {badmatch,false} 与
%% group-20261030T195423Z 复现）：
%%   A. imboy_sup:init（imboy boot）→ configure_pg_pool/1 把 start_mfa 从
%%      {epgsql, connect, Args} 换成 {imboy_pg_connection, connect, Args}
%%      （epgsql:connect 同参包装 + statement_timeout，见
%%      src/lib/imboy_pg_connection.erl），但不回写 application env；
%%   B. eunit_runner:ensure_default_pool/0 / 本模块 restore_default_pool/1
%%      → 按当前 env 原样建池（epgsql 形态）。
%% 全量形态下 boot 与重建的先后不可控，快照/挂起/恢复的比对必须接受两种
%% 形态；除 start_mfa 包装模块外的一切键（name/init_count/max_count/连接
%% 参数）仍然逐字段强校验，配置漂移检测不被削弱。
pool_shape_matches(Conf, Dump) ->
    Filtered = maps:with(maps:keys(Conf), Dump),
    Filtered =:= Conf orelse Filtered =:= sup_shape(Conf).

sup_shape(#{start_mfa := {epgsql, connect, Args}} = Conf) ->
    Conf#{start_mfa := {imboy_pg_connection, connect, Args}};
sup_shape(Conf) ->
    Conf.

suspend_default_pool(State) ->
    case maps:get(prior_default_pool, State) of
        #{absent := true} ->
            undefined = whereis(pgsql);
        #{pid := Pid, supervisor := Sup, config := Conf} ->
            Pid = whereis(pgsql),
            Sup = whereis(pooler_pgsql_pool_sup),
            true = pool_shape_matches(Conf, gen_server:call(Pid, dump_pool)),
            ok = pooler:rm_pool(pgsql)
    end,
    State.

release_owned_pool(State) ->
    %% Restoration must still be attempted if stopping our fixture pool fails.
    Results = [
        cleanup_capture(F)
     || F <- [
            fun() -> stop_fixture_pool(State) end,
            fun() -> restore_default_pool(State) end
        ]
    ],
    case [E || {error, _, _, _} = E <- Results] of
        [] -> ok;
        Errors -> error({intbe02_pool_release_failed, Errors})
    end.

stop_fixture_pool(State) ->
    case maps:find(owned_pool_pid, State) of
        error ->
            ok;
        {ok, {Pid, Sup}} ->
            case is_process_alive(Sup) of
                false ->
                    ok;
                true ->
                    Sup = whereis(pooler_pgsql_pool_sup),
                    Pid = whereis(pgsql),
                    true = lists:member(
                        {pooler, Pid, worker, [pooler]}, supervisor:which_children(Sup)
                    ),
                    ok = pooler:rm_pool(pgsql)
            end
    end.

restore_default_pool(State) ->
    case maps:find(prior_default_pool, State) of
        %% No ownership proof was obtained: never touch the pool.
        error ->
            ok;
        {ok, #{absent := true, config := Conf}} ->
            undefined = whereis(pgsql),
            _ = create_checked_pool(Conf),
            ok;
        {ok, #{pid := PriorPid, supervisor := PriorSup, config := Conf}} ->
            case whereis(pgsql) of
                undefined ->
                    _ = create_checked_pool(Conf),
                    ok;
                PriorPid ->
                    PriorSup = whereis(pooler_pgsql_pool_sup),
                    true = pool_shape_matches(
                        Conf,
                        gen_server:call(PriorPid, dump_pool)
                    ),
                    ok;
                _ ->
                    error(foreign_pool_blocks_default_restore)
            end
    end.

-spec teardown_all(map()) -> ok.
teardown_all(State) ->
    Results = [
        cleanup_capture(F)
     || F <- [
            fun() -> teardown_existing(State) end,
            fun() -> teardown_storage(maps:get(storage_fixture, State)) end
        ]
    ],
    case [E || {error, _, _, _} = E <- Results] of
        [] -> ok;
        Errors -> error({intbe02_full_teardown_failed, Errors})
    end.

teardown_existing(State) ->
    Results = [
        cleanup_capture(F)
     || F <- [
            fun() -> stop_owned_listener(State) end,
            fun() -> delete_owned_covered(State) end,
            fun() -> stop_limiter_owner(maps:get(limiter_owner, State, undefined)) end,
            fun() -> application:unset_env(imboy, enterprise_internal_cursor_signing_key) end,
            fun() -> application:unset_env(imboy, enterprise_internal_rate_limits) end,
            fun() ->
                case maps:get(previous_throttle_rates, State, missing) of
                    missing -> ok;
                    {ok, PreviousRates} -> application:set_env(throttle, rates, PreviousRates);
                    undefined -> application:unset_env(throttle, rates)
                end
            end,
            fun() -> release_owned_pool(State) end
        ]
    ],
    case [E || {error, _, _, _} = E <- Results] of
        [] -> ok;
        Errors -> error({intbe02_teardown_failed, Errors})
    end.

%% Only these new resources are owned here. Keep the prior registry and URL,
%% including an unset value, intact across successful and failed setup.
setup_storage() ->
    %% The marker DB helper also loads imboy. Load first so its later call
    %% cannot merge -config over our owned root, URL, or synthetic key.
    case application:load(imboy) of
        ok -> ok;
        {error, {already_loaded, imboy}} -> ok;
        {error, LoadReason} -> erlang:error({intbe02_app_load_failed, LoadReason})
    end,
    %% V3-14 S3-only：garage env 必须先于 install_asset_registry——fixture
    %% 在安装期用 elib_oss:endpoint()/get_bucket/1 冻结 registry Record。
    %% application:load(imboy) 以 -config 装载 env 会覆盖此前 set_env，
    %% 因此真链调用方（enterprise_internal_file_garage_checks）必须在
    %% setup_all 前用 declare_real_garage/1 显式声明本测试拥有的 env；
    %% 未声明（conformance 离线形态）一律合成 endpoint（DNS 不可达，仅
    %% meck S3 替身截获）+ 合成凭据 + 替身安装——绝不能触碰宿主常驻
    %% Garage（127.0.0.1:3900 开发实例）。
    PriorGarage = application:get_env(imboy, garage),
    GarageMode =
        case persistent_term:get(?REAL_GARAGE_KEY, undefined) of
            undefined ->
                ok = application:set_env(imboy, garage, #{
                    endpoint => ?S3_STUB_ENDPOINT,
                    bucket => ?S3_STUB_BUCKET,
                    region => <<"garage">>,
                    access_key => <<"intbe02-synthetic-ak">>,
                    secret_key => <<"intbe02-synthetic-sk">>
                }),
                stub;
            {ok, Declared} = Real ->
                _ = Real,
                ok = application:set_env(imboy, garage, Declared),
                real
        end,
    Registry = eb_pg_test_fixture:install_asset_registry(),
    State = #{
        registry => Registry,
        base_url => application:get_env(imboy, base_url),
        webhook_key => application:get_env(imboy, postgre_aes_key),
        jwt_key => application:get_env(imboy, jwt_key),
        gateway_tmp => os:getenv("IMBOY_V5_GATEWAY_TMP"),
        garage => PriorGarage,
        garage_mode => GarageMode
    },
    try
        Tmp = filename:join(maps:get(root, Registry), "gateway-tmp"),
        ok = file:make_dir(Tmp),
        ok = file:change_mode(Tmp, 8#700),
        true = os:putenv("IMBOY_V5_GATEWAY_TMP", Tmp),
        ok = application:set_env(imboy, base_url, ?UPLOAD_ORIGIN),
        %% This no-app-boot fixture owns disposable webhook secrets and their
        %% encrypted idempotency responses. Do not rely on a prior suite's key.
        ok = application:set_env(imboy, postgre_aes_key, crypto:strong_rand_bytes(32)),
        %% Application loading does not run the boot secret policy. Upload
        %% capabilities need a real, fixture-owned signing key as well.
        ok = application:set_env(imboy, jwt_key, crypto:strong_rand_bytes(32)),
        case GarageMode of
            stub -> s3_stub_install();
            real -> ok
        end,
        State
    catch
        Class:Reason:Stack ->
            teardown_storage(State),
            erlang:raise(Class, Reason, Stack)
    end.

teardown_storage(#{
    registry := Registry,
    base_url := Base,
    webhook_key := Key,
    jwt_key := JwtKey,
    gateway_tmp := Tmp,
    garage := PriorGarage,
    garage_mode := GarageMode
}) ->
    try
        case Base of
            undefined -> application:unset_env(imboy, base_url);
            {ok, Value} -> application:set_env(imboy, base_url, Value)
        end,
        case Key of
            undefined -> application:unset_env(imboy, postgre_aes_key);
            {ok, Value1} -> application:set_env(imboy, postgre_aes_key, Value1)
        end,
        case JwtKey of
            undefined -> application:unset_env(imboy, jwt_key);
            {ok, Value2} -> application:set_env(imboy, jwt_key, Value2)
        end,
        case Tmp of
            false -> os:unsetenv("IMBOY_V5_GATEWAY_TMP");
            Value0 -> os:putenv("IMBOY_V5_GATEWAY_TMP", Value0)
        end,
        case GarageMode of
            stub -> s3_stub_uninstall();
            real -> ok
        end,
        persistent_term:erase(?REAL_GARAGE_KEY),
        case PriorGarage of
            undefined -> application:unset_env(imboy, garage);
            {ok, Garage} -> application:set_env(imboy, garage, Garage)
        end
    after
        eb_pg_test_fixture:restore_asset_registry(Registry)
    end.

ensure_limiter_owner() ->
    case whereis(login_attempt_ds) of
        undefined ->
            PreviousRates = application:get_env(imboy, limiter_ip_rates),
            application:set_env(imboy, limiter_ip_rates, #{
                passport_per_ip => {100000, 60000},
                api_per_ip => {100000, 60000},
                oidc_request => {1000000, 3600000}
            }),
            try
                {ok, Owner} = login_attempt_ds:start_link(),
                ok = login_attempt_ds:activate_empty(Owner),
                unlink(Owner),
                Owner
            catch
                %% start_link 成功但 activate_empty 等后续步骤失败：此刻
                %% 异常尚未交回 setup_with_limiter 的 catch（LimiterOwner 未
                %% 绑定），若不自停，这个无监督 owner（imboy app 未 boot 时
                %% start_link 直启、不挂 imboy_sup）将带着注册名泄漏到后续
                %% 根——limiter_test_support:restart_owner 杀掉它后无人重启，
                %% 即 {limiter_owner_not_restarted, Pid} 的另一条来源。
                Class:Reason:Stack ->
                    case whereis(login_attempt_ds) of
                        undefined -> ok;
                        Leaked -> catch gen_server:stop(Leaked)
                    end,
                    erlang:raise(Class, Reason, Stack)
            after
                case PreviousRates of
                    undefined -> application:unset_env(imboy, limiter_ip_rates);
                    {ok, Rates} -> application:set_env(imboy, limiter_ip_rates, Rates)
                end
            end;
        _ ->
            true = login_attempt_ds:ready(),
            undefined
    end.

stop_limiter_owner(undefined) ->
    ok;
stop_limiter_owner(Owner) ->
    try
        gen_server:stop(Owner)
    catch
        exit:noproc -> ok
    end.

%%%===================================================================
%%% 夹具常量访问（用例侧按 key 取，避免 magic number 扩散）
%%%===================================================================

fixture(org_a, _) -> ?ORG_A;
fixture(org_b, _) -> ?ORG_B;
fixture(ws_a1, _) -> ?WS_A1;
fixture(ws_a2, _) -> ?WS_A2;
fixture(ws_b1, _) -> ?WS_B1;
fixture(grp_human, _) -> ?GRP_HUMAN;
fixture(grp_b, _) -> ?GRP_B;
fixture(prj_a1, _) -> ?PRJ_A1;
fixture(chn_a1, _) -> ?CHN_A1;
fixture(chn_a2, _) -> ?CHN_A2;
fixture(ext_h1, _) -> ?EXT_H1;
fixture(ext_h2, _) -> ?EXT_H2;
fixture(ext_h3, _) -> ?EXT_H3;
fixture(ext_fr_a, _) -> ?EXT_FR_A;
fixture(ext_fr_b, _) -> ?EXT_FR_B;
fixture(ext_alice, _) -> ?EXT_ALICE;
fixture(h_a1, _) -> ?H_A1;
fixture(h_a2, _) -> ?H_A2;
fixture(h_a3, _) -> ?H_A3;
fixture(fr_a, _) -> ?FR_A;
fixture(fr_b, _) -> ?FR_B;
fixture(alice, _) -> ?ALICE;
fixture(redirect, _) -> ?REDIRECT;
fixture(nonce, _) -> ?NONCE;
fixture(Key, _) -> erlang:error({intbe02_unknown_fixture, Key}).

%%%===================================================================
%%% 合成租户 seed（COMMIT）
%%%===================================================================

seed_matrix(C) ->
    lists:foreach(
        fun(Uid) -> seed_user(C, Uid) end,
        [?OWNER_A, ?OWNER_B, ?H_A1, ?H_A2, ?H_A3, ?PRIN_A, ?FR_A, ?FR_B, ?ALICE, ?H_B1]
    ),
    seed_org(C, ?ORG_A, ?OWNER_A, <<"intbe02-org-a">>),
    seed_org(C, ?ORG_B, ?OWNER_B, <<"intbe02-org-b">>),
    ok = sql_exec(
        C,
        <<"INSERT INTO organization_business_identity(id,organization_id,function_key,display_name) VALUES (995701,995101,'customer_service','synthetic-api-seat'),(995702,995102,'customer_service','synthetic-foreign-seat')">>
    ),
    lists:foreach(
        fun(Uid) -> seed_org_member(C, ?ORG_A, Uid) end,
        [?H_A1, ?H_A2, ?H_A3, ?PRIN_A, ?ALICE]
    ),
    seed_org_member(C, ?ORG_B, ?H_B1),
    seed_workspace(C, ?WS_A1, ?ORG_A, ?OWNER_A, <<"active">>),
    seed_workspace(C, ?WS_A2, ?ORG_A, ?OWNER_A, <<"active">>),
    seed_workspace(C, ?WS_B1, ?ORG_B, ?OWNER_B, <<"active">>),
    lists:foreach(
        fun(Uid) -> seed_ws_member(C, ?WS_A1, Uid) end,
        [?OWNER_A, ?H_A1, ?H_A2, ?H_A3, ?PRIN_A]
    ),
    seed_ws_member(C, ?WS_A2, ?H_A2),
    seed_ws_member(C, ?WS_B1, ?H_B1),
    seed_project(C, ?PRJ_A1, ?WS_A1, ?H_A1),
    seed_channel(C, ?CHN_A1, ?WS_A1, 1),
    seed_channel(C, ?CHN_A2, ?WS_A2, 1),
    %% 人类群（无 enterprise_group_origin）：INT-26 origin 过滤的不可见负例
    seed_group(C, ?GRP_HUMAN, ?WS_A1, ?H_A1, 1),
    seed_group_member(C, ?GRP_HUMAN_MEMBER, ?GRP_HUMAN, ?H_A1, 1),
    %% ORG_B 群：跨 Org 同体 404 负例
    seed_group(C, ?GRP_B, ?WS_B1, ?H_B1, 1),
    %% App A：principal=PRIN_A（INT-12/23 webhook 面依赖）+ org 全域 Grant
    {ok, AppA} = enterprise_application_repo:create_tx(
        C, ?ORG_A, ?APP_KEY_A, <<"intbe02 org A oa"/utf8>>, ?PRIN_A
    ),
    CredA = issue_credential(C, ?ORG_A, maps:get(<<"id">>, AppA), ?SECRET_A),
    ok = seed_mapping(C, ?ORG_A, maps:get(<<"id">>, AppA), ?EXT_H1, ?H_A1),
    ok = seed_mapping(C, ?ORG_A, maps:get(<<"id">>, AppA), ?EXT_H2, ?H_A2),
    ok = seed_mapping(C, ?ORG_A, maps:get(<<"id">>, AppA), ?EXT_H3, ?H_A3),
    %% App C：显式 Workspace Grant（仅 WS_A1）——未覆盖 W 负例
    {ok, AppC} = enterprise_application_repo:create_tx(
        C, ?ORG_A, ?APP_KEY_C, <<"intbe02 ws narrow oa"/utf8>>, ?PRIN_A
    ),
    CredC = issue_credential(C, ?ORG_A, maps:get(<<"id">>, AppC), ?SECRET_C),
    %% App W：同 org 对照应用——「未覆盖 Workspace 的写操作被边界拒绝」负例
    {ok, AppW} = enterprise_application_repo:create_tx(
        C, ?ORG_A, ?APP_KEY_W, <<"intbe02 ws narrow write oa"/utf8>>, ?PRIN_A
    ),
    CredW = issue_credential(C, ?ORG_A, maps:get(<<"id">>, AppW), ?SECRET_W),
    %% App D：同 org 对照应用（另一有效凭证的边界负例用）
    {ok, AppD} = enterprise_application_repo:create_tx(
        C, ?ORG_A, ?APP_KEY_D, <<"intbe02 zero grant oa"/utf8>>, ?PRIN_A
    ),
    CredD = issue_credential(C, ?ORG_A, maps:get(<<"id">>, AppD), ?SECRET_D),
    %% App RO：同 org 对照应用（跨应用归属负例用）
    {ok, AppRO} = enterprise_application_repo:create_tx(
        C, ?ORG_A, ?APP_KEY_RO, <<"intbe02 ro oa"/utf8>>, ?PRIN_A
    ),
    CredRO = issue_credential(C, ?ORG_A, maps:get(<<"id">>, AppRO), ?SECRET_RO),
    %% App SSO：INT-14 用（redirect allowlist + ALICE 映射）
    {ok, AppSso} = enterprise_application_repo:create_tx(
        C, ?ORG_A, ?APP_KEY_SSO, <<"intbe02 sso oa"/utf8>>, null, [
            ?REDIRECT
        ]
    ),
    CredSso = issue_credential(C, ?ORG_A, maps:get(<<"id">>, AppSso), ?SECRET_SSO),
    ok = seed_mapping(C, ?ORG_A, maps:get(<<"id">>, AppSso), ?EXT_ALICE, ?ALICE),
    #{
        cred_a => CredA,
        cred_c => CredC,
        cred_w => CredW,
        cred_d => CredD,
        cred_ro => CredRO,
        cred_sso => CredSso
    }.

seed_user(C, Uid) ->
    sql_exec(C, [
        <<"INSERT INTO \"user\" (id, password, account, account_type, status, reg_ip, reg_cosv) VALUES (">>,
        integer_to_binary(Uid),
        ", 'x', 't995_u",
        integer_to_binary(Uid),
        "', 0, 1, '127.0.0.1', 'x')"
    ]).

seed_org(C, OrgId, OwnerUid, Name) ->
    sql_exec(C, [
        <<"INSERT INTO organization (id, name, owner_id, status, branding, settings, created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        ", '",
        Name,
        "', ",
        integer_to_binary(OwnerUid),
        <<", 'active', '{}'::jsonb, '{}'::jsonb, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_org_member(C, OrgId, Uid) ->
    sql_exec(C, [
        <<"INSERT INTO organization_member (organization_id, user_id, role, status, joined_at, created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        ", ",
        integer_to_binary(Uid),
        <<", 'member', 'active', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_workspace(C, WsId, OrgId, OwnerUid, Status) ->
    sql_exec(C, [
        <<"INSERT INTO workspace (id, name, owner_id, status, organization_id, created_at, updated_at) VALUES (">>,
        integer_to_binary(WsId),
        ", 't995_ws_",
        integer_to_binary(WsId),
        "', ",
        integer_to_binary(OwnerUid),
        ", '",
        Status,
        "', ",
        integer_to_binary(OrgId),
        <<", CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_ws_member(C, WsId, Uid) ->
    sql_exec(C, [
        <<"INSERT INTO workspace_member (workspace_id, user_id, role, status, joined_at, created_at, updated_at) VALUES (">>,
        integer_to_binary(WsId),
        ", ",
        integer_to_binary(Uid),
        <<", 'member', 'active', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_project(C, PId, WsId, OwnerUid) ->
    sql_exec(C, [
        <<"INSERT INTO project (id, workspace_id, name, description, owner_id, status, created_at, updated_at) VALUES (">>,
        integer_to_binary(PId),
        ", ",
        integer_to_binary(WsId),
        ", 'intbe02-p1', 'd', ",
        integer_to_binary(OwnerUid),
        <<", 'active', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_channel(C, ChId, WsId, Status) ->
    sql_exec(C, [
        <<"INSERT INTO channel (id, name, creator_uid, status, scope, workspace_id, created_at, updated_at) VALUES (">>,
        integer_to_binary(ChId),
        ", 'intbe02-ch', ",
        integer_to_binary(?H_A1),
        ", ",
        integer_to_binary(Status),
        <<", 'workspace', ">>,
        integer_to_binary(WsId),
        <<", CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_group(C, Gid, WsId, OwnerUid, Status) ->
    sql_exec(C, [
        <<"INSERT INTO \"group\" (id, type, join_limit, owner_uid, creator_uid, member_max, member_count, title, status, scope, workspace_id, created_at, updated_at) VALUES (">>,
        integer_to_binary(Gid),
        ", 2, 3, ",
        integer_to_binary(OwnerUid),
        ", ",
        integer_to_binary(OwnerUid),
        ", 500, 1, 't995_g_",
        integer_to_binary(Gid),
        "', ",
        integer_to_binary(Status),
        <<", 'workspace', ">>,
        integer_to_binary(WsId),
        <<", CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_group_member(C, MId, Gid, Uid, Role) ->
    sql_exec(C, [
        <<"INSERT INTO group_member (id, group_id, user_id, role, is_join, status, created_at, updated_at) VALUES (">>,
        integer_to_binary(MId),
        ", ",
        integer_to_binary(Gid),
        ", ",
        integer_to_binary(Uid),
        ", ",
        integer_to_binary(Role),
        ", true, 1, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"
    ]).

seed_mapping(C, OrgId, AppId, Ext, Uid) ->
    case enterprise_external_identity_repo:bind_tx(C, OrgId, AppId, Ext, Uid) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({intbe02_mapping_seed_failed, Ext, Reason})
    end.

issue_credential(C, OrgId, AppId, Secret) ->
    {ok, #{credential := Full}} =
        enterprise_internal_ops:issue_credential_tx(C, OrgId, AppId, Secret, undefined),
    Full.

app_id(C, Key) ->
    maps:get(
        <<"id">>,
        one(C, <<"SELECT id FROM enterprise_application WHERE application_key = $1">>, [Key])
    ).

%%%===================================================================
%%% pooler 池（指向 marker 库）
%%%===================================================================

ensure_pool(State) ->
    Server = maps:get(server, State),
    Db = maps:get(db, State),
    #{host := Host, port := Port, username := User, password := Pass} = Server,
    ConnOpts = #{
        host => Host,
        port => Port,
        username => User,
        password => Pass,
        database => Db,
        ssl => false,
        timeout => 10000,
        codecs => [{epgsql_codec_rfc3339_bin, []}]
    },
    {ok, _} = application:ensure_all_started(pooler),
    undefined = whereis(pgsql),
    create_checked_pool(#{
        name => pgsql,
        max_count => 8,
        init_count => 2,
        start_mfa => {epgsql, connect, [ConnOpts]}
    }).

create_checked_pool(Conf) ->
    Parent = whereis(pooler_sup),
    true = is_pid(Parent),
    {ok, Sup} = pooler:new_pool(Conf),
    %% Acquired Sup is already owned, even if any following proof fails.
    try
        Sup = whereis(pooler_pgsql_pool_sup),
        Pid = whereis(pgsql),
        true = lists:member({pooler, Pid, worker, [pooler]}, supervisor:which_children(Sup)),
        true = maps:with(maps:keys(Conf), gen_server:call(Pid, dump_pool)) =:= Conf,
        {Pid, Sup}
    catch
        Class:Reason:Stack ->
            raise_after_cleanup(
                Class,
                Reason,
                Stack,
                [fun() -> release_created_pool(Parent, Sup) end]
            )
    end.

release_created_pool(Parent, Sup) ->
    %% new_pool returns the pool supervisor, not its registered worker.
    Id = pooler_pgsql_pool_sup,
    case lists:keyfind(Id, 1, supervisor:which_children(Parent)) of
        {Id, Sup, supervisor, [pooler_pool_sup]} ->
            %% Stop our exact PID. Never route stop through a replaced name.
            ok = gen_server:stop(Sup, shutdown, 5000),
            release_created_pool(Parent, Sup);
        {Id, undefined, supervisor, [pooler_pool_sup]} ->
            false = is_process_alive(Sup),
            ok = supervisor:delete_child(Parent, Id);
        false ->
            false = is_process_alive(Sup),
            ok;
        _ ->
            error(foreign_pool_supervisor_replaced)
    end.

%%%===================================================================
%%% HTTP（裸 TCP，定长 body，返回 #{status, headers, body, json}）
%%%===================================================================

%% FullCredential 是 issue_credential_tx 返回的完整凭证（ib_int_<id>.<secret>）。
auth(<<"ib_int_", _/binary>> = FullCredential) ->
    #{signed_credential => FullCredential};
auth(FullCredential) when is_binary(FullCredential) ->
    #{<<"authorization">> => <<"Bearer ", FullCredential/binary>>}.

idem(Key) ->
    #{<<"idempotency-key">> => Key}.

http(Port, Method, Path) ->
    http(Port, Method, Path, <<>>, #{}).

http(Port, Method, Path, Body) when is_binary(Body); is_map(Body) ->
    http(Port, Method, Path, Body, #{}).

http(Port, Method, Path, Body0, Headers0) ->
    Body =
        case Body0 of
            B when is_binary(B) -> B;
            M when is_map(M) -> jsone:encode(M);
            _ -> <<>>
        end,
    Headers1 = maps:merge(
        #{
            <<"host">> => request_host(),
            %% 协议矩阵模拟独立客户端；IP 限流用例须显式传入稳定 XFF。
            <<"x-forwarded-for">> =>
                <<"2001:db8:", (integer_to_binary(Port, 16))/binary, "::",
                    (integer_to_binary(erlang:unique_integer([positive]) rem 65536, 16))/binary>>,
            <<"connection">> => <<"close">>,
            <<"content-type">> => <<"application/json">>,
            <<"content-length">> => integer_to_binary(byte_size(Body))
        },
        Headers0
    ),
    Headers =
        case {Path, maps:take(signed_credential, Headers1)} of
            {<<"/api/internal/v1/", _/binary>>, {Credential, Unsigned}} ->
                signed_headers(Credential, Method, Path, Body, Unsigned, sha256);
            {_, error} ->
                Headers1;
            {_, {_Credential, HumanHeaders}} ->
                HumanHeaders
        end,
    HeaderBin = iolist_to_binary([[K, <<": ">>, V, <<"\r\n">>] || {K, V} <- maps:to_list(Headers)]),
    Req = iolist_to_binary([
        Method, <<" ">>, Path, <<" HTTP/1.1\r\n">>, HeaderBin, <<"\r\n">>, Body
    ]),
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}], 15000),
    ok = gen_tcp:send(Socket, Req),
    Raw = recv_all(Socket, []),
    ok = gen_tcp:close(Socket),
    parse(Raw).

%% Match the listener's configured host; wildcard dispatch accepts localhost.
request_host() ->
    case config_ds:env(host, '_') of
        '_' -> <<"localhost">>;
        Host when is_binary(Host) -> Host;
        Host when is_list(Host) -> list_to_binary(Host)
    end.

recv_all(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0, 15000) of
        {ok, Data} -> recv_all(Socket, [Data | Acc]);
        {error, closed} -> iolist_to_binary(lists:reverse(Acc));
        {error, _Timeout} -> iolist_to_binary(lists:reverse(Acc))
    end.

parse(Raw) ->
    case binary:split(Raw, <<"\r\n\r\n">>) of
        [Head, Body] ->
            [StatusLine | HeaderLines] = binary:split(Head, <<"\r\n">>, [global]),
            [_, StatusBin | _] = binary:split(StatusLine, <<" ">>, [global]),
            Headers = headers(HeaderLines),
            #{
                status => binary_to_integer(StatusBin),
                headers => Headers,
                body => decode_chunked(Body, Headers),
                raw => Raw
            };
        [_Only] ->
            #{status => 0, headers => #{}, body => <<>>, raw => Raw}
    end.

headers(Lines) ->
    maps:from_list([
        {string:lowercase(K), V}
     || Line <- Lines,
        [K, V] <- [binary:split(Line, <<": ">>)],
        K =/= <<>>
    ]).

decode_chunked(Body, #{<<"transfer-encoding">> := <<"chunked">>}) ->
    chunked(Body, []);
decode_chunked(Body, _Headers) ->
    Body.

chunked(<<>>, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
chunked(Bin, Acc) ->
    case binary:split(Bin, <<"\r\n">>) of
        [SizeBin, Rest] ->
            case catch binary_to_integer(SizeBin, 16) of
                0 ->
                    iolist_to_binary(lists:reverse(Acc));
                Size when is_integer(Size), Size > 0 ->
                    <<Chunk:Size/binary, _CRLF:2/binary, Tail/binary>> = Rest,
                    chunked(Tail, [Chunk | Acc]);
                _ ->
                    iolist_to_binary(lists:reverse(Acc))
            end;
        _Incomplete ->
            iolist_to_binary(lists:reverse(Acc))
    end.

json_header_val(#{headers := Headers}, Name) ->
    maps:get(Name, Headers, undefined).

%%%===================================================================
%%% 覆盖登记（covered_operation_ids 集合断言的数据源）
%%%===================================================================

cover(Id, _) when is_binary(Id) ->
    true = ets:insert_new(?COVERED_TAB, {Id, true}),
    ok.

covered_ids() ->
    lists:sort([Id || {Id, _} <- ets:tab2list(?COVERED_TAB)]).

%%%===================================================================
%%% 外部服务替身（meck 窗口）
%%%===================================================================

%% V3-14 S3-only 直传合同（OA 后端本人是 S3 上传客户端）：presign 视图必须
%% 是 s3_put descriptor（无受控网关能力、无 JWT upload_capability）；PUT 打
%% 向 presign URL（合成 endpoint，meck S3 替身收字节），随后从替身存储读回
%% 校验——confirm 的真实读流（HEAD/GET）同样由替身供数。
upload_file(_Port, Registration, Bytes) when is_binary(Bytes) ->
    #{<<"transport">> := <<"s3_put">>, <<"method">> := <<"PUT">>, <<"url">> := Url} =
        maps:get(<<"upload">>, Registration),
    false = maps:is_key(<<"gateway">>, Registration),
    false = maps:is_key(<<"upload_capability">>, Registration),
    Url = maps:get(<<"put_url">>, Registration),
    case uri_string:parse(Url) of
        #{host := <<"intbe02-s3.invalid">>} -> ok;
        Other -> erlang:error({invalid_direct_upload_url, Other})
    end,
    Mime =
        case maps:get(<<"mime_type">>, Registration, undefined) of
            M when is_binary(M), M =/= <<>> -> M;
            _ -> <<"application/octet-stream">>
        end,
    Req = {binary_to_list(Url), [], binary_to_list(Mime), Bytes},
    {ok, {{_, 200, _}, _, <<>>}} =
        httpc:request(put, Req, [{timeout, 10000}], [{body_format, binary}]),
    Key = maps:get(<<"object_key">>, Registration),
    {Mime, Bytes} = s3_stub_object(Key),
    ok.

%% Real Garage fixture: direct-v1 confirms the winner before the group reference.
enable_group_direct() ->
    Registry = attachment_storage:registry(),
    Garage = application:get_env(imboy, garage, #{}),
    Namespace = <<"intbe02-group-garage">>,
    Root = #{
        driver => <<"garage">>,
        namespace => Namespace,
        endpoint => maps:get(endpoint, Garage),
        container => maps:get(bucket, Garage)
    },
    Roots = maps:get(roots, Registry, #{}),
    Uploads = maps:get(upload_namespaces, Registry, #{}),
    application:set_env(
        imboy,
        attachment_storage_registry,
        Registry#{
            roots => Roots#{Namespace => Root},
            upload_namespaces => Uploads#{<<"group">> => Namespace}
        }
    ).

direct_group_file(Gid, Uid, FileName, Bytes) ->
    GidBin = integer_to_binary(elib_cnv:safe_to_integer(Gid)),
    Sha = binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
    {ok, #{object_key := Key}} = attach_direct_logic:register(
        Uid, <<"group">>, GidBin, elib_uuid:gen_v7(), FileName
    ),
    ok = attach_direct_logic:declare(Key, Sha, byte_size(Bytes), <<"text/plain">>),
    {ok, #{grant := #{url := Url, required_headers := Hs}, attempt_id := Attempt}} =
        attach_direct_logic:grant(Key, Sha),
    Headers = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Hs],
    {ok, {{_, Status, _}, _, _}} = httpc:request(
        put,
        {binary_to_list(Url), Headers, "text/plain", Bytes},
        [{timeout, 30000}, {autoredirect, false}],
        [{body_format, binary}]
    ),
    true = Status >= 200 andalso Status < 300,
    {ok, #{state := <<"CONFIRMED">>}} = attach_direct_logic:finalize(Key, Attempt),
    {ok, Ref} = group_file_ds:bind_file(
        elib_cnv:safe_to_integer(Gid), Uid, FileName, <<"text/plain">>, Key, byte_size(Bytes), <<>>
    ),
    {Ref, Key}.

start_direct_edge(BackendPort) ->
    {ok, Listener} = gen_tcp:listen(0, [{ip, {127, 0, 0, 1}}, {active, false}]),
    {ok, {_, EdgePort}} = inet:sockname(Listener),
    ok = gen_tcp:close(Listener),
    Token = binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(32), lowercase)),
    PrevToken = application:get_env(imboy, edge_internal_token),
    PrevBase = application:get_env(imboy, edge_base_url),
    Script = direct_edge_script(),
    Paths = lists:join(":", code:get_path()),
    try
        ok = application:set_env(imboy, edge_internal_token, list_to_binary(Token)),
        ok = application:set_env(
            imboy,
            edge_base_url,
            iolist_to_binary(["http://127.0.0.1:", integer_to_list(EdgePort), "/edge"])
        ),
        Edge = open_port({spawn_executable, os:find_executable("escript")}, [
            exit_status,
            stderr_to_stdout,
            {args, [Script]},
            {env, [
                {"EDGE_PORT", integer_to_list(EdgePort)},
                {"EDGE_AUTHORIZE_URL",
                    "http://127.0.0.1:" ++ integer_to_list(BackendPort) ++
                        "/api/v1/attachment/edge/authorize"},
                {"EDGE_INTERNAL_TOKEN", Token},
                {"EDGE_EBIN", filename:dirname(code:which(attachment_edge_proxy_handler))},
                {"EDGE_CODE_PATHS", lists:flatten(Paths)}
            ]}
        ]),
        State = {Edge, PrevToken, PrevBase},
        try
            ok = wait_direct_edge(EdgePort, 100),
            State
        catch
            Class0:Reason0:Stack0 ->
                stop_direct_edge(State),
                erlang:raise(Class0, Reason0, Stack0)
        end
    catch
        Class:Reason:Stack ->
            restore_direct_edge_env(edge_internal_token, PrevToken),
            restore_direct_edge_env(edge_base_url, PrevBase),
            erlang:raise(Class, Reason, Stack)
    end.

direct_edge_script() ->
    Local = filename:absname("priv/workers/download_edge.escript"),
    case filelib:is_regular(Local) of
        true -> Local;
        false -> filename:join([code:priv_dir(imboy), "workers", "download_edge.escript"])
    end.

wait_direct_edge(_, 0) ->
    error(edge_not_ready);
wait_direct_edge(Port, Retries) ->
    case gen_tcp:connect({127, 0, 0, 1}, Port, [{active, false}], 100) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            ok;
        _ ->
            timer:sleep(100),
            wait_direct_edge(Port, Retries - 1)
    end.

stop_direct_edge({Edge, PrevToken, PrevBase}) ->
    try
        case erlang:port_info(Edge, os_pid) of
            {os_pid, Pid} ->
                _ = os:cmd("kill -TERM " ++ integer_to_list(Pid)),
                wait_direct_edge_exit(Pid, 30);
            undefined ->
                ok
        end
    after
        catch erlang:port_close(Edge),
        restore_direct_edge_env(edge_internal_token, PrevToken),
        restore_direct_edge_env(edge_base_url, PrevBase)
    end.

wait_direct_edge_exit(_, 0) ->
    error(edge_process_not_stopped);
wait_direct_edge_exit(Pid, Retries) ->
    case string:trim(os:cmd("ps -p " ++ integer_to_list(Pid) ++ " -o stat=")) of
        "" ->
            ok;
        [$Z | _] ->
            ok;
        _ ->
            timer:sleep(100),
            wait_direct_edge_exit(Pid, Retries - 1)
    end.

restore_direct_edge_env(Key, {ok, Value}) -> application:set_env(imboy, Key, Value);
restore_direct_edge_env(Key, undefined) -> application:unset_env(imboy, Key).

follow_direct_edge(#{status := 302, headers := #{<<"location">> := Url}}) ->
    {ok, {{_, Status, _}, _, Body}} = httpc:request(
        get,
        {binary_to_list(Url), []},
        [{autoredirect, false}, {timeout, 10000}],
        [{body_format, binary}]
    ),
    #{status => Status, body => Body};
follow_direct_edge(Response) ->
    Response.

%%%===================================================================
%%% V3-14 S3 直传替身（meck httpc passthrough 分流 + driver 供数）
%%%===================================================================

%% 安装：meck httpc（仅截获打向 ?S3_STUB_ENDPOINT 的同步请求，其余
%% passthrough 原实现）+ meck attachment_storage_garage（head_snapshot/
%% read_snapshot 供数替身，其余 passthrough——presign 签名仍走真实现）。
s3_stub_install() ->
    catch meck:new(httpc, [unstick, passthrough, no_link]),
    ok = meck:expect(httpc, request, fun
        (put, {Url, _Headers, Ct, Body} = Request, Opts, To) ->
            case s3_stub_claim(Url) of
                {ok, Key} ->
                    s3_stub_serve(put, Key, {unicode:characters_to_binary(Ct), Body});
                pass ->
                    meck:passthrough([put, Request, Opts, To])
            end;
        (get, {Url, _Headers} = Request, Opts, To) ->
            case s3_stub_claim(Url) of
                {ok, Key} -> s3_stub_serve(get, Key, none);
                pass -> meck:passthrough([get, Request, Opts, To])
            end;
        (Method, Request, Opts, To) ->
            meck:passthrough([Method, Request, Opts, To])
    end),
    catch meck:new(attachment_storage_garage, [unstick, passthrough, no_link]),
    ok = meck:expect(attachment_storage_garage, head_snapshot, fun(I) ->
        Key = maps:get(key, I),
        case s3_stub_object(Key) of
            {_Mime, Bytes} ->
                {ok, #{
                    size => byte_size(Bytes),
                    mtime => undefined,
                    etag => binary:encode_hex(crypto:hash(md5, Bytes), lowercase)
                }};
            undefined ->
                {error, not_found}
        end
    end),
    ok = meck:expect(attachment_storage_garage, read_snapshot, fun(I) ->
        Key = maps:get(key, I),
        case s3_stub_object(Key) of
            {_Mime, Bytes} -> {ok, Bytes};
            undefined -> {error, not_found}
        end
    end),
    [persistent_term:erase(K) || {{?S3_OBJ_NS, _} = K, _} <- persistent_term:get()],
    ok.

s3_stub_uninstall() ->
    catch meck:unload(attachment_storage_garage),
    catch meck:unload(httpc),
    [persistent_term:erase(K) || {{?S3_OBJ_NS, _} = K, _} <- persistent_term:get()],
    ok.

%% 仅认领 ?S3_STUB_ENDPOINT 桶前缀 URL（返回物理键）；其余一律 pass——
%% 替身绝不能吞噬非本 fixture 的请求。
s3_stub_claim(Url0) ->
    Url = unicode:characters_to_binary(Url0),
    Prefix = <<?S3_STUB_ENDPOINT/binary, "/", ?S3_STUB_BUCKET/binary, "/">>,
    PrefixSize = byte_size(Prefix),
    case Url of
        <<Prefix:PrefixSize/binary, PathQ/binary>> ->
            %% presign URL 带 ?X-Amz-... 签名查询；物理键为 path 段。
            [Path | _] = binary:split(PathQ, <<"?">>),
            {ok, Path};
        _ ->
            pass
    end.

s3_stub_serve(put, Key, {Ct, Body}) ->
    persistent_term:put({?S3_OBJ_NS, Key}, {Ct, Body}),
    {ok, {{"HTTP/1.1", 200, "OK"}, [{"content-length", "0"}], <<>>}};
s3_stub_serve(get, Key, none) ->
    case s3_stub_object(Key) of
        {Mime, Bytes} ->
            {ok, {
                {"HTTP/1.1", 200, "OK"},
                [
                    {"content-type", binary_to_list(Mime)},
                    {"content-length", integer_to_binary(byte_size(Bytes))}
                ],
                Bytes
            }};
        undefined ->
            {ok, {{"HTTP/1.1", 404, "NotFound"}, [], <<>>}}
    end.

%% 真 Garage 模式声明：EnvMap 为本测试拥有的完整 garage env
%% （endpoint/bucket/region/access_key/secret_key）。必须在 setup_all/0
%% 之前调用；teardown 自动复原并清除声明。
-spec declare_real_garage(map()) -> ok.
declare_real_garage(EnvMap) when is_map(EnvMap) ->
    persistent_term:put(?REAL_GARAGE_KEY, {ok, EnvMap}),
    ok.

-spec s3_stub_object(binary()) -> {binary(), binary()} | undefined.
s3_stub_object(Key) ->
    try persistent_term:get({?S3_OBJ_NS, Key}) of
        Entry -> Entry
    catch
        error:badarg -> undefined
    end.

%% INT-12 SSRF 守卫 DNS 替身：公网假 IP（with_public_dns 同款）。
with_public_dns(Fun) ->
    try
        ok = meck:new(inet, [unstick, passthrough, no_link])
    catch
        _:{already_started, _} -> ok
    end,
    try
        ok = meck:expect(
            inet, getaddrs, fun("oa.customer.example.com", inet) -> {ok, [?PUBLIC_IP]} end
        )
    catch
        _:_ -> ok
    end,
    try
        Fun()
    after
        catch meck:unload(inet)
    end.

%%%===================================================================
%%% marker 直连 SQL（seed 已提交后的用例侧 SQL 断言/修补）
%%%===================================================================

sql_exec(C, IoData) ->
    sql_exec(C, IoData, []).

sql_exec(C, IoData, Params) ->
    Sql = iolist_to_binary(IoData),
    case elib_pg:query(C, Sql, Params) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({intbe02_sql_error, Reason, Sql})
    end.

one(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, [Row | _]} -> Row;
        {ok, []} -> #{};
        {error, Reason} -> erlang:error({intbe02_sql_error, Reason, Sql})
    end.

one(C, Sql) ->
    one(C, Sql, []).

sql_exec_quiet(C, IoData) ->
    _ = elib_pg:query(C, iolist_to_binary(IoData), []),
    ok.

%% A real test client. It signs the wire bytes and never changes server ACLs.
signed_headers(Credential, Method, Target, Body, Headers, sha256 = Algorithm) ->
    [Prefix, Secret] = binary:split(Credential, <<".">>),
    {Path, Query} =
        case binary:split(Target, <<"?">>) of
            [P, Q] -> {P, Q};
            [P] -> {P, <<>>}
        end,
    Timestamp = integer_to_binary(erlang:system_time(second)),
    Nonce = binary:encode_hex(crypto:strong_rand_bytes(24), lowercase),
    Canonical = iolist_to_binary(
        lists:join(<<"\n">>, [
            <<"IMBOY-INTERNAL-V1">>,
            atom_to_binary(Algorithm),
            Prefix,
            maps:get(<<"x-imboy-organization">>, Headers, <<>>),
            Method,
            Path,
            Query,
            maps:get(<<"content-type">>, Headers, <<>>),
            maps:get(<<"idempotency-key">>, Headers, <<>>),
            binary:encode_hex(crypto:hash(sha256, Body), lowercase),
            Timestamp,
            Nonce
        ])
    ),
    Hex = binary:encode_hex(crypto:mac(hmac, Algorithm, Secret, Canonical), lowercase),
    Headers#{
        <<"x-imboy-credential">> => Prefix,
        <<"x-imboy-timestamp">> => Timestamp,
        <<"x-imboy-nonce">> => Nonce,
        <<"x-imboy-signature">> => <<(atom_to_binary(Algorithm))/binary, "=", Hex/binary>>
    }.

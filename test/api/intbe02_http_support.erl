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
%%%      同款 ?MIDDLEWARES），HTTP 层零 mock；仅**外部 DNS 服务替身**走 meck：
%%%      `inet:getaddrs`（INT-12 SSRF 守卫的 DNS 解析，
%%%      enterprise_msg_asset_webhook_pg_tests 同款 with_public_dns）。
%%%      INT-07/08 使用独占 Disk 根，经真实 capability PUT 固定内容后确认。
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

%% 窄写应用 scope：groups:write（+ groups:read 便于分组判别）——供「未覆盖
%% Workspace 写拒绝」负例使用（scope 必须满足、Grant 覆盖必须失败，二者
%% 才能把 403 归因到边界而不是 scope）。

%% App A：17 个固定 scope（除 sso:exchange——该 scope 走独立 SSO 应用），
%% org 全域 Grant；主链全部正例/幂等用例都用它。

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
            teardown_storage(Storage),
            erlang:raise(Class, Reason, Stack)
    end.

setup_with_limiter() ->
    LimiterOwner = ensure_limiter_owner(),
    try
        setup_all(LimiterOwner)
    catch
        Class:Reason:Stack ->
            stop_limiter_owner(LimiterOwner),
            erlang:raise(Class, Reason, Stack)
    end.

setup_all(LimiterOwner) ->
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    %% throttle rates 必须在 ensure_all_started 之前注入（throttle_app:start 读 env 初始化桶）。
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
    C = maps:get(conn, State),
    ensure_pool(State),
    ok = sql_exec(C, <<"BEGIN">>),
    Creds =
        try
            Creds0 = seed_matrix(C),
            ok = sql_exec(C, <<"COMMIT">>),
            Creds0
        catch
            Class:Reason:Stack ->
                _ = sql_exec_quiet(C, <<"ROLLBACK">>),
                release_pool(),
                inttest_marker_db:release(State),
                erlang:raise(Class, {intbe02_seed_failed, Reason}, Stack)
        end,
    %% named_table 跨套件幂等：33 个 intbe02 系套件共用本 setup_all，若前序
    %% 套件异常退出未走 teardown_all（catch ets:delete），残留表会让本套件
    %% ets:new 直接 badarg（run7 三连失败根因）——复用旧表并清空。
    Covered =
        case ets:whereis(?COVERED_TAB) of
            undefined -> ets:new(?COVERED_TAB, [named_table, public, set]);
            Existing -> Existing
        end,
    ets:delete_all_objects(Covered),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(ranch),
    Dispatch = cowboy_router:compile(imboy_router:get_routes()),
    %% 全量顺序下前序 http-check 模块可能已留下同名 listener（teardown 缺失或
    %% 复用场景）：already_started 时复用现有 listener，路由同候选等价。
    case
        cowboy:start_clear(
            ?LISTENER,
            [{port, 0}],
            #{env => #{dispatch => Dispatch}, middlewares => ?MIDDLEWARES}
        )
    of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    Port = ranch:get_port(?LISTENER),
    State#{
        limiter_owner => LimiterOwner,
        conn => C,
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
    }.

-spec teardown_all(map()) -> ok.
teardown_all(State) ->
    try
        teardown_existing(State)
    after
        teardown_storage(maps:get(storage_fixture, State))
    end.

teardown_existing(State) ->
    try
        ok = cowboy:stop_listener(?LISTENER)
    catch
        _:_ -> ok
    end,
    catch ets:delete(?COVERED_TAB),
    stop_limiter_owner(maps:get(limiter_owner, State, undefined)),
    application:unset_env(imboy, enterprise_internal_cursor_signing_key),
    application:unset_env(imboy, enterprise_internal_rate_limits),
    release_pool(),
    %% 释放前关 marker 连接由 release/1 承担
    ok.

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
    Registry = eb_pg_test_fixture:install_asset_registry(),
    State = #{
        registry => Registry,
        base_url => application:get_env(imboy, base_url),
        webhook_key => application:get_env(imboy, postgre_aes_key),
        jwt_key => application:get_env(imboy, jwt_key),
        gateway_tmp => os:getenv("IMBOY_V5_GATEWAY_TMP")
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
    gateway_tmp := Tmp
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
    %% App W：窄写 Grant（groups:write 显式 WS_A1）——「未覆盖 W 写拒绝」负例
    {ok, AppW} = enterprise_application_repo:create_tx(
        C, ?ORG_A, ?APP_KEY_W, <<"intbe02 ws narrow write oa"/utf8>>, ?PRIN_A
    ),
    CredW = issue_credential(C, ?ORG_A, maps:get(<<"id">>, AppW), ?SECRET_W),
    %% App D：零 Grant（allowed_scopes 有 read4）——零 Grant deny 负例
    {ok, AppD} = enterprise_application_repo:create_tx(
        C, ?ORG_A, ?APP_KEY_D, <<"intbe02 zero grant oa"/utf8>>, ?PRIN_A
    ),
    CredD = issue_credential(C, ?ORG_A, maps:get(<<"id">>, AppD), ?SECRET_D),
    %% App RO：仅 application:read——缺 scope 负例（访问 groups:write）
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
    catch pooler:rm_pool(pgsql),
    {ok, _} = pooler:new_pool(#{
        name => pgsql,
        max_count => 8,
        init_count => 2,
        start_mfa => {epgsql, connect, [ConnOpts]}
    }),
    ok.

release_pool() ->
    catch pooler:rm_pool(pgsql),
    %% teardown 不留毒（2026-10-01）：只 rm 不重建会让共享 pgsql 池永久缺失，
    %% 其后非本链套件 take_member noproc 级联 cancelled（CS 全组 no_pool，
    %% 全量 run9 实证）。按 pg_conf 恢复默认池，与本 VM boot 语义一致；
    %% best-effort（失败不炸 teardown——后续套件 try_db 的 noproc 自愈兜底）。
    _ = eunit_runner:ensure_default_pool(),
    ok.

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

%% The reserved origin is a fixture alias for this loopback listener. Preserve
%% the returned raw path/query and capability; never construct an upload token
%% or substitute a storage result. V5 confirm reads the actual frozen bytes.
upload_file(Port, Registration, Bytes) when is_binary(Bytes) ->
    true = maps:get(<<"gateway">>, Registration),
    Url = maps:get(<<"put_url">>, Registration),
    Prefix = <<?UPLOAD_ORIGIN/binary, "/api/v1/attachment/gateway/">>,
    PrefixSize = byte_size(Prefix),
    Path =
        case Url of
            <<Prefix:PrefixSize/binary, Rest/binary>> ->
                <<"/api/v1/attachment/gateway/", Rest/binary>>;
            _ ->
                erlang:error(invalid_fixture_upload_origin)
        end,
    #{status := 200, body := Body} = http(
        Port,
        <<"PUT">>,
        Path,
        Bytes,
        #{<<"content-type">> => <<"text/plain">>}
    ),
    Sha = binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
    Size = byte_size(Bytes),
    #{<<"pinned">> := true, <<"sha256">> := Sha, <<"size">> := Size} = jsone:decode(Body),
    Key = maps:get(<<"object_key">>, Registration),
    {ok, #{driver := disk} = Snapshot} = attachment_storage:session_location(Key),
    {ok, Bytes} = attachment_storage_disk:read_snapshot(Snapshot),
    ok.

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
signed_headers(Credential, Method, Target, Body, Headers, Algorithm) ->
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

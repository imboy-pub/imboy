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
%%%      同款 ?MIDDLEWARES），HTTP 层零 mock；仅两类**外部服务替身**走 meck：
%%%      `elib_oss:head_object`（INT-08 confirm 的对象核实——对象存储不
%%%      在被测边界内）与 `inet:getaddrs`（INT-12 SSRF 守卫的 DNS 解析，
%%%      enterprise_msg_asset_webhook_pg_tests 同款 with_public_dns）。
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
    auth/1,
    idem/1,
    json_header_val/2,
    cover/2,
    covered_ids/0,
    sql_exec/2,
    sql_exec/3,
    one/2,
    one/3,
    with_oss_head/2,
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

-define(SCOPES_READ4, [
    <<"workspaces:read">>,
    <<"groups:read">>,
    <<"projects:read">>,
    <<"channels:read">>,
    <<"customer_service:read">>
]).

%% 窄写应用 scope：groups:write（+ groups:read 便于分组判别）——供「未覆盖
%% Workspace 写拒绝」负例使用（scope 必须满足、Grant 覆盖必须失败，二者
%% 才能把 403 归因到边界而不是 scope）。
-define(SCOPES_W_NARROW, [
    <<"groups:read">>, <<"groups:write">>, <<"workspaces:write">>, <<"channels:write">>
]).

%% App A：17 个固定 scope（除 sso:exchange——该 scope 走独立 SSO 应用），
%% org 全域 Grant；主链全部正例/幂等用例都用它。
-define(SCOPES_APP_A, [
    <<"application:read">>,
    <<"identities:read">>,
    <<"identities:write">>,
    <<"groups:read">>,
    <<"groups:write">>,
    <<"workspaces:read">>,
    <<"projects:read">>,
    <<"channels:read">>,
    <<"files:write">>,
    <<"messages:send">>,
    <<"messages:send_as_human">>,
    <<"friend_requests:create">>,
    <<"webhooks:manage">>,
    <<"customer_service:read">>,
    <<"customer_service:write">>,
    <<"workspaces:write">>,
    <<"channels:write">>
]).

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
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    lists:foreach(
        fun(Name) ->
            case lists:member(Name, elib_tsid:registered()) of
                true -> ok;
                false -> elib_tsid:register(Name)
            end
        end,
        [
            workspace,
            channel,
            channel_admin,
            channel_subscription,
            group_info,
            group_member,
            enterprise_message,
            enterprise_audit_event,
            msg_c2c,
            msg_c2g,
            msg_s2c,
            enterprise_application,
            enterprise_external_identity,
            attachment
        ]
    ),
    %% throttle rates 必须在 ensure_all_started 之前注入（throttle_app:start
    %% 读 env 初始化桶）；成功路径会穿过 throttle_middleware 的 api_per_ip。
    application:set_env(throttle, rates, [
        {api_per_user, 100000, per_minute},
        {api_per_ip, 100000, per_minute}
    ]),
    {ok, _} = application:ensure_all_started(throttle),
    catch throttle:setup(api_per_ip, 100000, per_minute),
    catch throttle:setup(api_per_user, 100000, per_minute),
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
    Covered = ets:new(?COVERED_TAB, [named_table, public, set]),
    ets:delete_all_objects(Covered),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(ranch),
    Dispatch = cowboy_router:compile(imboy_router:get_routes()),
    {ok, _} = cowboy:start_clear(
        ?LISTENER,
        [{port, 0}],
        #{env => #{dispatch => Dispatch}, middlewares => ?MIDDLEWARES}
    ),
    Port = ranch:get_port(?LISTENER),
    State#{
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
teardown_all(_State) ->
    try
        ok = cowboy:stop_listener(?LISTENER)
    catch
        _:_ -> ok
    end,
    catch ets:delete(?COVERED_TAB),
    application:unset_env(imboy, enterprise_internal_cursor_signing_key),
    application:unset_env(imboy, enterprise_internal_rate_limits),
    release_pool(),
    %% 释放前关 marker 连接由 release/1 承担
    ok.

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
        C, ?ORG_A, ?APP_KEY_A, <<"intbe02 org A oa"/utf8>>, {?PRIN_A, ?SCOPES_APP_A}
    ),
    CredA = issue_credential(C, ?ORG_A, maps:get(<<"id">>, AppA), ?SECRET_A),
    ok = issue_grant(
        C, ?ORG_A, maps:get(<<"id">>, AppA), ?SCOPES_APP_A, <<"intbe02-g-a">>, none, []
    ),
    ok = seed_mapping(C, ?ORG_A, maps:get(<<"id">>, AppA), ?EXT_H1, ?H_A1),
    ok = seed_mapping(C, ?ORG_A, maps:get(<<"id">>, AppA), ?EXT_H2, ?H_A2),
    ok = seed_mapping(C, ?ORG_A, maps:get(<<"id">>, AppA), ?EXT_H3, ?H_A3),
    %% App C：显式 Workspace Grant（仅 WS_A1）——未覆盖 W 负例
    {ok, AppC} = enterprise_application_repo:create_tx(
        C, ?ORG_A, ?APP_KEY_C, <<"intbe02 ws narrow oa"/utf8>>, {?PRIN_A, ?SCOPES_READ4}
    ),
    CredC = issue_credential(C, ?ORG_A, maps:get(<<"id">>, AppC), ?SECRET_C),
    ok = issue_grant(
        C, ?ORG_A, maps:get(<<"id">>, AppC), ?SCOPES_READ4, <<"intbe02-g-c">>, explicit, [?WS_A1]
    ),
    %% App W：窄写 Grant（groups:write 显式 WS_A1）——「未覆盖 W 写拒绝」负例
    {ok, AppW} = enterprise_application_repo:create_tx(
        C, ?ORG_A, ?APP_KEY_W, <<"intbe02 ws narrow write oa"/utf8>>, {?PRIN_A, ?SCOPES_W_NARROW}
    ),
    CredW = issue_credential(C, ?ORG_A, maps:get(<<"id">>, AppW), ?SECRET_W),
    ok = issue_grant(
        C,
        ?ORG_A,
        maps:get(<<"id">>, AppW),
        ?SCOPES_W_NARROW,
        <<"intbe02-g-w">>,
        explicit,
        [?WS_A1]
    ),
    %% App D：零 Grant（allowed_scopes 有 read4）——零 Grant deny 负例
    {ok, AppD} = enterprise_application_repo:create_tx(
        C, ?ORG_A, ?APP_KEY_D, <<"intbe02 zero grant oa"/utf8>>, {?PRIN_A, ?SCOPES_READ4}
    ),
    CredD = issue_credential(C, ?ORG_A, maps:get(<<"id">>, AppD), ?SECRET_D),
    %% App RO：仅 application:read——缺 scope 负例（访问 groups:write）
    {ok, AppRO} = enterprise_application_repo:create_tx(
        C, ?ORG_A, ?APP_KEY_RO, <<"intbe02 ro oa"/utf8>>, {?PRIN_A, [<<"application:read">>]}
    ),
    CredRO = issue_credential(C, ?ORG_A, maps:get(<<"id">>, AppRO), ?SECRET_RO),
    ok = issue_grant(
        C, ?ORG_A, maps:get(<<"id">>, AppRO), [<<"application:read">>], <<"intbe02-g-ro">>, none, []
    ),
    %% App SSO：INT-14 用（redirect allowlist + ALICE 映射）
    {ok, AppSso} = enterprise_application_repo:create_tx(
        C, ?ORG_A, ?APP_KEY_SSO, <<"intbe02 sso oa"/utf8>>, {null, [<<"sso:exchange">>]}, [
            ?REDIRECT
        ]
    ),
    CredSso = issue_credential(C, ?ORG_A, maps:get(<<"id">>, AppSso), ?SECRET_SSO),
    ok = issue_grant(
        C, ?ORG_A, maps:get(<<"id">>, AppSso), [<<"sso:exchange">>], <<"intbe02-g-sso">>, none, []
    ),
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
        <<"INSERT INTO group_member (id, group_id, user_id, role, status, created_at, updated_at) VALUES (">>,
        integer_to_binary(MId),
        ", ",
        integer_to_binary(Gid),
        ", ",
        integer_to_binary(Uid),
        ", ",
        integer_to_binary(Role),
        ", 1, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"
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

issue_grant(C, OrgId, AppId, Scopes, Key, Kind, WsIds) ->
    case
        enterprise_internal_ops:issue_grant_tx(C, OrgId, AppId, #{
            scopes => Scopes,
            workspace_scope_kind => Kind,
            workspace_ids => WsIds,
            idempotency_key => Key,
            expires_at => <<"2099-01-01T00:00:00Z">>
        })
    of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({intbe02_grant_seed_failed, Key, Reason})
    end.

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
    ok.

%%%===================================================================
%%% HTTP（裸 TCP，定长 body，返回 #{status, headers, body, json}）
%%%===================================================================

%% FullCredential 是 issue_credential_tx 返回的完整凭证（ib_int_<id>.<secret>）。
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
    Headers = maps:merge(
        #{
            <<"host">> => <<"localhost">>,
            <<"connection">> => <<"close">>,
            <<"content-type">> => <<"application/json">>,
            <<"content-length">> => integer_to_binary(byte_size(Body))
        },
        Headers0
    ),
    HeaderBin = iolist_to_binary([[K, <<": ">>, V, <<"\r\n">>] || {K, V} <- maps:to_list(Headers)]),
    Req = iolist_to_binary([
        Method, <<" ">>, Path, <<" HTTP/1.1\r\n">>, HeaderBin, <<"\r\n">>, Body
    ]),
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}], 15000),
    ok = gen_tcp:send(Socket, Req),
    Raw = recv_all(Socket, []),
    ok = gen_tcp:close(Socket),
    parse(Raw).

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

%% INT-08 confirm 的对象核实（HEAD）：对象存储不在被测边界，替身返回
%% 固定 size/content-type（enterprise_msg_asset_webhook_pg_tests 同款）。
with_oss_head(Size, Fun) ->
    ok = meck:new(elib_oss, [passthrough, no_link]),
    ok = meck:expect(
        elib_oss,
        head_object,
        fun(_Bucket, _Key) -> {ok, #{size => Size, content_type => <<"text/plain">>}} end
    ),
    try
        Fun()
    after
        catch meck:unload(elib_oss)
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

%% enterprise_full_api_pg_tests
%% FULL-02 — 完整 identity/directory/group/file/message API 真库集成测试
%% （plan-full §3.1 逐条 + §7 安全硬门；Gate INTERNAL_API_PASS）。
%%
%% 一次性 marker 库（inttest_marker_db 配方，env 前缀 FULL02_INTTEST；未设该
%% 环境变量时回落 eunit VM 的 pg_conf，与其余企业域套件同款）。setup 阶段
%% COMMIT 提交双 Org × 双 Workspace 基线夹具；业务用例每条 BEGIN ... ROLLBACK，
%% 不留数据。存储侧（Garage）一律 meck elib_oss——eunit VM 无真实对象存储。
%%
%% 覆盖（计划条目 → 用例）：
%%   ① external identity：revoke / rebind / 批量 resolve / **受限 cursor
%%      directory**；**无界导出负例**（page_size 超上限拒绝 + 仓储层硬闸
%%      clamp 实测 + 导出面机械断言 + 逐页遍历终止性）
%%   ② 企业群：detail/update/archive 生命周期、成员角色、Application
%%      membership（origin 归属）、同 org/workspace 强约束
%%   ③ 企业附件：内容策略（mime allowlist / size cap，只可收紧）、message
%%      原子绑定（未 confirm 引用被拒且 origin/audit/msg 三表皆无）、
%%      retention/hold/purge 不变量（应用层 + DB 层双证）
%%   ④ 企业托管消息：direct/group 双路径 origin 双痕迹（Application +
%%      Human）、非 E2EE
%%   ⑤ FULL-01 Grant 读取面接线：enterprise_internal_boundary:enforce/4
%%      逐 kind 行为 + 撤权/降权下一请求即失败 + 未知路由/畸形 ctx fail-closed
%%      + 冻结路由表机械对齐 + **handler 实际调用接线点**（beam 抽象码断言）
%%   ⑥ 聚合计量：usage 列集封闭（无正文/PII）+ metric 枚举 + 只增不减
-module(enterprise_full_api_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

%% ---- 夹具（992 段独立 ID，与 987/988/989/991 段互不冲突） ----

-define(ORG_A, 992101).
-define(ORG_B, 992102).

-define(OWNER_A, 992001).
-define(OWNER_B, 992002).
-define(H_A1, 992011).
-define(H_A2, 992012).
-define(H_A3, 992013).
-define(H_A4, 992014).
-define(PRIN_A, 992015).
-define(H_A5, 992017).
-define(BOT_A, 992016).
-define(H_B1, 992021).

-define(WS_A1, 992201).
-define(WS_A2, 992202).
-define(WS_B1, 992211).

-define(EXT_A1, <<"f02-ext-a1">>).
-define(EXT_A2, <<"f02-ext-a2">>).
-define(EXT_A3, <<"f02-ext-a3">>).
-define(EXT_A4, <<"f02-ext-a4">>).
-define(EXT_B1, <<"f02-ext-b1">>).

-define(SCOPES_FULL, [
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
    <<"messages:send_as_human">>
]).

-define(FAR_FUTURE, <<"2099-01-01T00:00:00Z">>).

%% CP-CON-01：INT-16/17 游标迁移 CURSOR-V2（§10.1）——真库链路的签名密钥。
-define(CURSOR_KEY_CFG, enterprise_internal_cursor_signing_key).
-define(CURSOR_SECRET, <<"full02_cursor_signing_key_0123456789abcdef">>).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup_conn() ->
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
            group_info,
            group_member,
            enterprise_message,
            enterprise_audit_event,
            enterprise_message_origin,
            enterprise_external_identity,
            enterprise_application,
            attachment,
            msg_c2c,
            msg_c2g
        ]
    ),
    {ok, _} = application:ensure_all_started(throttle),
    %% CP-CON-01：INT-16/17 游标走 CURSOR-V2 验签（§10.1）——套件级注入
    %% 签名密钥（与 enterprise_internal_read_pg_tests 同款配方）。
    ok = application:set_env(imboy, ?CURSOR_KEY_CFG, ?CURSOR_SECRET),
    State = inttest_marker_db:provision(#{
        env_prefix => <<"FULL02_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    C = maps:get(conn, State),
    ok = exec(C, <<"BEGIN">>),
    try
        seed_matrix(C),
        ok = exec(C, <<"COMMIT">>)
    catch
        Class:Reason:Stack ->
            _ = exec_quiet(C, <<"ROLLBACK">>),
            application:unset_env(imboy, ?CURSOR_KEY_CFG),
            inttest_marker_db:release(State),
            erlang:raise(Class, {fixture_seed_failed, Reason}, Stack)
    end,
    {AppA, AppB} = app_ids(C),
    {PrinAppA, PrinAppB} = prin_app_ids(C),
    State#{conn => C, app_a => AppA, app_b => AppB, prin_a => PrinAppA, prin_b => PrinAppB}.

close_conn(State) ->
    application:unset_env(imboy, ?CURSOR_KEY_CFG),
    inttest_marker_db:release(State),
    ok.

with_tx(C, TestFun) ->
    ?_test(begin
        ok = exec(C, <<"BEGIN">>),
        try
            TestFun(C),
            ok
        after
            exec(C, <<"ROLLBACK">>)
        end
    end).

%% 期望失败的 DB 断言：独立事务包裹（失败即 ROLLBACK，不污染其他用例）。
with_tx_expect_error(C, Fun) ->
    ?_test(begin
        ok = exec(C, <<"BEGIN">>),
        try
            Fun(C)
        after
            exec(C, <<"ROLLBACK">>)
        end
    end).

exec(C, IoData) ->
    Sql = iolist_to_binary(IoData),
    case elib_pg:query(C, Sql, []) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

exec_params(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

%% 期望被 DB 拒绝的写：返回 error code（binary），未拒绝则 not_rejected。
%% 包 SAVEPOINT（见 in_savepoint/2），使同一用例可连续跑多条负例。
rejected_code(C, Sql, Params) ->
    in_savepoint(C, fun() ->
        case elib_pg:query(C, Sql, Params) of
            {ok, _} -> not_rejected;
            {error, #error{code = Code}} -> Code
        end
    end).

%% 期望失败的 DB 语句必须包在 SAVEPOINT 里：PG 中语句失败会让整个事务进入
%% aborted 状态（25P02），后续语句全部报 in_failed_sql_transaction。
%% ROLLBACK TO SAVEPOINT 恢复事务，使同一用例可连续断言多条 DB 负例。
in_savepoint(C, Fun) ->
    ok = exec(C, <<"SAVEPOINT full02_sp">>),
    try
        Fun()
    after
        _ = elib_pg:query(C, <<"ROLLBACK TO SAVEPOINT full02_sp">>, []),
        _ = elib_pg:query(C, <<"RELEASE SAVEPOINT full02_sp">>, [])
    end.

exec_quiet(C, IoData) ->
    _ = elib_pg:query(C, iolist_to_binary(IoData), []),
    ok.

one(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, [Row | _]} -> Row;
        {ok, []} -> #{};
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

scalar(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, [Row | _]} -> hd(maps:values(Row));
        {ok, []} -> undefined;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

%% ---- 夹具矩阵 ----

seed_matrix(C) ->
    seed_user(C, ?OWNER_A, 0, 1),
    seed_user(C, ?OWNER_B, 0, 1),
    seed_user(C, ?H_A1, 0, 1),
    seed_user(C, ?H_A2, 0, 1),
    seed_user(C, ?H_A3, 0, 1),
    seed_user(C, ?H_A4, 0, 1),
    seed_user(C, ?H_A5, 0, 1),
    seed_user(C, ?PRIN_A, 0, 1),
    seed_user(C, ?BOT_A, 1, 1),
    seed_user(C, ?H_B1, 0, 1),
    seed_org(C, ?ORG_A, ?OWNER_A, <<"f02-org-a">>),
    seed_org(C, ?ORG_B, ?OWNER_B, <<"f02-org-b">>),
    %% org owner 的成员行由 trg_organization_owner_member_sync 随 organization
    %% INSERT 自动落库（migration 113），不得重复显式插入。
    seed_org_member(C, ?ORG_A, ?H_A1, <<"member">>, <<"active">>),
    seed_org_member(C, ?ORG_A, ?H_A2, <<"member">>, <<"active">>),
    seed_org_member(C, ?ORG_A, ?H_A3, <<"member">>, <<"active">>),
    seed_org_member(C, ?ORG_A, ?H_A4, <<"member">>, <<"active">>),
    seed_org_member(C, ?ORG_A, ?H_A5, <<"member">>, <<"active">>),
    seed_org_member(C, ?ORG_A, ?PRIN_A, <<"member">>, <<"active">>),
    seed_org_member(C, ?ORG_A, ?BOT_A, <<"member">>, <<"active">>),
    seed_org_member(C, ?ORG_B, ?H_B1, <<"member">>, <<"active">>),
    seed_workspace(C, ?WS_A1, ?ORG_A, ?OWNER_A, <<"active">>),
    seed_workspace(C, ?WS_A2, ?ORG_A, ?OWNER_A, <<"active">>),
    seed_workspace(C, ?WS_B1, ?ORG_B, ?OWNER_B, <<"active">>),
    seed_ws_member(C, ?WS_A1, ?H_A1, <<"member">>),
    seed_ws_member(C, ?WS_A1, ?H_A2, <<"member">>),
    seed_ws_member(C, ?WS_A2, ?H_A3, <<"member">>),
    seed_ws_member(C, ?WS_B1, ?H_B1, <<"member">>),
    {ok, AppA} = enterprise_internal_ops:create_application_tx(
        C, ?ORG_A, <<"f02-oa-a">>, <<"f02 org A oa"/utf8>>, ?SCOPES_FULL
    ),
    {ok, AppB} = enterprise_internal_ops:create_application_tx(
        C, ?ORG_B, <<"f02-oa-b">>, <<"f02 org B oa"/utf8>>, ?SCOPES_FULL
    ),
    AppAId = maps:get(<<"id">>, AppA),
    AppBId = maps:get(<<"id">>, AppB),
    ok = seed_mapping(C, ?ORG_A, AppAId, ?EXT_A1, ?H_A1),
    ok = seed_mapping(C, ?ORG_A, AppAId, ?EXT_A2, ?H_A2),
    ok = seed_mapping(C, ?ORG_A, AppAId, ?EXT_A3, ?H_A3),
    ok = seed_mapping(C, ?ORG_A, AppAId, ?EXT_A4, ?H_A4),
    ok = seed_mapping(C, ?ORG_B, AppBId, ?EXT_B1, ?H_B1),
    %% principal 绑定（application 模式发送主体）
    ok = bind_principal(C, ?ORG_A, AppAId, ?PRIN_A),
    ok = bind_principal(C, ?ORG_B, AppBId, ?PRIN_A),
    ok.

bind_principal(C, _OrgId, AppId, Uid) ->
    exec_params(
        C,
        <<"UPDATE enterprise_application SET principal_user_id = $1 WHERE id = $2">>,
        [Uid, AppId]
    ).

seed_user(C, Uid, AccountType, Status) ->
    exec(C, [
        <<"INSERT INTO \"user\" (id, password, account, account_type, status, reg_ip, reg_cosv) VALUES (">>,
        integer_to_binary(Uid),
        ", 'x', 't992_u",
        integer_to_binary(Uid),
        "', ",
        integer_to_binary(AccountType),
        ", ",
        integer_to_binary(Status),
        <<", '127.0.0.1', 'x')">>
    ]).

seed_org(C, OrgId, OwnerUid, Name) ->
    exec(C, [
        <<"INSERT INTO organization (id, name, owner_id, status, branding, settings, created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        ", '",
        Name,
        "', ",
        integer_to_binary(OwnerUid),
        <<", 'active', '{}'::jsonb, '{}'::jsonb, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_org_member(C, OrgId, Uid, Role, Status) ->
    exec(C, [
        <<"INSERT INTO organization_member (organization_id, user_id, role, status, joined_at, created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        ", ",
        integer_to_binary(Uid),
        ", '",
        Role,
        "', '",
        Status,
        <<"', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_workspace(C, WsId, OrgId, OwnerUid, Status) ->
    exec(C, [
        <<"INSERT INTO workspace (id, name, owner_id, status, organization_id, created_at, updated_at) VALUES (">>,
        integer_to_binary(WsId),
        ", 't992_ws_",
        integer_to_binary(WsId),
        "', ",
        integer_to_binary(OwnerUid),
        ", '",
        Status,
        "', ",
        integer_to_binary(OrgId),
        <<", CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_ws_member(C, WsId, Uid, Role) ->
    exec(C, [
        <<"INSERT INTO workspace_member (workspace_id, user_id, role, status, joined_at, created_at, updated_at) VALUES (">>,
        integer_to_binary(WsId),
        ", ",
        integer_to_binary(Uid),
        ", '",
        Role,
        <<"', 'active', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_mapping(C, OrgId, AppId, Ext, Uid) ->
    case enterprise_external_identity_repo:bind_tx(C, OrgId, AppId, Ext, Uid) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({seed_mapping_failed, Ext, Reason})
    end.

app_ids(C) ->
    A = one(C, <<"SELECT id FROM enterprise_application WHERE organization_id = $1">>, [?ORG_A]),
    B = one(C, <<"SELECT id FROM enterprise_application WHERE organization_id = $1">>, [?ORG_B]),
    {maps:get(<<"id">>, A), maps:get(<<"id">>, B)}.

prin_app_ids(C) ->
    Rows = elib_pg:query(
        C,
        <<"SELECT id, organization_id FROM enterprise_application WHERE principal_user_id = $1">>,
        [?PRIN_A]
    ),
    {ok, R} = Rows,
    A = [maps:get(<<"id">>, X) || X <- R, maps:get(<<"organization_id">>, X) =:= ?ORG_A],
    B = [maps:get(<<"id">>, X) || X <- R, maps:get(<<"organization_id">>, X) =:= ?ORG_B],
    {hd(A), hd(B)}.

%% ---- ctx 构造 ----

%% 未受管 ctx（零 Grant 行）：grant_governed=false（广州期口径）。
ctx_unmanaged(State) ->
    #{
        organization_id => ?ORG_A,
        application_id => maps:get(app_a, State),
        granted_scopes => ?SCOPES_FULL,
        grant_governed => false,
        principal_user_id => ?PRIN_A
    }.

%% 受管 ctx：经**真链路** context_tx/4 求值（Grant × allowed_scopes 交集），
%% 与 handler 从认证链拿到的 ctx 同形状、同来源。
ctx_managed(C, State) ->
    AppId = maps:get(app_a, State),
    {ok, #{grant_governed := Governed, effective_scopes := Effective}} =
        enterprise_application_grant_logic:context_tx(C, ?ORG_A, AppId, ?SCOPES_FULL),
    #{
        organization_id => ?ORG_A,
        application_id => AppId,
        granted_scopes => Effective,
        grant_governed => Governed,
        principal_user_id => ?PRIN_A
    }.

issue_org_grant(C, State, Scopes, Key) ->
    enterprise_internal_ops:issue_grant_tx(C, ?ORG_A, maps:get(app_a, State), #{
        scopes => Scopes, idempotency_key => Key, expires_at => ?FAR_FUTURE
    }).

issue_ws_grant(C, State, Scopes, Workspaces, Key) ->
    enterprise_internal_ops:issue_grant_tx(C, ?ORG_A, maps:get(app_a, State), #{
        scopes => Scopes,
        workspace_scope_kind => explicit,
        workspace_ids => Workspaces,
        idempotency_key => Key,
        expires_at => ?FAR_FUTURE
    }).

%% ---- 群/附件夹具（用例内构造，随事务回滚） ----

create_group(C, State, WsId, Members) ->
    {ok, Result} = enterprise_group_logic:create_group_tx(C, ctx_unmanaged(State), #{
        workspace_id => WsId,
        title => <<"f02 group"/utf8>>,
        members => Members,
        owner_external_user_id => hd(Members)
    }),
    maps:get(<<"group_id">>, Result).

insert_attachment_row(C, State, ObjectKey) ->
    insert_attachment_row_at(C, State, ObjectKey, elib_tsid:generate(attachment)).

insert_attachment_row_at(C, State, ObjectKey, AttId) ->
    OrgId = ?ORG_A,
    AppId = maps:get(app_a, State),
    ScopeRef = enterprise_asset_repo:scope_ref(OrgId, AppId),
    exec_params(
        C,
        <<
            "INSERT INTO attachment (id, file_hash256, mime_type, ext, name, path, url,"
            " size, info, referer_time, last_referer_user_id, last_referer_at,"
            " creator_user_id, scope, scope_ref, cipher, status, created_at, updated_at)"
            " VALUES ($1,'', 'application/pdf', '.pdf', 'a.pdf', $2, $2, 1024, '{}'::jsonb,"
            " 1, 0, NOW(), 0, 'enterprise', $3, null, 1, NOW(), NOW())"
        >>,
        [AttId, ObjectKey, ScopeRef]
    ),
    AttId.

with_oss(Expectations, Fun) ->
    meck:new(elib_oss, [passthrough, no_passthrough_cover]),
    try
        lists:foreach(
            fun({F, A, Impl}) -> meck:expect(elib_oss, F, A, Impl) end,
            Expectations
        ),
        Fun()
    after
        meck:unload(elib_oss)
    end.

%%%===================================================================
%%% Suite
%%%===================================================================

enterprise_full_api_pg_test_() ->
    {timeout, 900,
        {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
            C = maps:get(conn, State),
            [
                %% ① external identity：revoke / rebind / 受限 cursor directory
                {"identity_revoke_then_rebind_and_resolve",
                    with_tx(C, fun(C1) -> id_revoke_rebind(C1, State) end)},
                {"identity_revoke_negatives",
                    with_tx(C, fun(C1) -> id_revoke_negatives(C1, State) end)},
                {"directory_mappings_cursor_walk_bounded",
                    with_tx(C, fun(C1) -> dir_walk(C1, State) end)},
                {"directory_page_size_over_cap_rejected",
                    with_tx(C, fun(C1) -> dir_over_cap(C1, State) end)},
                {"directory_cursor_malformed_or_cross_family_rejected",
                    with_tx(C, fun(C1) -> dir_bad_cursor(C1, State) end)},
                {"directory_repo_hard_ceiling_no_unbounded_form",
                    with_tx(C, fun(C1) -> dir_repo_ceiling(C1, State) end)},
                {"directory_users_minimal_fields_and_ws_filter",
                    with_tx(C, fun(C1) -> dir_users(C1, State) end)},
                {"directory_export_surface_frozen", dir_export_surface_test()},
                %% ② 企业群生命周期 / 成员角色 / Application membership
                {"group_create_records_application_origin",
                    with_tx(C, fun(C1) -> grp_origin(C1, State) end)},
                {"group_detail_update_lifecycle",
                    with_tx(C, fun(C1) -> grp_detail_update(C1, State) end)},
                {"group_member_roles_and_negatives",
                    with_tx(C, fun(C1) -> grp_roles(C1, State) end)},
                {"group_archive_terminal_and_ownership_bound",
                    with_tx(C, fun(C1) -> grp_archive(C1, State) end)},
                {"group_origin_db_invariants",
                    with_tx_expect_error(C, fun(C1) -> grp_origin_db(C1, State) end)},
                {"group_cross_org_and_workspace_constraints",
                    with_tx(C, fun(C1) -> grp_cross_org(C1, State) end)},
                %% ③ 企业附件：内容策略 / 原子绑定 / retention·hold·purge
                {"asset_content_policy_presign_and_confirm",
                    with_tx(C, fun(C1) -> asset_policy(C1, State) end)},
                {"asset_effective_limit_never_widens_global", asset_limit_test()},
                {"asset_retention_hold_purge_lifecycle",
                    with_tx(C, fun(C1) -> asset_retention(C1, State) end)},
                {"asset_retention_db_invariants",
                    with_tx_expect_error(C, fun(C1) -> asset_retention_db(C1, State) end)},
                {"asset_confirm_registers_retention_atomically",
                    with_tx(C, fun(C1) -> asset_confirm(C1, State) end)},
                {"message_file_binding_atomic_no_partial_state",
                    with_tx(C, fun(C1) -> msg_file_binding(C1, State) end)},
                %% ④ 企业托管消息：origin 双痕迹（direct/group）+ 非 E2EE
                {"message_origin_dual_trace_direct_and_group",
                    with_tx(C, fun(C1) -> msg_origin(C1, State) end)},
                {"message_origin_db_invariants",
                    with_tx_expect_error(C, fun(C1) -> msg_origin_db(C1, State) end)},
                %% ⑤ FULL-01 Grant 读取面接线（boundary）
                {"boundary_zero_grant_denies_all_scoped_routes",
                    with_tx(C, fun(C1) -> bnd_unmanaged(C1, State) end)},
                {"boundary_org_grant_covers_org_routes_only",
                    with_tx(C, fun(C1) -> bnd_org_grant(C1, State) end)},
                {"boundary_explicit_ws_grant_does_not_cover_org",
                    with_tx(C, fun(C1) -> bnd_ws_not_org(C1, State) end)},
                {"boundary_ws_grant_covers_own_ws_only",
                    with_tx(C, fun(C1) -> bnd_ws_scope(C1, State) end)},
                {"boundary_revocation_next_request_fails",
                    with_tx(C, fun(C1) -> bnd_revoke(C1, State) end)},
                {"boundary_downgrade_next_request_fails",
                    with_tx(C, fun(C1) -> bnd_downgrade(C1, State) end)},
                {"boundary_fail_closed_unknown_route_and_malformed_ctx",
                    with_tx(C, fun(C1) -> bnd_fail_closed(C1, State) end)},
                {"boundary_spec_aligns_with_frozen_route_table", bnd_spec_align_test()},
                {"boundary_handlers_actually_call_enforce", bnd_handler_wiring_test()},
                %% ⑥ 聚合计量
                {"usage_table_columns_closed_no_pii", usage_shape_test()},
                {"usage_aggregate_counters_only", with_tx(C, fun(C1) -> usage_meter(C1, State) end)}
            ]
        end}}.

%%%===================================================================
%%% ① external identity
%%%===================================================================

id_revoke_rebind(C, State) ->
    Ctx = ctx_unmanaged(State),
    %% revoke 后 resolve 立刻不再返回（active-only 读取面）
    {ok, #{<<"revoked">> := true, <<"status">> := <<"removed">>}} =
        enterprise_identity_logic:revoke_mapping_tx(C, Ctx, ?EXT_A2),
    {ok, [Mapped]} = enterprise_identity_logic:resolve_mappings_tx(C, Ctx, [?EXT_A1, ?EXT_A2]),
    ?assertEqual(?EXT_A1, maps:get(<<"external_user_id">>, Mapped)),
    %% rebind 同一 external 可复活（upsert 覆盖 + status -> active）
    {ok, #{<<"status">> := <<"active">>}} =
        enterprise_identity_logic:bind_mapping_tx(C, Ctx, ?EXT_A2, ?H_A5),
    {ok, Found} = enterprise_identity_logic:resolve_mappings_tx(C, Ctx, [?EXT_A2]),
    ?assertEqual(?H_A5, maps:get(<<"user_id">>, hd(Found))),
    %% 撤销是软删：行仍在（status=removed）
    ?assertEqual(
        1,
        scalar(
            C,
            <<
                "SELECT COUNT(*) FROM enterprise_external_identity"
                " WHERE external_user_id = $1"
            >>,
            [?EXT_A2]
        )
    ),
    %% 撤销计量（聚合计数 +1）
    ?assertEqual(
        1,
        scalar(
            C,
            <<
                "SELECT counter FROM enterprise_application_usage"
                " WHERE organization_id = $1 AND application_id = $2 AND metric = 'identity.revoked'"
            >>,
            [?ORG_A, maps:get(app_a, State)]
        )
    ).

id_revoke_negatives(C, State) ->
    Ctx = ctx_unmanaged(State),
    %% 已撤销再撤（换 key）：不区分不存在/已撤销 -> resource_not_found
    {ok, _} = enterprise_identity_logic:revoke_mapping_tx(C, Ctx, ?EXT_A3),
    ?assertMatch(
        {error, {<<"resource_not_found">>, _}},
        enterprise_identity_logic:revoke_mapping_tx(C, Ctx, ?EXT_A3)
    ),
    ?assertMatch(
        {error, {<<"resource_not_found">>, _}},
        enterprise_identity_logic:revoke_mapping_tx(C, Ctx, <<"f02-ext-ghost">>)
    ),
    %% 参数矩阵
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_identity_logic:revoke_mapping_tx(C, Ctx, <<>>)
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_identity_logic:revoke_mapping_tx(C, Ctx, undefined)
    ),
    %% 跨 Org 的 external 不因撤销而泄露存在性（B 的映射在 A 视图不可见）
    ?assertMatch(
        {error, {<<"resource_not_found">>, _}},
        enterprise_identity_logic:revoke_mapping_tx(C, Ctx, ?EXT_B1)
    ).

%% @doc 受限 cursor directory：逐页遍历（page_size=2），终止性 + 无重复 +
%% 每页 <= page_size + 跨 App 不可见。
dir_walk(C, State) ->
    Ctx = ctx_unmanaged(State),
    {ok, P1} = enterprise_directory_logic:page_mappings_tx(C, Ctx, #{page_size => 2}),
    ?assertEqual(2, length(maps:get(<<"items">>, P1))),
    ?assertEqual(true, maps:get(<<"has_more">>, P1)),
    ?assertEqual(2, maps:get(<<"page_size">>, P1)),
    {Ids, Pages} =
        case maps:get(<<"next_cursor">>, P1) of
            null ->
                {[maps:get(<<"external_user_id">>, I) || I <- maps:get(<<"items">>, P1)], 1};
            Cur ->
                dir_walk_loop(
                    C,
                    Ctx,
                    Cur,
                    [
                        maps:get(<<"external_user_id">>, I)
                     || I <- maps:get(<<"items">>, P1)
                    ],
                    1
                )
        end,
    ?assert(length(Ids) >= 3),
    ?assert(Pages =< length(Ids)),
    %% 遍历结果与「本 (org, app) active 映射数」一致（不多不少、不重复）
    Expected = scalar(
        C,
        <<
            "SELECT COUNT(*) FROM enterprise_external_identity"
            " WHERE organization_id = $1 AND application_id = $2 AND status = 'active'"
        >>,
        [?ORG_A, maps:get(app_a, State)]
    ),
    ?assertEqual(Expected, length(Ids)),
    ?assertEqual(Expected, length(lists:usort(Ids))).

dir_walk_loop(_C, _Ctx, null, Acc, Pages) ->
    {Acc, Pages};
dir_walk_loop(C, Ctx, Cursor, Acc, Pages) ->
    {ok, Page} = enterprise_directory_logic:page_mappings_tx(C, Ctx, #{
        cursor => Cursor, page_size => 2
    }),
    Items = maps:get(<<"items">>, Page),
    NewAcc = Acc ++ [maps:get(<<"external_user_id">>, I) || I <- Items],
    ?assert(lists:usort(NewAcc) =:= lists:sort(NewAcc)),
    case maps:get(<<"has_more">>, Page) of
        true -> dir_walk_loop(C, Ctx, maps:get(<<"next_cursor">>, Page), NewAcc, Pages + 1);
        false -> {NewAcc, Pages + 1}
    end.

%% @doc **无界导出负例**：page_size 超出上限一律拒绝（不静默截断、不 clamp 到上限）。
dir_over_cap(C, State) ->
    Ctx = ctx_unmanaged(State),
    Max = enterprise_directory_logic:max_page(),
    lists:foreach(
        fun(Bad) ->
            ?assertMatch(
                {error, {<<"invalid_request">>, _}},
                enterprise_directory_logic:page_mappings_tx(C, Ctx, #{page_size => Bad})
            ),
            ?assertMatch(
                {error, {<<"invalid_request">>, _}},
                enterprise_directory_logic:page_users_tx(C, Ctx, #{page_size => Bad})
            )
        end,
        [Max + 1, 1000, 100000, 0, -1]
    ),
    %% 边界值本身合法（上限内）
    ?assertMatch(
        {ok, #{<<"page_size">> := Max}},
        enterprise_directory_logic:page_mappings_tx(C, Ctx, #{page_size => Max})
    ),
    %% 缺省页大小恒有界
    {ok, Def} = enterprise_directory_logic:page_mappings_tx(C, Ctx, #{}),
    ?assertEqual(enterprise_directory_logic:default_page(), maps:get(<<"page_size">>, Def)),
    ?assert(length(maps:get(<<"items">>, Def)) =< enterprise_directory_logic:default_page()).

dir_bad_cursor(C, State) ->
    Ctx = ctx_unmanaged(State),
    lists:foreach(
        fun(Bad) ->
            ?assertMatch(
                {error, {<<"invalid_request">>, _}},
                enterprise_directory_logic:page_mappings_tx(C, Ctx, #{cursor => Bad})
            )
        end,
        [
            <<"not-base64!">>,
            <<"Zm9v">>,
            base64:encode(<<"mappings">>),
            base64:encode(<<"mappings:">>)
        ]
    ),
    %% 换页族复用：users 族游标喂给 mappings → 拒（游标不可跨族）
    {ok, Users} = enterprise_directory_logic:page_users_tx(C, Ctx, #{page_size => 1}),
    UserCursor =
        case maps:get(<<"next_cursor">>, Users) of
            null ->
                %% 成员不足一页时构造一个旧形态 users 游标（验签必拒，同样 400）
                base64:encode(<<"users:992011">>);
            Cur ->
                Cur
        end,
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_directory_logic:page_mappings_tx(C, Ctx, #{cursor => UserCursor})
    ),
    %% users 族游标里塞入非数字键 → 拒
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_directory_logic:page_users_tx(C, Ctx, #{
            cursor => base64:encode(<<"users:not-a-number">>)
        })
    ),
    %% ---- CP-CON-01：CURSOR-V2（§10.1）五类用例的真库侧增量 ----
    %% 真签 mappings 首页游标（fixture 有 4 条 active 映射，page_size=1 必 has_more）
    {ok, P1} = enterprise_directory_logic:page_mappings_tx(C, Ctx, #{page_size => 1}),
    RealCursor = maps:get(<<"next_cursor">>, P1),
    ?assert(is_binary(RealCursor), "has_more 页必须签发真签游标"),
    %% ② tampered：篡改 1 字节（payload 首字符——首字符 6 bit 恒为有效载荷位）
    [EncP, EncM] = binary:split(RealCursor, <<".">>, [global]),
    Tampered = <<(dir_flip_first(EncP))/binary, ".", EncM/binary>>,
    TamperedMac = <<EncP/binary, ".", (dir_flip_first(EncM))/binary>>,
    lists:foreach(
        fun(Bad) ->
            ?assertMatch(
                {error, {<<"invalid_request">>, _}},
                enterprise_directory_logic:page_mappings_tx(C, Ctx, #{cursor => Bad})
            )
        end,
        [Tampered, TamperedMac]
    ),
    %% ⑤ expired：issued_at 早于 24h 窗口的真签游标（同族同绑定，仅时间过期）
    Now = os:system_time(second),
    {ok, Key} = enterprise_cursor_v2:signing_key(),
    ExpiredPayload = enterprise_cursor_v2:build_payload(
        <<"identity_mappings">>,
        ?ORG_A,
        maps:get(app_a, State),
        #{},
        [<<"f02-ext-a1">>],
        Now - enterprise_cursor_v2:ttl_seconds() - 1
    ),
    {ok, Expired} = enterprise_cursor_v2:sign(ExpiredPayload, Key),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_directory_logic:page_mappings_tx(C, Ctx, #{cursor => Expired})
    ),
    %% ⑥ 旧 unsigned 形态（无 HMAC 段）：一律拒——无验签游标不得翻页。
    %%    （f02-ext-a1 是 fixture 里真实存在的 active 映射键，旧实现会放行。）
    lists:foreach(
        fun(Bad) ->
            ?assertMatch(
                {error, {<<"invalid_request">>, _}},
                enterprise_directory_logic:page_mappings_tx(C, Ctx, #{cursor => Bad})
            )
        end,
        [
            base64:encode(<<"mappings:f02-ext-a1">>),
            base64:encode(<<"users:992011">>)
        ]
    ),
    %% ⑦ 绑定：真签名但换 Org 的游标 → 拒（跨 Org 重放不构成越权）
    ForeignOrgPayload = enterprise_cursor_v2:build_payload(
        <<"identity_mappings">>,
        ?ORG_B,
        maps:get(app_a, State),
        #{},
        [<<"f02-ext-a1">>],
        Now
    ),
    {ok, ForeignOrg} = enterprise_cursor_v2:sign(ForeignOrgPayload, Key),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_directory_logic:page_mappings_tx(C, Ctx, #{cursor => ForeignOrg})
    ),
    %% ① valid 对照：真签真绑定的游标续页放行（真签游标链路非仅负例）
    ?assertMatch(
        {ok, #{<<"page_size">> := 1}},
        enterprise_directory_logic:page_mappings_tx(C, Ctx, #{
            cursor => RealCursor, page_size => 1
        })
    ).

%% 翻转 base64url 首字符（同 enterprise_cursor_v2_tests：避开尾字符低位
%% 填充比特可能无显著性的问题）。
dir_flip_first(Bin) ->
    Size = byte_size(Bin),
    Head = binary:part(Bin, 1, Size - 1),
    <<(dir_flip(binary:first(Bin))):8, Head/binary>>.

dir_flip(C) when C >= $a, C =< $y -> C + 1;
dir_flip($z) -> $a;
dir_flip(C) when C >= $A, C =< $Y -> C + 1;
dir_flip($Z) -> $A;
dir_flip(C) when C >= $0, C =< $8 -> C + 1;
dir_flip($9) -> $0;
dir_flip(C) -> C bxor 1.

%% @doc 仓储层硬闸（第二道防线）：即使调用方传 100000，也最多取 MAX_PAGE + 1 行
%% ——「无界导出」在任何调用路径上都不可能（LIMIT 恒生效）。
dir_repo_ceiling(C, State) ->
    OrgId = ?ORG_A,
    AppId = maps:get(app_a, State),
    {ok, HugeMapping} = enterprise_directory_repo:page_mappings_tx(
        C, OrgId, AppId, undefined, 100000
    ),
    ?assert(length(HugeMapping) =< enterprise_directory_repo:max_page() + 1),
    {ok, HugeMembers} = enterprise_directory_repo:page_members_tx(
        C, OrgId, AppId, undefined, 100000
    ),
    ?assert(length(HugeMembers) =< enterprise_directory_repo:max_page() + 1),
    {ok, HugeWs} = enterprise_directory_repo:page_members_in_workspace_tx(
        C, OrgId, AppId, ?WS_A1, undefined, 100000
    ),
    ?assert(length(HugeWs) =< enterprise_directory_repo:max_page() + 1),
    %% 反证：库里确实有行（否则上面的「有界」可能只是空集造成的假绿）
    ?assert(length(HugeMembers) > 0),
    ?assert(length(HugeMapping) > 0).

dir_users(C, State) ->
    %% V2.1 D4：workspace 过滤走 require_workspace_tx（零 Grant 恒拒）——
    %% 先建 org 全域 Grant 再用真链路 ctx（与 handler 同形状）。
    {ok, _} = issue_org_grant(C, State, [<<"identities:read">>], <<"k-dir-users">>),
    Ctx = ctx_managed(C, State),
    {ok, Page} = enterprise_directory_logic:page_users_tx(C, Ctx, #{page_size => 50}),
    Items = maps:get(<<"items">>, Page),
    ?assert(length(Items) > 0),
    %% 最小字段：恰好四个键，无 nickname/phone/email 等 PII
    lists:foreach(
        fun(I) ->
            ?assertEqual(
                [<<"external_user_id">>, <<"member_role">>, <<"member_status">>, <<"user_id">>],
                lists:sort(maps:keys(I))
            )
        end,
        Items
    ),
    %% 非 active Human（bot）不在目录里
    ?assertEqual(
        false,
        lists:member(?BOT_A, [maps:get(<<"user_id">>, I) || I <- Items])
    ),
    %% workspace 过滤：WS_A1 只含 H_A1/H_A2（H_A3 在 WS_A2）
    {ok, WsPage} = enterprise_directory_logic:page_users_tx(C, Ctx, #{
        page_size => 50, workspace_id => ?WS_A1
    }),
    WsUids = lists:sort([maps:get(<<"user_id">>, I) || I <- maps:get(<<"items">>, WsPage)]),
    ?assertEqual([?H_A1, ?H_A2], WsUids),
    %% mapping 命中：A1/A2 有 external（未映射成员返回 external_user_id=null）
    ?assertEqual(
        ?EXT_A1,
        proplists:get_value(
            ?H_A1,
            [
                {maps:get(<<"user_id">>, I), maps:get(<<"external_user_id">>, I)}
             || I <- maps:get(<<"items">>, WsPage)
            ]
        )
    ),
    %% 跨 Org workspace 过滤：V2.1 边界 fail-closed——require_workspace_tx 对
    %% 「W 不在本 Org 视图内」与「同 O 未覆盖」统一 organization_boundary_violation
    %% （与 bnd_ws_scope 对 WS_B1 的冻结断言同款；不回显目标是否存在）
    ?assertEqual(
        {error, {<<"organization_boundary_violation">>, grant_boundary}},
        enterprise_directory_logic:page_users_tx(C, Ctx, #{
            page_size => 50, workspace_id => ?WS_B1
        })
    ),
    %% workspace_id 非法
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_directory_logic:page_users_tx(C, Ctx, #{workspace_id => <<"nope">>})
    ).

%% @doc 导出面机械断言：identity/directory 层**没有**任何 list-all/export 形态。
dir_export_surface_test() ->
    ?_test(begin
        assert_no_unbounded_exports(enterprise_identity_logic, [
            bind_mapping_tx, resolve_mappings_tx, revoke_mapping_tx
        ]),
        assert_no_unbounded_exports(enterprise_directory_logic, [
            page_mappings_tx, page_users_tx, max_page, default_page
        ]),
        assert_no_unbounded_exports(enterprise_directory_repo, [
            page_mappings_tx, page_members_tx, page_members_in_workspace_tx, max_page
        ]),
        %% 每页上限两侧一致（logic 与 repo 同值）
        ?assertEqual(
            enterprise_directory_repo:max_page(),
            enterprise_directory_logic:max_page()
        )
    end).

assert_no_unbounded_exports(Mod, Expected) ->
    Exports = lists:sort([F || {F, _} <- Mod:module_info(exports), F =/= module_info]),
    ?assertEqual(lists:sort(Expected), Exports),
    Bad = [
        F
     || {F, _} <- Mod:module_info(exports),
        binary:match(
            atom_to_binary(F, utf8),
            [<<"all">>, <<"export">>, <<"list">>, <<"dump">>, <<"scan">>]
        ) =/= nomatch
    ],
    ?assertEqual([], Bad).

%%%===================================================================
%%% ② 企业群
%%%===================================================================

grp_origin(C, State) ->
    Gid = create_group(C, State, ?WS_A1, [?EXT_A1, ?EXT_A2]),
    Origin = one(
        C,
        <<
            "SELECT organization_id, application_id, workspace_id, status FROM enterprise_group_origin"
            " WHERE group_id = $1"
        >>,
        [Gid]
    ),
    ?assertEqual(?ORG_A, maps:get(<<"organization_id">>, Origin)),
    ?assertEqual(maps:get(app_a, State), maps:get(<<"application_id">>, Origin)),
    ?assertEqual(?WS_A1, maps:get(<<"workspace_id">>, Origin)),
    ?assertEqual(<<"active">>, maps:get(<<"status">>, Origin)),
    %% 详情带归属视图 + mine=true
    {ok, Detail} = enterprise_group_logic:group_detail_tx(C, ctx_unmanaged(State), Gid),
    ?assertMatch(
        #{<<"mine">> := true, <<"status">> := <<"active">>},
        maps:get(<<"origin">>, Detail)
    ),
    ?assertEqual(2, maps:get(<<"member_count">>, Detail)),
    %% 直接建群（不经 INT-04）没有归属行 -> origin = null
    Raw = ?WS_A1 + 90000,
    exec_params(
        C,
        <<
            "INSERT INTO \"group\" (id, type, join_limit, owner_uid, creator_uid, introduction,"
            " title, status, member_count, scope, workspace_id, created_at, updated_at)"
            " VALUES ($1, 2, 3, $2, $2, '', 'raw', 1, 1, 'workspace', $3, NOW(), NOW())"
        >>,
        [Raw, ?OWNER_A, ?WS_A1]
    ),
    {ok, RawDetail} = enterprise_group_logic:group_detail_tx(C, ctx_unmanaged(State), Raw),
    ?assertEqual(null, maps:get(<<"origin">>, RawDetail)),
    %% 无归属行的群不可被 OA 归档（Application membership 边界）
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_group_logic:archive_group_tx(C, ctx_unmanaged(State), Raw)
    ).

grp_detail_update(C, State) ->
    Ctx = ctx_unmanaged(State),
    Gid = create_group(C, State, ?WS_A1, [?EXT_A1, ?EXT_A2]),
    {ok, Detail} = enterprise_group_logic:group_detail_tx(C, Ctx, Gid),
    ?assertEqual(<<"f02 group"/utf8>>, maps:get(<<"title">>, Detail)),
    ?assertEqual(?H_A1, maps:get(<<"owner_user_id">>, Detail)),
    Members = maps:get(<<"members">>, Detail),
    ?assertEqual(2, length(Members)),
    ?assertEqual([?H_A1, ?H_A2], lists:sort([maps:get(<<"user_id">>, M) || M <- Members])),
    %% 部分更新：只给 title
    {ok, U1} = enterprise_group_logic:update_group_tx(C, Ctx, Gid, #{title => <<"renamed"/utf8>>}),
    ?assertEqual(<<"renamed"/utf8>>, maps:get(<<"title">>, U1)),
    ?assertEqual(<<>>, maps:get(<<"introduction">>, U1)),
    %% 只给 introduction
    {ok, U2} = enterprise_group_logic:update_group_tx(C, Ctx, Gid, #{
        introduction => <<"intro"/utf8>>
    }),
    ?assertEqual(<<"renamed"/utf8>>, maps:get(<<"title">>, U2)),
    ?assertEqual(<<"intro"/utf8>>, maps:get(<<"introduction">>, U2)),
    %% 空更新 / 非法 title
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_group_logic:update_group_tx(C, Ctx, Gid, #{})
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_group_logic:update_group_tx(C, Ctx, Gid, #{title => <<>>})
    ),
    %% 跨 Org / 不存在群：resource_not_found（不泄露存在性）
    ?assertMatch(
        {error, {<<"resource_not_found">>, _}},
        enterprise_group_logic:group_detail_tx(C, ctx_b(State), Gid)
    ),
    ?assertMatch(
        {error, {<<"resource_not_found">>, _}},
        enterprise_group_logic:update_group_tx(C, Ctx, 999999999, #{title => <<"x">>})
    ).

ctx_b(State) ->
    #{
        organization_id => ?ORG_B,
        application_id => maps:get(app_b, State),
        granted_scopes => ?SCOPES_FULL,
        grant_governed => false
    }.

grp_roles(C, State) ->
    Ctx = ctx_unmanaged(State),
    Gid = create_group(C, State, ?WS_A1, [?EXT_A1, ?EXT_A2]),
    %% 合法角色：1 成员 / 2 嘉宾 / 3 管理员 / 5 副群主
    {ok, R} = enterprise_group_logic:set_member_roles_tx(C, Ctx, Gid, [
        #{external_user_id => ?EXT_A2, role => 3}
    ]),
    ?assertEqual(1, maps:get(<<"updated">>, R)),
    ?assertEqual(0, maps:get(<<"unchanged">>, R)),
    %% 幂等：同角色再设 -> unchanged
    {ok, R2} = enterprise_group_logic:set_member_roles_tx(C, Ctx, Gid, [
        #{external_user_id => ?EXT_A2, role => 3}
    ]),
    ?assertEqual(0, maps:get(<<"updated">>, R2)),
    ?assertEqual(1, maps:get(<<"unchanged">>, R2)),
    %% 负例：群主角色不可经 OA 分配（4）；0 未定义非法；表外值非法
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_group_logic:set_member_roles_tx(C, Ctx, Gid, [
            #{external_user_id => ?EXT_A2, role => 4}
        ])
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_group_logic:set_member_roles_tx(C, Ctx, Gid, [
            #{external_user_id => ?EXT_A2, role => 0}
        ])
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_group_logic:set_member_roles_tx(C, Ctx, Gid, [
            #{external_user_id => ?EXT_A2, role => 6}
        ])
    ),
    %% 不可改群主（owner_uid）的角色
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_group_logic:set_member_roles_tx(C, Ctx, Gid, [
            #{external_user_id => ?EXT_A1, role => 1}
        ])
    ),
    %% 未映射 / 非群成员
    ?assertMatch(
        {error, {<<"identity_not_mapped">>, _}},
        enterprise_group_logic:set_member_roles_tx(C, Ctx, Gid, [
            #{external_user_id => <<"f02-ext-ghost">>, role => 1}
        ])
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_group_logic:set_member_roles_tx(C, Ctx, Gid, [
            #{external_user_id => ?EXT_A4, role => 1}
        ])
    ),
    %% 形态非法
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_group_logic:set_member_roles_tx(C, Ctx, Gid, [])
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_group_logic:set_member_roles_tx(C, Ctx, Gid, [#{role => 1}])
    ).

grp_archive(C, State) ->
    Ctx = ctx_unmanaged(State),
    Gid = create_group(C, State, ?WS_A1, [?EXT_A1, ?EXT_A2]),
    {ok, A1} = enterprise_group_logic:archive_group_tx(C, Ctx, Gid),
    ?assertMatch(#{<<"archived">> := true, <<"already">> := false}, A1),
    %% 群状态 0（禁用）+ 归属 archived
    ?assertEqual(0, scalar(C, <<"SELECT status FROM \"group\" WHERE id = $1">>, [Gid])),
    ?assertEqual(
        <<"archived">>,
        scalar(C, <<"SELECT status FROM enterprise_group_origin WHERE group_id = $1">>, [Gid])
    ),
    %% 归档后：详情/成员增删/角色/消息发送全部 fail-closed（resource_not_found）
    ?assertMatch(
        {error, {<<"resource_not_found">>, _}},
        enterprise_group_logic:group_detail_tx(C, Ctx, Gid)
    ),
    ?assertMatch(
        {error, {<<"resource_not_found">>, _}},
        enterprise_group_logic:add_members_tx(C, Ctx, Gid, [?EXT_A3])
    ),
    ?assertMatch(
        {error, {<<"resource_not_found">>, _}},
        enterprise_group_logic:set_member_roles_tx(C, Ctx, Gid, [
            #{external_user_id => ?EXT_A2, role => 3}
        ])
    ),
    %% 幂等重复归档
    {ok, A2} = enterprise_group_logic:archive_group_tx(C, Ctx, Gid),
    ?assertEqual(true, maps:get(<<"already">>, A2)),
    %% 跨 Org / 不存在
    ?assertMatch(
        {error, {<<"resource_not_found">>, _}},
        enterprise_group_logic:archive_group_tx(C, ctx_b(State), Gid)
    ),
    %% 他人归属：B 造一个群归属本 app 之外（用 B 的 app 建群于 WS_B1），
    %% A 视角不可见 -> resource_not_found（跨租户不泄露）
    GidB = create_group_b(C, State),
    ?assertMatch(
        {error, {<<"resource_not_found">>, _}},
        enterprise_group_logic:archive_group_tx(C, Ctx, GidB)
    ).

create_group_b(C, State) ->
    {ok, Result} = enterprise_group_logic:create_group_tx(C, ctx_b(State), #{
        workspace_id => ?WS_B1,
        title => <<"f02 group b"/utf8>>,
        members => [?EXT_B1],
        owner_external_user_id => ?EXT_B1
    }),
    maps:get(<<"group_id">>, Result).

%% @doc 归属/治理行的 DB 层不变量（绕过应用层直写）：DELETE 拒绝、归档单向、
%% 跨 Org 复合 FK 拒绝。
grp_origin_db(C, State) ->
    Gid = create_group(C, State, ?WS_A1, [?EXT_A1]),
    ?assertEqual(
        <<"23514">>,
        rejected_code(
            C,
            <<"DELETE FROM enterprise_group_origin WHERE group_id = $1">>,
            [Gid]
        )
    ),
    ?assertEqual(
        <<"23514">>,
        rejected_code(
            C,
            <<"UPDATE enterprise_group_origin SET organization_id = $2 WHERE group_id = $1">>,
            [Gid, ?ORG_B]
        )
    ),
    %% 归档后不可回退
    exec_params(
        C,
        <<
            "UPDATE enterprise_group_origin SET status = 'archived', archived_at = NOW()"
            " WHERE group_id = $1"
        >>,
        [Gid]
    ),
    ?assertEqual(
        <<"23514">>,
        rejected_code(
            C,
            <<
                "UPDATE enterprise_group_origin SET status = 'active', archived_at = NULL"
                " WHERE group_id = $1"
            >>,
            [Gid]
        )
    ),
    %% CHECK：status=archived 必须带 archived_at；缺一即拒
    ?assertEqual(
        <<"23514">>,
        rejected_code(
            C,
            <<
                "UPDATE enterprise_group_origin SET status = 'archived', archived_at = NULL"
                " WHERE group_id = $1"
            >>,
            [Gid]
        )
    ).

grp_cross_org(C, State) ->
    Ctx = ctx_unmanaged(State),
    %% 跨 Org workspace 建群 -> resource_not_found
    ?assertMatch(
        {error, {<<"resource_not_found">>, _}},
        enterprise_group_logic:create_group_tx(C, Ctx, #{
            workspace_id => ?WS_B1,
            title => <<"cross"/utf8>>,
            members => [?EXT_A1]
        })
    ),
    %% 个人群不可被 OA 管理（scope='' 的群）
    Personal = ?WS_A1 + 80000,
    exec_params(
        C,
        <<
            "INSERT INTO \"group\" (id, type, join_limit, owner_uid, creator_uid, introduction,"
            " title, status, member_count, scope, created_at, updated_at)"
            " VALUES ($1, 1, 1, $2, $2, '', 'personal', 1, 1, 'personal', NOW(), NOW())"
        >>,
        [Personal, ?OWNER_A]
    ),
    ?assertMatch(
        {error, {<<"resource_not_found">>, _}},
        enterprise_group_logic:add_members_tx(C, Ctx, Personal, [?EXT_A1])
    ),
    ?assertMatch(
        {error, {<<"resource_not_found">>, _}},
        enterprise_group_logic:group_detail_tx(C, Ctx, Personal)
    ),
    %% 非 workspace 成员的 human sender 不可入群（强约束）
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_group_logic:create_group_tx(C, Ctx, #{
            workspace_id => ?WS_A2,
            title => <<"ws2"/utf8>>,
            members => [?EXT_A1]
        })
    ).

%%%===================================================================
%%% ③ 企业附件
%%%===================================================================

asset_policy(C, State) ->
    OrgId = ?ORG_A,
    AppId = maps:get(app_a, State),
    Ctx = ctx_unmanaged(State),
    %% 策略：只允许 pdf，且上限 = 全局的一半（严格收紧，可区分是策略拒绝
    %% 而非全局上限拒绝）
    Global = elib_oss:max_file_size(),
    AppCap = Global div 2,
    ?assert(AppCap > 1),
    ok = enterprise_application_repo:update_content_policy_tx(
        C, OrgId, AppId, [<<"application/pdf">>], AppCap
    ),
    %% presign：允许类型通过
    {ok, P1} = enterprise_asset_logic:presign_tx(C, Ctx, #{
        file_name => <<"a.pdf">>, mime_type => <<"application/pdf">>, size_bytes => 100
    }),
    ?assertMatch(#{<<"object_key">> := _, <<"put_url">> := _}, P1),
    ?assert(is_binary(maps:get(<<"object_key">>, P1))),
    %% presign：策略外类型拒
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_asset_logic:presign_tx(C, Ctx, #{
            file_name => <<"a.png">>, mime_type => <<"image/png">>
        })
    ),
    %% presign：超过应用上限拒，但仍在全局上限内 -> 证明是**策略**收紧
    ?assert(AppCap + 1 =< Global),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_asset_logic:presign_tx(C, Ctx, #{
            file_name => <<"a.pdf">>, mime_type => <<"application/pdf">>, size_bytes => AppCap + 1
        })
    ),
    ?assertMatch(
        {ok, #{<<"object_key">> := _}},
        enterprise_asset_logic:presign_tx(C, Ctx, #{
            file_name => <<"a.pdf">>, mime_type => <<"application/pdf">>, size_bytes => AppCap
        })
    ),
    %% 策略元素级守卫（DB）：通配/重复/形态非法/非法上限（各自 savepoint）
    lists:foreach(
        fun({Mimes, Max}) ->
            ?assertEqual(
                {error, invalid_policy},
                in_savepoint(C, fun() ->
                    enterprise_application_repo:update_content_policy_tx(
                        C, OrgId, AppId, Mimes, Max
                    )
                end)
            )
        end,
        [
            {[<<"*/*">>], undefined},
            {[<<"application/pdf">>, <<"application/pdf">>], undefined},
            {[<<"notamime">>], undefined},
            {[<<"application/pdf">>], 0}
        ]
    ),
    %% confirm：HEAD 真实类型不在策略内 -> 拒 + 删对象（meck 计数）
    ObjectKey = maps:get(<<"object_key">>, P1),
    _ = enterprise_asset_repo:pending_add_tx(
        C, ObjectKey, elib_oss:get_bucket(<<"enterprise">>), <<"enterprise">>
    ),
    with_oss(
        [
            {head_object, 2, fun(_B, _K) ->
                {ok, #{size => 100, content_type => <<"image/gif">>}}
            end},
            {delete_object, 2, fun(_B, _K) -> ok end}
        ],
        fun() ->
            ?assertMatch(
                {error, {<<"invalid_request">>, _}},
                enterprise_asset_logic:confirm_tx(C, Ctx, #{object_key => ObjectKey})
            ),
            ?assertEqual(1, meck:num_calls(elib_oss, delete_object, 2))
        end
    ),
    %% 策略读取：不存在的 app -> not_found（logic 层归一 fail-closed）
    ?assertEqual({error, not_found}, enterprise_application_repo:policy_tx(C, ?ORG_A, 999999999)),
    ?assertMatch(
        {error, {<<"internal_error">>, _}},
        enterprise_asset_retention_logic:content_policy_tx(
            C, #{organization_id => ?ORG_A, application_id => 999999999}, #{}
        )
    ),
    ok.

asset_limit_test() ->
    ?_test(begin
        Global = elib_oss:max_file_size(),
        %% 应用值小于全局：生效值 = 应用值（收紧）
        ?assertEqual(
            1024,
            enterprise_asset_retention_logic:effective_max_bytes(
                #{max_file_size_bytes => 1024}, undefined
            )
        ),
        %% 应用值大于全局：生效值 = 全局（**不放大**）
        ?assertEqual(
            Global,
            enterprise_asset_retention_logic:effective_max_bytes(
                #{max_file_size_bytes => Global * 10}, undefined
            )
        ),
        %% 未配置：生效值 = 全局
        ?assertEqual(
            Global,
            enterprise_asset_retention_logic:effective_max_bytes(
                #{max_file_size_bytes => undefined}, undefined
            )
        ),
        %% 空 allowlist = 沿用全局白名单；非空 = 逐字精确匹配（无通配）
        ?assert(
            enterprise_asset_retention_logic:mime_allowed(
                #{allowed_mime_types => []}, <<"image/png">>
            )
        ),
        ?assertNot(
            enterprise_asset_retention_logic:mime_allowed(
                #{allowed_mime_types => [<<"application/pdf">>]}, <<"image/png">>
            )
        ),
        ?assertNot(
            enterprise_asset_retention_logic:mime_allowed(
                #{allowed_mime_types => [<<"application/pdf">>]}, <<"application/pdf+x">>
            )
        )
    end).

asset_retention(C, State) ->
    OrgId = ?ORG_A,
    AppId = maps:get(app_a, State),
    Ctx = ctx_unmanaged(State),
    ObjectKey = <<"eoa/992101/1/20260922/file_full02/a.pdf">>,
    AttId = insert_attachment_row(C, State, ObjectKey),
    %% 治理行：默认留存登记（未到期）
    ok = enterprise_asset_retention_logic:register_retention_tx(C, {AttId, OrgId, AppId}),
    ?assertEqual(
        <<"live">>,
        scalar(
            C,
            <<"SELECT purge_state FROM enterprise_attachment_retention WHERE attachment_id = $1">>,
            [AttId]
        )
    ),
    ?assertEqual(
        false,
        element(2, enterprise_attachment_retention_repo:is_purgeable_tx(C, AttId))
    ),
    %% 法务 hold
    {ok, H} = enterprise_asset_retention_logic:governance_tx(C, Ctx, #{
        op => <<"hold">>, object_key => ObjectKey, reason => <<"litigation"/utf8>>
    }),
    ?assertEqual(true, maps:get(<<"held">>, H)),
    ?assertEqual(<<"held">>, maps:get(<<"hold_state">>, H)),
    %% ① hold 生效中 purge 被拒（DB 触发器，归一 invalid_request）
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_asset_retention_logic:governance_tx(C, Ctx, #{
            op => <<"purge">>, object_key => ObjectKey
        })
    ),
    %% ② 释放 hold 后：留存窗口未到 -> purge 仍拒
    {ok, _} = enterprise_asset_retention_logic:governance_tx(C, Ctx, #{
        op => <<"release_hold">>, object_key => ObjectKey
    }),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_asset_retention_logic:governance_tx(C, Ctx, #{
            op => <<"purge">>, object_key => ObjectKey
        })
    ),
    %% 未持有时再 release 拒
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_asset_retention_logic:governance_tx(C, Ctx, #{
            op => <<"release_hold">>, object_key => ObjectKey
        })
    ),
    %% ③ 延长留存（合法延长）后仍不可 purge
    {ok, Ext} = enterprise_asset_retention_logic:governance_tx(C, Ctx, #{
        op => <<"set_retention">>, object_key => ObjectKey, retention_days => 400
    }),
    ?assertEqual(400, maps:get(<<"retention_days">>, Ext)),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_asset_retention_logic:governance_tx(C, Ctx, #{
            op => <<"purge">>, object_key => ObjectKey
        })
    ),
    %% ④ 留存窗口参数校验（0 / 超上限一律拒）
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_asset_retention_logic:governance_tx(C, Ctx, #{
            op => <<"set_retention">>, object_key => ObjectKey, retention_days => 0
        })
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_asset_retention_logic:governance_tx(C, Ctx, #{
            op => <<"set_retention">>, object_key => ObjectKey, retention_days => 99999
        })
    ),
    %% ⑤ 第二个附件：留存已到期（直写治理行，避开「只可延长」守卫）-> purge 成功
    DueKey = <<"eoa/992101/1/20260922/file_full02_due/b.pdf">>,
    DueAtt = insert_attachment_row(C, State, DueKey),
    ok = exec_params(
        C,
        <<
            "INSERT INTO enterprise_attachment_retention"
            " (attachment_id, organization_id, application_id, retention_until)"
            " VALUES ($1, $2, $3, NOW() - interval '1 hour')"
        >>,
        [DueAtt, OrgId, AppId]
    ),
    ?assertEqual(true, element(2, enterprise_attachment_retention_repo:is_purgeable_tx(C, DueAtt))),
    with_oss([{delete_object, 2, fun(_B, _K) -> ok end}], fun() ->
        {ok, Purged} = enterprise_asset_retention_logic:governance_tx(C, Ctx, #{
            op => <<"purge">>, object_key => DueKey
        }),
        ?assertEqual(true, maps:get(<<"purged">>, Purged)),
        ?assertEqual(1, meck:num_calls(elib_oss, delete_object, 2))
    end),
    ?assertEqual(
        true,
        scalar(
            C,
            <<
                "SELECT purged_at IS NOT NULL FROM enterprise_attachment_retention"
                " WHERE attachment_id = $1"
            >>,
            [DueAtt]
        )
    ),
    %% ⑥ 重复 purge：already_purged；purged 终态上不可再 hold
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_asset_retention_logic:governance_tx(C, Ctx, #{
            op => <<"purge">>, object_key => DueKey
        })
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_asset_retention_logic:governance_tx(C, Ctx, #{
            op => <<"hold">>, object_key => DueKey, reason => <<"late"/utf8>>
        })
    ),
    %% ⑦ 跨 org/app 的 object_key 不可治理（不泄露存在性）
    ?assertMatch(
        {error, {<<"resource_not_found">>, _}},
        enterprise_asset_retention_logic:governance_tx(C, ctx_b(State), #{
            op => <<"hold">>, object_key => ObjectKey, reason => <<"x"/utf8>>
        })
    ),
    %% ⑧ op 非法 / 缺 object_key / hold 缺 reason
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_asset_retention_logic:governance_tx(C, Ctx, #{
            op => <<"bogus">>, object_key => ObjectKey
        })
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_asset_retention_logic:governance_tx(C, Ctx, #{op => <<"hold">>})
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_asset_retention_logic:governance_tx(C, Ctx, #{
            op => <<"hold">>, object_key => ObjectKey, reason => <<>>
        })
    ).

%% @doc retention/hold/purge 的 **DB 层**不变量（绕过应用层直写，证明不是
%% 「只有应用层说不行」）。
asset_retention_db(C, State) ->
    OrgId = ?ORG_A,
    AppId = maps:get(app_a, State),
    ObjectKey = <<"eoa/992101/1/20260922/file_full02_db/a.pdf">>,
    AttId = insert_attachment_row(C, State, ObjectKey),
    ok = exec_params(
        C,
        <<
            "INSERT INTO enterprise_attachment_retention"
            " (attachment_id, organization_id, application_id, retention_until, hold_state,"
            "  hold_reason, hold_set_at, purge_state)"
            " VALUES ($1, $2, $3, NOW() + interval '1 day', 'held', 'hold', NOW(), 'live')"
        >>,
        [AttId, OrgId, AppId]
    ),
    %% ① hold 生效中 purge -> 23514
    ?assertEqual(
        <<"23514">>,
        rejected_code(
            C,
            <<
                "UPDATE enterprise_attachment_retention SET purge_state = 'purged', purged_at = NOW()"
                " WHERE attachment_id = $1"
            >>,
            [AttId]
        )
    ),
    ok = exec_params(
        C,
        <<
            "UPDATE enterprise_attachment_retention SET hold_state = 'none', hold_reason = NULL,"
            " hold_set_at = NULL WHERE attachment_id = $1"
        >>,
        [AttId]
    ),
    %% ② 留存未到期 purge -> 23514
    ?assertEqual(
        <<"23514">>,
        rejected_code(
            C,
            <<
                "UPDATE enterprise_attachment_retention SET purge_state = 'purged', purged_at = NOW()"
                " WHERE attachment_id = $1"
            >>,
            [AttId]
        )
    ),
    %% ③ 留存只可延长 -> 缩短 23514
    ?assertEqual(
        <<"23514">>,
        rejected_code(
            C,
            <<
                "UPDATE enterprise_attachment_retention SET retention_until = NOW()"
                " WHERE attachment_id = $1"
            >>,
            [AttId]
        )
    ),
    %% ④ 治理行禁删
    ?assertEqual(
        <<"23514">>,
        rejected_code(
            C,
            <<"DELETE FROM enterprise_attachment_retention WHERE attachment_id = $1">>,
            [AttId]
        )
    ),
    %% ⑤ hold 一致性 CHECK（held 必须带 reason/时间）
    ?assertEqual(
        <<"23514">>,
        rejected_code(
            C,
            <<
                "UPDATE enterprise_attachment_retention SET hold_state = 'held'"
                " WHERE attachment_id = $1"
            >>,
            [AttId]
        )
    ),
    %% ⑥ 跨 Org 复合 FK（换一个「尚无治理行」的 attachment_id，避免先撞 PK）
    OtherAtt = AttId + 1,
    ?assertEqual(
        <<"23503">>,
        rejected_code(
            C,
            <<
                "INSERT INTO enterprise_attachment_retention"
                " (attachment_id, organization_id, application_id, retention_until)"
                " VALUES ($1, $2, $3, NOW())"
            >>,
            [OtherAtt, 999999, AppId]
        )
    ),
    %% ⑦ 到期后允许 purge（正例，证明前面的红不是恒红；另起一行——留存只可延长，
    %% 第一行不可再缩短，故用「登记即到期」的第二行做正例）
    DueAtt = OtherAtt + 1,
    DueKey = <<"eoa/992101/1/20260922/file_full02_db_due/b.pdf">>,
    DueKey = DueKey,
    _ = insert_attachment_row_at(C, State, DueKey, DueAtt),
    ok = exec_params(
        C,
        <<
            "INSERT INTO enterprise_attachment_retention"
            " (attachment_id, organization_id, application_id, retention_until)"
            " VALUES ($1, $2, $3, NOW() - interval '1 hour')"
        >>,
        [DueAtt, OrgId, AppId]
    ),
    ok = exec_params(
        C,
        <<
            "UPDATE enterprise_attachment_retention SET purge_state = 'purged', purged_at = NOW()"
            " WHERE attachment_id = $1"
        >>,
        [DueAtt]
    ),
    ?assertEqual(
        <<"purged">>,
        scalar(
            C,
            <<"SELECT purge_state FROM enterprise_attachment_retention WHERE attachment_id = $1">>,
            [DueAtt]
        )
    ),
    %% ⑧ purged 终态不可回退
    ?assertEqual(
        <<"23514">>,
        rejected_code(
            C,
            <<
                "UPDATE enterprise_attachment_retention SET purge_state = 'live', purged_at = NULL"
                " WHERE attachment_id = $1"
            >>,
            [DueAtt]
        )
    ).

%% @doc confirm 与留存登记同事务原子：attachment 行 + 治理行 + 计量同时落库。
asset_confirm(C, State) ->
    Ctx = ctx_unmanaged(State),
    FileName = <<"atomic.pdf">>,
    {ok, P} = enterprise_asset_logic:presign_tx(C, Ctx, #{
        file_name => FileName, mime_type => <<"application/pdf">>
    }),
    ObjectKey = maps:get(<<"object_key">>, P),
    with_oss(
        [
            {head_object, 2, fun(_B, _K) ->
                {ok, #{size => 2048, content_type => <<"application/pdf">>}}
            end}
        ],
        fun() ->
            {ok, Confirmed} = enterprise_asset_logic:confirm_tx(C, Ctx, #{
                object_key => ObjectKey
            }),
            AttId = maps:get(<<"file_id">>, Confirmed),
            ?assertEqual(2048, maps:get(<<"size">>, Confirmed)),
            %% 治理行与附件行同时存在
            ?assertEqual(
                1,
                scalar(
                    C,
                    <<
                        "SELECT COUNT(*) FROM enterprise_attachment_retention"
                        " WHERE attachment_id = $1 AND purge_state = 'live'"
                    >>,
                    [AttId]
                )
            ),
            ?assertEqual(
                <<"enterprise">>,
                scalar(C, <<"SELECT scope FROM attachment WHERE id = $1">>, [AttId])
            ),
            %% 聚合计量 +1（不含正文）
            ?assertEqual(
                1,
                scalar(
                    C,
                    <<
                        "SELECT counter FROM enterprise_application_usage"
                        " WHERE organization_id = $1 AND application_id = $2"
                        " AND metric = 'file.confirmed'"
                    >>,
                    [?ORG_A, maps:get(app_a, State)]
                )
            )
        end
    ).

%% @doc message 与附件原子绑定：引用未 confirm 的对象 -> 全体回滚
%% （msg / origin / audit 三表皆无痕迹）。
msg_file_binding(C, State) ->
    Ctx = ctx_unmanaged(State),
    Input = #{
        sender_mode => <<"human">>,
        sender_user_id => ?EXT_A1,
        recipient_user_id => ?EXT_A2,
        msg_type => <<"file">>,
        object_key => <<"eoa/992101/1/20990101/never/file.pdf">>
    },
    ?assertMatch(
        {error, {<<"resource_not_found">>, _}},
        enterprise_message_logic:direct_tx(C, Ctx, Input)
    ),
    ?assertEqual(
        0,
        scalar(
            C,
            <<"SELECT COUNT(*) FROM enterprise_message_origin WHERE organization_id = $1">>,
            [?ORG_A]
        )
    ),
    %% 已 confirm 的文件：msg + origin + audit 同事务落库，且 payload.file 与
    %% attachment 行一致（原子绑定正例）。
    ObjectKey = <<"eoa/992101/1/20260922/file_full02_bind/b.pdf">>,
    AttId = insert_attachment_row(C, State, ObjectKey),
    ok = enterprise_asset_retention_logic:register_retention_tx(
        C, {AttId, ?ORG_A, maps:get(app_a, State)}
    ),
    {ok, Result} = enterprise_message_logic:direct_tx(
        C, Ctx, Input#{object_key => ObjectKey}
    ),
    MsgId = maps:get(<<"msg_id">>, Result),
    Payload = jsone:decode(
        scalar(C, <<"SELECT payload FROM msg_c2c WHERE msg_id = $1">>, [MsgId])
    ),
    ?assertEqual(AttId, maps:get(<<"file_id">>, maps:get(<<"file">>, Payload))),
    ?assertEqual(
        1,
        scalar(
            C,
            <<"SELECT COUNT(*) FROM enterprise_message_origin WHERE msg_id = $1">>,
            [MsgId]
        )
    ).

%%%===================================================================
%%% ④ 企业托管消息 origin
%%%===================================================================

msg_origin(C, State) ->
    Ctx = ctx_unmanaged(State),
    AppId = maps:get(app_a, State),
    %% direct + human sender
    {ok, D} = enterprise_message_logic:direct_tx(C, Ctx, #{
        sender_mode => <<"human">>,
        sender_user_id => ?EXT_A1,
        recipient_user_id => ?EXT_A2,
        msg_type => <<"text">>,
        content => <<"hi"/utf8>>
    }),
    MsgId = maps:get(<<"msg_id">>, D),
    Origin = one(
        C,
        <<
            "SELECT conversation_kind, organization_id, application_id, sender_kind,"
            " sender_user_id, non_e2ee FROM enterprise_message_origin WHERE msg_id = $1"
        >>,
        [MsgId]
    ),
    %% 双痕迹：Application origin（非空）+ Human sender 同时存在
    ?assertEqual(<<"direct">>, maps:get(<<"conversation_kind">>, Origin)),
    ?assertEqual(?ORG_A, maps:get(<<"organization_id">>, Origin)),
    ?assertEqual(AppId, maps:get(<<"application_id">>, Origin)),
    ?assertEqual(<<"human">>, maps:get(<<"sender_kind">>, Origin)),
    ?assertEqual(?H_A1, maps:get(<<"sender_user_id">>, Origin)),
    ?assertEqual(true, maps:get(<<"non_e2ee">>, Origin)),
    %% 消息主体非 E2EE + 审计 actor 是 Application
    ?assertEqual(
        null,
        scalar(C, <<"SELECT e2ee FROM msg_c2c WHERE msg_id = $1">>, [MsgId])
    ),
    ?assertEqual(
        AppId,
        scalar(
            C,
            <<
                "SELECT (detail->>'origin_application_id')::bigint FROM enterprise_audit_event"
                " WHERE action = 'message.enterprise.accepted' AND detail->>'msg_id' = $1"
            >>,
            [MsgId]
        )
    ),
    %% group + human sender
    Gid = create_group(C, State, ?WS_A1, [?EXT_A1, ?EXT_A2]),
    {ok, G} = enterprise_message_logic:group_tx(C, Ctx, Gid, #{
        sender_mode => <<"human">>,
        sender_user_id => ?EXT_A2,
        msg_type => <<"text">>,
        content => <<"group hi"/utf8>>
    }),
    GMsgId = maps:get(<<"msg_id">>, G),
    GOrigin = one(
        C,
        <<
            "SELECT conversation_kind, application_id, sender_kind, sender_user_id"
            " FROM enterprise_message_origin WHERE msg_id = $1"
        >>,
        [GMsgId]
    ),
    ?assertEqual(<<"group">>, maps:get(<<"conversation_kind">>, GOrigin)),
    ?assertEqual(AppId, maps:get(<<"application_id">>, GOrigin)),
    ?assertEqual(<<"human">>, maps:get(<<"sender_kind">>, GOrigin)),
    ?assertEqual(?H_A2, maps:get(<<"sender_user_id">>, GOrigin)),
    ?assertEqual(
        null,
        scalar(C, <<"SELECT e2ee FROM msg_c2g WHERE msg_id = $1">>, [GMsgId])
    ),
    %% application 模式：无 Human sender，但 Application origin 仍在
    {ok, A} = enterprise_message_logic:direct_tx(C, Ctx, #{
        sender_mode => <<"application">>,
        recipient_user_id => ?EXT_A3,
        msg_type => <<"text">>,
        content => <<"app hi"/utf8>>
    }),
    AMsgId = maps:get(<<"msg_id">>, A),
    AOrigin = one(
        C,
        <<
            "SELECT application_id, sender_kind, sender_user_id FROM enterprise_message_origin"
            " WHERE msg_id = $1"
        >>,
        [AMsgId]
    ),
    ?assertEqual(AppId, maps:get(<<"application_id">>, AOrigin)),
    ?assertEqual(<<"application">>, maps:get(<<"sender_kind">>, AOrigin)),
    ?assertEqual(null, maps:get(<<"sender_user_id">>, AOrigin)),
    %% 计量：message.accepted 计 3
    ?assertEqual(
        3,
        scalar(
            C,
            <<
                "SELECT counter FROM enterprise_application_usage"
                " WHERE organization_id = $1 AND application_id = $2"
                " AND metric = 'message.accepted'"
            >>,
            [?ORG_A, AppId]
        )
    ).

%% @doc origin 账本的 DB 层不变量：application_id 非空、human 必带 sender、
%% non_e2ee 恒真、行禁删、跨 Org 复合 FK。
msg_origin_db(C, State) ->
    AppId = maps:get(app_a, State),
    %% application_id 是 NOT NULL 列（真实 Application origin 永不缺失）：显式
    %% NULL 被 NOT NULL 约束先拒（23502）——语义同为「不可能只留 Human 痕迹」
    ?assertEqual(
        <<"23502">>,
        rejected_code(
            C,
            <<
                "INSERT INTO enterprise_message_origin"
                " (id, conversation_kind, organization_id, application_id, sender_kind,"
                "  msg_row_id, msg_id) VALUES (992900001, 'direct', $1, NULL, 'application', 1, 'x')"
            >>,
            [?ORG_A]
        )
    ),
    ?assertEqual(
        <<"23514">>,
        rejected_code(
            C,
            <<
                "INSERT INTO enterprise_message_origin"
                " (id, conversation_kind, organization_id, application_id, sender_kind,"
                "  msg_row_id, msg_id) VALUES (992900002, 'direct', $1, $2, 'human', 1, 'x')"
            >>,
            [
                ?ORG_A, AppId
            ]
        )
    ),
    ?assertEqual(
        <<"23514">>,
        rejected_code(
            C,
            <<
                "INSERT INTO enterprise_message_origin"
                " (id, conversation_kind, organization_id, application_id, sender_kind,"
                "  sender_user_id, non_e2ee, msg_row_id, msg_id)"
                " VALUES (992900003, 'group', $1, $2, 'human', $3, false, 1, 'x')"
            >>,
            [
                ?ORG_A, AppId, ?H_A1
            ]
        )
    ),
    ?assertEqual(
        <<"23514">>,
        rejected_code(
            C,
            <<
                "INSERT INTO enterprise_message_origin"
                " (id, conversation_kind, organization_id, application_id, sender_kind,"
                "  msg_row_id, msg_id) VALUES (992900004, 'channel', $1, $2, 'application', 1, 'x')"
            >>,
            [
                ?ORG_A, AppId
            ]
        )
    ),
    ?assertEqual(
        <<"23503">>,
        rejected_code(
            C,
            <<
                "INSERT INTO enterprise_message_origin"
                " (id, conversation_kind, organization_id, application_id, sender_kind,"
                "  msg_row_id, msg_id) VALUES (992900005, 'direct', 999999, $1, 'application', 1, 'x')"
            >>,
            [
                AppId
            ]
        )
    ),
    %% 正例 + 禁删
    ok = exec_params(
        C,
        <<
            "INSERT INTO enterprise_message_origin"
            " (id, conversation_kind, organization_id, application_id, sender_kind,"
            "  sender_user_id, msg_row_id, msg_id)"
            " VALUES (992900006, 'group', $1, $2, 'human', $3, 1, 'x')"
        >>,
        [?ORG_A, AppId, ?H_A2]
    ),
    ?assertEqual(
        <<"23514">>,
        rejected_code(C, <<"DELETE FROM enterprise_message_origin WHERE id = 992900006">>, [])
    ).

%%%===================================================================
%%% ⑤ FULL-01 Grant 边界接线
%%%===================================================================

%% V2.1 D4（plan §5.2 / F-09 / D-06）：零 Grant（grant_governed=false）不再有
%% allowed_scopes 回退旁路——生效 scope 恒为空集，org/workspace/list 类路由
%% 一律 insufficient_scope；仅 kind=none（application self 面，认证链 scope
%% gate 在中间件层）不在此判定内。
bnd_unmanaged(C, State) ->
    Ctx = ctx_managed(C, State),
    ?assertEqual(false, maps:get(grant_governed, Ctx)),
    ?assertEqual([], maps:get(granted_scopes, Ctx)),
    ?assertEqual(
        {error, insufficient_scope},
        enterprise_internal_boundary:enforce(C, Ctx, <<"INT-02">>, undefined)
    ),
    ?assertEqual(
        {error, insufficient_scope},
        enterprise_internal_boundary:enforce(C, Ctx, <<"INT-07">>, undefined)
    ),
    ?assertEqual(
        {error, insufficient_scope},
        enterprise_internal_boundary:enforce(C, Ctx, <<"INT-05">>, ?WS_A2)
    ),
    %% INT-24/26（kind=list）同样拒绝（scope 不在生效集）
    ?assertEqual(
        {error, insufficient_scope},
        enterprise_internal_boundary:enforce(C, Ctx, <<"INT-24">>, undefined)
    ),
    ?assertEqual(
        {error, insufficient_scope},
        enterprise_internal_boundary:enforce(C, Ctx, <<"INT-26">>, undefined)
    ).

bnd_org_grant(C, State) ->
    {ok, Grant} = issue_org_grant(C, State, [<<"identities:read">>], <<"k-org-read">>),
    ?assertEqual(1, maps:get(<<"version">>, Grant)),
    Ctx = ctx_managed(C, State),
    ?assertEqual(true, maps:get(grant_governed, Ctx)),
    %% 交集：Grant 只给 identities:read -> 生效 scope 仅此一项
    ?assertEqual([<<"identities:read">>], maps:get(granted_scopes, Ctx)),
    ?assertEqual(ok, enterprise_internal_boundary:enforce(C, Ctx, <<"INT-16">>, undefined)),
    ?assertEqual(ok, enterprise_internal_boundary:enforce(C, Ctx, <<"INT-03">>, undefined)),
    %% 未授予 scope：insufficient_scope（不因 allowed_scopes 里有就放行）
    ?assertEqual(
        {error, insufficient_scope},
        enterprise_internal_boundary:enforce(C, Ctx, <<"INT-15">>, undefined)
    ),
    ?assertEqual(
        {error, insufficient_scope},
        enterprise_internal_boundary:enforce(C, Ctx, <<"INT-07">>, undefined)
    ),
    %% workspace 类路由：scope 未授予 -> insufficient_scope（org 全域 Grant
    %% 不会把未授予的 scope 变成可用；workspace 覆盖判定只在 scope 通过后才发生）
    ?assertEqual(
        {error, insufficient_scope},
        enterprise_internal_boundary:enforce(C, Ctx, <<"INT-05">>, ?WS_A2)
    ).

bnd_ws_not_org(C, State) ->
    {ok, _} = issue_ws_grant(C, State, [<<"files:write">>], [?WS_A1], <<"k-ws-files">>),
    Ctx = ctx_managed(C, State),
    ?assertEqual([<<"files:write">>], maps:get(granted_scopes, Ctx)),
    %% scope 在生效集内，但显式 Workspace Grant **不**覆盖 org 级路由 -> 边界违规
    ?assertEqual(
        {error, organization_boundary_violation},
        enterprise_internal_boundary:enforce(C, Ctx, <<"INT-07">>, undefined)
    ),
    ?assertEqual(
        {error, organization_boundary_violation},
        enterprise_internal_boundary:enforce(C, Ctx, <<"INT-22">>, undefined)
    ),
    %% 该 workspace 上的 workspace 级路由也不通（scope 是 files:write 不是 groups:write）
    ?assertEqual(
        {error, insufficient_scope},
        enterprise_internal_boundary:enforce(C, Ctx, <<"INT-05">>, ?WS_A1)
    ).

bnd_ws_scope(C, State) ->
    %% V2.1：INT-18 读操作降为 groups:read（§6.2/§7）——Grant 需同时覆盖
    %% groups:read 与 groups:write 才能让 INT-05 与 INT-18 同时通过。
    {ok, _} = issue_ws_grant(
        C, State, [<<"groups:read">>, <<"groups:write">>], [?WS_A1], <<"k-ws-groups">>
    ),
    Ctx = ctx_managed(C, State),
    ?assertEqual(
        lists:sort([<<"groups:read">>, <<"groups:write">>]), maps:get(granted_scopes, Ctx)
    ),
    ?assertEqual(ok, enterprise_internal_boundary:enforce(C, Ctx, <<"INT-05">>, ?WS_A1)),
    ?assertEqual(ok, enterprise_internal_boundary:enforce(C, Ctx, <<"INT-18">>, ?WS_A1)),
    %% V2.1 新增只读面（workspace 边界同 Grant）
    ?assertEqual(ok, enterprise_internal_boundary:enforce(C, Ctx, <<"INT-27">>, ?WS_A1)),
    %% 同 Org 内**其他** workspace：organization_boundary_violation（不是 insufficient_scope）
    ?assertEqual(
        {error, organization_boundary_violation},
        enterprise_internal_boundary:enforce(C, Ctx, <<"INT-05">>, ?WS_A2)
    ),
    %% 跨 Org workspace 同样拒（org 全域 Grant 不会成为跨租户旁路——此处连
    %% workspace 归属都不在本 Org 视图内）
    ?assertEqual(
        {error, organization_boundary_violation},
        enterprise_internal_boundary:enforce(C, Ctx, <<"INT-05">>, ?WS_B1)
    ).

bnd_revoke(C, State) ->
    {ok, Grant} = issue_org_grant(C, State, [<<"identities:read">>], <<"k-revoke">>),
    Ctx1 = ctx_managed(C, State),
    ?assertEqual(ok, enterprise_internal_boundary:enforce(C, Ctx1, <<"INT-16">>, undefined)),
    %% 撤权（CAS）→ 生效 scope 立刻为空 → 下一请求即失败
    ok = enterprise_internal_ops:revoke_grant_tx(
        C, ?ORG_A, maps:get(app_a, State), maps:get(<<"id">>, Grant), 1, ?OWNER_A
    ),
    Ctx2 = ctx_managed(C, State),
    ?assertEqual(true, maps:get(grant_governed, Ctx2)),
    ?assertEqual([], maps:get(granted_scopes, Ctx2)),
    ?assertEqual(
        {error, insufficient_scope},
        enterprise_internal_boundary:enforce(C, Ctx2, <<"INT-16">>, undefined)
    ),
    ?assertEqual(
        {error, insufficient_scope},
        enterprise_internal_boundary:enforce(C, Ctx2, <<"INT-05">>, ?WS_A1)
    ).

bnd_downgrade(C, State) ->
    {ok, Grant} = issue_org_grant(
        C, State, [<<"identities:read">>, <<"identities:write">>], <<"k-downgrade">>
    ),
    Ctx1 = ctx_managed(C, State),
    ?assertEqual(ok, enterprise_internal_boundary:enforce(C, Ctx1, <<"INT-16">>, undefined)),
    ?assertEqual(ok, enterprise_internal_boundary:enforce(C, Ctx1, <<"INT-15">>, undefined)),
    %% 降级：移除 identities:read（CAS 版本 +1）
    ok = enterprise_internal_ops:set_grant_scopes_tx(
        C, ?ORG_A, maps:get(app_a, State), maps:get(<<"id">>, Grant), 1, [<<"identities:write">>]
    ),
    Ctx2 = ctx_managed(C, State),
    ?assertEqual([<<"identities:write">>], maps:get(granted_scopes, Ctx2)),
    %% 被移除的 scope 下一请求即失败；保留的 scope 仍放行
    ?assertEqual(
        {error, insufficient_scope},
        enterprise_internal_boundary:enforce(C, Ctx2, <<"INT-16">>, undefined)
    ),
    ?assertEqual(ok, enterprise_internal_boundary:enforce(C, Ctx2, <<"INT-15">>, undefined)).

bnd_fail_closed(C, State) ->
    Ctx = ctx_unmanaged(State),
    %% 未登记路由 fail-closed
    ?assertEqual(
        {error, security_gate_closed},
        enterprise_internal_boundary:enforce(C, Ctx, <<"INT-99">>, undefined)
    ),
    ?assertEqual(
        {error, security_gate_closed},
        enterprise_internal_boundary:enforce(C, Ctx, <<"">>, undefined)
    ),
    %% 动态 scope 路由走静态入口：接线错误 -> fail-closed
    ?assertEqual(
        {error, security_gate_closed},
        enterprise_internal_boundary:enforce(C, Ctx, <<"INT-09">>, undefined)
    ),
    %% workspace 类路由缺 workspace 上下文 -> fail-closed
    ?assertEqual(
        {error, security_gate_closed},
        enterprise_internal_boundary:enforce(C, Ctx, <<"INT-05">>, undefined)
    ),
    ?assertEqual(
        {error, security_gate_closed},
        enterprise_internal_boundary:enforce(C, Ctx, <<"INT-05">>, <<"not-int">>)
    ),
    %% 畸形 ctx（缺 grant_governed / granted_scopes）：fail-closed，绝不 no-op
    ?assertEqual(
        {error, security_gate_closed},
        enterprise_internal_boundary:enforce(
            C, #{organization_id => ?ORG_A, application_id => 1}, <<"INT-16">>, undefined
        )
    ),
    ?assertEqual(
        {error, security_gate_closed},
        enterprise_internal_boundary:enforce(
            C,
            #{organization_id => ?ORG_A, application_id => 1, grant_governed => <<"yes">>},
            <<"INT-16">>,
            undefined
        )
    ),
    %% 非 map ctx
    ?assertEqual(
        {error, security_gate_closed},
        enterprise_internal_boundary:enforce(C, not_a_map, <<"INT-16">>, undefined)
    ),
    %% 动态 scope 单点映射（handler 与 message logic 共用）
    ?assertEqual(
        {ok, <<"messages:send_as_human">>},
        enterprise_internal_boundary:required_scope_for_sender_mode(<<"human">>)
    ),
    ?assertEqual(
        {ok, <<"messages:send">>},
        enterprise_internal_boundary:required_scope_for_sender_mode(<<"application">>)
    ),
    ?assertEqual(error, enterprise_internal_boundary:required_scope_for_sender_mode(<<"bot">>)),
    ?assertEqual(
        {error, security_gate_closed},
        enterprise_internal_boundary:enforce_dynamic(
            C, ctx_managed(C, State), <<"INT-16">>, undefined, <<"identities:read">>
        )
    ).

%% @doc 机械对齐：边界 spec 覆盖**全部**冻结路由（少一条即红）；kind 与 manifest
%% 的 grant 语义逐条一致。INT-15..22 已由 A0 在 FULL-02 集成时登记进冻结表与
%% Router（并进 manifest/契约），故两侧都必须是 22 条全集。
bnd_spec_align_test() ->
    ?_test(begin
        FrozenIds = lists:sort([maps:get(id, R) || R <- enterprise_internal_routes:routes()]),
        ?assertEqual(
            FrozenIds,
            lists:sort(enterprise_internal_boundary:frozen_ids())
        ),
        %% kind 与 manifest grant 语义逐条对照（冻结表内）
        Expected = [
            {<<"INT-01">>, none},
            {<<"INT-02">>, org},
            {<<"INT-03">>, org},
            {<<"INT-04">>, workspace},
            {<<"INT-05">>, workspace},
            {<<"INT-06">>, workspace},
            {<<"INT-07">>, org},
            {<<"INT-08">>, org},
            {<<"INT-09">>, org},
            {<<"INT-10">>, workspace},
            {<<"INT-11">>, org},
            %% FULL-02 新增（A0 接线后进冻结表）
            {<<"INT-15">>, org},
            {<<"INT-16">>, org},
            {<<"INT-17">>, org},
            {<<"INT-18">>, workspace},
            {<<"INT-19">>, workspace},
            {<<"INT-20">>, workspace},
            {<<"INT-21">>, workspace},
            {<<"INT-22">>, org},
            {<<"INT-12">>, none},
            {<<"INT-13">>, none},
            {<<"INT-14">>, none}
        ],
        lists:foreach(
            fun({Id, Kind}) ->
                {ok, Spec} = enterprise_internal_boundary:spec(Id),
                ?assertEqual({Id, Kind}, {Id, maps:get(kind, Spec)})
            end,
            Expected
        ),
        %% 每条 spec 的 scope 必须是固定枚举成员或 dynamic
        lists:foreach(
            fun(Id) ->
                {ok, Spec} = enterprise_internal_boundary:spec(Id),
                case maps:get(scope, Spec) of
                    dynamic ->
                        ok;
                    Scope ->
                        ?assertEqual(
                            ok,
                            enterprise_internal_scope:authorize(Scope, [Scope])
                        )
                end
            end,
            enterprise_internal_boundary:ids()
        ),
        %% FULL-02 新增 id 已由 A0 在集成时登记进冻结表 + Router + manifest +
        %% 契约，因此边界 ids 与冻结表必须**完全相等**，不存在「待接线」差集；
        %% 若将来仍有未接线新增，此断言会立即变红（差集恒为空）。
        ?assertEqual([], lists:sort(enterprise_internal_boundary:ids()) -- FrozenIds),
        ?assertEqual(31, length(lists:usort(enterprise_internal_boundary:ids())))
    end).

%% @doc **接线点机械断言**：handler 模块的 beam 抽象码里必须真实存在对
%% enterprise_internal_boundary 的调用（不是文档承诺，而是编译产物事实）。
bnd_handler_wiring_test() ->
    ?_test(begin
        Handlers = [
            {enterprise_identity_handler, enforce},
            {enterprise_group_handler, enforce},
            {enterprise_asset_handler, enforce},
            {enterprise_message_handler, enforce_dynamic},
            {enterprise_directory_handler, enforce}
        ],
        lists:foreach(
            fun({Mod, Fun}) ->
                Calls = boundary_calls(Mod),
                case lists:member(Fun, Calls) of
                    true -> ok;
                    false -> erlang:error({handler_not_wired, Mod, Fun, Calls})
                end
            end,
            Handlers
        ),
        %% message handler 既用动态边界，也读 sender_mode 单点映射
        ?assert(
            lists:member(
                {enterprise_internal_boundary, required_scope_for_sender_mode},
                collect_remote(enterprise_message_handler)
            )
        )
    end).

boundary_calls(Mod) ->
    Remote = collect_remote(Mod),
    lists:usort([F || {enterprise_internal_boundary, F} <- Remote]).

collect_remote(Mod) ->
    Beam = code:which(Mod),
    ?assert(is_binary(Beam) orelse is_list(Beam)),
    case beam_lib:chunks(Beam, [abstract_code]) of
        {ok, {_, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            lists:usort(collect_calls(Forms, []));
        {ok, {_, [{abstract_code, no_abstract_code}]}} ->
            erlang:error({no_debug_info, Mod})
    end.

collect_calls(Term, Acc) when is_tuple(Term) ->
    Acc1 =
        case Term of
            {call, _, {remote, _, {atom, _, M}, {atom, _, F}}, _Args} -> [{M, F} | Acc];
            _ -> Acc
        end,
    lists:foldl(fun collect_calls/2, Acc1, tuple_to_list(Term));
collect_calls(List, Acc) when is_list(List) ->
    lists:foldl(fun collect_calls/2, Acc, List);
collect_calls(_Other, Acc) ->
    Acc.

%%%===================================================================
%%% ⑥ 聚合计量
%%%===================================================================

usage_shape_test() ->
    ?_test(begin
        %% metric 枚举是固定集合（与 DB CHECK 同源）
        ?assertEqual(
            6,
            length(enterprise_application_usage_repo:metrics())
        ),
        %% 固定枚举里没有任何「正文/内容」类自由文本键
        lists:foreach(
            fun(M) ->
                ?assertEqual(
                    nomatch,
                    binary:match(M, [<<"content">>, <<"body">>, <<"text">>, <<"pii">>])
                )
            end,
            enterprise_application_usage_repo:metrics()
        )
    end).

usage_meter(C, State) ->
    OrgId = ?ORG_A,
    AppId = maps:get(app_a, State),
    %% 未知 metric 拒（不落自由文本）
    ?assertEqual(
        {error, invalid_metric},
        enterprise_application_usage_repo:bump_tx(C, OrgId, AppId, <<"freedom.text">>)
    ),
    %% 同月累计（幂等 upsert）
    ok = enterprise_application_usage_repo:bump_tx(C, OrgId, AppId, <<"message.accepted">>),
    ok = enterprise_application_usage_repo:bump_tx(C, OrgId, AppId, <<"message.accepted">>),
    ok = enterprise_application_usage_repo:bump_tx(C, OrgId, AppId, <<"message.accepted">>, 3),
    ?assertEqual(
        5,
        scalar(
            C,
            <<
                "SELECT counter FROM enterprise_application_usage"
                " WHERE organization_id = $1 AND application_id = $2"
                " AND metric = 'message.accepted'"
            >>,
            [OrgId, AppId]
        )
    ),
    %% 列集封闭（无正文/PII 列）+ 计量行禁删
    Cols = [
        maps:get(<<"column_name">>, R)
     || R <-
            element(
                2,
                elib_pg:query(
                    C,
                    <<
                        "SELECT column_name FROM information_schema.columns"
                        " WHERE table_name = 'enterprise_application_usage'"
                    >>,
                    []
                )
            )
    ],
    ?assertEqual(
        [
            <<"application_id">>,
            <<"counter">>,
            <<"metric">>,
            <<"organization_id">>,
            <<"period_start">>,
            <<"updated_at">>
        ],
        lists:sort(Cols)
    ),
    ?assertEqual(
        <<"23514">>,
        rejected_code(
            C,
            <<"DELETE FROM enterprise_application_usage WHERE organization_id = $1">>,
            [OrgId]
        )
    ).

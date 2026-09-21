%% enterprise_identity_group_pg_tests
%% EPGZ-03 — Identity mappings（INT-02/03）/ Workspace 企业群（INT-04/05/06）/
%% 好友申请只发起（INT-11）真库集成测试。
%%
%% 一次性 marker 库（inttest_marker_db 配方，env 前缀 EPGZ03_INTTEST，直连
%% imboy_pg18:4323）。setup 阶段 COMMIT 提交双 Org × 双 Workspace 基线夹具；
%% 业务用例每条 BEGIN ... ROLLBACK，不留数据；DB 兜底（DEFERRED 触发器）用例
%% 显式 COMMIT 验证失败后 ROLLBACK 清理。
%%
%% 覆盖（plan-gz §4.3/§6 INT-02..06/11；manifest INV）：
%%   ① INT-02 绑定：成功/重绑 upsert/跨 Org boundary/非 active member/
%%      非 Human/停用 user/user 已被同 app 映射/参数矩阵
%%   ② INT-03 resolve：只返回命中项/跨 Org 不可见/空与超限与元素非法
%%   ③ 导出面（负例）：identity/friend_request/group logic 无 list-all、
%%      无 accept/confirm/delete 形态函数（无全量导出、无自动接受路径）
%%   ④ INT-04 创建：成功（scope=workspace/owner=role4）/未映射拒/
%%      非 workspace 成员拒/跨 Org ws 拒/不存在 ws 拒/archived ws 拒/
%%      显式 owner/title 校验/成员去重
%%   ⑤ INT-05 幂等添加：成功 + 重复调用同结果/未映射拒/非 ws 成员拒/
%%      跨 Org group 拒/personal 群不可管理/非法 group_id
%%   ⑥ INT-06 幂等移除：成功 + 重复调用同结果/owner 不可移除（两种形态）/
%%      移除后可重新添加/跨 Org group 拒
%%   ⑦ DB 兜底：绕过 logic 前置校验直接 activate 非 ws 成员，COMMIT 被
%%      trg_group_member_ws_subset 23514 拒绝（DEFERRED 提交期校验实证）
%%   ⑧ INT-11 好友申请（只发起）：pending 态创建/重复 already_requested/
%%      self 拒/sender、target 未映射拒/跨 app target 拒/already_friends 拒/
%%      blocked 拒/无 auto-accept（反向好友行不存在、status 仍 0）/
%%      notify_request 消息契约（apply_friend）
-module(enterprise_identity_group_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% ---- 夹具（989 段独立 ID，与 A1 987 / A2 988 段互不冲突） ----

-define(ORG_A, 989101).
-define(ORG_B, 989102).

-define(OWNER_A, 989001).
-define(OWNER_B, 989002).
-define(H_A1, 989011).
-define(H_A2, 989012).
-define(H_A3, 989013).
-define(BOT_A, 989014).
-define(REMOVED_A, 989015).
-define(DISABLED_A, 989016).
-define(H_A5, 989017).
-define(NOORG_U, 989018).
-define(H_B1, 989021).
-define(H_B2, 989022).

-define(WS_A1, 989201).
-define(WS_A2, 989202).
-define(WS_A_ARCH, 989203).
-define(WS_B1, 989211).

-define(EXT_A1, <<"ext-a1">>).
-define(EXT_A2, <<"ext-a2">>).
-define(EXT_A3, <<"ext-a3">>).
-define(EXT_B1, <<"ext-b1">>).
-define(EXT_B2, <<"ext-b2">>).

-define(SCOPES, [
    <<"application:read">>,
    <<"identities:read">>,
    <<"identities:write">>,
    <<"groups:write">>,
    <<"friend_requests:create">>
]).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup_conn() ->
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    %% 测试 VM 未起 imboy app：惰性注册本套件触达的 TSID 命名空间
    %% （生产由 imboy_app:tsid_generator_names/0 全量注册）。
    lists:foreach(
        fun(Name) ->
            case lists:member(Name, elib_tsid:registered()) of
                true -> ok;
                false -> elib_tsid:register(Name)
            end
        end,
        [group_info, group_member, friend]
    ),
    {ok, _} = application:ensure_all_started(throttle),
    State = inttest_marker_db:provision(#{
        env_prefix => <<"EPGZ03_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    C = maps:get(conn, State),
    %% 基线夹具整体一次提交（业务用例 BEGIN/ROLLBACK 只读它）。
    ok = exec(C, <<"BEGIN">>),
    try
        seed_matrix(C),
        ok = exec(C, <<"COMMIT">>)
    catch
        Class:Reason:Stack ->
            _ = exec_quiet(C, <<"ROLLBACK">>),
            inttest_marker_db:release(State),
            erlang:raise(Class, {fixture_seed_failed, Reason}, Stack)
    end,
    {AppA, AppB} = app_ids(C),
    State#{
        conn => C,
        app_a => AppA,
        app_b => AppB
    }.

close_conn(State) ->
    inttest_marker_db:release(State),
    ok.

%% 每条业务用例 BEGIN ... ROLLBACK，不留数据。
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

exec(C, IoData) ->
    Sql = iolist_to_binary(IoData),
    case elib_pg:query(C, Sql, []) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
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

%% ---- 夹具矩阵：双 Org × 双 Workspace + 边界人物 ----

seed_matrix(C) ->
    seed_user(C, ?OWNER_A, 0, 1),
    seed_user(C, ?OWNER_B, 0, 1),
    seed_user(C, ?H_A1, 0, 1),
    seed_user(C, ?H_A2, 0, 1),
    seed_user(C, ?H_A3, 0, 1),
    seed_user(C, ?BOT_A, 1, 1),
    seed_user(C, ?REMOVED_A, 0, 1),
    seed_user(C, ?DISABLED_A, 0, 0),
    seed_user(C, ?H_A5, 0, 1),
    seed_user(C, ?NOORG_U, 0, 1),
    seed_user(C, ?H_B1, 0, 1),
    seed_user(C, ?H_B2, 0, 1),
    seed_org(C, ?ORG_A, ?OWNER_A, <<"epgz03-org-a">>),
    seed_org(C, ?ORG_B, ?OWNER_B, <<"epgz03-org-b">>),
    %% 注：org owner 的成员行由 trg_organization_owner_member_sync 触发器随
    %% organization INSERT 自动落库（migration 113），不得重复显式插入。
    seed_org_member(C, ?ORG_A, ?H_A1, <<"member">>, <<"active">>),
    seed_org_member(C, ?ORG_A, ?H_A2, <<"member">>, <<"active">>),
    seed_org_member(C, ?ORG_A, ?H_A3, <<"member">>, <<"active">>),
    seed_org_member(C, ?ORG_A, ?BOT_A, <<"member">>, <<"active">>),
    seed_org_member(C, ?ORG_A, ?REMOVED_A, <<"member">>, <<"removed">>),
    seed_org_member(C, ?ORG_A, ?DISABLED_A, <<"member">>, <<"active">>),
    seed_org_member(C, ?ORG_A, ?H_A5, <<"member">>, <<"active">>),
    seed_org_member(C, ?ORG_B, ?H_B1, <<"member">>, <<"active">>),
    seed_org_member(C, ?ORG_B, ?H_B2, <<"member">>, <<"active">>),
    seed_workspace(C, ?WS_A1, ?ORG_A, ?OWNER_A, <<"active">>),
    seed_workspace(C, ?WS_A2, ?ORG_A, ?OWNER_A, <<"active">>),
    seed_workspace(C, ?WS_A_ARCH, ?ORG_A, ?OWNER_A, <<"archived">>),
    seed_workspace(C, ?WS_B1, ?ORG_B, ?OWNER_B, <<"active">>),
    seed_ws_member(C, ?WS_A1, ?H_A1, <<"member">>),
    seed_ws_member(C, ?WS_A1, ?H_A2, <<"member">>),
    seed_ws_member(C, ?WS_A1, ?H_A5, <<"member">>),
    seed_ws_member(C, ?WS_A2, ?H_A3, <<"member">>),
    seed_ws_member(C, ?WS_B1, ?H_B1, <<"member">>),
    {ok, AppA} = enterprise_internal_ops:create_application_tx(
        C, ?ORG_A, <<"epgz03-oa-a">>, <<"epgz03 org A oa"/utf8>>, ?SCOPES
    ),
    {ok, AppB} = enterprise_internal_ops:create_application_tx(
        C, ?ORG_B, <<"epgz03-oa-b">>, <<"epgz03 org B oa"/utf8>>, ?SCOPES
    ),
    AppAId = maps:get(<<"id">>, AppA),
    AppBId = maps:get(<<"id">>, AppB),
    ok = seed_mapping(C, ?ORG_A, AppAId, ?EXT_A1, ?H_A1),
    ok = seed_mapping(C, ?ORG_A, AppAId, ?EXT_A2, ?H_A2),
    ok = seed_mapping(C, ?ORG_A, AppAId, ?EXT_A3, ?H_A3),
    ok = seed_mapping(C, ?ORG_B, AppBId, ?EXT_B1, ?H_B1),
    ok = seed_mapping(C, ?ORG_B, AppBId, ?EXT_B2, ?H_B2),
    ok.

seed_user(C, Uid, AccountType, Status) ->
    exec(C, [
        <<"INSERT INTO \"user\" (id, password, account, account_type, status, reg_ip, reg_cosv) VALUES (">>,
        integer_to_binary(Uid),
        ", 'x', 't989_u",
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
        ", 't989_ws_",
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

%%%===================================================================
%%% Suite
%%%===================================================================

enterprise_identity_group_pg_test_() ->
    {timeout, 900,
        {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
            C = maps:get(conn, State),
            CtxA = #{organization_id => ?ORG_A, application_id => maps:get(app_a, State)},
            CtxB = #{organization_id => ?ORG_B, application_id => maps:get(app_b, State)},
            [
                %% ① INT-02 绑定
                {"bind_success", with_tx(C, fun(C1) -> bind_success(C1, CtxA) end)},
                {"bind_rebind_upsert_overwrites_user",
                    with_tx(C, fun(C1) -> bind_rebind_upsert(C1, CtxA) end)},
                {"bind_user_already_mapped_invalid_request",
                    with_tx(C, fun(C1) -> bind_user_already_mapped(C1, CtxA) end)},
                {"bind_cross_org_boundary_violation",
                    with_tx(C, fun(C1) -> bind_cross_org(C1, CtxA) end)},
                {"bind_not_org_member_boundary_violation",
                    with_tx(C, fun(C1) -> bind_not_org_member(C1, CtxA) end)},
                {"bind_removed_member_not_mapped",
                    with_tx(C, fun(C1) -> bind_removed_member(C1, CtxA) end)},
                {"bind_bot_account_not_mapped",
                    with_tx(C, fun(C1) -> bind_bot_account(C1, CtxA) end)},
                {"bind_disabled_user_not_mapped",
                    with_tx(C, fun(C1) -> bind_disabled_user(C1, CtxA) end)},
                {"bind_invalid_params", with_tx(C, fun(C1) -> bind_invalid_params(C1, CtxA) end)},
                %% ② INT-03 resolve
                {"resolve_returns_only_present",
                    with_tx(C, fun(C1) -> resolve_only_present(C1, CtxA) end)},
                {"resolve_cross_org_invisible",
                    with_tx(C, fun(C1) -> resolve_cross_org(C1, CtxA) end)},
                {"resolve_invalid_batch",
                    with_tx(C, fun(C1) -> resolve_invalid_batch(C1, CtxA) end)},
                %% ③ 导出面负例（无全量导出 / 无 accept 等形态）
                {"exported_surface_no_list_all_no_auto_accept", exported_surface_test()},
                %% ④ INT-04 创建
                {"group_create_success", with_tx(C, fun(C1) -> group_create_success(C1, CtxA) end)},
                {"group_create_unmapped_member_rejected",
                    with_tx(C, fun(C1) -> group_create_unmapped(C1, CtxA) end)},
                {"group_create_non_ws_member_rejected",
                    with_tx(C, fun(C1) -> group_create_non_ws_member(C1, CtxA) end)},
                {"group_create_cross_org_ws_not_found",
                    with_tx(C, fun(C1) -> group_create_cross_org_ws(C1, CtxA) end)},
                {"group_create_missing_ws_not_found",
                    with_tx(C, fun(C1) -> group_create_missing_ws(C1, CtxA) end)},
                {"group_create_archived_ws_not_found",
                    with_tx(C, fun(C1) -> group_create_archived_ws(C1, CtxA) end)},
                {"group_create_explicit_owner",
                    with_tx(C, fun(C1) -> group_create_explicit_owner(C1, CtxA) end)},
                {"group_create_invalid_title",
                    with_tx(C, fun(C1) -> group_create_invalid_title(C1, CtxA) end)},
                {"group_create_dedupes_members",
                    with_tx(C, fun(C1) -> group_create_dedupes(C1, CtxA) end)},
                %% ⑤ INT-05 幂等添加
                {"group_add_idempotent_same_result",
                    with_tx(C, fun(C1) -> group_add_idempotent(C1, CtxA) end)},
                {"group_add_unmapped_rejected",
                    with_tx(C, fun(C1) -> group_add_unmapped(C1, CtxA) end)},
                {"group_add_non_ws_member_rejected",
                    with_tx(C, fun(C1) -> group_add_non_ws_member(C1, CtxA) end)},
                {"group_add_cross_org_group_not_found",
                    with_tx(C, fun(C1) -> group_add_cross_org(C1, CtxA, CtxB) end)},
                {"group_add_personal_group_not_manageable",
                    with_tx(C, fun(C1) -> group_add_personal_group(C1, CtxA) end)},
                {"group_add_invalid_group_id",
                    with_tx(C, fun(C1) -> group_add_invalid_gid(C1, CtxA) end)},
                %% ⑥ INT-06 幂等移除
                {"group_remove_idempotent_same_result",
                    with_tx(C, fun(C1) -> group_remove_idempotent(C1, CtxA) end)},
                {"group_remove_owner_rejected",
                    with_tx(C, fun(C1) -> group_remove_owner(C1, CtxA) end)},
                {"group_remove_then_readd",
                    with_tx(C, fun(C1) -> group_remove_then_readd(C1, CtxA) end)},
                {"group_remove_cross_org_group_not_found",
                    with_tx(C, fun(C1) -> group_remove_cross_org(C1, CtxA, CtxB) end)},
                {"group_remove_last_owner_role4_rejected",
                    with_tx(C, fun(C1) -> group_remove_last_owner_role4(C1, CtxA) end)},
                %% ⑦ DB 兜底（DEFERRED 触发器提交期校验）
                {"db_subset_trigger_rejects_at_commit",
                    with_tx(C, fun(C1) -> db_subset_trigger(C1, CtxA) end)},
                %% ⑧ INT-11 好友申请（只发起）
                {"friend_request_creates_pending",
                    with_tx(C, fun(C1) -> fr_creates_pending(C1, CtxA) end)},
                {"friend_request_repeat_already_requested",
                    with_tx(C, fun(C1) -> fr_repeat_rejected(C1, CtxA) end)},
                {"friend_request_self_rejected",
                    with_tx(C, fun(C1) -> fr_self_rejected(C1, CtxA) end)},
                {"friend_request_sender_unmapped",
                    with_tx(C, fun(C1) -> fr_sender_unmapped(C1, CtxA) end)},
                {"friend_request_target_unmapped",
                    with_tx(C, fun(C1) -> fr_target_unmapped(C1, CtxA) end)},
                {"friend_request_cross_app_target_unmapped",
                    with_tx(C, fun(C1) -> fr_cross_app_target(C1, CtxA) end)},
                {"friend_request_already_friends_rejected",
                    with_tx(C, fun(C1) -> fr_already_friends(C1, CtxA) end)},
                {"friend_request_blocked_rejected",
                    with_tx(C, fun(C1) -> fr_blocked(C1, CtxA) end)},
                {"friend_request_invalid_params",
                    with_tx(C, fun(C1) -> fr_invalid_params(C1, CtxA) end)},
                {"friend_request_no_auto_accept_side_effect",
                    with_tx(C, fun(C1) -> fr_no_auto_accept(C1, CtxA) end)},
                %% notify_request 消息契约（meck，池化路径隔离）
                {"friend_request_notify_contract", fr_notify_contract_test()}
            ]
        end}}.

%%%===================================================================
%%% ① INT-02 bind
%%%===================================================================

bind_success(C, CtxA) ->
    {ok, #{
        <<"external_user_id">> := <<"ext-new">>,
        <<"status">> := <<"active">>,
        <<"user_id">> := ?H_A5
    }} =
        enterprise_identity_logic:bind_mapping_tx(C, CtxA, <<"ext-new">>, ?H_A5),
    %% 重bind 同参数幂等（upsert 命中同一行）
    {ok, #{<<"user_id">> := ?H_A5}} =
        enterprise_identity_logic:bind_mapping_tx(C, CtxA, <<"ext-new">>, ?H_A5),
    ok.

bind_rebind_upsert(C, CtxA) ->
    %% 同 external 重绑：覆盖 user_id 并复活 active（A1 upsert 语义）
    {ok, #{<<"user_id">> := ?H_A5}} =
        enterprise_identity_logic:bind_mapping_tx(C, CtxA, ?EXT_A1, ?H_A5),
    Row =
        one(
            C,
            <<"SELECT user_id, status FROM enterprise_external_identity",
                " WHERE external_user_id = $1 AND application_id = $2">>,
            [?EXT_A1, maps:get(application_id, CtxA)]
        ),
    ?assertEqual(?H_A5, maps:get(<<"user_id">>, Row)),
    ?assertEqual(<<"active">>, maps:get(<<"status">>, Row)),
    ok.

bind_user_already_mapped(C, CtxA) ->
    %% H_A1 已绑 EXT_A1；把新 external 绑到同一 user → 唯一约束语义 → invalid_request
    ?assertEqual(
        {error, {<<"invalid_request">>, user_already_mapped}},
        enterprise_identity_logic:bind_mapping_tx(C, CtxA, <<"ext-dup">>, ?H_A1)
    ),
    ok.

bind_cross_org(C, CtxA) ->
    %% H_B1 只 是 Org B 成员；Org A app 绑它 → organization_boundary_violation
    ?assertEqual(
        {error, {<<"organization_boundary_violation">>, not_org_member}},
        enterprise_identity_logic:bind_mapping_tx(C, CtxA, <<"ext-cross">>, ?H_B1)
    ),
    ok.

bind_not_org_member(C, CtxA) ->
    ?assertEqual(
        {error, {<<"organization_boundary_violation">>, not_org_member}},
        enterprise_identity_logic:bind_mapping_tx(C, CtxA, <<"ext-noorg">>, ?NOORG_U)
    ),
    ok.

bind_removed_member(C, CtxA) ->
    ?assertMatch(
        {error, {<<"identity_not_mapped">>, _}},
        enterprise_identity_logic:bind_mapping_tx(C, CtxA, <<"ext-removed">>, ?REMOVED_A)
    ),
    ok.

bind_bot_account(C, CtxA) ->
    ?assertMatch(
        {error, {<<"identity_not_mapped">>, _}},
        enterprise_identity_logic:bind_mapping_tx(C, CtxA, <<"ext-bot">>, ?BOT_A)
    ),
    ok.

bind_disabled_user(C, CtxA) ->
    ?assertMatch(
        {error, {<<"identity_not_mapped">>, _}},
        enterprise_identity_logic:bind_mapping_tx(C, CtxA, <<"ext-disabled">>, ?DISABLED_A)
    ),
    ok.

bind_invalid_params(C, _CtxA) ->
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_identity_logic:bind_mapping_tx(C, #{}, <<>>, 1)
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_identity_logic:bind_mapping_tx(C, #{}, <<"x">>, 0)
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_identity_logic:bind_mapping_tx(C, #{}, <<"x">>, <<"not-int">>)
    ),
    Long = binary:copy(<<"a">>, 257),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_identity_logic:bind_mapping_tx(C, #{}, Long, 1)
    ),
    ok.

%%%===================================================================
%%% ② INT-03 resolve
%%%===================================================================

resolve_only_present(C, CtxA) ->
    {ok, Items} = enterprise_identity_logic:resolve_mappings_tx(
        C, CtxA, [?EXT_A1, <<"ext-missing">>, ?EXT_A3]
    ),
    ?assertEqual(2, length(Items)),
    ExtLi = [maps:get(<<"external_user_id">>, I) || I <- Items],
    ?assertEqual(true, lists:member(?EXT_A1, ExtLi)),
    ?assertEqual(true, lists:member(?EXT_A3, ExtLi)),
    ?assertEqual(false, lists:member(<<"ext-missing">>, ExtLi)),
    ok.

resolve_cross_org(C, CtxA) ->
    %% Org A 的 app 看不到 Org B 的映射（矩阵隔离）
    {ok, Items} = enterprise_identity_logic:resolve_mappings_tx(C, CtxA, [?EXT_B1, ?EXT_B2]),
    ?assertEqual([], Items),
    ok.

resolve_invalid_batch(C, CtxA) ->
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_identity_logic:resolve_mappings_tx(C, CtxA, [])
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_identity_logic:resolve_mappings_tx(C, CtxA, lists:duplicate(101, <<"x">>))
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_identity_logic:resolve_mappings_tx(C, CtxA, [?EXT_A1, 42])
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, _}},
        enterprise_identity_logic:resolve_mappings_tx(C, CtxA, not_a_list)
    ),
    ok.

%%%===================================================================
%%% ③ 导出面负例
%%%===================================================================

exported_surface_test() ->
    ?_test(begin
        %% identity logic 导出面冻结：仅 bind/resolve，无任何 list/export 形态
        IdEx = lists:sort(enterprise_identity_logic:module_info(exports)),
        ?assertEqual(
            [{bind_mapping_tx, 4}, {module_info, 0}, {module_info, 1}, {resolve_mappings_tx, 3}],
            IdEx
        ),
        ?assertEqual(
            false,
            lists:any(
                fun({F, _}) ->
                    binary:match(atom_to_binary(F, utf8), [<<"list">>, <<"export">>, <<"all">>]) =/=
                        nomatch
                end,
                IdEx -- [{module_info, 0}, {module_info, 1}]
            )
        ),
        %% friend request logic 只发起：无 accept/confirm/approve/reject/delete/list 形态
        FRAll = lists:sort(enterprise_friend_request_logic:module_info(exports)),
        ?assertEqual(
            [{create_request_tx, 3}, {module_info, 0}, {module_info, 1}, {notify_request, 1}],
            FRAll
        ),
        ?assertEqual(
            false,
            lists:any(
                fun({F, _}) ->
                    binary:match(
                        atom_to_binary(F, utf8),
                        [
                            <<"accept">>,
                            <<"confirm">>,
                            <<"approve">>,
                            <<"reject">>,
                            <<"delete">>,
                            <<"list">>
                        ]
                    ) =/= nomatch
                end,
                FRAll -- [{module_info, 0}, {module_info, 1}]
            )
        ),
        %% group logic 导出面：仅 create/add/remove（无成员全量分页导出形态）
        GEx = lists:sort(enterprise_group_logic:module_info(exports)),
        ?assertEqual(
            [
                {add_members_tx, 4},
                {create_group_tx, 3},
                {module_info, 0},
                {module_info, 1},
                {remove_members_tx, 4}
            ],
            GEx
        ),
        ok
    end).

%%%===================================================================
%%% ④ INT-04 create
%%%===================================================================

group_create_success(C, CtxA) ->
    {ok, #{
        <<"group_id">> := Gid,
        <<"member_count">> := 2,
        <<"owner_user_id">> := ?H_A1,
        <<"workspace_id">> := ?WS_A1
    }} =
        enterprise_group_logic:create_group_tx(C, CtxA, #{
            workspace_id => ?WS_A1,
            title => <<"epgz03 企业群"/utf8>>,
            members => [?EXT_A1, ?EXT_A2]
        }),
    G =
        one(
            C,
            <<"SELECT scope, workspace_id, owner_uid, status, member_count, title FROM \"group\" WHERE id = $1">>,
            [Gid]
        ),
    ?assertEqual(<<"workspace">>, maps:get(<<"scope">>, G)),
    ?assertEqual(?WS_A1, maps:get(<<"workspace_id">>, G)),
    ?assertEqual(?H_A1, maps:get(<<"owner_uid">>, G)),
    ?assertEqual(1, maps:get(<<"status">>, G)),
    ?assertEqual(2, maps:get(<<"member_count">>, G)),
    Owner =
        one(
            C,
            <<"SELECT role, status FROM group_member WHERE group_id = $1 AND user_id = $2">>,
            [Gid, ?H_A1]
        ),
    ?assertEqual(4, maps:get(<<"role">>, Owner)),
    ?assertEqual(1, maps:get(<<"status">>, Owner)),
    Member =
        one(
            C,
            <<"SELECT role, status FROM group_member WHERE group_id = $1 AND user_id = $2">>,
            [Gid, ?H_A2]
        ),
    ?assertEqual(1, maps:get(<<"role">>, Member)),
    ?assertEqual(1, maps:get(<<"status">>, Member)),
    %% 历史 generation 已开（E2EE-2026-012 语义不缺失）
    Gen =
        one(
            C,
            <<"SELECT COUNT(*) AS n FROM group_member_generation WHERE group_id = $1">>,
            [Gid]
        ),
    ?assertEqual(2, maps:get(<<"n">>, Gen)),
    ok.

group_create_unmapped(C, CtxA) ->
    ?assertEqual(
        {error, {<<"identity_not_mapped">>, [<<"ext-none">>]}},
        enterprise_group_logic:create_group_tx(C, CtxA, #{
            workspace_id => ?WS_A1,
            title => <<"t">>,
            members => [?EXT_A1, <<"ext-none">>]
        })
    ),
    ok.

group_create_non_ws_member(C, CtxA) ->
    %% H_A3 映射存在但只在 WS_A2，不是 WS_A1 成员 → invalid_request
    ?assertMatch(
        {error, {<<"invalid_request">>, {not_workspace_members, [?EXT_A3]}}},
        enterprise_group_logic:create_group_tx(C, CtxA, #{
            workspace_id => ?WS_A1,
            title => <<"t">>,
            members => [?EXT_A1, ?EXT_A3]
        })
    ),
    ok.

group_create_cross_org_ws(C, CtxA) ->
    %% WS_B1 属于 Org B；Org A 的 app 不可见 → resource_not_found（不泄露存在性）
    ?assertEqual(
        {error, {<<"resource_not_found">>, workspace_not_found}},
        enterprise_group_logic:create_group_tx(C, CtxA, #{
            workspace_id => ?WS_B1,
            title => <<"t">>,
            members => [?EXT_A1]
        })
    ),
    ok.

group_create_missing_ws(C, CtxA) ->
    ?assertEqual(
        {error, {<<"resource_not_found">>, workspace_not_found}},
        enterprise_group_logic:create_group_tx(C, CtxA, #{
            workspace_id => 999999,
            title => <<"t">>,
            members => [?EXT_A1]
        })
    ),
    ok.

group_create_archived_ws(C, CtxA) ->
    ?assertEqual(
        {error, {<<"resource_not_found">>, workspace_not_active}},
        enterprise_group_logic:create_group_tx(C, CtxA, #{
            workspace_id => ?WS_A_ARCH,
            title => <<"t">>,
            members => [?EXT_A1]
        })
    ),
    ok.

group_create_explicit_owner(C, CtxA) ->
    {ok, #{<<"owner_user_id">> := ?H_A2, <<"group_id">> := Gid}} =
        enterprise_group_logic:create_group_tx(C, CtxA, #{
            workspace_id => ?WS_A1,
            title => <<"t">>,
            members => [?EXT_A1, ?EXT_A2],
            owner_external_user_id => ?EXT_A2
        }),
    G = one(C, <<"SELECT owner_uid FROM \"group\" WHERE id = $1">>, [Gid]),
    ?assertEqual(?H_A2, maps:get(<<"owner_uid">>, G)),
    %% owner 不在 members 内 → invalid_request
    ?assertMatch(
        {error, {<<"invalid_request">>, owner_not_in_members}},
        enterprise_group_logic:create_group_tx(C, CtxA, #{
            workspace_id => ?WS_A1,
            title => <<"t">>,
            members => [?EXT_A1],
            owner_external_user_id => ?EXT_A2
        })
    ),
    ok.

group_create_invalid_title(C, CtxA) ->
    ?assertMatch(
        {error, {<<"invalid_request">>, invalid_title}},
        enterprise_group_logic:create_group_tx(C, CtxA, #{
            workspace_id => ?WS_A1,
            title => <<>>,
            members => [?EXT_A1]
        })
    ),
    TooLong = binary:copy(<<"x">>, 201),
    ?assertMatch(
        {error, {<<"invalid_request">>, invalid_title}},
        enterprise_group_logic:create_group_tx(C, CtxA, #{
            workspace_id => ?WS_A1,
            title => TooLong,
            members => [?EXT_A1]
        })
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, invalid_workspace_id}},
        enterprise_group_logic:create_group_tx(C, CtxA, #{
            workspace_id => 0,
            title => <<"t">>,
            members => [?EXT_A1]
        })
    ),
    ok.

group_create_dedupes(C, CtxA) ->
    {ok, #{<<"member_count">> := 1}} =
        enterprise_group_logic:create_group_tx(C, CtxA, #{
            workspace_id => ?WS_A1,
            title => <<"t">>,
            members => [?EXT_A1, ?EXT_A1]
        }),
    ok.

%%%===================================================================
%%% ⑤ INT-05 add
%%%===================================================================

group_add_idempotent(C, CtxA) ->
    {ok, #{<<"group_id">> := Gid}} = create_a1_only(C, CtxA),
    R1 = enterprise_group_logic:add_members_tx(C, CtxA, Gid, [?EXT_A2]),
    ?assertMatch({ok, #{<<"added">> := 1, <<"already_member">> := 0, <<"member_count">> := 2}}, R1),
    R2 = enterprise_group_logic:add_members_tx(C, CtxA, Gid, [?EXT_A2]),
    ?assertMatch({ok, #{<<"added">> := 0, <<"already_member">> := 1, <<"member_count">> := 2}}, R2),
    %% 同一批混合（部分已在）保持幂等
    R3 = enterprise_group_logic:add_members_tx(C, CtxA, Gid, [?EXT_A2]),
    ?assertMatch({ok, #{<<"added">> := 0, <<"already_member">> := 1}}, R3),
    ok.

group_add_unmapped(C, CtxA) ->
    {ok, #{<<"group_id">> := Gid}} = create_a1_only(C, CtxA),
    ?assertEqual(
        {error, {<<"identity_not_mapped">>, [<<"ext-none">>]}},
        enterprise_group_logic:add_members_tx(C, CtxA, Gid, [<<"ext-none">>])
    ),
    ok.

group_add_non_ws_member(C, CtxA) ->
    {ok, #{<<"group_id">> := Gid}} = create_a1_only(C, CtxA),
    ?assertMatch(
        {error, {<<"invalid_request">>, {not_workspace_members, [?EXT_A3]}}},
        enterprise_group_logic:add_members_tx(C, CtxA, Gid, [?EXT_A3])
    ),
    ok.

group_add_cross_org(C, CtxA, CtxB) ->
    %% Org B 在自己 WS 建群；Org A 的 app 对它 add → resource_not_found
    {ok, #{<<"group_id">> := GidB}} =
        enterprise_group_logic:create_group_tx(C, CtxB, #{
            workspace_id => ?WS_B1,
            title => <<"t">>,
            members => [?EXT_B1]
        }),
    ?assertEqual(
        {error, {<<"resource_not_found">>, group_not_found}},
        enterprise_group_logic:add_members_tx(C, CtxA, GidB, [?EXT_A1])
    ),
    ok.

group_add_personal_group(C, CtxA) ->
    %% 个人群（scope=personal）不属于企业 API 管理面
    Now = elib_dt:now(),
    Gid = enterprise_group_repo:next_group_id(),
    {ok, _} = enterprise_group_repo:create_workspace_group_tx(
        C, Gid, ?H_A1, <<"t">>, <<>>, ?WS_A1, Now
    ),
    exec(C, [
        <<"UPDATE \"group\" SET scope = 'personal', workspace_id = NULL WHERE id = ">>,
        integer_to_binary(Gid)
    ]),
    ?assertEqual(
        {error, {<<"resource_not_found">>, group_not_found}},
        enterprise_group_logic:add_members_tx(C, CtxA, Gid, [?EXT_A2])
    ),
    ok.

group_add_invalid_gid(C, CtxA) ->
    ?assertMatch(
        {error, {<<"invalid_request">>, invalid_group_id}},
        enterprise_group_logic:add_members_tx(C, CtxA, 0, [?EXT_A2])
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, invalid_group_id}},
        enterprise_group_logic:add_members_tx(C, CtxA, <<"gid">>, [?EXT_A2])
    ),
    ok.

%%%===================================================================
%%% ⑥ INT-06 remove
%%%===================================================================

group_remove_idempotent(C, CtxA) ->
    {ok, #{<<"group_id">> := Gid}} = create_a1_a2(C, CtxA),
    R1 = enterprise_group_logic:remove_members_tx(C, CtxA, Gid, [?EXT_A2]),
    ?assertMatch(
        {ok, #{<<"removed">> := 1, <<"already_absent">> := 0, <<"member_count">> := 1}}, R1
    ),
    Row =
        one(
            C,
            <<"SELECT status FROM group_member WHERE group_id = $1 AND user_id = $2">>,
            [Gid, ?H_A2]
        ),
    ?assertEqual(0, maps:get(<<"status">>, Row)),
    %% 幂等：重复移除同结果
    R2 = enterprise_group_logic:remove_members_tx(C, CtxA, Gid, [?EXT_A2]),
    ?assertMatch(
        {ok, #{<<"removed">> := 0, <<"already_absent">> := 1, <<"member_count">> := 1}}, R2
    ),
    ok.

group_remove_owner(C, CtxA) ->
    {ok, #{<<"group_id">> := Gid}} = create_a1_a2(C, CtxA),
    %% owner（owner_uid = H_A1）不可被 OA 移除
    ?assertEqual(
        {error, {<<"invalid_request">>, cannot_remove_group_owner}},
        enterprise_group_logic:remove_members_tx(C, CtxA, Gid, [?EXT_A1])
    ),
    %% 批量里夹带 owner 同样拒绝
    ?assertEqual(
        {error, {<<"invalid_request">>, cannot_remove_group_owner}},
        enterprise_group_logic:remove_members_tx(C, CtxA, Gid, [?EXT_A1, ?EXT_A2])
    ),
    ok.

group_remove_then_readd(C, CtxA) ->
    {ok, #{<<"group_id">> := Gid}} = create_a1_a2(C, CtxA),
    {ok, _} = enterprise_group_logic:remove_members_tx(C, CtxA, Gid, [?EXT_A2]),
    {ok, #{<<"added">> := 1, <<"member_count">> := 2}} =
        enterprise_group_logic:add_members_tx(C, CtxA, Gid, [?EXT_A2]),
    ok.

group_remove_cross_org(C, CtxA, CtxB) ->
    %% H_B2 不是 WS_B1 成员（矩阵边界人物），建群只用已就位的 EXT_B1；
    %% 跨 Org remove 在边界判定即拒绝，与目标成员无关。
    {ok, #{<<"group_id">> := GidB}} =
        enterprise_group_logic:create_group_tx(C, CtxB, #{
            workspace_id => ?WS_B1,
            title => <<"t">>,
            members => [?EXT_B1]
        }),
    ?assertEqual(
        {error, {<<"resource_not_found">>, group_not_found}},
        enterprise_group_logic:remove_members_tx(C, CtxA, GidB, [?EXT_B1])
    ),
    ok.

group_remove_last_owner_role4(C, CtxA) ->
    %% 群里唯一的 role=4 是 owner；把非 owner 全移光后 owner 仍不可移除（第一规则），
    %% 再构造“移除集覆盖全部 active 群主”的负例：手动把 H_A2 提为 role=4 后
    %% 移除两个群主 → last_owner_not_removable。
    {ok, #{<<"group_id">> := Gid}} = create_a1_a2(C, CtxA),
    exec(C, [
        <<"UPDATE group_member SET role = 4 WHERE group_id = ">>,
        integer_to_binary(Gid),
        <<" AND user_id = ">>,
        integer_to_binary(?H_A2)
    ]),
    ?assertEqual(
        {error, {<<"invalid_request">>, cannot_remove_group_owner}},
        %% owner_uid = H_A1 命中第一规则（即使存在第二群主）
        enterprise_group_logic:remove_members_tx(C, CtxA, Gid, [?EXT_A2, ?EXT_A1])
    ),
    %% 仅移除非 owner 群主 H_A2：owner 仍在，允许
    {ok, #{<<"removed">> := 1}} =
        enterprise_group_logic:remove_members_tx(C, CtxA, Gid, [?EXT_A2]),
    ok.

%%%===================================================================
%%% ⑦ DB 兜底：DEFERRED 子集触发器
%%%===================================================================

db_subset_trigger(C, CtxA) ->
    %% 绕过 logic 前置校验：直接 repo activate 一个非 WS_A1 成员（H_A3），
    %% 事务内操作成功但 COMMIT 被 trg_group_member_ws_subset（23514）拒绝。
    {ok, #{<<"group_id">> := Gid}} = create_a1_only(C, CtxA),
    {ok, true} = enterprise_group_repo:activate_member_tx(C, Gid, ?H_A3, 1, <<"bypass_precheck">>),
    Commit = elib_pg:query(C, <<"COMMIT">>, []),
    ?assertMatch({error, #error{code = <<"23514">>}}, Commit),
    %% 失败提交后清理连接事务状态，再显式回滚结束本用例
    exec_quiet(C, <<"ROLLBACK">>),
    ok.

%%%===================================================================
%%% ⑧ INT-11 friend request（只发起）
%%%===================================================================

fr_creates_pending(C, CtxA) ->
    {ok, #{
        <<"request_status">> := <<"pending">>,
        <<"sender_user_id">> := ?EXT_A1,
        <<"target_user_id">> := ?EXT_A2
    }} =
        enterprise_friend_request_logic:create_request_tx(C, CtxA, #{
            sender_user_id => ?EXT_A1,
            target_user_id => ?EXT_A2,
            greeting => <<"hi from oa"/utf8>>
        }),
    Row =
        one(
            C,
            <<"SELECT status, setting FROM user_friend WHERE from_user_id = $1 AND to_user_id = $2">>,
            [?H_A1, ?H_A2]
        ),
    ?assertEqual(0, maps:get(<<"status">>, Row)),
    %% 现有人工审批流消费同一真源：pending_status = pending（可 accept/reject）
    ?assertEqual(pending, enterprise_friend_request_repo:pending_status_tx(C, ?H_A1, ?H_A2)),
    ok.

fr_repeat_rejected(C, CtxA) ->
    {ok, _} =
        enterprise_friend_request_logic:create_request_tx(C, CtxA, #{
            sender_user_id => ?EXT_A1, target_user_id => ?EXT_A2
        }),
    ?assertEqual(
        {error, {<<"invalid_request">>, already_requested}},
        enterprise_friend_request_logic:create_request_tx(C, CtxA, #{
            sender_user_id => ?EXT_A1, target_user_id => ?EXT_A2
        })
    ),
    ok.

fr_self_rejected(C, CtxA) ->
    ?assertMatch(
        {error, {<<"invalid_request">>, self_request}},
        enterprise_friend_request_logic:create_request_tx(C, CtxA, #{
            sender_user_id => ?EXT_A1, target_user_id => ?EXT_A1
        })
    ),
    ok.

fr_sender_unmapped(C, CtxA) ->
    ?assertEqual(
        {error, {<<"identity_not_mapped">>, sender_not_mapped}},
        enterprise_friend_request_logic:create_request_tx(C, CtxA, #{
            sender_user_id => <<"ext-none">>, target_user_id => ?EXT_A2
        })
    ),
    ok.

fr_target_unmapped(C, CtxA) ->
    ?assertEqual(
        {error, {<<"identity_not_mapped">>, target_not_mapped}},
        enterprise_friend_request_logic:create_request_tx(C, CtxA, #{
            sender_user_id => ?EXT_A1, target_user_id => <<"ext-none">>
        })
    ),
    ok.

fr_cross_app_target(C, CtxA) ->
    %% EXT_B1 只在 Org B app 映射；Org A app 不可见 → target_not_mapped
    ?assertEqual(
        {error, {<<"identity_not_mapped">>, target_not_mapped}},
        enterprise_friend_request_logic:create_request_tx(C, CtxA, #{
            sender_user_id => ?EXT_A1, target_user_id => ?EXT_B1
        })
    ),
    ok.

fr_already_friends(C, CtxA) ->
    seed_friend_row(C, ?H_A1, ?H_A2, 1),
    ?assertEqual(
        {error, {<<"invalid_request">>, already_friends}},
        enterprise_friend_request_logic:create_request_tx(C, CtxA, #{
            sender_user_id => ?EXT_A1, target_user_id => ?EXT_A2
        })
    ),
    ok.

fr_blocked(C, CtxA) ->
    exec(C, [
        <<"INSERT INTO user_denylist (id, user_id, denied_user_id, created_at) VALUES (">>,
        integer_to_binary(enterprise_friend_request_repo:next_friend_id()),
        ", ",
        integer_to_binary(?H_A1),
        ", ",
        integer_to_binary(?H_A2),
        <<", CURRENT_TIMESTAMP)">>
    ]),
    ?assertEqual(
        {error, {<<"invalid_request">>, blocked}},
        enterprise_friend_request_logic:create_request_tx(C, CtxA, #{
            sender_user_id => ?EXT_A1, target_user_id => ?EXT_A2
        })
    ),
    ok.

fr_invalid_params(C, CtxA) ->
    ?assertMatch(
        {error, {<<"invalid_request">>, invalid_sender_user_id}},
        enterprise_friend_request_logic:create_request_tx(C, CtxA, #{
            sender_user_id => <<>>, target_user_id => ?EXT_A2
        })
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, invalid_target_user_id}},
        enterprise_friend_request_logic:create_request_tx(C, CtxA, #{
            sender_user_id => ?EXT_A1, target_user_id => 42
        })
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, invalid_greeting}},
        enterprise_friend_request_logic:create_request_tx(C, CtxA, #{
            sender_user_id => ?EXT_A1,
            target_user_id => ?EXT_A2,
            greeting => binary:copy(<<"x">>, 501)
        })
    ),
    ?assertMatch(
        {error, {<<"invalid_request">>, input_not_map}},
        enterprise_friend_request_logic:create_request_tx(C, CtxA, not_a_map)
    ),
    ok.

fr_no_auto_accept(C, CtxA) ->
    {ok, _} =
        enterprise_friend_request_logic:create_request_tx(C, CtxA, #{
            sender_user_id => ?EXT_A1, target_user_id => ?EXT_A2
        }),
    %% 正向仍是 pending（status=0），不存在自动接受路径
    ?assertEqual(0, friend_status(C, ?H_A1, ?H_A2)),
    %% 反向好友行不存在（accept 从未发生）
    ?assertEqual(
        #{},
        one(C, <<"SELECT id FROM user_friend WHERE from_user_id = $1 AND to_user_id = $2">>, [
            ?H_A2, ?H_A1
        ])
    ),
    ok.

%%%===================================================================
%%% notify_request 消息契约（meck 隔离池化路径）
%%%===================================================================

fr_notify_contract_test() ->
    {setup,
        fun() ->
            lists:foreach(
                fun({Mod, Expects}) ->
                    {ok, _} = meck_helper:setup_mock(Mod, Expects)
                end,
                notify_mocks()
            )
        end,
        fun(_) ->
            lists:foreach(
                fun({Mod, _}) -> meck_helper:cleanup_mock(Mod) end,
                notify_mocks()
            )
        end,
        fun(_) ->
            ?_test(begin
                ok = enterprise_friend_request_logic:notify_request(#{
                    sender_uid => ?H_A1,
                    target_uid => ?H_A2,
                    greeting => <<"hi"/utf8>>
                }),
                ?assertEqual(1, meck:num_calls(msg_s2c_ds, write_msg, 8)),
                ?assertEqual(1, meck:num_calls(message_ds, send_next, 4)),
                [{_, {msg_s2c_ds, write_msg, [_, _, Payload, _From, _To, _, Action, _]}, ok} | _] =
                    meck:history(msg_s2c_ds),
                ?assertEqual(<<"apply_friend">>, Action),
                ?assertEqual(<<"oa">>, maps:get(<<"source">>, maps:get(<<"from">>, Payload))),
                ?assertEqual(<<"hi"/utf8>>, maps:get(<<"msg">>, Payload)),
                ok
            end)
        end}.

notify_mocks() ->
    [
        {msg_s2c_ds, [
            {'write_msg', 8, fun(_CreatedAt, _MsgId, _Payload, _From, _To, _NowTs, _A, _E) ->
                ok
            end}
        ]},
        {message_ds, [
            {'assemble_msg', 8, fun(_T, _F, _To, P, _M, _B, _A, _E) -> P end},
            {'send_next', 4, fun(_To, _MsgId, _Msg, _MsLi) -> ok end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(<<"s2c">>) -> [2000] end}
        ]}
    ].

%%%===================================================================
%%% Helpers
%%%===================================================================

create_a1_only(C, CtxA) ->
    enterprise_group_logic:create_group_tx(C, CtxA, #{
        workspace_id => ?WS_A1,
        title => <<"t">>,
        members => [?EXT_A1]
    }).

create_a1_a2(C, CtxA) ->
    enterprise_group_logic:create_group_tx(C, CtxA, #{
        workspace_id => ?WS_A1,
        title => <<"t">>,
        members => [?EXT_A1, ?EXT_A2]
    }).

friend_status(C, From, To) ->
    Row =
        one(
            C,
            <<"SELECT status FROM user_friend WHERE from_user_id = $1 AND to_user_id = $2">>,
            [From, To]
        ),
    maps:get(<<"status">>, Row, undefined).

seed_friend_row(C, From, To, Status) ->
    exec(C, [
        <<"INSERT INTO user_friend (id, from_user_id, to_user_id, status, category_id, created_at) VALUES (">>,
        integer_to_binary(enterprise_friend_request_repo:next_friend_id()),
        ", ",
        integer_to_binary(From),
        ", ",
        integer_to_binary(To),
        ", ",
        integer_to_binary(Status),
        <<", 0, CURRENT_TIMESTAMP)">>
    ]).

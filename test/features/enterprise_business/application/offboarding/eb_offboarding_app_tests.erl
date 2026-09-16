%%% @doc EB-08 离职冻结、交接与完成证明的用例层验收套件（真库）。
%%%
%%% 依计划 v4.1 EB-08（§「EB-08：离职冻结、交接和完成证明」）与
%%% `control/required-acceptance.tsv` 的 `EB-08-A01..A08`。
%%%
%%% **三个内部 Gate（严格串行）**：
%%%   * S1 `suspend+freeze`：旧 JWT 企业访问立即失败；case snapshot 固定；不得执行 rebind。
%%%   * S2 `transfer+CAS`：identity A->B；并发仅一方成功；失败项可重试；owner/resource 不变。
%%%   * S3 `verify+finalize`：校验残留、ID/Org/hash/count；未 verify 不得 removed；
%%%     最终 DB guard 同点复核。
%%%
%%% 只使用 `eb_pg_test_fixture` 的合成租户（随机 TSID、无真实账号/联系方式/生产资源）。
-module(eb_offboarding_app_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, eb_pg_test_fixture).
-define(APP, eb_offboarding_app).
-define(PROBE, eb08_auth_probe).

offboarding_app_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} -> {ok, Conn};
        {error, Reason} -> {error, Reason}
    end.

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

%% S1/S2/S3 的用例（每个 Gate 落地时追加，见 logs/s{1,2,3}-{red,green}.log）。
cases({ok, _Conn}) ->
    [
        {timeout, 90, fun s1_open_suspends_leaver_and_freezes_snapshot/0},
        {timeout, 90, fun s1_old_jwt_enterprise_access_fails_immediately/0},
        {timeout, 90, fun s1_snapshot_is_fixed_and_no_rebind_happened/0},
        {timeout, 60, fun s1_suspend_write_path_is_owned_by_this_card/0},
        {timeout, 90, fun s1_pre_suspended_leaver_can_open_without_duplicate_suspend/0},
        {timeout, 90, fun s2_execute_rebinds_identity_a_to_b/0},
        {timeout, 90, fun s2_execute_keeps_resource_id_org_hash_count/0},
        {timeout, 90, fun s2_stale_expected_version_is_rejected_with_zero_writes/0},
        {timeout, 90, fun s2_failed_item_is_retryable/0},
        {timeout, 90, fun s3_verify_checks_residuals_id_org_hash_count/0},
        {timeout, 60, fun s3_snapshot_hash_tamper_is_refused/0},
        {timeout, 90, fun s3_unverified_case_cannot_be_removed/0},
        {timeout, 90, fun s3_verify_detects_residual_and_parks_case_failed/0},
        {timeout, 90, fun s3_finalize_removes_member_once_and_is_idempotent/0},
        {timeout, 120, fun s3_final_remove_is_rechecked_by_db_guard_at_the_same_point/0}
    ];
cases(Other) ->
    erlang:error({eb08_suite_db_unavailable, Other}).

%% ===================================================================
%% S1：suspend + freeze
%% ===================================================================

%% A01 前半：建档即撤权（suspend 落库）且 case 冻结。
s1_open_suspends_leaver_and_freezes_snapshot() ->
    Scope = ?FIX:new_scope(),
    {Org, Ws} = tenant(Scope),
    Leaver = maps:get(actor_user_id, Scope),
    Successor = maps:get(peer_user_id, Scope),
    try
        ok = insert_member(Org, Successor, <<"member">>),
        %% 前置：撤权前企业访问是通的（证明后面的「拒」不是恒真）
        ?assertMatch({ok, _}, enterprise_access(Org, Ws, Leaver)),
        ?assertEqual({ok, active}, member_status(Org, Leaver)),

        {ok, Result} = ?APP:open_offboarding(Org, #{
            workspace_id => Ws,
            leaver_user_id => Leaver,
            successor_user_id => Successor,
            reason => <<"eb08-s1-synthetic">>,
            actor_user_id => maps:get(owner_user_id, Scope)
        }),
        ?assertEqual(frozen, maps:get(status, Result)),
        ?assertEqual(1, maps:get(items_total, Result)),
        ?assertEqual(true, maps:get(member_suspended, Result)),

        %% 写路径落库：organization_member.status = suspended（不改个人账号任何行）
        ?assertEqual({ok, suspended}, member_status(Org, Leaver)),
        Cases = ?FIX:scalar(
            <<"SELECT count(*) FROM enterprise_offboarding_case WHERE organization_id=$1">>,
            [Org],
            -1
        ),
        ?assertEqual(1, Cases),
        Items = ?FIX:scalar(
            <<"SELECT count(*) FROM enterprise_offboarding_item WHERE organization_id=$1">>,
            [Org],
            -1
        ),
        ?assertEqual(1, Items),
        %% 审计：撤权一条、建档一条，各恰一次
        ?assertEqual(1, audit_count(Org, <<"offboarding.member.suspend">>)),
        ?assertEqual(1, audit_count(Org, <<"offboarding.open">>))
    after
        scrub(Scope)
    end.

%% A01 后半：旧 JWT 的企业访问**立即**失败（逐请求读事实，不依赖 token 过期）。
s1_old_jwt_enterprise_access_fails_immediately() ->
    Scope = ?FIX:new_scope(),
    {Org, Ws} = tenant(Scope),
    Leaver = maps:get(actor_user_id, Scope),
    Successor = maps:get(peer_user_id, Scope),
    try
        ok = insert_member(Org, Successor, <<"member">>),
        ?PROBE:reset(),
        {ok, Before} = enterprise_access(Org, Ws, Leaver),
        ?assertEqual(enterprise_member, maps:get(auth_context, Before)),
        ?assertEqual(1, ?PROBE:count()),

        {ok, _} = ?APP:open_offboarding(Org, #{
            workspace_id => Ws,
            leaver_user_id => Leaver,
            successor_user_id => Successor,
            actor_user_id => maps:get(owner_user_id, Scope)
        }),

        %% 同一个「旧 JWT」再访问一次：立刻被拒
        Route = enterprise_route(),
        Credential = old_jwt(Leaver),
        Denied = enterprise_access(Org, Ws, Leaver),
        ?assertEqual({error, {member_not_active, suspended}}, Denied),
        %% 逐请求装载：每次判定都重新读一次事实（计数精确 +1），不复用上一次结论
        ?assertEqual(2, ?PROBE:count()),
        %% JWT 自报的成员状态与角色一律不采信
        SelfClaimed = eb_auth_app:authorize_via_port(
            ?PROBE,
            Route,
            request(Org, Ws, Leaver, Credential#{
                claims => #{
                    member_status => <<"active">>,
                    role => <<"owner">>,
                    permissions => [<<"contact.read">>]
                }
            })
        ),
        ?assertEqual({error, {member_not_active, suspended}}, SelfClaimed)
    after
        scrub(Scope)
    end.

%% A01 后半 + S1 硬约束「不得执行 rebind」：snapshot 固定、经办关系一字未动。
s1_snapshot_is_fixed_and_no_rebind_happened() ->
    Scope = ?FIX:new_scope(),
    {Org, Ws} = tenant(Scope),
    Leaver = maps:get(actor_user_id, Scope),
    Successor = maps:get(peer_user_id, Scope),
    Identity = maps:get(sales_identity_id, Scope),
    try
        ok = insert_member(Org, Successor, <<"member">>),
        BeforeRows = leaver_assignment_rows(Scope, Leaver),
        ?assertEqual(1, length(BeforeRows)),

        {ok, Result} = ?APP:open_offboarding(Org, #{
            workspace_id => Ws,
            leaver_user_id => Leaver,
            successor_user_id => Successor,
            actor_user_id => maps:get(owner_user_id, Scope)
        }),
        CaseId = maps:get(case_id, Result),
        [SnapItem] = maps:get(items, Result),
        ?assertEqual(Identity, maps:get(business_identity_id, SnapItem)),
        ?assertEqual(Leaver, maps:get(from_user_id, SnapItem)),
        ?assertEqual(Successor, maps:get(to_user_id, SnapItem)),
        ?assertEqual(pending, maps:get(status, SnapItem)),
        ?assert(is_binary(maps:get(idempotency_key, SnapItem))),

        %% ① 不得 rebind：leaver 仍是 sales 的 active 经办人，版本未推进
        AfterRows = leaver_assignment_rows(Scope, Leaver),
        ?assertEqual(BeforeRows, AfterRows),
        ?assertEqual(active, active_assignee_status(Scope, Identity)),
        ?assertEqual(Leaver, active_assignee(Scope, Identity)),
        %% ② successor 未被写入任何经办关系（rebind 没发生）
        ?assertEqual([], leaver_assignment_rows(Scope, Successor)),
        %% ③ snapshot 与落库行逐字一致，且重复读取不漂移
        {ok, [Row1]} = eb_pg_store:list_offboarding_items(Org, Ws, CaseId),
        {ok, [Row2]} = eb_pg_store:list_offboarding_items(Org, Ws, CaseId),
        ?assertEqual(Row1, Row2),
        ?assertEqual(frozen_fields(SnapItem), frozen_fields(Row1)),
        ?assertEqual(0, maps:get(attempt, Row1)),
        ?assertEqual(pending, maps:get(status, Row1)),
        ?assertEqual(undefined, maps:get(failure_reason, Row1)),
        %% ④ 同一 leaver 的第二个未完成 case 被数据库裁决拒绝（不静默新建）
        ?assertMatch(
            {error, _},
            ?APP:open_offboarding(Org, #{
                workspace_id => Ws,
                leaver_user_id => Leaver,
                successor_user_id => Successor,
                actor_user_id => maps:get(owner_user_id, Scope)
            })
        )
    after
        scrub(Scope)
    end.

%% A08：member 状态的**写**路径归本卡（落库）；读事实仍走最小只读 Port。
s1_suspend_write_path_is_owned_by_this_card() ->
    Scope = ?FIX:new_scope(),
    {Org, Ws} = tenant(Scope),
    Leaver = maps:get(actor_user_id, Scope),
    try
        %% ① 写：公开入口（facade 面）→ 落库
        {ok, SuspendResult} = ?APP:suspend_member(Org, #{
            workspace_id => Ws,
            member_user_id => Leaver,
            reason => <<"eb08-a08-synthetic">>,
            actor_user_id => maps:get(owner_user_id, Scope)
        }),
        ?assertEqual(<<"suspended">>, maps:get(status, SuspendResult)),
        ?assertEqual({ok, suspended}, member_status(Org, Leaver)),
        ?assertEqual(1, audit_count(Org, <<"offboarding.member.suspend">>)),
        %% 幂等：对已 suspended 的成员再撤权 ⇒ 明确拒绝且不加审计
        ?assertMatch(
            {error, {409, _}},
            ?APP:suspend_member(Org, #{
                workspace_id => Ws,
                member_user_id => Leaver,
                reason => <<"eb08-a08-again">>,
                actor_user_id => maps:get(owner_user_id, Scope)
            })
        ),
        ?assertEqual(1, audit_count(Org, <<"offboarding.member.suspend">>)),

        %% ② 读：事实仍只经最小只读 Port（写路径不寄生在事实 Port 上）
        ?assertEqual({ok, suspended}, member_status_via_port(Org, Leaver)),
        ?assertEqual([], write_callbacks(eb_member_fact_port)),
        %% ③ 通用性：Core 侧的写路径是通用 suspend（不认识任何纵切单元模块）
        ?assertEqual(true, erlang:function_exported(organization_member_logic, suspend, 3)),
        ?assertEqual(0, reverse_reference_count())
    after
        scrub(Scope)
    end.

%% 计划要求先 suspend 再建 case；open 必须消费该状态，且不能重复撤权审计。
s1_pre_suspended_leaver_can_open_without_duplicate_suspend() ->
    Scope = ?FIX:new_scope(),
    {Org, Ws} = tenant(Scope),
    Leaver = maps:get(actor_user_id, Scope),
    Successor = maps:get(peer_user_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    try
        ok = insert_member(Org, Successor, <<"member">>),
        {ok, _} = ?APP:suspend_member(Org, #{
            workspace_id => Ws,
            member_user_id => Leaver,
            reason => <<"eb08-pre-suspend">>,
            actor_user_id => Owner
        }),
        {ok, Opened} = open_case(Scope, Leaver, Successor),
        ?assertEqual(frozen, maps:get(status, Opened)),
        ?assertEqual(true, maps:get(member_suspended, Opened)),
        ?assertEqual({ok, suspended}, member_status(Org, Leaver)),
        ?assertEqual(1, audit_count(Org, <<"offboarding.member.suspend">>)),
        ?assertEqual(1, audit_count(Org, <<"offboarding.open">>))
    after
        scrub(Scope)
    end.

%% ===================================================================
%% S2：transfer + CAS
%% ===================================================================

%% A02 前半：identity 的 active 经办人从 leaver 换成 successor；旧行保留为 ended
%% （历史与 owner 都不丢），资源 owner 仍是同一个 identity。
s2_execute_rebinds_identity_a_to_b() ->
    Scope = ?FIX:new_scope(),
    {Org, Ws} = tenant(Scope),
    Leaver = maps:get(actor_user_id, Scope),
    Successor = maps:get(peer_user_id, Scope),
    Identity = maps:get(sales_identity_id, Scope),
    try
        ok = insert_member(Org, Successor, <<"member">>),
        {ok, Opened} = open_case(Scope, Leaver, Successor),
        CaseId = maps:get(case_id, Opened),
        ?assertEqual(frozen, maps:get(status, Opened)),

        {ok, Executed} = ?APP:execute_offboarding(Org, #{
            workspace_id => Ws,
            case_id => CaseId,
            expected_version => maps:get(version, Opened),
            actor_user_id => maps:get(owner_user_id, Scope)
        }),
        %% 全部成功 ⇒ 停在 transferring（verify 才能推进到 verifying）
        ?assertEqual(transferring, maps:get(status, Executed)),
        ?assertEqual(1, maps:get(item_success, Executed)),
        ?assertEqual(0, maps:get(item_failed, Executed)),

        %% 经办人已换成 B；旧行仍在（ended），不是删除后重建
        ?assertEqual(Successor, active_assignee(Scope, Identity)),
        Rows = identity_rows(Scope, Identity),
        ?assertEqual(2, length(Rows)),
        ?assertEqual(1, length([R || R <- Rows, maps:get(status, R) =:= ended])),
        ?assertEqual(Leaver, ended_assignee(Scope, Identity)),
        %% 审计恰一次
        ?assertEqual(1, audit_count(Org, <<"offboarding.execute">>)),
        %% 项状态 success，attempt 未因成功而膨胀
        {ok, [Item]} = eb_pg_store:list_offboarding_items(Org, Ws, CaseId),
        ?assertEqual(success, maps:get(status, Item)),
        ?assertEqual(0, maps:get(attempt, Item)),
        ?assertEqual(undefined, maps:get(failure_reason, Item)),
        %% 重复执行不再产生第二次转移/审计（幂等效果）
        ?assertMatch(
            {error, _},
            ?APP:execute_offboarding(Org, #{
                workspace_id => Ws,
                case_id => CaseId,
                expected_version => maps:get(version, Executed),
                actor_user_id => maps:get(owner_user_id, Scope)
            })
        ),
        ?assertEqual(1, audit_count(Org, <<"offboarding.execute">>))
    after
        scrub(Scope)
    end.

%% A02 后半：资源 ID / Org / hash / count 在交接前后**不变**（owner 与资源不变）。
s2_execute_keeps_resource_id_org_hash_count() ->
    Scope = ?FIX:new_scope(),
    {Org, Ws} = tenant(Scope),
    Leaver = maps:get(actor_user_id, Scope),
    Successor = maps:get(peer_user_id, Scope),
    Identity = maps:get(sales_identity_id, Scope),
    try
        ok = insert_member(Org, Successor, <<"member">>),
        Before = resource_fingerprint(Scope, Identity),
        %% 前置非空：conversation 与 contact 都确实挂在被交接的 identity 上
        ?assert(maps:get(conversation_count, Before) >= 1),
        ?assert(maps:get(contact_count, Before) >= 1),

        {ok, CaseId, _Version} = open_and_execute(Scope, Leaver, Successor),
        After = resource_fingerprint(Scope, Identity),

        ?assertEqual(maps:get(identity_id, Before), maps:get(identity_id, After)),
        ?assertEqual(maps:get(organization_id, Before), maps:get(organization_id, After)),
        ?assertEqual(maps:get(identity_hash, Before), maps:get(identity_hash, After)),
        ?assertEqual(maps:get(conversation_count, Before), maps:get(conversation_count, After)),
        ?assertEqual(maps:get(contact_count, Before), maps:get(contact_count, After)),
        %% 同时确认「交接真发生了」——即上面的「不变」不是「什么都没做」
        ?assertEqual(Successor, active_assignee(Scope, Identity)),
        {ok, _Case} = eb_pg_store:fetch_offboarding_case(Org, Ws, CaseId)
    after
        scrub(Scope)
    end.

%% CAS：期望版本不匹配 ⇒ 零写入（不 rebind、不推进 case、不加审计）。
s2_stale_expected_version_is_rejected_with_zero_writes() ->
    Scope = ?FIX:new_scope(),
    {Org, Ws} = tenant(Scope),
    Leaver = maps:get(actor_user_id, Scope),
    Successor = maps:get(peer_user_id, Scope),
    Identity = maps:get(sales_identity_id, Scope),
    try
        ok = insert_member(Org, Successor, <<"member">>),
        {ok, Opened} = open_case(Scope, Leaver, Successor),
        CaseId = maps:get(case_id, Opened),
        {ok, Case} = eb_pg_store:fetch_offboarding_case(Org, Ws, CaseId),
        Stale = maps:get(version, Case) + 7,
        ?assertMatch(
            {error, {stale_version, _, _}},
            ?APP:execute_offboarding(Org, #{
                workspace_id => Ws,
                case_id => CaseId,
                expected_version => Stale,
                actor_user_id => maps:get(owner_user_id, Scope)
            })
        ),
        %% 零写入
        ?assertEqual(Leaver, active_assignee(Scope, Identity)),
        {ok, Unchanged} = eb_pg_store:fetch_offboarding_case(Org, Ws, CaseId),
        ?assertEqual(maps:get(version, Case), maps:get(version, Unchanged)),
        ?assertEqual(frozen, maps:get(status, Unchanged)),
        {ok, [Item]} = eb_pg_store:list_offboarding_items(Org, Ws, CaseId),
        ?assertEqual(pending, maps:get(status, Item)),
        ?assertEqual(0, audit_count(Org, <<"offboarding.execute">>))
    after
        scrub(Scope)
    end.

%% A04 前半：失败项留下可归因原因、case 置 failed；**重试**（failed -> transferring）
%% 后成功，且幂等键逐字不变（不产生第二条事实）。
s2_failed_item_is_retryable() ->
    Scope = ?FIX:new_scope(),
    {Org, Ws} = tenant(Scope),
    Leaver = maps:get(actor_user_id, Scope),
    Successor = maps:get(peer_user_id, Scope),
    Third = maps:get(owner_user_id, Scope),
    Identity = maps:get(sales_identity_id, Scope),
    try
        ok = insert_member(Org, Successor, <<"member">>),
        {ok, Opened} = open_case(Scope, Leaver, Successor),
        CaseId = maps:get(case_id, Opened),
        {ok, [SnapItem]} = eb_pg_store:list_offboarding_items(Org, Ws, CaseId),
        Key = maps:get(idempotency_key, SnapItem),

        %% 注入失败：把该 identity 的 active 经办人换成第三人（既不是 leaver 也不是
        %% successor）⇒ 交接项必须失败，而不是被静默改成「交给别人」。
        ok = rebind_directly(Scope, Identity, Leaver, Third),
        {ok, Failed} = ?APP:execute_offboarding(Org, #{
            workspace_id => Ws,
            case_id => CaseId,
            expected_version => maps:get(version, Opened),
            actor_user_id => maps:get(owner_user_id, Scope)
        }),
        ?assertEqual(failed, maps:get(status, Failed)),
        ?assertEqual(0, maps:get(item_success, Failed)),
        ?assertEqual(1, maps:get(item_failed, Failed)),
        {ok, [FailedItem]} = eb_pg_store:list_offboarding_items(Org, Ws, CaseId),
        ?assertEqual(failed, maps:get(status, FailedItem)),
        ?assert(is_binary(maps:get(failure_reason, FailedItem))),
        ?assertNotEqual(
            nomatch, binary:match(maps:get(failure_reason, FailedItem), <<"assignee">>)
        ),
        %% 失败事实不改变幂等键（重试不得产生第二条事实）
        ?assertEqual(Key, maps:get(idempotency_key, FailedItem)),
        ?assertEqual(Third, active_assignee(Scope, Identity)),

        %% 重试：先把第三人的经办结束，再从 failed 重跑 ⇒ 成功
        ok = end_active_assignment(Scope, Identity),
        {ok, Retried} = ?APP:execute_offboarding(Org, #{
            workspace_id => Ws,
            case_id => CaseId,
            expected_version => maps:get(version, Failed),
            actor_user_id => maps:get(owner_user_id, Scope)
        }),
        ?assertEqual(transferring, maps:get(status, Retried)),
        ?assertEqual(1, maps:get(item_success, Retried)),
        ?assertEqual(0, maps:get(item_failed, Retried)),
        {ok, [RetriedItem]} = eb_pg_store:list_offboarding_items(Org, Ws, CaseId),
        ?assertEqual(success, maps:get(status, RetriedItem)),
        ?assertEqual(Key, maps:get(idempotency_key, RetriedItem)),
        %% 失败原因在回到 success 时必须清空（列约束 ck_eoi_failure_reason 的口径）
        ?assertEqual(undefined, maps:get(failure_reason, RetriedItem)),
        ?assertEqual(Successor, active_assignee(Scope, Identity)),
        %% 两次 execute（一次失败一次重试）各审计一次，共 2 条，且不重复
        ?assertEqual(2, audit_count(Org, <<"offboarding.execute">>))
    after
        scrub(Scope)
    end.

%% ===================================================================
%% S3：verify + finalize
%% ===================================================================

%% 校验残留 / ID / Org / hash / count；全部通过才把 case 推进到 verifying。
s3_verify_checks_residuals_id_org_hash_count() ->
    Scope = ?FIX:new_scope(),
    {Org, Ws} = tenant(Scope),
    Leaver = maps:get(actor_user_id, Scope),
    Successor = maps:get(peer_user_id, Scope),
    Identity = maps:get(sales_identity_id, Scope),
    try
        ok = insert_member(Org, Successor, <<"member">>),
        Before = resource_fingerprint(Scope, Identity),
        {ok, Opened} = open_case(Scope, Leaver, Successor),
        CaseId = maps:get(case_id, Opened),
        SnapshotHash = maps:get(snapshot_hash, Opened),
        {ok, Executed} = ?APP:execute_offboarding(Org, #{
            workspace_id => Ws,
            case_id => CaseId,
            expected_version => maps:get(version, Opened),
            actor_user_id => maps:get(owner_user_id, Scope)
        }),
        {ok, Verified} = ?APP:verify_offboarding(Org, #{
            workspace_id => Ws,
            case_id => CaseId,
            expected_snapshot_hash => SnapshotHash,
            actor_user_id => maps:get(owner_user_id, Scope)
        }),
        ?assertEqual(verifying, maps:get(status, Verified)),
        ?assertEqual(maps:get(version, Executed) + 1, maps:get(version, Verified)),
        ?assertEqual(1, maps:get(items_total, Verified)),
        ?assertEqual(1, maps:get(item_success, Verified)),
        ?assertEqual(0, maps:get(item_failed, Verified)),
        %% 快照指纹与 S1 逐字相同（快照固定）
        ?assertEqual(SnapshotHash, maps:get(snapshot_hash, Verified)),
        %% 残留：leaver 在该 Org 已无任何 active 经办
        ?assertEqual([], maps:get(residual_assignments, Verified)),
        %% ID / Org / hash / count 逐项不变
        [ItemReport] = maps:get(items, Verified),
        ?assertEqual(maps:get(identity_id, Before), maps:get(identity_id, ItemReport)),
        ?assertEqual(maps:get(organization_id, Before), maps:get(organization_id, ItemReport)),
        ?assertEqual(maps:get(identity_hash, Before), maps:get(identity_hash, ItemReport)),
        ?assertEqual(
            maps:get(conversation_count, Before), maps:get(conversation_count, ItemReport)
        ),
        ?assertEqual(maps:get(contact_count, Before), maps:get(contact_count, ItemReport)),
        ?assertEqual(Successor, maps:get(assignee, ItemReport)),
        ?assertEqual(1, audit_count(Org, <<"offboarding.verify">>))
    after
        scrub(Scope)
    end.

%% 快照防篡改：传入与 S1 不符的指纹 ⇒ verify 拒绝，且 case 不被推进。
s3_snapshot_hash_tamper_is_refused() ->
    Scope = ?FIX:new_scope(),
    {Org, Ws} = tenant(Scope),
    Leaver = maps:get(actor_user_id, Scope),
    Successor = maps:get(peer_user_id, Scope),
    try
        ok = insert_member(Org, Successor, <<"member">>),
        {ok, Opened} = open_case(Scope, Leaver, Successor),
        CaseId = maps:get(case_id, Opened),
        {ok, _Executed} = ?APP:execute_offboarding(Org, #{
            workspace_id => Ws,
            case_id => CaseId,
            expected_version => maps:get(version, Opened),
            actor_user_id => maps:get(owner_user_id, Scope)
        }),
        {ok, Case} = eb_pg_store:fetch_offboarding_case(Org, Ws, CaseId),
        ?assertMatch(
            {error, {snapshot_mismatch, _, _}},
            ?APP:verify_offboarding(Org, #{
                workspace_id => Ws,
                case_id => CaseId,
                expected_snapshot_hash => <<"deadbeef">>,
                actor_user_id => maps:get(owner_user_id, Scope)
            })
        ),
        {ok, Unchanged} = eb_pg_store:fetch_offboarding_case(Org, Ws, CaseId),
        ?assertEqual(maps:get(version, Case), maps:get(version, Unchanged)),
        ?assertEqual(0, audit_count(Org, <<"offboarding.verify">>))
    after
        scrub(Scope)
    end.

%% A04 后半：**未 verify 不得 removed**。
%%
%% 关键点：execute 之后 leaver 已无 active 经办（DB 守卫本身会放行移除），
%% 所以拦住 finalize 的必须是「verify 未完成」这道闸门，而不是 DB 守卫的副作用。
s3_unverified_case_cannot_be_removed() ->
    Scope = ?FIX:new_scope(),
    {Org, Ws} = tenant(Scope),
    Leaver = maps:get(actor_user_id, Scope),
    Successor = maps:get(peer_user_id, Scope),
    try
        ok = insert_member(Org, Successor, <<"member">>),
        {ok, Opened} = open_case(Scope, Leaver, Successor),
        CaseId = maps:get(case_id, Opened),

        %% ① frozen（刚建档，尚未 execute）：拒绝，且零写入
        ?assertMatch(
            {error, {not_verified, frozen}},
            ?APP:finalize_offboarding(Org, #{
                workspace_id => Ws,
                case_id => CaseId,
                actor_user_id => maps:get(owner_user_id, Scope)
            })
        ),
        ?assertEqual({ok, suspended}, member_status(Org, Leaver)),
        ?assertEqual(0, audit_count(Org, <<"offboarding.finalize">>)),

        %% ② transferring（execute 之后、verify 之前）
        {ok, Executed} = ?APP:execute_offboarding(Org, #{
            workspace_id => Ws,
            case_id => CaseId,
            expected_version => maps:get(version, Opened),
            actor_user_id => maps:get(owner_user_id, Scope)
        }),
        ?assertEqual(transferring, maps:get(status, Executed)),
        %% 前置事实：leaver 已无残留 active 经办 ⇒ DB 守卫不会再挡移除
        ?assertEqual([], leaver_active_rows(Scope, Leaver)),
        ?assertMatch(
            {error, {not_verified, transferring}},
            ?APP:finalize_offboarding(Org, #{
                workspace_id => Ws,
                case_id => CaseId,
                actor_user_id => maps:get(owner_user_id, Scope)
            })
        ),
        %% 「未完成不能 removed」：成员仍是 suspended，未被移除
        ?assertEqual({ok, suspended}, member_status(Org, Leaver)),
        {ok, StillTransferring} = eb_pg_store:fetch_offboarding_case(Org, Ws, CaseId),
        ?assertEqual(transferring, maps:get(status, StillTransferring)),
        ?assertEqual(0, audit_count(Org, <<"offboarding.finalize">>))
    after
        scrub(Scope)
    end.

%% verify 的**残留**校验：交接范围之外的 active 经办必须被判红（case 落 failed）。
s3_verify_detects_residual_and_parks_case_failed() ->
    Scope = ?FIX:new_scope(),
    {Org, Ws} = tenant(Scope),
    Leaver = maps:get(actor_user_id, Scope),
    Successor = maps:get(peer_user_id, Scope),
    Service = maps:get(service_identity_id, Scope),
    try
        ok = insert_member(Org, Successor, <<"member">>),
        {ok, Opened} = open_case(Scope, Leaver, Successor),
        CaseId = maps:get(case_id, Opened),
        {ok, Executed} = ?APP:execute_offboarding(Org, #{
            workspace_id => Ws,
            case_id => CaseId,
            expected_version => maps:get(version, Opened),
            actor_user_id => maps:get(owner_user_id, Scope)
        }),
        %% 注入残留：leaver 在交接范围外又持有 customer_service 的 active 经办
        ok = bind_identity(Scope, Service, <<"customer_service">>, Leaver, Leaver),
        {error, {verification_failed, Reasons}} = ?APP:verify_offboarding(Org, #{
            workspace_id => Ws,
            case_id => CaseId,
            actor_user_id => maps:get(owner_user_id, Scope)
        }),
        ?assert(lists:keymember(residual_assignments, 1, Reasons)),
        {ok, Parked} = eb_pg_store:fetch_offboarding_case(Org, Ws, CaseId),
        ?assertEqual(failed, maps:get(status, Parked)),
        ?assertEqual(maps:get(version, Executed) + 1, maps:get(version, Parked)),
        %% 失败后仍然不能被移除
        ?assertMatch(
            {error, {not_verified, _}},
            ?APP:finalize_offboarding(Org, #{
                workspace_id => Ws,
                case_id => CaseId,
                actor_user_id => maps:get(owner_user_id, Scope)
            })
        ),
        ?assertEqual({ok, suspended}, member_status(Org, Leaver))
    after
        scrub(Scope)
    end.

%% 完成证明：finalize 移除成员并完成 case，审计恰一次；重复调用幂等且不再审计。
s3_finalize_removes_member_once_and_is_idempotent() ->
    Scope = ?FIX:new_scope(),
    {Org, Ws} = tenant(Scope),
    Leaver = maps:get(actor_user_id, Scope),
    Successor = maps:get(peer_user_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    try
        ok = insert_member(Org, Successor, <<"member">>),
        {ok, CaseId, _Version} = open_verify(Scope, Leaver, Successor),
        {ok, Finalized} = ?APP:finalize_offboarding(Org, #{
            workspace_id => Ws,
            case_id => CaseId,
            actor_user_id => Owner
        }),
        ?assertEqual(completed, maps:get(status, Finalized)),
        ?assertEqual(true, maps:get(member_removed, Finalized)),
        ?assertEqual(false, maps:get(idempotent, Finalized, false)),
        ?assertEqual({ok, removed}, member_status(Org, Leaver)),
        {ok, Completed} = eb_pg_store:fetch_offboarding_case(Org, Ws, CaseId),
        ?assertEqual(completed, maps:get(status, Completed)),
        ?assertEqual(1, audit_count(Org, <<"offboarding.finalize">>)),
        %% 幂等：重复 finalize 返回 ok（idempotent）且不追加审计、不重复移除
        {ok, Again} = ?APP:finalize_offboarding(Org, #{
            workspace_id => Ws,
            case_id => CaseId,
            actor_user_id => Owner
        }),
        ?assertEqual(completed, maps:get(status, Again)),
        ?assertEqual(true, maps:get(idempotent, Again)),
        ?assertEqual({ok, removed}, member_status(Org, Leaver)),
        ?assertEqual(1, audit_count(Org, <<"offboarding.finalize">>))
    after
        scrub(Scope)
    end.

%% A07：最终 remove 前的 **DB guard 同点复核**（双层守卫的第二层）。
%%
%% 场景（就是「检查与使用之间」的窗口）：verify 通过（那一刻无残留）之后，另一会话
%% 给 leaver 新建了一条 active 经办；finalize 仍然必须被**数据库**在同一语句内拒绝。
s3_final_remove_is_rechecked_by_db_guard_at_the_same_point() ->
    Scope = ?FIX:new_scope(),
    {Org, Ws} = tenant(Scope),
    Leaver = maps:get(actor_user_id, Scope),
    Successor = maps:get(peer_user_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    Service = maps:get(service_identity_id, Scope),
    Guard = <<"trg_organization_member_offboarding_guard">>,
    try
        ok = insert_member(Org, Successor, <<"member">>),
        {ok, CaseId, _Version} = open_verify(Scope, Leaver, Successor),
        %% verify 之后注入残留（模拟并发会话在「检查」之后、「使用」之前写入）
        ok = bind_identity(Scope, Service, <<"customer_service">>, Leaver, Leaver),
        ?assertMatch(
            {error, {409, _}},
            ?APP:finalize_offboarding(Org, #{
                workspace_id => Ws,
                case_id => CaseId,
                actor_user_id => Owner
            })
        ),
        %% case 未被推进、成员未被移除（拒绝是原子的）
        {ok, StillVerifying} = eb_pg_store:fetch_offboarding_case(Org, Ws, CaseId),
        ?assertEqual(verifying, maps:get(status, StillVerifying)),
        ?assertEqual({ok, suspended}, member_status(Org, Leaver)),
        ?assertEqual(0, audit_count(Org, <<"offboarding.finalize">>)),

        %% DB 纵深：绕过应用直接改 removed 也必须是 23514，且不落行
        Direct = ?FIX:exec(
            <<
                "UPDATE organization_member SET status = 'removed'"
                " WHERE organization_id = $1 AND user_id = $2"
            >>,
            [Org, Leaver]
        ),
        ?assertMatch({error, _}, Direct),
        ?assertEqual({ok, suspended}, member_status(Org, Leaver)),

        %% 负例（有牙齿）：验证拒绝确实来自该触发器 —— 在**同一事务内**禁用触发器后
        %% 同一条 UPDATE 会成功（1 行），事务回滚后触发器与数据都复原。
        Probe = ?FIX:tx(fun(Conn) ->
            Ddl =
                <<"ALTER TABLE organization_member DISABLE TRIGGER ", Guard/binary>>,
            1 = disable_trigger_result(elib_pg:execute(Conn, Ddl, [])),
            throw(
                {probe,
                    elib_pg:execute(
                        Conn,
                        <<
                            "UPDATE organization_member SET status = 'removed'"
                            " WHERE organization_id = $1 AND user_id = $2"
                        >>,
                        [Org, Leaver]
                    )}
            )
        end),
        ?assertEqual({rollback, {probe, {ok, 1}}}, Probe),
        %% 回滚后：触发器恢复、成员仍是 suspended（禁用它只是为了证明它有牙齿）
        ?assertEqual(true, trigger_enabled(Guard)),
        ?assertEqual({ok, suspended}, member_status(Org, Leaver))
    after
        scrub(Scope)
    end.

%% ===================================================================
%% 夹具辅助
%% ===================================================================

tenant(Scope) ->
    {maps:get(org_id, Scope), maps:get(workspace_id, Scope)}.

open_case(Scope, Leaver, Successor) ->
    {Org, Ws} = tenant(Scope),
    ?APP:open_offboarding(Org, #{
        workspace_id => Ws,
        leaver_user_id => Leaver,
        successor_user_id => Successor,
        reason => <<"eb08-synthetic">>,
        actor_user_id => maps:get(owner_user_id, Scope)
    }).

open_and_execute(Scope, Leaver, Successor) ->
    {Org, Ws} = tenant(Scope),
    {ok, Opened} = open_case(Scope, Leaver, Successor),
    CaseId = maps:get(case_id, Opened),
    {ok, Executed} = ?APP:execute_offboarding(Org, #{
        workspace_id => Ws,
        case_id => CaseId,
        expected_version => maps:get(version, Opened),
        actor_user_id => maps:get(owner_user_id, Scope)
    }),
    {ok, CaseId, maps:get(version, Executed)}.

%% open -> execute -> verify（S3 的前置链）。
open_verify(Scope, Leaver, Successor) ->
    {Org, Ws} = tenant(Scope),
    {ok, Opened} = open_case(Scope, Leaver, Successor),
    CaseId = maps:get(case_id, Opened),
    {ok, _Executed} = ?APP:execute_offboarding(Org, #{
        workspace_id => Ws,
        case_id => CaseId,
        expected_version => maps:get(version, Opened),
        actor_user_id => maps:get(owner_user_id, Scope)
    }),
    {ok, Verified} = ?APP:verify_offboarding(Org, #{
        workspace_id => Ws,
        case_id => CaseId,
        expected_snapshot_hash => maps:get(snapshot_hash, Opened),
        actor_user_id => maps:get(owner_user_id, Scope)
    }),
    ?assertEqual(verifying, maps:get(status, Verified)),
    {ok, CaseId, maps:get(version, Verified)}.

%% 直接经 store 把某个 identity 的 active 经办交给 To（合成前置）。
bind_identity(Scope, Identity, FunctionKey, From, To) ->
    {Org, Ws} = tenant(Scope),
    ok = end_active_assignment(Scope, Identity),
    {ok, _} = eb_pg_store:insert_assignment(Org, Ws, #{
        id => ?FIX:id(),
        business_identity_id => Identity,
        function_key => FunctionKey,
        user_id => To,
        assigned_by => From
    }),
    ok.

leaver_active_rows(Scope, UserId) ->
    {Org, Ws} = tenant(Scope),
    {ok, Rows} = eb_pg_store:list_assignments(Org, Ws),
    [
        R
     || R <- Rows,
        maps:get(user_id, R, undefined) =:= UserId,
        maps:get(status, R, undefined) =:= active
    ].

trigger_enabled(TriggerName) ->
    1 =:=
        ?FIX:scalar(
            <<"SELECT count(*) FROM pg_trigger WHERE tgname = $1 AND tgenabled = 'O'">>,
            [TriggerName],
            -1
        ).

%% DDL 的结果形态随命令标签而变（`{ok, []}` / `{ok, N}` / `{ok, N, Rows}`）：
%% 归一成「语句已执行」。
disable_trigger_result({ok, _}) -> 1;
disable_trigger_result({ok, _, _}) -> 1;
disable_trigger_result(Other) -> Other.

%% 直接经 store 的 CAS 改经办人（模拟「交接前经办人已被别人接走」的脏前置）。
rebind_directly(Scope, Identity, From, To) ->
    {Org, Ws} = tenant(Scope),
    ok = end_active_assignment(Scope, Identity),
    {ok, _} = eb_pg_store:insert_assignment(Org, Ws, #{
        id => ?FIX:id(),
        business_identity_id => Identity,
        function_key => <<"sales">>,
        user_id => To,
        assigned_by => From
    }),
    ok.

end_active_assignment(Scope, Identity) ->
    {Org, Ws} = tenant(Scope),
    case active_rows(Scope, Identity) of
        [] -> ok;
        _ -> eb_pg_store:advance_assignment(Org, Ws, Identity, active, ended)
    end.

%% 资源指纹：identity 自身的 ID/Org/hash + 挂在该 identity 上的会话/客户数。
%% 口径：**owner 与资源**（不是 assignee 记录）——assignee 变更本来就会新增历史行。
resource_fingerprint(Scope, IdentityId) ->
    {Org, Ws} = tenant(Scope),
    {ok, Identity} = eb_pg_store:fetch_identity(Org, Ws, IdentityId),
    {ok, Conversations} = eb_pg_store:list_conversations(Org, Ws),
    {ok, Contacts} = eb_pg_store:list_contacts(Org, Ws),
    #{
        identity_id => maps:get(id, Identity),
        organization_id => maps:get(organization_id, Identity),
        identity_hash => stable_hash(Identity),
        conversation_count =>
            length([
                C
             || C <- Conversations,
                maps:get(business_identity_id, C, undefined) =:= IdentityId
            ]),
        contact_count =>
            length([
                C
             || C <- Contacts,
                maps:get(created_by_business_identity_id, C, undefined) =:= IdentityId
            ])
    }.

stable_hash(Map) ->
    Bin = term_to_binary(lists:sort(maps:to_list(Map))),
    <<<<(nibble(N))>> || <<N:4>> <= crypto:hash(sha256, Bin)>>.

nibble(N) when N < 10 -> $0 + N;
nibble(N) -> $a + N - 10.

identity_rows(Scope, IdentityId) ->
    {Org, Ws} = tenant(Scope),
    {ok, Rows} = eb_pg_store:list_assignments(Org, Ws),
    [
        R
     || R <- Rows,
        maps:get(business_identity_id, R, undefined) =:= IdentityId
    ].

ended_assignee(Scope, IdentityId) ->
    case [R || R <- identity_rows(Scope, IdentityId), maps:get(status, R) =:= ended] of
        [R | _] -> maps:get(user_id, R);
        [] -> none
    end.

enterprise_route() ->
    #{
        auth_context => enterprise_member,
        surface => tenant,
        path => <<"/api/v1/enterprise/organizations/1/conversations">>,
        required_function => <<"sales">>,
        required_permission => <<"contact.read">>
    }.

old_jwt(UserId) ->
    #{class => imboy_jwt, user_id => UserId, claims => #{}}.

request(Org, WorkspaceId, UserId, Credential) ->
    #{
        organization_id => Org,
        workspace_id => WorkspaceId,
        user_id => UserId,
        credential => Credential
    }.

%% 真实企业访问判定：装配好的只读事实 Port + eb_auth_app 的纯决策函数。
enterprise_access(Org, Ws, UserId) ->
    eb_auth_app:authorize_via_port(
        ?PROBE, enterprise_route(), request(Org, Ws, UserId, old_jwt(UserId))
    ).

member_status(Org, UserId) ->
    member_status_via_port(Org, UserId).

member_status_via_port(Org, UserId) ->
    {ok, MemberFact} = eb_infra_ports:resolve(member_fact),
    MemberFact:member_status(Org, UserId).

%% 只读 Port 上不得出现任何写形状的 callback（A10 口径的机械判据）。
write_callbacks(PortModule) ->
    Callbacks = PortModule:behaviour_info(callbacks),
    [
        {Name, Arity}
     || {Name, Arity} <- Callbacks,
        lists:member(Name, [insert, update, delete, advance, append, purge, upsert, remove]) orelse
            lists:prefix("insert_", atom_to_list(Name)) orelse
            lists:prefix("update_", atom_to_list(Name)) orelse
            lists:prefix("delete_", atom_to_list(Name)) orelse
            lists:prefix("advance_", atom_to_list(Name)) orelse
            lists:prefix("append_", atom_to_list(Name))
    ].

%% 依赖方向：Core 侧文件零 enterprise_*/customer_service_* 模块引用（A06）。
reverse_reference_count() ->
    {ok, Src} = file:read_file(reverse_reference_path()),
    length(
        [
            M
         || M <- re_matches(Src, "\\b(enterprise_[a-z0-9_]+|customer_service_[a-z0-9_]+)\\b"),
            true
        ]
    ).

reverse_reference_path() ->
    "src/logic/organization_member_logic.erl".

re_matches(Subject, Pattern) ->
    case re:run(Subject, Pattern, [global, {capture, first, binary}]) of
        {match, Matches} -> [M || [M] <- Matches];
        nomatch -> []
    end.

insert_member(Org, Uid, Role) ->
    ?FIX:exec(
        <<
            "INSERT INTO organization_member(organization_id,user_id,role,status)"
            " VALUES ($1,$2,$3,'active')"
            " ON CONFLICT (organization_id,user_id) DO UPDATE SET role=EXCLUDED.role,"
            " status='active'"
        >>,
        [Org, Uid, Role]
    ).

leaver_assignment_rows(Scope, UserId) ->
    {Org, Ws} = tenant(Scope),
    {ok, Rows} = eb_pg_store:list_assignments(Org, Ws),
    [
        {
            maps:get(business_identity_id, R),
            maps:get(user_id, R),
            maps:get(status, R),
            maps:get(version, R)
        }
     || R <- Rows,
        maps:get(user_id, R, undefined) =:= UserId
    ].

active_assignee_status(Scope, IdentityId) ->
    case active_rows(Scope, IdentityId) of
        [R | _] -> maps:get(status, R);
        [] -> none
    end.

active_assignee(Scope, IdentityId) ->
    case active_rows(Scope, IdentityId) of
        [R | _] -> maps:get(user_id, R);
        [] -> none
    end.

active_rows(Scope, IdentityId) ->
    {Org, Ws} = tenant(Scope),
    {ok, Rows} = eb_pg_store:list_assignments(Org, Ws),
    [
        R
     || R <- Rows,
        maps:get(business_identity_id, R, undefined) =:= IdentityId,
        maps:get(status, R, undefined) =:= active
    ].

frozen_fields(Item) ->
    {
        maps:get(case_id, Item, undefined),
        maps:get(business_identity_id, Item, undefined),
        maps:get(function_key, Item, undefined),
        maps:get(from_user_id, Item, undefined),
        maps:get(to_user_id, Item, undefined),
        maps:get(idempotency_key, Item, undefined)
    }.

audit_count(Org, Action) ->
    ?FIX:scalar(
        <<
            "SELECT count(*) FROM enterprise_audit_event"
            " WHERE organization_id=$1 AND action=$2"
        >>,
        [Org, Action],
        -1
    ).

%% 清场：先删本卡合成的交接行（item 先于 case），再交给 EB-03 夹具清场，
%% 最后删掉本套件补建的成员行（夹具只清 actor 行）。
scrub(Scope) ->
    Org = maps:get(org_id, Scope, undefined),
    case is_integer(Org) of
        true ->
            _ = ?FIX:exec(<<"DELETE FROM enterprise_offboarding_item WHERE organization_id=$1">>, [
                Org
            ]),
            _ = ?FIX:exec(<<"DELETE FROM enterprise_offboarding_case WHERE organization_id=$1">>, [
                Org
            ]);
        false ->
            ok
    end,
    _ = ?FIX:cleanup(Scope),
    case is_integer(Org) of
        true ->
            _ = ?FIX:exec(<<"DELETE FROM organization_member WHERE organization_id=$1">>, [Org]);
        false ->
            ok
    end,
    ok.

%%% @doc eb_offboarding 纯函数测试（零 mock / 零 I/O / 零隐式时钟）。
%%%
%%% 覆盖（EB-02-A01）：
%%%   * case_statuses/0 值域冻结；
%%%   * 完整状态机真值表（每条合法迁移正例 + 每条非法迁移负例）；
%%%   * rebind/3 的 CAS —— version 不匹配返回 stale_version，
%%%     self-handover 被拒，成功时资源 id / owner 逐字不变；
%%%   * unfinished_case_unique/1 —— 同一 (org, leaver) 最多一个未完成 case；
%%%   * resume_after_failure/1 —— 保留 reason、幂等键不变、attempt 递增。
-module(eb_offboarding_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% case_statuses/0
%% ===================================================================

case_statuses_frozen_test() ->
    ?assertEqual(
        [draft, frozen, transferring, verifying, completed, failed],
        eb_offboarding:case_statuses()
    ).

%% ===================================================================
%% valid_transition/2 —— 合法迁移
%% ===================================================================

valid_draft_to_frozen_test() ->
    ?assertEqual(ok, eb_offboarding:valid_transition(draft, frozen)).

valid_frozen_to_transferring_test() ->
    ?assertEqual(ok, eb_offboarding:valid_transition(frozen, transferring)).

valid_transferring_to_verifying_test() ->
    ?assertEqual(ok, eb_offboarding:valid_transition(transferring, verifying)).

valid_transferring_to_failed_test() ->
    ?assertEqual(ok, eb_offboarding:valid_transition(transferring, failed)).

valid_verifying_to_completed_test() ->
    ?assertEqual(ok, eb_offboarding:valid_transition(verifying, completed)).

valid_verifying_to_failed_test() ->
    ?assertEqual(ok, eb_offboarding:valid_transition(verifying, failed)).

valid_failed_to_transferring_retry_test() ->
    ?assertEqual(ok, eb_offboarding:valid_transition(failed, transferring)).

%% ===================================================================
%% valid_transition/2 —— 非法迁移（全覆盖负例）
%% ===================================================================

invalid_draft_to_transferring_test() ->
    ?assertEqual(
        {error, {invalid_transition, draft, transferring}},
        eb_offboarding:valid_transition(draft, transferring)
    ).

invalid_draft_to_verifying_test() ->
    ?assertEqual(
        {error, {invalid_transition, draft, verifying}},
        eb_offboarding:valid_transition(draft, verifying)
    ).

invalid_draft_to_completed_test() ->
    ?assertEqual(
        {error, {invalid_transition, draft, completed}},
        eb_offboarding:valid_transition(draft, completed)
    ).

invalid_draft_to_failed_test() ->
    ?assertEqual(
        {error, {invalid_transition, draft, failed}},
        eb_offboarding:valid_transition(draft, failed)
    ).

invalid_draft_to_draft_test() ->
    ?assertEqual(
        {error, {invalid_transition, draft, draft}},
        eb_offboarding:valid_transition(draft, draft)
    ).

invalid_frozen_to_verifying_test() ->
    ?assertEqual(
        {error, {invalid_transition, frozen, verifying}},
        eb_offboarding:valid_transition(frozen, verifying)
    ).

invalid_frozen_to_completed_test() ->
    ?assertEqual(
        {error, {invalid_transition, frozen, completed}},
        eb_offboarding:valid_transition(frozen, completed)
    ).

invalid_frozen_to_failed_test() ->
    ?assertEqual(
        {error, {invalid_transition, frozen, failed}},
        eb_offboarding:valid_transition(frozen, failed)
    ).

invalid_frozen_to_draft_test() ->
    ?assertEqual(
        {error, {invalid_transition, frozen, draft}},
        eb_offboarding:valid_transition(frozen, draft)
    ).

invalid_transferring_to_completed_test() ->
    %% 不得跳过 verifying：完成证明是独立 Gate。
    ?assertEqual(
        {error, {invalid_transition, transferring, completed}},
        eb_offboarding:valid_transition(transferring, completed)
    ).

invalid_transferring_to_draft_test() ->
    ?assertEqual(
        {error, {invalid_transition, transferring, draft}},
        eb_offboarding:valid_transition(transferring, draft)
    ).

invalid_transferring_to_transferring_test() ->
    ?assertEqual(
        {error, {invalid_transition, transferring, transferring}},
        eb_offboarding:valid_transition(transferring, transferring)
    ).

invalid_verifying_to_transferring_test() ->
    ?assertEqual(
        {error, {invalid_transition, verifying, transferring}},
        eb_offboarding:valid_transition(verifying, transferring)
    ).

invalid_verifying_to_verifying_test() ->
    ?assertEqual(
        {error, {invalid_transition, verifying, verifying}},
        eb_offboarding:valid_transition(verifying, verifying)
    ).

invalid_failed_to_completed_test() ->
    ?assertEqual(
        {error, {invalid_transition, failed, completed}},
        eb_offboarding:valid_transition(failed, completed)
    ).

invalid_failed_to_failed_test() ->
    ?assertEqual(
        {error, {invalid_transition, failed, failed}},
        eb_offboarding:valid_transition(failed, failed)
    ).

invalid_completed_is_terminal_test() ->
    %% completed 是终态：不得再迁出。
    ?assertEqual(
        {error, {invalid_transition, completed, failed}},
        eb_offboarding:valid_transition(completed, failed)
    ),
    ?assertEqual(
        {error, {invalid_transition, completed, transferring}},
        eb_offboarding:valid_transition(completed, transferring)
    ),
    ?assertEqual(
        {error, {invalid_transition, completed, completed}},
        eb_offboarding:valid_transition(completed, completed)
    ).

invalid_unknown_status_test() ->
    ?assertEqual(
        {error, {invalid_transition, archived, completed}},
        eb_offboarding:valid_transition(archived, completed)
    ).

state_machine_is_exhaustively_frozen_test() ->
    %% A01 的穷举证据：case_statuses/0 的**全部** 6x6 = 36 个有序对逐个对照
    %% 冻结真值表（7 条合法 + 29 条非法，每条非法对都有负例断言）。
    Statuses = eb_offboarding:case_statuses(),
    Legal = [
        {draft, frozen},
        {frozen, transferring},
        {transferring, verifying},
        {transferring, failed},
        {verifying, completed},
        {verifying, failed},
        {failed, transferring}
    ],
    AllPairs = [{From, To} || From <- Statuses, To <- Statuses],
    ?assertEqual(36, length(AllPairs)),
    ?assertEqual(7, length(Legal)),
    lists:foreach(
        fun({From, To} = Pair) ->
            case lists:member(Pair, Legal) of
                true ->
                    ?assertEqual(ok, eb_offboarding:valid_transition(From, To));
                false ->
                    %% 每条非法迁移都必须返回带 From/To 的 invalid_transition
                    ?assertEqual(
                        {error, {invalid_transition, From, To}},
                        eb_offboarding:valid_transition(From, To)
                    ),
                    ?assertEqual(
                        {error, {invalid_transition, From, To}},
                        eb_offboarding:transition(From, To)
                    )
            end
        end,
        AllPairs
    ).

%% ===================================================================
%% transition/2
%% ===================================================================

transition_ok_test() ->
    ?assertEqual({ok, transferring}, eb_offboarding:transition(frozen, transferring)).

transition_illegal_test() ->
    ?assertEqual(
        {error, {invalid_transition, frozen, completed}},
        eb_offboarding:transition(frozen, completed)
    ).

transition_happy_path_chain_test() ->
    %% draft -> frozen -> transferring -> verifying -> completed 全链可走通。
    ?assertEqual({ok, frozen}, eb_offboarding:transition(draft, frozen)),
    ?assertEqual({ok, transferring}, eb_offboarding:transition(frozen, transferring)),
    ?assertEqual({ok, verifying}, eb_offboarding:transition(transferring, verifying)),
    ?assertEqual({ok, completed}, eb_offboarding:transition(verifying, completed)).

%% ===================================================================
%% rebind/3 —— CAS
%% ===================================================================

rebind_ok_test() ->
    {ok, New} = eb_offboarding:rebind(assignment(), 3, 1_700_000_000),
    ?assertEqual(100, maps:get(business_identity_id, New)),
    ?assertEqual(100, maps:get(identity_id, New)),
    ?assertEqual(100, maps:get(resource_id, New)),
    ?assertEqual(1, maps:get(organization_id, New)),
    ?assertEqual(10, maps:get(user_id, New)),
    ?assertEqual(active, maps:get(status, New)),
    ?assertEqual(4, maps:get(version, New)),
    ?assertEqual(undefined, maps:get(ended_at, New)),
    ?assertEqual(1_700_000_000, maps:get(assigned_at, New)).

rebind_stale_version_test() ->
    ?assertEqual(
        {error, {stale_version, 2, 3}},
        eb_offboarding:rebind(assignment(), 2, 1_700_000_000)
    ).

rebind_stale_version_ahead_test() ->
    %% 期望 version 大于实际（并发已推进过）同样必须失败。
    ?assertEqual(
        {error, {stale_version, 9, 3}},
        eb_offboarding:rebind(assignment(), 9, 1_700_000_000)
    ).

rebind_self_handover_rejected_test() ->
    A = (assignment())#{to_user => 9},
    ?assertEqual(
        {error, self_handover},
        eb_offboarding:rebind(A, 3, 1_700_000_000)
    ).

rebind_preserves_owner_and_resource_id_test() ->
    {ok, New} = eb_offboarding:rebind(assignment(), 3, 1_700_000_000),
    Old = assignment(),
    ?assertEqual(maps:get(organization_id, Old), maps:get(organization_id, New)),
    ?assertEqual(maps:get(resource_id, Old), maps:get(resource_id, New)),
    ?assertEqual(maps:get(identity_id, Old), maps:get(identity_id, New)),
    %% 只换 assignee（输入用 from_user/to_user，产出用 assignment 列名 user_id）。
    ?assertNotEqual(maps:get(from_user, Old), maps:get(user_id, New)),
    ?assertEqual(maps:get(to_user, Old), maps:get(user_id, New)).

rebind_not_a_map_test() ->
    ?assertEqual(
        {error, invalid_assignment},
        eb_offboarding:rebind(undefined, 3, 1_700_000_000)
    ).

rebind_missing_version_test() ->
    A = maps:remove(version, assignment()),
    ?assertEqual(
        {error, invalid_assignment},
        eb_offboarding:rebind(A, 3, 1_700_000_000)
    ).

%% ===================================================================
%% unfinished_case_unique/1
%% ===================================================================

unfinished_case_unique_empty_test() ->
    ?assertEqual(ok, eb_offboarding:unfinished_case_unique([])).

unfinished_case_unique_completed_plus_draft_test() ->
    Cases = [
        #{organization_id => 1, leaver_user_id => 9, status => completed},
        #{organization_id => 1, leaver_user_id => 9, status => draft}
    ],
    ?assertEqual(ok, eb_offboarding:unfinished_case_unique(Cases)).

unfinished_case_unique_failed_counts_as_unfinished_test() ->
    Cases = [
        #{organization_id => 1, leaver_user_id => 9, status => failed},
        #{organization_id => 1, leaver_user_id => 9, status => transferring}
    ],
    ?assertEqual(
        {error, {duplicate_unfinished_case, {1, 9}}},
        eb_offboarding:unfinished_case_unique(Cases)
    ).

unfinished_case_unique_same_leaver_other_org_test() ->
    Cases = [
        #{organization_id => 1, leaver_user_id => 9, status => draft},
        #{organization_id => 2, leaver_user_id => 9, status => draft}
    ],
    ?assertEqual(ok, eb_offboarding:unfinished_case_unique(Cases)).

unfinished_case_unique_other_leaver_test() ->
    Cases = [
        #{organization_id => 1, leaver_user_id => 9, status => draft},
        #{organization_id => 1, leaver_user_id => 10, status => draft}
    ],
    ?assertEqual(ok, eb_offboarding:unfinished_case_unique(Cases)).

unfinished_case_unique_not_a_list_test() ->
    ?assertEqual(
        {error, invalid_cases},
        eb_offboarding:unfinished_case_unique(undefined)
    ).

%% ===================================================================
%% resume_after_failure/1
%% ===================================================================

resume_after_failure_ok_test() ->
    Failed = item(failed, timeout, 1),
    {ok, Item} = eb_offboarding:resume_after_failure(Failed),
    %% 幂等键不变：重试不得产生第二条消息/审计。
    ?assertEqual(<<"k-1">>, maps:get(idempotency_key, Item)),
    ?assertEqual(pending, maps:get(status, Item)),
    ?assertEqual(2, maps:get(attempt, Item)),
    %% reason 保留，并额外留痕为 last_reason。
    ?assertEqual(timeout, maps:get(reason, Item)),
    ?assertEqual(timeout, maps:get(last_reason, Item)).

resume_after_failure_second_retry_test() ->
    {ok, Item} = eb_offboarding:resume_after_failure(item(failed, db_conflict, 2)),
    ?assertEqual(3, maps:get(attempt, Item)),
    ?assertEqual(db_conflict, maps:get(last_reason, Item)).

resume_after_failure_not_failed_test() ->
    ?assertEqual(
        {error, {not_failed, pending}},
        eb_offboarding:resume_after_failure(item(pending, undefined, 0))
    ).

resume_after_failure_success_not_resumable_test() ->
    ?assertEqual(
        {error, {not_failed, success}},
        eb_offboarding:resume_after_failure(item(success, undefined, 1))
    ).

resume_after_failure_not_a_map_test() ->
    ?assertEqual({error, invalid_item}, eb_offboarding:resume_after_failure([])).

%% ===================================================================
%% 辅助
%% ===================================================================

assignment() ->
    #{
        organization_id => 1,
        identity_id => 100,
        resource_id => 100,
        from_user => 9,
        to_user => 10,
        version => 3
    }.

item(Status, Reason, Attempt) ->
    #{
        idempotency_key => <<"k-1">>,
        status => Status,
        reason => Reason,
        attempt => Attempt
    }.

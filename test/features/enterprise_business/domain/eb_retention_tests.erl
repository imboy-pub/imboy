%%% @doc eb_retention 纯函数测试（零 mock / 零 I/O / 零隐式时钟）。
%%%
%%% 覆盖（EB-02-A05）：注入时钟下的 policy snapshot / retain_until、
%%% retain_until 不可缩短、hold scope 覆盖判定与 released_at 失效、
%%% purge eligibility（含 retain_until == Now 的明确边界）、
%%% policy 版本只允许延长。
-module(eb_retention_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DAY, 86400).

%% ===================================================================
%% policy_snapshot/3 —— 注入时钟，retain_until = AcceptedAt + Days
%% ===================================================================

policy_snapshot_ok_test() ->
    Policy = #{policy_id => 7, policy_version => 3, retention_days => 1095},
    {ok, Snap} = eb_retention:policy_snapshot(Policy, 1_700_000_000, 1_700_000_100),
    ?assertEqual(7, maps:get(policy_id, Snap)),
    ?assertEqual(3, maps:get(policy_version, Snap)),
    ?assertEqual(1095, maps:get(retention_days, Snap)),
    ?assertEqual(1_700_000_000 + 1095 * ?DAY, maps:get(retain_until, Snap)).

policy_snapshot_exact_boundary_now_test() ->
    %% AcceptedAt == Clock 合法（接受瞬间的快照）。
    Now = 1_700_000_000,
    Policy = #{policy_id => 7, policy_version => 1, retention_days => 1095},
    ?assertMatch({ok, _}, eb_retention:policy_snapshot(Policy, Now, Now)).

policy_snapshot_uses_injected_clock_only_test() ->
    %% 同一组输入必须给出同一结果：任何隐式 os:timestamp/rand 都会破坏该等式。
    Policy = #{policy_id => 1, policy_version => 1, retention_days => 30},
    A = eb_retention:policy_snapshot(Policy, 1_000, 2_000),
    B = eb_retention:policy_snapshot(Policy, 1_000, 2_000),
    ?assertEqual(A, B).

policy_snapshot_future_accepted_at_rejected_test() ->
    Policy = #{policy_id => 1, policy_version => 1, retention_days => 30},
    ?assertEqual(
        {error, {accepted_at_in_future, 5_000, 1_000}},
        eb_retention:policy_snapshot(Policy, 5_000, 1_000)
    ).

policy_snapshot_unknown_version_rejected_test() ->
    %% policy_version 未知/缺失 → fail-closed，不得回退默认期限。
    Policy = #{policy_id => 1, retention_days => 30},
    ?assertEqual(
        {error, {invalid_policy, policy_version}},
        eb_retention:policy_snapshot(Policy, 1_000, 2_000)
    ).

policy_snapshot_missing_retention_days_rejected_test() ->
    Policy = #{policy_id => 1, policy_version => 1},
    ?assertEqual(
        {error, {invalid_policy, retention_days}},
        eb_retention:policy_snapshot(Policy, 1_000, 2_000)
    ).

policy_snapshot_negative_retention_days_rejected_test() ->
    Policy = #{policy_id => 1, policy_version => 1, retention_days => -1},
    ?assertEqual(
        {error, {invalid_policy, retention_days}},
        eb_retention:policy_snapshot(Policy, 1_000, 2_000)
    ).

policy_snapshot_zero_days_test() ->
    %% 0 天是合法值（保留期由 policy 决定，不由 domain 决定）。
    Policy = #{policy_id => 1, policy_version => 1, retention_days => 0},
    {ok, Snap} = eb_retention:policy_snapshot(Policy, 1_000, 2_000),
    ?assertEqual(1_000, maps:get(retain_until, Snap)).

%% ===================================================================
%% shorten_forbidden/2 —— 只允许后移
%% ===================================================================

shorten_forbidden_later_ok_test() ->
    ?assertEqual(ok, eb_retention:shorten_forbidden(1_000, 2_000)).

shorten_forbidden_identical_ok_test() ->
    ?assertEqual(ok, eb_retention:shorten_forbidden(1_000, 1_000)).

shorten_forbidden_earlier_rejected_test() ->
    ?assertEqual(
        {error, {retention_shorten_forbidden, 2_000, 1_000}},
        eb_retention:shorten_forbidden(2_000, 1_000)
    ).

%% ===================================================================
%% hold_covers/2
%% ===================================================================

hold_covers_workspace_scope_test() ->
    Hold = hold(workspace, 1, 10, 100, 1000, undefined),
    ?assert(eb_retention:hold_covers(Hold, target(1, 10, 400, 4000))),
    ?assert(eb_retention:hold_covers(Hold, target(1, 10, 999, 9999))).

hold_covers_workspace_scope_other_workspace_test() ->
    Hold = hold(workspace, 1, 10, 100, 1000, undefined),
    ?assertNot(eb_retention:hold_covers(Hold, target(1, 11, 400, 4000))).

hold_covers_workspace_scope_other_org_test() ->
    Hold = hold(workspace, 1, 10, 100, 1000, undefined),
    ?assertNot(eb_retention:hold_covers(Hold, target(2, 10, 400, 4000))).

hold_covers_conversation_scope_test() ->
    Hold = hold(conversation, 1, 10, 100, 1000, undefined),
    ?assert(eb_retention:hold_covers(Hold, target(1, 10, 100, 4000))),
    ?assert(eb_retention:hold_covers(Hold, target(1, 10, 100, 9999))),
    ?assertNot(eb_retention:hold_covers(Hold, target(1, 10, 101, 4000))).

hold_covers_message_scope_test() ->
    Hold = hold(message, 1, 10, 100, 1000, undefined),
    ?assert(eb_retention:hold_covers(Hold, target(1, 10, 100, 1000))),
    ?assertNot(eb_retention:hold_covers(Hold, target(1, 10, 100, 1001))).

hold_covers_released_is_inactive_test() ->
    Released = hold(message, 1, 10, 100, 1000, 2_000),
    ?assertNot(eb_retention:hold_covers(Released, target(1, 10, 100, 1000))).

hold_covers_unknown_scope_is_false_test() ->
    Hold = hold(galaxy, 1, 10, 100, 1000, undefined),
    ?assertNot(eb_retention:hold_covers(Hold, target(1, 10, 100, 1000))).

%% ===================================================================
%% purge_eligible/3
%% ===================================================================

purge_eligible_before_retain_until_test() ->
    Msg = message(1, 10, 100, 1000, 5_000),
    ?assertEqual(
        {ineligible, retain_not_reached},
        eb_retention:purge_eligible(Msg, [], 4_999)
    ).

purge_eligible_at_retain_until_boundary_test() ->
    %% 边界冻结：Now == retain_until 视为"已到期"，无 active hold 即可 purge。
    Msg = message(1, 10, 100, 1000, 5_000),
    ?assertEqual(eligible, eb_retention:purge_eligible(Msg, [], 5_000)).

purge_eligible_after_retain_until_test() ->
    Msg = message(1, 10, 100, 1000, 5_000),
    ?assertEqual(eligible, eb_retention:purge_eligible(Msg, [], 5_001)).

purge_eligible_active_hold_beats_retain_until_test() ->
    Msg = message(1, 10, 100, 1000, 5_000),
    Holds = [hold(conversation, 1, 10, 100, 900, undefined)],
    ?assertEqual(
        {ineligible, active_hold},
        eb_retention:purge_eligible(Msg, Holds, 9_999)
    ).

purge_eligible_released_hold_does_not_block_test() ->
    Msg = message(1, 10, 100, 1000, 5_000),
    Holds = [hold(conversation, 1, 10, 100, 900, 6_000)],
    ?assertEqual(eligible, eb_retention:purge_eligible(Msg, Holds, 9_999)).

purge_eligible_other_org_hold_does_not_block_test() ->
    Msg = message(1, 10, 100, 1000, 5_000),
    Holds = [hold(workspace, 2, 10, 100, 900, undefined)],
    ?assertEqual(eligible, eb_retention:purge_eligible(Msg, Holds, 9_999)).

purge_eligible_missing_retain_until_is_fail_closed_test() ->
    %% 无 retain_until 一律不清理（宁可多保留）。
    Msg = #{
        organization_id => 1,
        workspace_id => 10,
        conversation_id => 100,
        message_id => 1000,
        visibility => visible
    },
    ?assertEqual(
        {ineligible, missing_retain_until},
        eb_retention:purge_eligible(Msg, [], 9_999)
    ).

purge_eligible_client_hidden_does_not_change_eligibility_test() ->
    %% 客户端隐藏/未来撤回只改可见性，不改变保留裁决。
    Visible = message(1, 10, 100, 1000, 5_000),
    Hidden = Visible#{visibility => tombstoned},
    ?assertEqual(
        eb_retention:purge_eligible(Visible, [], 5_000),
        eb_retention:purge_eligible(Hidden, [], 5_000)
    ).

%% ===================================================================
%% policy_extension_only/2 —— 版本只允许延长
%% ===================================================================

policy_extension_only_no_existing_ok_test() ->
    ?assertEqual(ok, eb_retention:policy_extension_only(1095, [])).

policy_extension_only_equal_ok_test() ->
    ?assertEqual(ok, eb_retention:policy_extension_only(1095, [30, 1095])).

policy_extension_only_longer_ok_test() ->
    ?assertEqual(ok, eb_retention:policy_extension_only(2000, [30, 1095])).

policy_extension_only_shorter_rejected_test() ->
    ?assertEqual(
        {error, {retention_shorten_forbidden, 1095, 365}},
        eb_retention:policy_extension_only(365, [30, 1095])
    ).

policy_extension_only_accepts_policy_maps_test() ->
    Existing = [
        #{policy_id => 1, policy_version => 1, retention_days => 30},
        #{policy_id => 2, policy_version => 2, retention_days => 1095}
    ],
    ?assertEqual(ok, eb_retention:policy_extension_only(1095, Existing)),
    ?assertEqual(
        {error, {retention_shorten_forbidden, 1095, 90}},
        eb_retention:policy_extension_only(90, Existing)
    ).

policy_extension_only_invalid_new_days_test() ->
    ?assertEqual(
        {error, {invalid_retention_days, foo}},
        eb_retention:policy_extension_only(foo, [30])
    ).

%% ===================================================================
%% 辅助
%% ===================================================================

hold(Scope, OrgId, WorkspaceId, ConversationId, MessageId, ReleasedAt) ->
    #{
        scope => Scope,
        organization_id => OrgId,
        workspace_id => WorkspaceId,
        conversation_id => ConversationId,
        message_id => MessageId,
        released_at => ReleasedAt
    }.

target(OrgId, WorkspaceId, ConversationId, MessageId) ->
    #{
        organization_id => OrgId,
        workspace_id => WorkspaceId,
        conversation_id => ConversationId,
        message_id => MessageId
    }.

message(OrgId, WorkspaceId, ConversationId, MessageId, RetainUntil) ->
    #{
        organization_id => OrgId,
        workspace_id => WorkspaceId,
        conversation_id => ConversationId,
        message_id => MessageId,
        retain_until => RetainUntil,
        visibility => visible
    }.

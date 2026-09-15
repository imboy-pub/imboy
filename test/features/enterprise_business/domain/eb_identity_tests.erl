%%% @doc eb_identity 纯函数测试（零 mock / 零 I/O / 零时间依赖）。
%%%
%%% 覆盖（EB-02-A01 / EB-02-A02）：
%%%   * function_keys/0 值域冻结（V1 仅 sales|customer_service）；
%%%   * identity 与 assignment 状态迁移真值表全枚举（每条非法迁移都有负例）；
%%%   * assignment_invariants/1 四条崩溃不变量（identity 基数、
%%%     (org,user,function) 基数、active 必须有 user、active 不得有 ended_at）；
%%%   * new_identity/1 参数收敛（org 整数、function_key 值域、display_name 非空）；
%%%   * owner/assignee/actor 三分（含 user_id 被当作 owner 的负例）。
-module(eb_identity_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% function_keys/0 —— V1 值域冻结（不得悄悄扩容）
%% ===================================================================

function_keys_frozen_test() ->
    ?assertEqual(
        [<<"sales">>, <<"customer_service">>],
        eb_identity:function_keys()
    ).

function_keys_rejects_undefined_additions_test() ->
    %% manager/admin 未经新决策不得出现（plan EB-D11：Stop 项）。
    Keys = eb_identity:function_keys(),
    ?assertEqual(false, lists:member(<<"manager">>, Keys)),
    ?assertEqual(false, lists:member(<<"admin">>, Keys)),
    ?assertEqual(false, lists:member(<<"customer_service_manager">>, Keys)).

%% ===================================================================
%% valid_transition/2 —— identity
%% ===================================================================

identity_active_to_retired_ok_test() ->
    ?assertEqual(ok, eb_identity:valid_transition(identity, {active, retired})).

identity_active_to_active_duplicate_test() ->
    ?assertEqual(
        {error, duplicate_occupation},
        eb_identity:valid_transition(identity, {active, active})
    ).

identity_retired_to_active_invalid_test() ->
    ?assertEqual(
        {error, {invalid_transition, retired, active}},
        eb_identity:valid_transition(identity, {retired, active})
    ).

identity_retired_to_retired_invalid_test() ->
    ?assertEqual(
        {error, {invalid_transition, retired, retired}},
        eb_identity:valid_transition(identity, {retired, retired})
    ).

%% ===================================================================
%% valid_transition/2 —— assignment
%% ===================================================================

assignment_active_to_ended_ok_test() ->
    ?assertEqual(ok, eb_identity:valid_transition(assignment, {active, ended})).

assignment_active_to_active_duplicate_test() ->
    ?assertEqual(
        {error, duplicate_occupation},
        eb_identity:valid_transition(assignment, {active, active})
    ).

assignment_ended_to_active_requires_reopen_test() ->
    %% ended -> active 不是自由迁移：必须携带显式 reopen 意图。
    ?assertEqual(
        {error, reopen_required},
        eb_identity:valid_transition(assignment, {ended, active})
    ).

assignment_ended_to_active_with_reopen_ok_test() ->
    ?assertEqual(
        ok,
        eb_identity:valid_transition(assignment, {{ended, reopen}, active})
    ).

assignment_ended_to_ended_invalid_test() ->
    ?assertEqual(
        {error, {invalid_transition, ended, ended}},
        eb_identity:valid_transition(assignment, {ended, ended})
    ).

assignment_reopen_intent_to_ended_invalid_test() ->
    ?assertEqual(
        {error, {invalid_transition, {ended, reopen}, ended}},
        eb_identity:valid_transition(assignment, {{ended, reopen}, ended})
    ).

unknown_transition_kind_test() ->
    ?assertMatch(
        {error, {unknown_transition_kind, pricing}},
        eb_identity:valid_transition(pricing, {active, ended})
    ).

transitions_are_exhaustively_frozen_test() ->
    %% A01 的穷举证据：identity 的全部 4 个有序对 + assignment 的全部 6 个
    %% 输入组合，逐个对照冻结真值表（多一个合法/少一个非法都会红）。
    IdentityPairs = [{active, retired}, {active, active}, {retired, active}, {retired, retired}],
    ?assertEqual(4, length(IdentityPairs)),
    lists:foreach(
        fun(Pair) ->
            ?assertEqual(
                expected_identity_transition(Pair),
                eb_identity:valid_transition(identity, Pair)
            )
        end,
        IdentityPairs
    ),
    AssignmentPairs = [
        {active, ended},
        {{ended, reopen}, active},
        {active, active},
        {ended, active},
        {ended, ended},
        {{ended, reopen}, ended}
    ],
    ?assertEqual(6, length(AssignmentPairs)),
    lists:foreach(
        fun(Pair) ->
            ?assertEqual(
                expected_assignment_transition(Pair),
                eb_identity:valid_transition(assignment, Pair)
            )
        end,
        AssignmentPairs
    ).

expected_identity_transition({active, retired}) ->
    ok;
expected_identity_transition({active, active}) ->
    {error, duplicate_occupation};
expected_identity_transition({retired, active}) ->
    {error, {invalid_transition, retired, active}};
expected_identity_transition({retired, retired}) ->
    {error, {invalid_transition, retired, retired}}.

expected_assignment_transition({active, ended}) ->
    ok;
expected_assignment_transition({{ended, reopen}, active}) ->
    ok;
expected_assignment_transition({active, active}) ->
    {error, duplicate_occupation};
expected_assignment_transition({ended, active}) ->
    {error, reopen_required};
expected_assignment_transition({ended, ended}) ->
    {error, {invalid_transition, ended, ended}};
expected_assignment_transition({{ended, reopen}, ended}) ->
    {error, {invalid_transition, {ended, reopen}, ended}}.

%% ===================================================================
%% assignment_invariants/1
%% ===================================================================

assignment_invariants_empty_ok_test() ->
    ?assertEqual(ok, eb_identity:assignment_invariants([])).

assignment_invariants_one_active_per_identity_ok_test() ->
    Assignments = [
        assign(1, 100, <<"sales">>, 9, active, undefined),
        assign(1, 100, <<"sales">>, 8, ended, 1000)
    ],
    ?assertEqual(ok, eb_identity:assignment_invariants(Assignments)).

assignment_invariants_multiple_active_identity_test() ->
    Assignments = [
        assign(1, 100, <<"sales">>, 9, active, undefined),
        assign(1, 100, <<"sales">>, 8, active, undefined)
    ],
    ?assertEqual(
        {error, {multiple_active_identity, {1, 100}}},
        eb_identity:assignment_invariants(Assignments)
    ).

assignment_invariants_two_functions_same_user_ok_test() ->
    %% 同一 user 在一个 Org 可同时持有一个 sales 与一个 customer_service。
    Assignments = [
        assign(1, 100, <<"sales">>, 9, active, undefined),
        assign(1, 200, <<"customer_service">>, 9, active, undefined)
    ],
    ?assertEqual(ok, eb_identity:assignment_invariants(Assignments)).

assignment_invariants_multiple_active_user_function_test() ->
    Assignments = [
        assign(1, 100, <<"sales">>, 9, active, undefined),
        assign(1, 200, <<"sales">>, 9, active, undefined)
    ],
    ?assertEqual(
        {error, {multiple_active_user_function, {1, 9, <<"sales">>}}},
        eb_identity:assignment_invariants(Assignments)
    ).

assignment_invariants_active_missing_user_test() ->
    Assignments = [
        #{
            organization_id => 1,
            business_identity_id => 100,
            function_key => <<"sales">>,
            user_id => undefined,
            status => active,
            assigned_at => 100,
            ended_at => undefined
        }
    ],
    ?assertEqual(
        {error, {active_missing_user, {1, 100}}},
        eb_identity:assignment_invariants(Assignments)
    ).

assignment_invariants_active_has_ended_at_test() ->
    Assignments = [
        #{
            organization_id => 1,
            business_identity_id => 100,
            function_key => <<"sales">>,
            user_id => 9,
            status => active,
            assigned_at => 100,
            ended_at => 500
        }
    ],
    ?assertEqual(
        {error, {active_has_ended_at, {1, 100}}},
        eb_identity:assignment_invariants(Assignments)
    ).

assignment_invariants_ended_without_ended_at_test() ->
    %% ended 行的时间区间必须闭合，否则历史不可证。
    Assignments = [
        #{
            organization_id => 1,
            business_identity_id => 100,
            function_key => <<"sales">>,
            user_id => 9,
            status => ended,
            assigned_at => 100,
            ended_at => undefined
        }
    ],
    ?assertEqual(
        {error, {ended_missing_ended_at, {1, 100}}},
        eb_identity:assignment_invariants(Assignments)
    ).

assignment_invariants_cross_org_isolated_test() ->
    %% 同 user/function 在另一个 Org 不算冲突（租户作用域）。
    Assignments = [
        assign(1, 100, <<"sales">>, 9, active, undefined),
        assign(2, 100, <<"sales">>, 9, active, undefined)
    ],
    ?assertEqual(ok, eb_identity:assignment_invariants(Assignments)).

assignment_invariants_not_a_list_test() ->
    ?assertEqual(
        {error, invalid_assignments},
        eb_identity:assignment_invariants(#{})
    ).

%% ===================================================================
%% new_identity/1 —— 参数收敛
%% ===================================================================

new_identity_ok_test() ->
    {ok, Identity} = eb_identity:new_identity(#{
        organization_id => 1,
        function_key => <<"sales">>,
        display_name => <<"华东销售 03">>
    }),
    ?assertEqual(1, maps:get(organization_id, Identity)),
    ?assertEqual(<<"sales">>, maps:get(function_key, Identity)),
    ?assertEqual(<<"华东销售 03">>, maps:get(display_name, Identity)),
    ?assertEqual(active, maps:get(status, Identity)),
    ?assertEqual(1, maps:get(version, Identity)).

new_identity_trims_display_name_test() ->
    {ok, Identity} = eb_identity:new_identity(#{
        organization_id => 1,
        function_key => <<"customer_service">>,
        display_name => <<"  售后坐席 07  ">>
    }),
    ?assertEqual(<<"售后坐席 07">>, maps:get(display_name, Identity)).

new_identity_unknown_function_key_test() ->
    ?assertEqual(
        {error, {unknown_function_key, <<"manager">>}},
        eb_identity:new_identity(#{
            organization_id => 1,
            function_key => <<"manager">>,
            display_name => <<"x">>
        })
    ).

new_identity_empty_display_name_test() ->
    ?assertEqual(
        {error, empty_display_name},
        eb_identity:new_identity(#{
            organization_id => 1,
            function_key => <<"sales">>,
            display_name => <<"   ">>
        })
    ).

new_identity_non_binary_display_name_test() ->
    ?assertEqual(
        {error, {invalid_display_name, 42}},
        eb_identity:new_identity(#{
            organization_id => 1,
            function_key => <<"sales">>,
            display_name => 42
        })
    ).

new_identity_non_integer_organization_id_test() ->
    ?assertEqual(
        {error, {invalid_organization_id, <<"1">>}},
        eb_identity:new_identity(#{
            organization_id => <<"1">>,
            function_key => <<"sales">>,
            display_name => <<"ok">>
        })
    ).

new_identity_missing_organization_id_test() ->
    ?assertEqual(
        {error, {invalid_organization_id, undefined}},
        eb_identity:new_identity(#{
            function_key => <<"sales">>,
            display_name => <<"ok">>
        })
    ).

new_identity_not_a_map_test() ->
    ?assertEqual({error, invalid_params}, eb_identity:new_identity(undefined)).

%% ===================================================================
%% owner_assignee_actor/1 —— 三分
%% ===================================================================

owner_assignee_actor_ok_test() ->
    ?assertEqual(
        {ok, #{owner => 1, assignee => 100, actor => 9}},
        eb_identity:owner_assignee_actor(#{
            organization_id => 1,
            business_identity_id => 100,
            actor_user_id => 9
        })
    ).

owner_assignee_actor_inbound_actor_undefined_test() ->
    %% 客户入站没有 actor。
    ?assertEqual(
        {ok, #{owner => 1, assignee => 100, actor => undefined}},
        eb_identity:owner_assignee_actor(#{
            organization_id => 1,
            business_identity_id => 100,
            actor_user_id => undefined
        })
    ).

owner_assignee_actor_user_as_owner_rejected_test() ->
    ?assertEqual(
        {error, user_cannot_be_owner},
        eb_identity:owner_assignee_actor(#{
            user_id => 9,
            business_identity_id => 100,
            actor_user_id => 9
        })
    ).

owner_assignee_actor_owner_id_user_rejected_test() ->
    ?assertEqual(
        {error, user_cannot_be_owner},
        eb_identity:owner_assignee_actor(#{
            owner_user_id => 9,
            organization_id => 1,
            business_identity_id => 100,
            actor_user_id => 9
        })
    ).

owner_assignee_actor_creator_user_id_as_owner_rejected_test() ->
    ?assertEqual(
        {error, user_cannot_be_owner},
        eb_identity:owner_assignee_actor(#{
            creator_user_id => 9,
            business_identity_id => 100,
            actor_user_id => 9
        })
    ).

owner_assignee_actor_user_id_as_actor_is_fine_test() ->
    %% user_id 出现在 map 里但 organization_id 才是 owner 时不得误判。
    ?assertEqual(
        {ok, #{owner => 1, assignee => 100, actor => 9}},
        eb_identity:owner_assignee_actor(#{
            user_id => 9,
            organization_id => 1,
            business_identity_id => 100,
            actor_user_id => 9
        })
    ).

owner_assignee_actor_missing_assignee_test() ->
    ?assertEqual(
        {error, {missing_assignee, business_identity_id}},
        eb_identity:owner_assignee_actor(#{
            organization_id => 1,
            actor_user_id => 9
        })
    ).

owner_assignee_actor_not_a_map_test() ->
    ?assertEqual({error, invalid_params}, eb_identity:owner_assignee_actor([])).

%% ===================================================================
%% 辅助
%% ===================================================================

assign(OrgId, IdentityId, FunctionKey, UserId, Status, EndedAt) ->
    #{
        organization_id => OrgId,
        business_identity_id => IdentityId,
        function_key => FunctionKey,
        user_id => UserId,
        status => Status,
        assigned_at => 100,
        ended_at => EndedAt
    }.

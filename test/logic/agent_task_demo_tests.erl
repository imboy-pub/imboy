-module(agent_task_demo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%
% agent_task_demo：PoC 演示驱动回归。
% 重点：run_demo/3 经 agent_task_observer:emit 产生 working + awaiting_approval 两个事件，
% 两者均 e2ee=>false（E2EE 红线：显式非 E2EE 才投递），且 group_id/member_uids 透传正确。
%%%

%% 捕获 emit 收到的每个 Event 到测试进程邮箱
capture_emit() ->
    [
        {'emit', 1, fun(Event) ->
            self() ! {emitted, Event},
            ok
        end}
    ].

collect_events(0, Acc) ->
    lists:reverse(Acc);
collect_events(N, Acc) ->
    receive
        {emitted, Event} -> collect_events(N - 1, [Event | Acc])
    after 200 -> lists:reverse(Acc)
    end.

%% run_demo 产生 working + awaiting_approval，均 e2ee=>false，group/members 正确
run_demo_emits_working_and_awaiting_approval_test_() ->
    ?WITH_MECKS(
        [
            {agent_task_observer, capture_emit()},
            %% 隔离 elib_tsid（避免依赖全局 init；对齐同批测试的 mock 惯例）
            {elib_tsid, [{'generate', 0, fun() -> 7777 end}]}
        ],
        fun() ->
            AgentUid = 100,
            GroupId = 200,
            Members = [100, 300, 400],
            ?assertEqual(ok, agent_task_demo:run_demo(AgentUid, GroupId, Members)),
            Events = collect_events(2, []),
            ?assertEqual(2, length(Events)),
            [Working, Approval] = Events,

            %% 第一个：过渡态 working
            ?assertEqual(working, maps:get(status, Working)),
            %% 第二个：durable 审批卡片
            ?assertEqual(awaiting_approval, maps:get(status, Approval)),

            %% E2EE 红线：两个事件都必须显式 e2ee=>false
            ?assertEqual(false, maps:get(e2ee, Working)),
            ?assertEqual(false, maps:get(e2ee, Approval)),

            %% group_id / agent_uid / member_uids 透传正确
            ?assertEqual(GroupId, maps:get(group_id, Working)),
            ?assertEqual(GroupId, maps:get(group_id, Approval)),
            ?assertEqual(AgentUid, maps:get(agent_uid, Working)),
            ?assertEqual(Members, maps:get(member_uids, Working)),

            %% 两个事件共享同一 task_id
            ?assertEqual(
                maps:get(task_id, Working),
                maps:get(task_id, Approval)
            ),
            ?assert(is_binary(maps:get(task_id, Working)))
        end
    ).

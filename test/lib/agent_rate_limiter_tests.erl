-module(agent_rate_limiter_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%
% agent_rate_limiter 单测：真实 ETS 原子计数器（无 mock），验证双维固定窗口边界。
% 每个测试前清空表 + 设定小阈值，保证确定性。
%%%

%% 建表（首次 allow 惰性建）后清空，并设小阈值
reset(PerReq, PerAgent) ->
    _ = agent_rate_limiter:allow(0, 0),
    case ets:whereis(agent_rate_limiter_ets) of
        undefined -> ok;
        _ -> ets:delete_all_objects(agent_rate_limiter_ets)
    end,
    application:set_env(imboy, agent_rl_per_requester, PerReq),
    application:set_env(imboy, agent_rl_per_agent, PerAgent),
    application:set_env(imboy, agent_rl_window_secs, 60),
    ok.

teardown() ->
    application:unset_env(imboy, agent_rl_per_requester),
    application:unset_env(imboy, agent_rl_per_agent),
    application:unset_env(imboy, agent_rl_window_secs),
    ok.

%% 单请求者达 per-requester 上限即拒（恰好放行 Max 次，第 Max+1 次拒）
requester_ceiling_test_() ->
    ?TEST_SIMPLE(fun() ->
        ok = reset(3, 100),
        try
            ?assertEqual(allow, agent_rate_limiter:allow(1001, 2002)),
            ?assertEqual(allow, agent_rate_limiter:allow(1001, 2002)),
            ?assertEqual(allow, agent_rate_limiter:allow(1001, 2002)),
            ?assertEqual({deny, requester_rate}, agent_rate_limiter:allow(1001, 2002))
        after
            teardown()
        end
    end).

%% 请求者不超但 agent 总量达上限即拒（多账号分布式刷 → agent_rate）
agent_total_ceiling_test_() ->
    ?TEST_SIMPLE(fun() ->
        ok = reset(100, 3),
        try
            ?assertEqual(allow, agent_rate_limiter:allow(1001, 7001)),
            ?assertEqual(allow, agent_rate_limiter:allow(1001, 7002)),
            ?assertEqual(allow, agent_rate_limiter:allow(1001, 7003)),
            %% 第 4 个不同请求者：各自 requester 计数=1 未超，但 agent 总量=4>3 → 拒
            ?assertEqual({deny, agent_rate}, agent_rate_limiter:allow(1001, 7004))
        after
            teardown()
        end
    end).

%% 不同请求者的 requester 计数相互独立（U7 被限不影响 U8）
independent_requesters_test_() ->
    ?TEST_SIMPLE(fun() ->
        ok = reset(2, 100),
        try
            ?assertEqual(allow, agent_rate_limiter:allow(1001, 7)),
            ?assertEqual(allow, agent_rate_limiter:allow(1001, 7)),
            ?assertEqual({deny, requester_rate}, agent_rate_limiter:allow(1001, 7)),
            %% U8 独立计数，仍放行
            ?assertEqual(allow, agent_rate_limiter:allow(1001, 8))
        after
            teardown()
        end
    end).

%% 不同 scope（不同 agent）计数相互独立
independent_scopes_test_() ->
    ?TEST_SIMPLE(fun() ->
        ok = reset(2, 100),
        try
            ?assertEqual(allow, agent_rate_limiter:allow(1001, 7)),
            ?assertEqual(allow, agent_rate_limiter:allow(1001, 7)),
            ?assertEqual({deny, requester_rate}, agent_rate_limiter:allow(1001, 7)),
            %% 另一 agent scope 独立
            ?assertEqual(allow, agent_rate_limiter:allow(2002, 7))
        after
            teardown()
        end
    end).

%% scope 可为 bot 名（binary）——覆盖 bot_* C2S 路径
binary_scope_test_() ->
    ?TEST_SIMPLE(fun() ->
        ok = reset(2, 100),
        try
            ?assertEqual(allow, agent_rate_limiter:allow(<<"bot_qian_fan">>, 7)),
            ?assertEqual(allow, agent_rate_limiter:allow(<<"bot_qian_fan">>, 7)),
            ?assertEqual({deny, requester_rate}, agent_rate_limiter:allow(<<"bot_qian_fan">>, 7))
        after
            teardown()
        end
    end).

%% 默认阈值（未配置）放行低速调用
defaults_apply_test_() ->
    ?TEST_SIMPLE(fun() ->
        teardown(),
        _ = agent_rate_limiter:allow(0, 0),
        case ets:whereis(agent_rate_limiter_ets) of
            undefined -> ok;
            _ -> ets:delete_all_objects(agent_rate_limiter_ets)
        end,
        ?assertEqual(allow, agent_rate_limiter:allow(9001, 9002))
    end).

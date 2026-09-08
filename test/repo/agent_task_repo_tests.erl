-module(agent_task_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%
% agent_task_repo 单测（DATA-01，真库）：三表 CRUD 契约与并发原语。
%   - ensure_task 幂等（ON CONFLICT 不改写既有行）
%   - cas_status 仅命中源态集合（0 行 = 未迁移）
%   - insert_event 幂等键去重；insert_decision task_id 唯一约束 first-writer-wins
% task_id / correlation_id 每运行唯一（共享库跨运行保留行）。
%%%

uid() ->
    Hex = binary:encode_hex(crypto:strong_rand_bytes(12), lowercase),
    <<"rt_", Hex/binary, "_end">>.

corr() ->
    Bin = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<"corr-", Bin/binary>>.

ensure_task_idempotent_test_() ->
    ?TEST_WITH_DB(fun() ->
        T = uid(),
        Data = base_data(T),
        {ok, Row1, true} = agent_task_repo:ensure_task(Data),
        ?assertEqual(<<"submitted">>, maps:get(<<"status">>, Row1)),
        %% 再插（同 id 不同参数）→ 不创建、不改写
        {ok, Row2, false} = agent_task_repo:ensure_task(
            Data#{tool => <<"hacked.tool">>}
        ),
        ?assertEqual(<<"demo.tool">>, maps:get(<<"tool">>, Row1)),
        ?assertEqual(<<"demo.tool">>, maps:get(<<"tool">>, Row2))
    end).

cas_status_matches_source_only_test_() ->
    ?TEST_WITH_DB(fun() ->
        T = uid(),
        {ok, _, true} = agent_task_repo:ensure_task(base_data(T)),
        %% 非法源（submitted→completed 不在边表）→ 不迁移
        {ok, not_matched} = agent_task_repo:cas_status(
            T, {[<<"working">>], <<"completed">>}
        ),
        %% 合法源（submitted→working）→ 迁移
        {ok, updated} = agent_task_repo:cas_status(
            T, {[<<"submitted">>, <<"approved">>, <<"working">>], <<"working">>}
        ),
        {ok, Row} = agent_task_repo:get_task(T),
        ?assertEqual(<<"working">>, maps:get(<<"status">>, Row)),
        %% working→working 自环（progress）→ updated
        {ok, updated} = agent_task_repo:cas_status(
            T, {[<<"submitted">>, <<"approved">>, <<"working">>], <<"working">>}
        )
    end).

event_dedup_by_idempotency_key_test_() ->
    ?TEST_WITH_DB(fun() ->
        T = uid(),
        C = corr(),
        {ok, _, true} = agent_task_repo:ensure_task(base_data(T)),
        Ev = #{
            id => <<"evt_", T/binary>>,
            task_id => T,
            status => <<"completed">>,
            seq => 1,
            correlation_id => C,
            idempotency_key => <<"evt:", T/binary, ":completed">>
        },
        agent_task_ds:with_tx(fun(Conn) ->
            {ok, inserted} = agent_task_repo:insert_event_tx(Conn, Ev),
            {ok, duplicate} = agent_task_repo:insert_event_tx(Conn, Ev),
            ok
        end)
    end).

decision_first_writer_wins_test_() ->
    ?TEST_WITH_DB(fun() ->
        T = uid(),
        C = corr(),
        {ok, _, true} = agent_task_repo:ensure_task(base_data(T)),
        D1 = #{
            id => <<"dec1_", T/binary>>,
            task_id => T,
            decision => <<"approved">>,
            approver_uid => 10,
            correlation_id => C
        },
        D2 = #{
            id => <<"dec2_", T/binary>>,
            task_id => T,
            decision => <<"rejected">>,
            approver_uid => 11,
            correlation_id => C
        },
        agent_task_ds:with_tx(fun(Conn) ->
            {ok, inserted} = agent_task_repo:insert_decision_tx(Conn, D1),
            {ok, duplicate} = agent_task_repo:insert_decision_tx(Conn, D2),
            ok
        end),
        {ok, Dec} = agent_task_repo:get_decision(T),
        ?assertEqual(<<"approved">>, maps:get(<<"decision">>, Dec)),
        ?assertEqual(10, maps:get(<<"approver_uid">>, Dec))
    end).

status_check_constraint_rejects_unknown_test_() ->
    ?TEST_WITH_DB(fun() ->
        T = uid(),
        BadData = (base_data(T))#{status => <<"totally_bogus_state">>},
        {error, _} = agent_task_repo:ensure_task(BadData),
        ok
    end).

%% ===================================================================
base_data(T) ->
    #{
        id => T,
        group_id => 5,
        agent_uid => 100,
        tool => <<"demo.tool">>,
        params_digest => <<>>,
        correlation_id => corr(),
        idempotency_key => <<"idem:", T/binary>>
    }.

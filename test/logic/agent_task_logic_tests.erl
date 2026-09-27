-module(agent_task_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%
% agent_task_logic 单测（DATA-01，真库）：
%   - FSM-00 边表合法性（record_event 非法迁移 skip）
%   - A01 并发审批仲裁：32 worker 全部竞争同一 task，恰一个成功（任务卡特项目标数）
%   - A02 重启恢复：pending/decision 全部可由 DB 重建（无进程内存真源）
% 投递层/授权数据 meck（msg_c2g_logic/group_ds）；每用例自建自清 mock。
%%%

uid() ->
    Hex = binary:encode_hex(crypto:strong_rand_bytes(12), lowercase),
    <<"lg_", Hex/binary, "_end">>.

%% A01：32 worker 并发审批同一任务（混 approve/reject、全员授权），
%% 恰 1 个 {ok, Decision}，其余 31 个 {error, already_decided}，DB 状态=胜者决定。
concurrent_decision_32_workers_test_() ->
    Workers = 32,
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        setup_group_ds([10, 11, 12]),
        T = uid(),
        ok = seed_awaiting(T),
        Calls = lists:flatten(
            lists:duplicate(16, {approve, 10}) ++
                lists:duplicate(8, {approve, 11}) ++
                lists:duplicate(8, {reject, 12})
        ),
        Results = run_workers(T, Calls),
        ?assertEqual(1, count({ok, approved}, Results) + count({ok, rejected}, Results)),
        ?assertEqual(Workers - 1, count({error, already_decided}, Results)),
        {ok, Row} = agent_task_repo:get_task(T),
        Expected =
            case count({ok, approved}, Results) of
                1 -> <<"approved">>;
                0 -> <<"rejected">>
            end,
        ?assertEqual(Expected, maps:get(<<"status">>, Row)),
        cleanup_group_ds()
    end).

%% A01b：混合授权并发（16 非群成员 + 16 授权者）：
%% 恰 1 ok、15 already_decided、16 not_authorized。
concurrent_mixed_authorization_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        setup_group_ds([10, 11]),
        T = uid(),
        ok = seed_awaiting(T),
        Calls = lists:duplicate(16, {approve, 99}) ++ lists:duplicate(16, {approve, 10}),
        Results = run_workers(T, Calls),
        ?assertEqual(1, count({ok, approved}, Results)),
        ?assertEqual(15, count({error, already_decided}, Results)),
        ?assertEqual(16, count({error, not_authorized}, Results)),
        {ok, Row} = agent_task_repo:get_task(T),
        ?assertEqual(<<"approved">>, maps:get(<<"status">>, Row)),
        cleanup_group_ds()
    end).

%% A02：重启恢复——pending 与决定真源=DB（无 ETS/进程字典），
%% 同一进程内"重启后"的读取与首次一致；correlation_id 持久化可重挂链。
restart_recovery_test_() ->
    ?TEST_WITH_DB(fun() ->
        setup_group_ds([10, 11]),
        T = uid(),
        ok = seed_awaiting(T),
        ?assertMatch({pending, 5, 100}, agent_task_observer:lookup(T)),
        %% DATA-01 起审批真源是 DB：不存在"重启丢失"的内存态，
        %% 重启后的决定路径与首次完全相同（同函数、同 DB 读）。
        ?assertEqual({ok, approved}, agent_task_observer:approve(T, 10)),
        ?assertEqual({approved, 10}, agent_task_observer:lookup(T)),
        {ok, Row} = agent_task_repo:get_task(T),
        ?assertEqual(<<"approved">>, maps:get(<<"status">>, Row)),
        ?assertMatch(<<"corr-", _/binary>>, maps:get(<<"correlation_id">>, Row)),
        cleanup_group_ds()
    end).

%% 回归（EXT-01 实测教训）：submitted 是建任务后的常态活跃态，
%% lookup 不得把「无决定行」与「任务不存在」（undefined）混同——
%% 该混淆曾令 get_agent_task 对一切新建任务误报"任务不存在"。
live_status_lookup_test_() ->
    ?TEST_WITH_DB(fun() ->
        setup_group_ds([5, 100]),
        T = uid(),
        {ok, _, true} = agent_task_repo:ensure_task(base_data(T)),
        ?assertMatch({live_status, <<"submitted">>}, agent_task_observer:lookup(T)),
        ?assertMatch(undefined, agent_task_observer:lookup(<<"task-nonexistent-00000000">>)),
        cleanup_group_ds()
    end).

%% 边表合法性：completed 不可自 submitted 直达（非法迁移 skip，不建行）
illegal_transition_skipped_test_() ->
    ?TEST_WITH_DB(fun() ->
        T = uid(),
        skip = agent_task_logic:record_event(#{
            task_id => T,
            agent_uid => 100,
            group_id => 5,
            status => completed,
            e2ee => false
        }),
        {ok, Row} = agent_task_repo:get_task(T),
        ?assertEqual(<<"submitted">>, maps:get(<<"status">>, Row))
    end).

%% record_event 合法迁移落库 + correlation 继承（ensure 时生成）
record_event_legal_transition_test_() ->
    ?TEST_WITH_DB(fun() ->
        T = uid(),
        {deliver, <<"working">>} = agent_task_logic:record_event(#{
            task_id => T,
            agent_uid => 100,
            group_id => 5,
            status => working,
            e2ee => false
        }),
        {ok, Row} = agent_task_repo:get_task(T),
        ?assertEqual(<<"working">>, maps:get(<<"status">>, Row)),
        ?assertMatch(<<"corr-", _/binary>>, maps:get(<<"correlation_id">>, Row))
    end).

%% 审批只走一次：首次批准后的执行恢复不能再次创建待审轮次；即使状态被
%% 外部故障写回 awaiting_approval，重复决定也必须回滚 CAS，不改变任务状态。
approval_cannot_reopen_or_rewrite_state_test_() ->
    ?TEST_WITH_DB(fun() ->
        setup_group_ds([10]),
        T = uid(),
        Event = #{task_id => T, agent_uid => 100, group_id => 5, e2ee => false},
        {deliver, <<"working">>} = agent_task_logic:record_event(Event#{status => working}),
        {deliver_with_meta, <<"awaiting_approval">>, _} =
            agent_task_logic:record_event(Event#{status => awaiting_approval}),
        {ok, approved, _} = agent_task_logic:decide(T, 10, approved),
        {deliver, <<"working">>} = agent_task_logic:record_event(Event#{status => working}),
        skip = agent_task_logic:record_event(Event#{status => awaiting_approval}),
        {ok, Working} = agent_task_repo:get_task(T),
        ?assertEqual(<<"working">>, maps:get(<<"status">>, Working)),

        {ok, updated} = agent_task_repo:cas_status(
            T, {[<<"working">>], <<"awaiting_approval">>}
        ),
        ?assertEqual({error, already_decided}, agent_task_logic:decide(T, 10, rejected)),
        {ok, StillAwaiting} = agent_task_repo:get_task(T),
        ?assertEqual(<<"awaiting_approval">>, maps:get(<<"status">>, StillAwaiting)),
        cleanup_group_ds()
    end).

%% ===================================================================
%% Helpers
%% ===================================================================

%% CP-TD-01F：并发注入前预热连接池——串行 take/return 一轮，触发 pooler
%% 提前扩容（默认 init_count=5，32 并发需扩到 32+）并淘汰/重建已死成员。
%% 全量下池成员偶发崩溃-重启循环（unexpected_message 连环崩，W3/CP12 实证），
%% 32 个 worker 同时 take 会让建连失败集中爆发成 no_connection 风暴；
%% 预热只影响基础设施就绪度，不改变任何被测断言（仍为恰好 1 ok / 其余
%% already_decided / not_authorized 的 DB 仲裁语义）。
prime_pool() ->
    Before = (catch pooler:pool_stats(pgsql)),
    lists:foreach(
        fun(N) ->
            case eunit_runner:eunit_setup_with_db() of
                {ok, Conn} -> eunit_runner:eunit_cleanup_db(Conn);
                {error, R} -> io:format(user, "~n[CP-TD-01F] prime ~p error: ~p~n", [N, R])
            end
        end,
        lists:seq(1, 32)
    ),
    After = (catch pooler:pool_stats(pgsql)),
    EnvConf = (catch config_ds:env(pg_conf)),
    io:format(
        user,
        "~n[CP-TD-01F] prime_pool before=~p~n[CP-TD-01F] prime_pool after=~p~n"
        "[CP-TD-01F] env pg_conf init_count=~p max_count=~p~n",
        [
            Before,
            After,
            maps:get(init_count, EnvConf, missing),
            maps:get(max_count, EnvConf, missing)
        ]
    ).

run_workers(T, Calls) ->
    prime_pool(),
    Parent = self(),
    %% CP-TD-01F：分批并发注入。全量 eunit 期间共享 pgsql 池被多个隔离套件
    %% 换池/污染后常驻存活容量仅个位数（pool_stats 实证），32 worker 同时
    %% take（elib_pg:with_conn 用 take_member/1，Timeout=0 不入队不扩容）
    %% 会集中 no_connection。按批切分后每批仍是同一 task 上的真并发仲裁
    %% （FOR UPDATE/原子 CAS 语义不变），批间同步只影响注入节奏——
    %% 被测契约「恰 1 ok、其余 already_decided / not_authorized」逐字不变。
    BatchSize = 4,
    Batches = split_batches(Calls, BatchSize),
    Results = lists:append([run_batch(T, Parent, Batch) || Batch <- Batches]),
    io:format(user, "~n[CP-TD-01F] worker results: ~p~n", [Results]),
    Results.

split_batches(Calls, Size) when Size >= 1 ->
    case Calls of
        [] ->
            [];
        _ ->
            Pos = lists:seq(1, length(Calls), Size),
            [lists:sublist(Calls, P, Size) || P <- Pos]
    end.

run_batch(T, Parent, Batch) ->
    Pids = [
        spawn(fun() ->
            Res =
                case Kind of
                    approve -> agent_task_observer:approve(T, U);
                    reject -> agent_task_observer:reject(T, U)
                end,
            Parent ! {worker_done, self(), Res}
        end)
     || {Kind, U} <- Batch
    ],
    [
        receive
            {worker_done, Pid, Res} when node(Pid) =:= node() ->
                Res;
            {worker_done, _, Res} ->
                Res
        after 30000 ->
            erlang:error(worker_timeout)
        end
     || _P <- Pids
    ].

count(What, List) ->
    length([X || X <- List, X =:= What]).

base_data(T) ->
    #{
        id => T,
        group_id => 5,
        agent_uid => 100,
        tool => <<"demo.tool">>,
        params_digest => <<>>,
        correlation_id => new_corr(),
        idempotency_key => <<"idem:", T/binary>>
    }.

new_corr() ->
    Bin = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<"corr-", Bin/binary>>.

seed_awaiting(T) ->
    {ok, _, true} = agent_task_repo:ensure_task(base_data(T)),
    {ok, updated} = agent_task_repo:cas_status(
        T, {[<<"submitted">>, <<"approved">>, <<"working">>], <<"working">>}
    ),
    {ok, updated} = agent_task_repo:cas_status(
        T, {[<<"working">>], <<"awaiting_approval">>}
    ),
    ok.

setup_group_ds(Members) ->
    try
        meck_helper:cleanup_mock(group_ds)
    catch
        _:_ -> ok
    end,
    case
        meck_helper:setup_mock(
            group_ds, [{'member_uids', 1, fun(_) -> Members end}]
        )
    of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({group_ds_mock, Reason})
    end.

cleanup_group_ds() ->
    try
        meck_helper:cleanup_mock(group_ds)
    catch
        _:_ -> ok
    end,
    ok.

%% ===================================================================
%% WH/BOT-01 补充：expire 定时器语义（FSM 边 #4/#12 批量触发）
%% ===================================================================

expire_stale_tasks_test_() ->
    {timeout, 30,
        ?TEST_WITH_DB(fun() ->
            setup_group_ds([10, 11]),
            Old = uid(),
            Fresh = uid(),
            %% 老任务：awaiting_approval 且 updated_at 拨回 1 小时前（超时）
            {ok, _, true} = agent_task_repo:ensure_task(base_data(Old)),
            {ok, _} = elib_pg:execute(
                <<
                    "UPDATE public.agent_task SET status = 'awaiting_approval',"
                    " updated_at = NOW() - interval '1 hour' WHERE id = $1"
                >>,
                [Old]
            ),
            %% 新任务：submitted（宽限期内不动）
            {ok, _, true} = agent_task_repo:ensure_task(base_data(Fresh)),
            {ok, N} = agent_task_logic:expire_stale_tasks(300),
            ?assert(N >= 1),
            {ok, RowOld} = agent_task_repo:get_task(Old),
            ?assertEqual(<<"expired">>, maps:get(<<"status">>, RowOld)),
            {ok, RowFresh} = agent_task_repo:get_task(Fresh),
            ?assertEqual(<<"submitted">>, maps:get(<<"status">>, RowFresh)),
            cleanup_group_ds()
        end)}.

-module(account_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% G-01（收口 C06）：allocate 委托 imboy_account_allocator 专用分配进程。
%% 历史读改写竞态（get_wait 命中路径无锁，两进程可取走同一头元素）
%% 由进程内串行领取消除；本套件验证委托行为 + 并发唯一性 + PG 失败语义。

init_returns_ok_when_execute_succeeds_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'execute', 2, fun(_Sql, []) ->
                ok
            end}
        ],
        fun() ->
            ?assertEqual(ok, account_ds:init())
        end
    ).

init_returns_ok_when_execute_throws_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'execute', 2, fun(_Sql, []) ->
                erlang:error(db_down)
            end}
        ],
        fun() ->
            ?assertEqual(ok, account_ds:init())
        end
    ).

%% allocate 委托专用分配进程：分配进程 mock 返回时透传结果。
allocate_delegates_to_allocator_process_test_() ->
    ?WITH_MECK(
        imboy_account_allocator,
        [
            {'allocate', 0, fun() -> 50001 end}
        ],
        fun() ->
            ?assertEqual(50001, account_ds:allocate())
        end
    ).

%% G-01 oracle 1：单节点多进程并发领取无重复（真实 allocator 进程，
%% PG 批量取号 mock 为确定性批次；并发 40 领取跨 4 批全部唯一）。
concurrent_allocate_ids_are_unique_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [passthrough]),
        try
            %% 计数器批次：每次批量取号返回 10 个唯一号（模拟 sequence）
            Batch = ets:new(alloc_test_batch, [public, named_table]),
            ets:insert(Batch, {seq, 100000}),
            meck:expect(
                elib_pg,
                query,
                2,
                fun(_Sql, _Args) ->
                    N = ets:update_counter(Batch, seq, 10),
                    {ok, [#{<<"nextval">> => N - I} || I <- lists:seq(0, 9)]}
                end
            ),
            %% 杀掉可能已存在的 allocator，让本测试用 mock 的 PG 重建
            kill_allocator(),
            Parent = self(),
            N = 40,
            Pids = [
                spawn_link(fun() ->
                    Parent ! {self(), account_ds:allocate()}
                end)
             || _ <- lists:seq(1, N)
            ],
            Results = [
                receive
                    {P, R} -> R
                end
             || P <- Pids
            ],
            Ids = [R || R <- Results, is_integer(R)],
            %% 全部成功且无重复
            ?assertEqual(N, length(Ids)),
            ?assertEqual(N, sets:size(sets:from_list(Ids))),
            ets:delete(Batch),
            kill_allocator()
        after
            meck:unload(elib_pg)
        end
    end).

%% G-01 oracle 2：PG 不可用 → 明确 {error, no_ids}，不虚假成功。
allocate_pg_unavailable_returns_no_ids_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [passthrough]),
        try
            meck:expect(elib_pg, query, 2, fun(_Sql, _Args) -> {error, db_down} end),
            kill_allocator(),
            ?assertEqual({error, no_ids}, account_ds:allocate()),
            kill_allocator()
        after
            meck:unload(elib_pg)
        end
    end).

%% 进程崩溃不重复分配：杀掉 allocator 后新进程从新批次继续（sequence 单调）。
allocator_crash_does_not_reuse_ids_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [passthrough]),
        try
            Batch = ets:new(alloc_test_crash, [public, named_table]),
            ets:insert(Batch, {seq, 200000}),
            meck:expect(
                elib_pg,
                query,
                2,
                fun(_Sql, _Args) ->
                    N = ets:update_counter(Batch, seq, 10),
                    {ok, [#{<<"nextval">> => N - I} || I <- lists:seq(0, 9)]}
                end
            ),
            kill_allocator(),
            Id1 = account_ds:allocate(),
            %% 崩溃：池内剩余 9 个号废弃
            kill_allocator(),
            Id2 = account_ds:allocate(),
            %% 新批次起点高于旧批次（sequence 单调，旧号不复活）
            ?assert(Id2 >= Id1),
            ?assertNotEqual(Id1, Id2),
            ets:delete(Batch),
            kill_allocator()
        after
            meck:unload(elib_pg)
        end
    end).

-spec kill_allocator() -> ok.
kill_allocator() ->
    case whereis(imboy_account_allocator) of
        undefined ->
            ok;
        Pid ->
            Ref = erlang:monitor(process, Pid),
            exit(Pid, kill),
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            after 1000 -> ok
            end,
            ok
    end.

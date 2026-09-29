#!/usr/bin/env escript
%% T-309: 30 分钟并发 soak（guard runtime + 生产缺省参数）
%%
%% 断言（任何一条失败立即退出非零并输出 errors）：
%%   - 全程无 typed 错误（capacity_exhausted / fenced / invalid）
%%   - 每轮窗口内 ID 严格唯一（usort 校验）
%%   - 轮与轮之间 slot 区间严格不重叠（cursor 单调 → 全局唯一）
%%   - lead（轮 max ts - 墙钟）始终 <= 缺省 max_logical_lead_ms=512
%%   - guard 状态始终 ready（fenced 即失败）
%%   - RSS 无持续线性增长（首尾对比 + 线性斜率输出，人工复核项）
%%
%% 负载：8 workers × 每 50ms 轮生成 250 ids（40/ms 合计，低于 2048/ms
%% 持续容量）；每 120s 一个 100k 突发 batch（瞬时 lead +49ms）。
%% 输出：STDOUT 末行 SOAK_JSON {...}
-module(tsid10_soak).
-export([main/1]).

-define(EPOCH_MS, 1735689600000).
-define(WORKERS, 8).
-define(PER_ROUND, 250).
-define(ROUND_MS, 50).
-define(DURATION_MS, 1800000).
%% 120s
-define(BURST_EVERY_ROUNDS, 2400).
-define(MAX_LEAD, 512).

main(_) ->
    code:add_patha(
        "/Users/leeyi/project/imboy.pub/.Codex/worktrees/tsid-correctness-hardening/ebin"
    ),
    Root = "/tmp/tsid10_soak_store_" ++ integer_to_list(erlang:system_time(microsecond)),
    filelib:ensure_dir(Root ++ "/x"),
    {ok, GPid} = elib_tsid_guard:start_link(#{
        root => Root,
        combined_node => 9,
        dc_bits => 3,
        names => [user, group_info],
        lock_provider => registry,
        store_bootstrap => fresh,
        fence_window_ms => 1000,
        fence_renew_margin_ms => 100,
        startup_clock_wait_timeout_ms => 1000,
        capacity_wait_timeout_ms => 5000
    }),
    ok = wait_ready(GPid, 100),
    Self = self(),
    StartTime = erlang:monotonic_time(millisecond),
    Workers = [
        spawn_link(fun() -> worker_loop(Self) end)
     || _ <- lists:seq(1, ?WORKERS)
    ],
    loop(0, none, StartTime, GPid, [], 0, 0, []),
    %% 不可达（loop 内 finish 后 halt）
    [exit(W, kill) || W <- Workers].

worker_loop(Self) ->
    T0 = erlang:monotonic_time(millisecond),
    worker_loop(Self, T0).

worker_loop(Self, NextTick) ->
    T = erlang:monotonic_time(millisecond),
    case T >= NextTick of
        false ->
            timer:sleep(max(1, NextTick - T)),
            worker_loop(Self, NextTick);
        true ->
            try
                Ids = elib_tsid:generate_n(user, ?PER_ROUND),
                case lists:usort(Ids) =:= Ids of
                    true ->
                        Self ! {range, hd(Ids), lists:last(Ids)},
                        worker_loop(Self, NextTick + ?ROUND_MS);
                    false ->
                        Self ! {fatal, worker_dup},
                        exit(dup)
                end
            catch
                error:{elib_tsid_capacity_exhausted, _} = E ->
                    Self ! {fatal, E},
                    exit(capacity);
                error:{elib_tsid_fenced, _} = E ->
                    Self ! {fatal, E},
                    exit(fenced);
                error:E ->
                    Self ! {fatal, E},
                    exit(other)
            end
    end.

%% 主循环：收齐一轮 8 个 range → 区间断言 → 定期突发/RSS/lead
loop(Rounds, _LastMaxUnused, T0, GPid, Rss, TotalIds, MaxLead, Pending) ->
    Elapsed = erlang:monotonic_time(millisecond) - T0,
    case Elapsed >= ?DURATION_MS of
        true ->
            finish(Rounds, none, T0, GPid, Rss, TotalIds, MaxLead, Elapsed);
        false ->
            ok
    end,
    %% ID→slot（(ts<<11)|seq）：ID 跨毫秒时夹带其他 node 的空洞，
    %% 不交检查必须在 slot 空间做（CAS 预约的是连续 slot 区间）
    Ranges0 = [
        {id_to_slot(Lo), id_to_slot(Hi)}
     || {Lo, Hi} <- collect(?WORKERS, [])
    ],
    %% 轮内两两不交 + 与上一轮区间两两不交（reservation 由 cursor CAS
    %% 串行化，相邻轮的区间对全不交 ⇒ 全局唯一）
    disjoint_or_exit(Ranges0),
    cross_or_exit(Pending, Ranges0),
    RoundMax = lists:max([Hi || {_, Hi} <- Ranges0]),
    %% lead 采样：轮内最大 ts（slot 高位）相对当前墙钟
    Lead = (RoundMax bsr 11) - (erlang:system_time(millisecond) - ?EPOCH_MS),
    true = Lead =< ?MAX_LEAD orelse exit({lead_exceeded, Lead}),
    %% 突发轮：100k batch（区间并入本轮窗口，与 worker 区间同批检查）
    {Ranges1, Total1} =
        case Rounds rem ?BURST_EVERY_ROUNDS =:= ?BURST_EVERY_ROUNDS - 1 of
            true ->
                Ids = elib_tsid:generate_n(user, 100000),
                true = lists:usort(Ids) =:= Ids orelse exit(burst_dup),
                BSlot = {id_to_slot(hd(Ids)), id_to_slot(lists:last(Ids))},
                BLead =
                    (element(2, BSlot) bsr 11) -
                        (erlang:system_time(millisecond) - ?EPOCH_MS),
                true = BLead =< ?MAX_LEAD orelse exit({burst_lead_exceeded, BLead}),
                {lists:usort([BSlot | Ranges0]), TotalIds + 100000};
            false ->
                {Ranges0, TotalIds}
        end,
    %% guard 状态抽查
    ready = elib_tsid_guard:status(GPid),
    %% RSS 每 30s（600 轮）采样一次
    Rss1 =
        case (Rounds + 1) rem 600 =:= 0 of
            true ->
                [{sample, Rounds + 1, erlang:memory(total)} | Rss];
            false ->
                Rss
        end,
    loop(
        Rounds + 1,
        none,
        T0,
        GPid,
        Rss1,
        Total1 + ?WORKERS * ?PER_ROUND,
        erlang:max(MaxLead, Lead),
        Ranges1
    ).

collect(0, Acc) ->
    Acc;
collect(K, Acc) ->
    receive
        {range, Lo, Hi} -> collect(K - 1, [{Lo, Hi} | Acc]);
        {fatal, Reason} -> fatal_exit(Reason)
    end.

%% 上一轮区间与本轮区间两两不交（并发下轮边界允许交错，
%% 不相交性即唯一性证明；总 max 单调由 cursor 结构保证）
cross_or_exit(none, _Ranges) ->
    ok;
cross_or_exit(Pending, Ranges) ->
    lists:foreach(
        fun({Lo1, Hi1}) ->
            lists:foreach(
                fun({Lo2, Hi2}) ->
                    true =
                        (Hi1 < Lo2 orelse Hi2 < Lo1) orelse
                            exit({cross_round_overlap, {Lo1, Hi1, Lo2, Hi2}})
                end,
                Ranges
            )
        end,
        Pending
    ).

%% 轮内各 worker 区间两两不重叠
disjoint_or_exit(Ranges) ->
    lists:foreach(
        fun({I, {Lo1, Hi1}}) ->
            lists:foreach(
                fun
                    ({J, {Lo2, Hi2}}) when J > I ->
                        true =
                            (Hi1 < Lo2 orelse Hi2 < Lo1) orelse
                                exit({range_overlap, {Lo1, Hi1, Lo2, Hi2}});
                    (_) ->
                        ok
                end,
                idx(Ranges)
            )
        end,
        idx(Ranges)
    ).

idx(L) -> lists:zip(lists:seq(1, length(L)), L).

id_to_slot(Id) -> ((Id bsr 21) bsl 11) bor (Id band 2047).

fatal_exit(Reason) ->
    io:format("FATAL ~p~n", [Reason]),
    erlang:halt(2).

finish(Rounds, _LastMax, _T0, GPid, Rss, TotalIds, MaxLead, Elapsed) ->
    Status = elib_tsid_guard:status(GPid),
    StoreDir = elib_tsid_guard:store_dir(GPid),
    MemList = lists:reverse([M || {sample, _, M} <- Rss]),
    Slope = rss_slope(MemList),
    io:format(
        "SOAK_JSON {\"duration_s\":~p,\"rounds\":~p,\"total_ids\":~p,"
        "\"workers\":~p,\"per_round\":~p,\"max_lead_ms\":~p,"
        "\"guard_status\":\"~p\",\"store_dir\":~p,"
        "\"rss_samples_bytes\":~p,\"rss_slope_bytes_per_sample\":~p,"
        "\"unique_window\":true,\"ranges_disjoint\":true,\"errors\":[]}~n",
        [
            Elapsed div 1000,
            Rounds,
            TotalIds,
            ?WORKERS,
            ?PER_ROUND,
            MaxLead,
            Status,
            filelib:is_file(StoreDir),
            MemList,
            Slope
        ]
    ),
    erlang:halt(0).

rss_slope([]) ->
    0;
rss_slope([_]) ->
    0;
rss_slope(Ms) ->
    N = length(Ms),
    Xs = lists:seq(1, N),
    MeanX = lists:sum(Xs) / N,
    MeanY = lists:sum(Ms) / N,
    Num = lists:sum([(X - MeanX) * (Y - MeanY) || {X, Y} <- lists:zip(Xs, Ms)]),
    Den = lists:sum([(X - MeanX) * (X - MeanX) || X <- Xs]),
    case Den of
        0 -> 0;
        _ -> round(Num / Den)
    end.

wait_ready(_Pid, 0) ->
    exit(guard_not_ready);
wait_ready(Pid, K) ->
    case elib_tsid_guard:status(Pid) of
        ready ->
            ok;
        _ ->
            timer:sleep(100),
            wait_ready(Pid, K - 1)
    end.

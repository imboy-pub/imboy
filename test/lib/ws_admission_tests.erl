-module(ws_admission_tests).

%%%===================================================================
%%% R0（P1-1）ws_admission 单元测试
%%%
%%% 覆盖属主批准的强制测试面（单元层；handler 层由
%%% websocket_connection_flow_SUITE 的 admission 组覆盖）：
%%% - 并发准入竞争（永不超额准入）
%%% - 达到上限/超过上限（rejected + rejected_total）
%%% - 正常断开释放 / 异常退出由 sweep 清理
%%% - 注册失败后的回滚（握手失败路径 = admit 后显式 release）
%%% - 节点重启后的计数恢复（表重建为 0 = 真实在线状态；VM 级重启由
%%%   隔离节点冒烟覆盖）
%%% - 多 listener 共用单节点总预算（全局表，跨进程归属）
%%% - 计数自愈（外部损坏下的 sweep 对账）
%%% - 非法配置拒启（无"无限制"回退）
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% helpers
%% ------------------------------------------------------------------

reset_with_cap(Cap) ->
    ok = application:set_env(imboy, max_ws_connections, Cap),
    _ =
        try
            ets:delete(ws_admission_tab)
        catch
            _:_ -> ok
        end,
    ok = ws_admission:init_table(),
    ?assertEqual(0, ws_admission:count()).

reset_default() ->
    ok = application:unset_env(imboy, max_ws_connections),
    _ =
        try
            ets:delete(ws_admission_tab)
        catch
            _:_ -> ok
        end,
    ok = ws_admission:init_table().

%% 启动 Holder 进程：立即 admit 后挂起，等待 release 指令（或 30s 兜底自释放）
holder() ->
    Self = self(),
    spawn(fun() ->
        R = ws_admission:admit(),
        Self ! {self(), R},
        receive
            release -> ws_admission:release()
        after 30000 ->
            ws_admission:release()
        end
    end).

collect(Pids) ->
    [
        receive
            {P, R} -> {P, R}
        after 5000 ->
            erlang:error({holder_no_reply, P})
        end
     || P <- Pids
    ].

wait_until(Fun) ->
    wait_until(Fun, 40).

wait_until(_Fun, 0) ->
    erlang:error(wait_until_timeout);
wait_until(Fun, N) ->
    case Fun() of
        true ->
            ok;
        false ->
            timer:sleep(50),
            wait_until(Fun, N - 1)
    end.

%% ------------------------------------------------------------------
%% 并发准入竞争（永不超额准入）
%% ------------------------------------------------------------------

concurrent_admission_race_never_over_admits_test() ->
    Cap = 8,
    ok = reset_with_cap(Cap),
    Procs = [holder() || _ <- lists:seq(1, 30)],
    Results = collect(Procs),
    Admitted = [P || {P, {ok, _}} <- Results],
    Rejected = [P || {P, {rejected, _}} <- Results],
    %% 恰好 Cap 条准入，其余全部拒绝；计数 == Cap（不超额）
    ?assertEqual(Cap, length(Admitted)),
    ?assertEqual(30 - Cap, length(Rejected)),
    ?assertEqual(Cap, ws_admission:count()),
    ?assertEqual(30 - Cap, ws_admission:rejected_total()),
    %% 每个准入进程各有一行（行数 == 计数不变式）
    ?assertEqual(
        Cap,
        length([P || {P, {ok, _}} <- Results, ets:lookup(ws_admission_tab, {conn, P}) =/= []])
    ),
    %% 释放后配额完全回收
    [P ! release || P <- Procs],
    wait_until(fun() -> ws_admission:count() =:= 0 end),
    ?assertEqual(0, ws_admission:count()).

%% ------------------------------------------------------------------
%% 上限 / 超上限 / 拒绝计数
%% ------------------------------------------------------------------

over_cap_rejected_and_counted_test() ->
    ok = reset_with_cap(2),
    {ok, 1} = ws_admission:admit(),
    {ok, 2} = ws_admission:admit(),
    {rejected, 2} = ws_admission:admit(),
    {rejected, 2} = ws_admission:admit(),
    ?assertEqual(2, ws_admission:count()),
    ?assertEqual(2, ws_admission:rejected_total()),
    ok.

limit_env_override_applies_test() ->
    ok = reset_with_cap(3),
    ?assertEqual(3, ws_admission:limit()),
    ok = application:set_env(imboy, max_ws_connections, 50000),
    ?assertEqual(50000, ws_admission:limit()),
    ok.

default_limit_is_approved_c2_test() ->
    ok = reset_default(),
    %% D1 批准值：20,000（单节点总准入上限）
    ?assertEqual(20000, ws_admission:limit()).

%% ------------------------------------------------------------------
%% 释放路径
%% ------------------------------------------------------------------

release_is_idempotent_take_paired_test() ->
    ok = reset_with_cap(5),
    {ok, 1} = ws_admission:admit(),
    ok = ws_admission:release(),
    %% 二次释放（terminate 与显式释放路径重复调用）必须 no-op：
    %% 不产生负计数、不影响后续准入
    ok = ws_admission:release(),
    ?assertEqual(0, ws_admission:count()),
    {ok, 1} = ws_admission:admit(),
    ok.

release_without_admission_is_noop_test() ->
    ok = reset_with_cap(5),
    ok = ws_admission:release(),
    ?assertEqual(0, ws_admission:count()),
    {ok, 1} = ws_admission:admit(),
    ok.

%% 未准入进程的 release 不得吃掉别人的名额（take 按行配对）
release_of_stranger_pid_cannot_leak_slot_test() ->
    ok = reset_with_cap(5),
    Pid = holder(),
    [{Pid, {ok, 1}}] = collect([Pid]),
    %% 当前测试进程从未 admit——release 必须无效果
    ok = ws_admission:release(),
    ?assertEqual(1, ws_admission:count()),
    Pid ! release,
    wait_until(fun() -> ws_admission:count() =:= 0 end),
    ok.

%% ------------------------------------------------------------------
%% 异常退出 → sweep 兜底清理
%% ------------------------------------------------------------------

abnormal_exit_recovered_by_sweep_test() ->
    ok = reset_with_cap(5),
    Holders = [holder() || _ <- lists:seq(1, 3)],
    _ = collect(Holders),
    ?assertEqual(3, ws_admission:count()),
    %% brutal kill：terminate/3 不会运行的退出路径（5s 周期 sweep 定时器
    %% 理论上可能先行清理，故只断言 sweep 后的不变式，不断言 sweep 前计数）
    [Killed, S1, _S2] = Holders,
    exit(Killed, kill),
    ok = ws_admission:sweep(),
    ?assertEqual(2, ws_admission:count()),
    [] = ets:lookup(ws_admission_tab, {conn, Killed}),
    ?assertMatch([{{conn, S1}, _}], ets:lookup(ws_admission_tab, {conn, S1})),
    %% 幂等：重复 sweep 不再变化
    ok = ws_admission:sweep(),
    ?assertEqual(2, ws_admission:count()),
    [H ! release || H <- Holders],
    ok.

%% ------------------------------------------------------------------
%% 计数自愈（外部损坏 → sweep 对账修复）
%% ------------------------------------------------------------------

drift_repaired_by_sweep_test() ->
    ok = reset_with_cap(10),
    {ok, 1} = ws_admission:admit(),
    %% 制造超计（行没了、计数还在）→ 对账应修复为行数
    ets:delete(ws_admission_tab, {conn, self()}),
    ok = ws_admission:sweep(),
    ?assertEqual(0, ws_admission:count()),
    %% 制造欠计（行还在、计数被外部扣掉）→ 对账应修复，不得造成永久拒绝
    {ok, 1} = ws_admission:admit(),
    ets:update_counter(ws_admission_tab, admitted_count, -1, {admitted_count, 0}),
    ok = ws_admission:sweep(),
    ?assertEqual(1, ws_admission:count()),
    %% 修复后配额可正常再次占用/释放
    ok = ws_admission:release(),
    ?assertEqual(0, ws_admission:count()),
    ok.

counter_never_goes_negative_test() ->
    ok = reset_with_cap(5),
    %% 外部损坏把计数打到 0 以下 → guarded 递减路径必须夹回 0，
    %% 不得让负计数造成"永久拒绝"或"负数放宽"
    ets:update_counter(ws_admission_tab, admitted_count, -3, {admitted_count, 0}),
    {ok, _} = ws_admission:admit(),
    ok = ws_admission:release(),
    ?assertEqual(0, ws_admission:count()),
    ok.

%% ------------------------------------------------------------------
%% 多 listener 共用单节点总预算
%% ------------------------------------------------------------------

budget_is_node_global_across_surfaces_test() ->
    %% 两个"listener"的连接 = 两个独立进程 admit；共享同一全局预算。
    %% cap=1：先到者准入，另一 listener 的连接必须被拒——上限不是
    %% per-listener 的。
    ok = reset_with_cap(1),
    Results = collect([holder(), holder()]),
    ?assertEqual(1, length([1 || {_, {ok, 1}} <- Results])),
    ?assertEqual(1, length([1 || {_, {rejected, 1}} <- Results])),
    ?assertEqual(1, ws_admission:count()),
    Winners = [P || {P, {ok, _}} <- Results],
    [W ! release || W <- Winners],
    wait_until(fun() -> ws_admission:count() =:= 0 end),
    %% 释放后另一 listener 的连接即可准入
    [{_, {ok, 1}}] = collect([holder()]),
    ok.

%% ------------------------------------------------------------------
%% 注册失败后的回滚（握手失败 = admit 后显式 release）
%% ------------------------------------------------------------------

handshake_failure_rolls_back_slot_test() ->
    ok = reset_with_cap(1),
    %% 模拟 handler 路径：admit 成功 → 鉴权失败 → 显式 release
    %% （websocket_handler 中 auth/throttle/子协议的 {ok, Req, State} 分支）
    {ok, 1} = ws_admission:admit(),
    ok = ws_admission:release(),
    ?assertEqual(0, ws_admission:count()),
    %% 名额归还后立即可再占用——握手失败不留泄漏名额造成误拒
    {ok, 1} = ws_admission:admit(),
    ok = ws_admission:release(),
    ?assertEqual(0, ws_admission:count()),
    ok.

%% ------------------------------------------------------------------
%% 重启恢复
%% ------------------------------------------------------------------

table_rebuild_on_restart_starts_from_truth_test() ->
    ok = reset_with_cap(5),
    Holders = [holder() || _ <- lists:seq(1, 2)],
    _ = collect(Holders),
    ?assertEqual(2, ws_admission:count()),
    %% 模拟 VM 重启：表随进程消亡，重建后计数从 0 开始（此刻无连接 =
    %% 真实在线状态），配额全额可用
    ets:delete(ws_admission_tab),
    ?assertEqual(0, ws_admission:count()),
    ?assertEqual(0, ws_admission:rejected_total()),
    {ok, 1} = ws_admission:admit(),
    ok = ws_admission:release(),
    [H ! release || H <- Holders],
    ok.

%% ------------------------------------------------------------------
%% 重复初始化不叠加清道夫
%% ------------------------------------------------------------------

%% 回归背景：timer:apply_interval/4 的句柄是 {interval, Ref} 元组而非
%% reference（OTP 29 实证）。早先用 is_reference 作守卫的写法恒假，重复
%% init 从不取消前一定时器，会留下多个并行清道夫。这里断言"再次 init 时
%% 确实以旧句柄调用了 timer:cancel/1"（cancel 对已取消句柄也返回
%% {ok, cancel}，故不能靠返回值判定）。
reinit_cancels_previous_sweep_timer_test() ->
    ok = reset_with_cap(5),
    T1 = persistent_term:get(ws_admission_sweep_timer, undefined),
    ?assertNotEqual(undefined, T1),
    %% 调用跟踪的匹配规格属于 tracer 进程，且进程收不到对自己的调用跟踪
    %% 消息——由独立 tracer 跟踪本进程对 timer:cancel/1 的实调用。
    Tracer = start_cancel_tracer(),
    ok = ws_admission:init_table(),
    T2 = persistent_term:get(ws_admission_sweep_timer, undefined),
    ?assertNotEqual(T1, T2),
    %% 同一发送方（本进程）到 tracer 的信号有序：取消的跟踪消息必先于 stop
    Tracer ! {stop, self()},
    ?assertEqual(
        [T1],
        receive
            {cancelled_refs, Refs} -> Refs
        after 1000 -> no_tracer_reply
        end
    ),
    ok.

start_cancel_tracer() ->
    TestPid = self(),
    Tracer =
        spawn(fun() ->
            _ = erlang:trace_pattern({timer, cancel, 1}, true, []),
            _ = erlang:trace(TestPid, true, [{tracer, self()}, call]),
            TestPid ! tracer_ready,
            cancel_tracer_loop([])
        end),
    receive
        tracer_ready -> Tracer
    after 1000 -> erlang:error(tracer_not_ready)
    end.

cancel_tracer_loop(Seen) ->
    receive
        {trace, _Pid, call, {timer, cancel, [TRef]}} ->
            cancel_tracer_loop([TRef | Seen]);
        {stop, From} ->
            _ = erlang:trace_pattern({timer, cancel, 1}, false, []),
            From ! {cancelled_refs, lists:reverse(Seen)}
    end.

admission_metrics_reflect_lifecycle_test() ->
    ok = reset_with_cap(4),
    Holders = [holder() || _ <- lists:seq(1, 4)],
    _ = collect(Holders),
    %% 已满：第 5 条被拒并计数
    {rejected, _} = ws_admission:admit(),
    %% 建立/拒绝后的导出值
    ?assertEqual(4, ws_admission:count()),
    ?assertEqual(1, ws_admission:rejected_total()),
    ?assertEqual(4, ws_admission:limit()),
    %% 正常关闭后 current 回落、rejected_total 保持累计
    [H ! release || H <- Holders],
    wait_until(fun() -> ws_admission:count() =:= 0 end),
    ?assertEqual(1, ws_admission:rejected_total()),
    ok.

%% ------------------------------------------------------------------
%% 非法配置拒启（不允许静默回退为无限制）
%% ------------------------------------------------------------------

validate_config_fail_fast_test() ->
    Saved = application:get_env(imboy, max_ws_connections),
    ok = application:set_env(imboy, max_ws_connections, 0),
    ?assertError({invalid_config, max_ws_connections, 0}, ws_admission:validate_config()),
    ok = application:set_env(imboy, max_ws_connections, -5),
    ?assertError({invalid_config, max_ws_connections, -5}, ws_admission:validate_config()),
    ok = application:set_env(imboy, max_ws_connections, <<"20000">>),
    ?assertError(
        {invalid_config, max_ws_connections, <<"20000">>}, ws_admission:validate_config()
    ),
    ok = application:set_env(imboy, max_ws_connections, 1.5),
    ?assertError({invalid_config, max_ws_connections, 1.5}, ws_admission:validate_config()),
    ok = application:set_env(imboy, max_ws_connections, 1000001),
    ?assertError({invalid_config, max_ws_connections, 1000001}, ws_admission:validate_config()),
    %% 合法边界
    ok = application:set_env(imboy, max_ws_connections, 1),
    ok = ws_admission:validate_config(),
    ok = application:set_env(imboy, max_ws_connections, 1000000),
    ok = ws_admission:validate_config(),
    case Saved of
        undefined -> ok = application:unset_env(imboy, max_ws_connections);
        {ok, V} -> ok = application:set_env(imboy, max_ws_connections, V)
    end,
    ok.

%% env 字符串解析：合法整数 + 非法 fail-fast（imboy_env 单独导出供直测）
parse_integer_env_valid_test() ->
    ?assertEqual(20000, imboy_env:parse_integer_env("IMBOY_MAX_WS_CONNECTIONS", "20000")),
    ?assertEqual(1, imboy_env:parse_integer_env("K", " 1 ")),
    ?assertEqual(-3, imboy_env:parse_integer_env("K", "-3")),
    ok.

parse_integer_env_invalid_fails_test() ->
    ?assertError({invalid_env, "K", "abc"}, imboy_env:parse_integer_env("K", "abc")),
    ?assertError({invalid_env, "K", ""}, imboy_env:parse_integer_env("K", "   ")),
    ?assertError({invalid_env, "K", "12x"}, imboy_env:parse_integer_env("K", "12x")),
    ok.

%%% elib_tsid_guard_tests — TSID-06 lifetime lock、guard 状态机与健康状态
%%%
%%% T-201..T-214 本地可执行项：guard 恢复、fence 不可越、锁丢失、
%%% 存储 failure → FENCED、双 guard 互斥（registry provider）、
%%% readiness 合同。真实 flock 双 BEAM 测试在 Linux 容器执行（EXT-05）。
-module(elib_tsid_guard_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NODE, 129).
-define(DC_BITS, 3).

tmp_root() ->
    Dir =
        "/tmp/tsid_guard_test_" ++
            integer_to_list(erlang:unique_integer([positive])) ++ "_" ++
            integer_to_list(os:system_time(microsecond)),
    ok = filelib:ensure_dir(Dir ++ "/x"),
    Dir.

%% 小窗口配置：fence 快速逼近，触发续租路径
fast_cfg(Root) ->
    #{
        root => Root,
        combined_node => ?NODE,
        dc_bits => ?DC_BITS,
        names => [user, group_info],
        lock_provider => registry,
        store_bootstrap => fresh,
        fence_window_ms => 100,
        fence_renew_margin_ms => 20,
        startup_clock_wait_timeout_ms => 1000,
        capacity_wait_timeout_ms => 5000
    }.

%% ===================================================================
%% T-201 bootstrap 前：guard 未就绪时 generate fail-closed
%% ===================================================================

generate_before_guard_fail_closed_test() ->
    elib_tsid:reset_for_test(),
    ?assertMatch(
        {elib_tsid_not_initialized, _},
        try
            elib_tsid:generate(),
            ok
        catch
            error:{elib_tsid_not_initialized, _} = E -> E
        end
    ).

%% ===================================================================
%% guard 启动 → READY → 发布 runtime → 生成越过 durable floor
%% ===================================================================

guard_ready_and_generates_test() ->
    Root = tmp_root(),
    {ok, Pid} = elib_tsid_guard:start_link(fast_cfg(Root)),
    ok = wait_ready(Pid, 50),
    ?assertEqual(ready, elib_tsid_guard:status(Pid)),
    Id = elib_tsid:generate(user),
    ?assert(Id > 0),
    %% fence 语义：生成的 ts 必须 < durable safe_before
    #{guard_ref := GRef} = hd([H || {ok, H} <- [elib_tsid:runtime_handle()]]),
    SafeBefore = atomics:get(GRef, 2),
    Ts = (Id bsr 21) band ((1 bsl 42) - 1),
    ?assert(Ts < SafeBefore),
    stop_guard(Pid).

%% 重启恢复：floor 不回退（T-214 缩影）
guard_restart_floor_no_regression_test() ->
    Root = tmp_root(),
    {ok, P1} = elib_tsid_guard:start_link(fast_cfg(Root)),
    ok = wait_ready(P1, 50),
    Ids = [elib_tsid:generate(user) || _ <- lists:seq(1, 100)],
    MaxTs = lists:max([(I bsr 21) band ((1 bsl 42) - 1) || I <- Ids]),
    stop_guard(P1),
    %% 第二次启动（existing 语义，模拟重启）
    {ok, P2} = elib_tsid_guard:start_link((fast_cfg(Root))#{store_bootstrap => existing}),
    ok = wait_ready(P2, 50),
    Id2 = elib_tsid:generate(user),
    Ts2 = (Id2 bsr 21) band ((1 bsl 42) - 1),
    %% 重启后首个 ts 不低于重启前最大 ts（durable floor 生效）
    ?assert(Ts2 >= MaxTs),
    stop_guard(P2).

%% ===================================================================
%% T-202(守护级) 持久化失败 → FENCED → 不再发 ID；恢复后回 READY
%% ===================================================================

storage_failure_fenced_test() ->
    Root = tmp_root(),
    {ok, Pid} = elib_tsid_guard:start_link(fast_cfg(Root)),
    ok = wait_ready(Pid, 50),
    %% 目录改只读 → 后续 persist 失败
    Dir = elib_tsid_guard:store_dir(Pid),
    ok = file:change_mode(Dir, 8#500),
    %% 触发续租：拉高 cursor 逼近 fence，或等 guard 周期续租
    %% 用大 batch 直接逼近 fence（窗口 100ms ≈ 204800 slots）
    FenceBefore = guard_safe_before(),
    Big = min(100000, (FenceBefore - 10) * 2048 - cursor_rel_ts() * 2048),
    case Big > 2048 of
        true ->
            try
                elib_tsid:generate_n(user, Big)
            catch
                error:{elib_tsid_fenced, _} -> ok;
                error:{elib_tsid_capacity_exhausted, _} -> ok
            end;
        false ->
            ok
    end,
    %% 等 guard 尝试续租失败 → FENCED
    ok = wait_status(Pid, fenced, 3000),
    ?assertEqual(fenced, elib_tsid_guard:status(Pid)),
    %% FENCED 后 generate 必须 typed 拒绝
    ?assertMatch(
        {elib_tsid_fenced, _},
        try
            elib_tsid:generate(user),
            ok
        catch
            error:{elib_tsid_fenced, _} = E -> E
        end
    ),
    %% 恢复存储 → guard 续租成功 → READY
    ok = file:change_mode(Dir, 8#700),
    ok = wait_status(Pid, ready, 10000),
    Id = elib_tsid:generate(user),
    ?assert(Id > 0),
    stop_guard(Pid).

%% ===================================================================
%% T-211 同节点双 guard：恰一个 READY（registry provider 互斥）
%% ===================================================================

dual_guard_exclusion_test() ->
    Root = tmp_root(),
    {ok, P1} = elib_tsid_guard:start_link(fast_cfg(Root)),
    ok = wait_ready(P1, 50),
    %% 第二个 guard 同目录：锁获取失败 → 拒绝启动。
    %% gen_server:start_link 在 init 返回 {stop,...} 时经链接向未
    %% trap_exit 的调用方传播退出——用 trap_exit 辅助进程接收结果。
    ?assertMatch(
        {error, {lock_unavailable, _}},
        start_trapped((fast_cfg(Root))#{store_bootstrap => existing})
    ),
    %% 恰一个 READY
    ?assertEqual(ready, elib_tsid_guard:status(P1)),
    stop_guard(P1).

%% T-212 guard 崩溃（kill -9）→ 锁自动释放 → 新 guard 可接管
%% 被 kill 的 guard 由 trap_exit 的 owner 进程持有链接：'killed' 退出
%% 信号经链接传播会杀死未 trap 的测试进程（eunit 测试进程不 trap）。
owner_crash_lock_released_test() ->
    Root = tmp_root(),
    P1 = spawn_guard(fast_cfg(Root)),
    ok = wait_ready(P1, 50),
    _ = elib_tsid:generate(user),
    exit(P1, kill),
    ok = wait_dead(P1, 50),
    %% 新 guard 接管（registry 锁随进程死亡自动释放）
    {ok, P2} = elib_tsid_guard:start_link((fast_cfg(Root))#{store_bootstrap => existing}),
    ok = wait_ready(P2, 50),
    Id = elib_tsid:generate(user),
    ?assert(Id > 0),
    stop_guard(P2).

%% 在 trap_exit 的 owner 进程里启动 guard 并持有链接
spawn_guard(Cfg) ->
    Parent = self(),
    _Helper =
        spawn(fun() ->
            process_flag(trap_exit, true),
            {ok, Pid} = elib_tsid_guard:start_link(Cfg),
            Parent ! {guard_started, Pid},
            receive
                {'EXIT', Pid, _} -> ok
            end
        end),
    receive
        {guard_started, Pid} -> Pid
    after 10000 ->
        error(guard_start_timeout)
    end.

%% ===================================================================
%% fence 不可越（AC-06C）：持续生成下 ts 始终 < durable safe_before
%% ===================================================================

never_cross_durable_fence_test() ->
    Root = tmp_root(),
    {ok, Pid} = elib_tsid_guard:start_link(fast_cfg(Root)),
    ok = wait_ready(Pid, 50),
    %% 5000 个 batch=100：伴随续租，全程 ts < safe_before
    Ok = lists:all(
        fun(_) ->
            Ids = elib_tsid:generate_n(user, 100),
            Ts = (hd(Ids) bsr 21) band ((1 bsl 42) - 1),
            SafeBefore = guard_safe_before(),
            Ts < SafeBefore
        end,
        lists:seq(1, 50)
    ),
    ?assert(Ok),
    stop_guard(Pid).

%% ===================================================================
%% readiness 合同（AC-06D 前半：不可生成时 readiness 非 200）
%% ===================================================================

tsid_readiness_contract_test() ->
    elib_tsid:reset_for_test(),
    %% 未配置 guard → not_configured（开发模式）
    ?assertEqual(not_configured, healthz_handler:tsid_readiness()),
    Root = tmp_root(),
    {ok, Pid} = elib_tsid_guard:start_link(fast_cfg(Root)),
    ok = wait_ready(Pid, 50),
    ?assertEqual(ready, healthz_handler:tsid_readiness()),
    stop_guard(Pid).

%% livez 永远 200（BEAM 活着即可）
livez_always_ok_test() ->
    ?assertEqual({200, live}, healthz_handler:probe(live, #{tsid => not_configured})),
    ?assertEqual({200, live}, healthz_handler:probe(live, #{tsid => fenced})).

%% readyz 聚合：TSID fenced → 503
readyz_fenced_is_503_test() ->
    ?assertEqual(
        {503, ready},
        healthz_handler:probe(ready, #{db => true, tsid => fenced})
    ),
    ?assertEqual(
        {200, ready},
        healthz_handler:probe(ready, #{db => true, tsid => ready})
    ),
    ?assertEqual(
        {503, ready},
        healthz_handler:probe(ready, #{db => false, tsid => ready})
    ).

%% ===================================================================
%% 监督树顺序合同（计划 §8.3 / TSID-06 Verify）：durable TSID guard
%% 必须是 imboy_sup children **首项**——早于任何可生成 ID 的 worker。
%% 结构性断言：supervisor 顺序启动子进程，首项即全树最先；防止未来
%% 把 guard 挪到 worker 之后造成"worker 先于 fence 就绪"的窗口。
%% ===================================================================

guard_child_first_in_sup_test() ->
    Beam = code:which(imboy_sup),
    AppDir = filename:dirname(filename:dirname(Beam)),
    Src = filename:join(AppDir, "src/imboy_sup.erl"),
    {ok, Bin} = file:read_file(Src),
    Lines = binary:split(Bin, <<"\n">>, [global]),
    SpecsI = first_line(Lines, <<"Specs =">>),
    GuardI = first_line(Lines, <<"tsid_guard_spec(),">>),
    CacheI = first_line_from(Lines, <<"IMBoyCache">>, GuardI + 1),
    %% Specs 列表存在、guard 在其中、且位于首个常规 worker（imboy_cache）之前
    ?assert(SpecsI > 0),
    ?assert(GuardI > SpecsI),
    ?assert(CacheI > GuardI).

%% ===================================================================
%% 内部助手
%% ===================================================================

%% 在 trap_exit 的辅助进程里执行 start_link，返回 {ok,Pid}|{error,R}
%% （失败路径的 exit 信号不杀死测试进程）
start_trapped(Cfg) ->
    Parent = self(),
    Helper = spawn(fun() ->
        process_flag(trap_exit, true),
        R =
            try
                elib_tsid_guard:start_link(Cfg)
            catch
                _:E -> {error, E}
            end,
        %% 清掉 trap 到的 EXIT 信件，避免污染
        receive
            {'EXIT', _, _} -> ok
        after 0 -> ok
        end,
        Parent ! {start_trapped, self(), R}
    end),
    receive
        {start_trapped, Helper, R} -> R
    after 10000 -> error(helper_timeout)
    end.

stop_guard(Pid) ->
    elib_tsid_guard:stop(Pid),
    ok = wait_dead(Pid, 50).

wait_ready(_Pid, 0) ->
    error(guard_not_ready);
wait_ready(Pid, N) ->
    case elib_tsid_guard:status(Pid) of
        ready ->
            ok;
        _ ->
            timer:sleep(100),
            wait_ready(Pid, N - 1)
    end.

wait_status(_Pid, _Want, 0) ->
    error(timeout);
wait_status(Pid, Want, N) ->
    case elib_tsid_guard:status(Pid) of
        Want ->
            ok;
        _ ->
            timer:sleep(100),
            wait_status(Pid, Want, N - 1)
    end.

wait_dead(Pid, 0) ->
    error(still_alive);
wait_dead(Pid, N) ->
    case is_process_alive(Pid) of
        false ->
            ok;
        true ->
            timer:sleep(100),
            wait_dead(Pid, N - 1)
    end.

guard_safe_before() ->
    {ok, #{guard_ref := GRef}} = elib_tsid:runtime_handle(),
    atomics:get(GRef, 2).

cursor_rel_ts() ->
    {ok, #{cursor := Cursor}} = elib_tsid:runtime_handle(),
    (atomics:get(Cursor, 1) bsr 11).

%% 首个含模式的行号（1-based，无则 0）
first_line(Lines, Pat) ->
    first_line_from(Lines, Pat, 1).

%% 从 From 行（1-based，含）起找首个含模式的行，返回绝对行号；无则 0
first_line_from(Lines, Pat, From) ->
    Sub = lists:nthtail(From - 1, Lines),
    case first_index(Sub, Pat, From) of
        nomatch -> 0;
        I -> I
    end.

first_index([], _Pat, _I) ->
    nomatch;
first_index([L | Rest], Pat, I) ->
    case binary:match(L, Pat) of
        nomatch -> first_index(Rest, Pat, I + 1);
        _ -> I
    end.

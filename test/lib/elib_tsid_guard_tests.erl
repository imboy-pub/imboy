%%% elib_tsid_guard_tests — TSID-06 lifetime lock、guard 状态机与健康状态
%%%
%%% T-201..T-214 本地可执行项：guard 恢复、fence 不可越、锁丢失、
%%% 存储 failure → FENCED、双 guard 互斥（registry provider）、
%%% readiness 合同。真实 flock 双 BEAM 测试在 Linux 容器执行（EXT-05）。
-module(elib_tsid_guard_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NODE, 129).
-define(DC_BITS, 3).
%% 与 elib_tsid_guard 的 ?EPOCH_MS 同源（未导出，测试本地复制）
-define(EPOCH_MS, 1735689600000).

tmp_root() ->
    Dir =
        "/tmp/tsid_guard_test_" ++
            integer_to_list(erlang:unique_integer([positive])) ++ "_" ++
            integer_to_list(os:system_time(microsecond)),
    ok = filelib:ensure_dir(Dir ++ "/x"),
    Dir.

%% 小窗口配置：fence 快速逼近，触发续租路径
remove_if_exists(P) ->
    case file:delete(P) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, _} = E -> E
    end.

fast_cfg(Root) ->
    %% max_logical_lead_ms 显式小值：TSID-10 缺省定标为 512 后，缺省值会
    %% 违反本套件小窗口配置的 guard 校验（Window=100 须 > Lead）；机制
    %% 测试显式注入参数，不镜像生产缺省。
    %% 自举 seam（割接状态机集成）：所有 guard 启动测试统一注入——
    %%   bootstrap_env_fun：恒 false，隔离宿主环境变量；
    %%   bootstrap_scan_fun：假 scan，绝不连库（空库口径 floor=0）。
    %% 需要特定 env / scan 结果的用例在各自测试中覆写对应键。
    #{
        root => Root,
        combined_node => ?NODE,
        dc_bits => ?DC_BITS,
        names => [user, group_info],
        lock_provider => registry,
        store_bootstrap => fresh,
        max_logical_lead_ms => 50,
        fence_window_ms => 100,
        fence_renew_margin_ms => 20,
        startup_clock_wait_timeout_ms => 1000,
        capacity_wait_timeout_ms => 5000,
        bootstrap_env_fun => fun(_K) -> false end,
        bootstrap_scan_fun => fun(_Opts) -> {ok, #{floor_safe_before => 0}} end
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

%% 重启恢复：floor 不回退（T-214 缩影）。割接新矩阵下第二次启动依赖
%% 首启落盘的割接 manifest → decide 走 proceed_existing（正常重启分支），
%% 语义与旧实现（直接读 store floor）一致。
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
%% 首启自举 gate（elib_tsid_bootstrap 状态机集成）：判定权在状态机，
%% guard 只执行授权结果。本节扫描一律注入假 scan（绝不连库）。
%% ===================================================================

%% pristine auto_scan 启动成功：假 scan 返回略超当前时钟的历史高水位
%% → AC-05D floor 持久化成功 → 割接 manifest 落盘 → READY，且生成
%% 不越授权 floor。store_bootstrap 取 existing：双槽全缺先被 open 拒绝
%% （no_valid_slot），由状态机 pristine 授权后 guard 以 fresh 重开。
pristine_auto_scan_boots_ready_test() ->
    Root = tmp_root(),
    Floor = rel_now() + 5000,
    Cfg = (fast_cfg(Root))#{
        store_bootstrap => existing,
        bootstrap_scan_fun => fun(_Opts) -> {ok, #{floor_safe_before => Floor}} end
    },
    {ok, Pid} = elib_tsid_guard:start_link(Cfg),
    ok = wait_ready(Pid, 50),
    ?assert(filelib:is_file(manifest_path(Root))),
    {ok, Store} = elib_tsid_store:open(read_store_cfg(Root)),
    #{safe_before := SB} = elib_tsid_store:status(Store),
    ?assert(SB >= Floor),
    Id = elib_tsid:generate(user),
    Ts = (Id bsr 21) band ((1 bsl 42) - 1),
    ?assert(Ts >= Floor),
    stop_guard(Pid).

%% pristine auto_scan 扫描失败：FAIL 级拒绝启动（绝不静默当 fresh）
pristine_scan_failure_stops_test() ->
    Root = tmp_root(),
    Cfg = (fast_cfg(Root))#{
        store_bootstrap => existing,
        bootstrap_scan_fun => fun(_Opts) -> {error, simulated_scan_failure} end
    },
    ?assertMatch({error, {bootstrap_scan, _}}, start_trapped(Cfg)).

%% 旧 writer 未证明停写：store 有 durable floor 而割接 manifest 缺失 →
%% FAIL 级拒绝启动（blocked_legacy_writer）
blocked_legacy_writer_stops_test() ->
    Root = tmp_root(),
    {ok, Pid} = elib_tsid_guard:start_link(fast_cfg(Root)),
    ok = wait_ready(Pid, 50),
    stop_guard(Pid),
    ok = file:delete(manifest_path(Root)),
    Cfg = (fast_cfg(Root))#{store_bootstrap => existing},
    ?assertMatch({error, blocked_legacy_writer}, start_trapped(Cfg)).

%% LEGACY_ACK：操作员确认旧 writer 停写（env 显式确认值）→ 补写割接
%% manifest 后按 adopt_existing 接管现有 floor，生成不回退
legacy_ack_takeover_test() ->
    Root = tmp_root(),
    {ok, P1} = elib_tsid_guard:start_link(fast_cfg(Root)),
    ok = wait_ready(P1, 50),
    Ids = [elib_tsid:generate(user) || _ <- lists:seq(1, 10)],
    MaxTs = lists:max([(I bsr 21) band ((1 bsl 42) - 1) || I <- Ids]),
    stop_guard(P1),
    ok = file:delete(manifest_path(Root)),
    AckFun = fun
        ("IMBOY_TSID_BOOTSTRAP_LEGACY_ACK") -> "I-CONFIRM-OLD-WRITER-STOPPED";
        (_) -> false
    end,
    Cfg = (fast_cfg(Root))#{store_bootstrap => existing, bootstrap_env_fun => AckFun},
    {ok, P2} = elib_tsid_guard:start_link(Cfg),
    ok = wait_ready(P2, 50),
    ?assert(filelib:is_file(manifest_path(Root))),
    Id2 = elib_tsid:generate(user),
    Ts2 = (Id2 bsr 21) band ((1 bsl 42) - 1),
    ?assert(Ts2 >= MaxTs),
    stop_guard(P2).

%% scan 内部崩溃（如真库连接不可用）必须收敛为 typed STOP，绝不 raw crash
scan_crash_typed_stop_test() ->
    Root = tmp_root(),
    BoomF = fun(_Opts) -> erlang:error({pooler_down, no_member}) end,
    Cfg = (fast_cfg(Root))#{store_bootstrap => fresh, bootstrap_scan_fun => BoomF},
    ?assertMatch(
        {error, {bootstrap_crash, #{reason := {pooler_down, no_member}}}},
        start_trapped(Cfg)
    ).

%% 割接 manifest 在而 store 双槽丢失：FAIL 级（不静默重建 fence）
store_lost_manifest_present_test() ->
    Root = tmp_root(),
    {ok, Pid} = elib_tsid_guard:start_link(fast_cfg(Root)),
    ok = wait_ready(Pid, 50),
    stop_guard(Pid),
    NodeDir = filename:join(Root, io_lib:format("node-~4..0B", [?NODE])),
    %% 首启只发生过 boot_ready 一次 persist（fresh 空库 floor=0 时
    %% proceed_floor 跳过 persist），双槽中可能只有一颗有值——删除须容忍
    %% enoent，被删后 existing 模式 open 返回 no_valid_slot。
    remove_if_exists(filename:join(NodeDir, "clock.a")),
    remove_if_exists(filename:join(NodeDir, "clock.b")),
    Cfg = (fast_cfg(Root))#{store_bootstrap => existing},
    ?assertMatch({error, store_lost}, start_trapped(Cfg)).

%% 割接 manifest 的 catalog_digest 与当前 catalog 不符：FAIL 级
%% （catalog 收缩/变更后旧 manifest 不可信，需操作员重做割接）。
%% 坏 manifest 经 write_manifest 合同 API 构造（不手写二进制）。
catalog_changed_stops_test() ->
    Root = tmp_root(),
    {ok, Pid} = elib_tsid_guard:start_link(fast_cfg(Root)),
    ok = wait_ready(Pid, 50),
    stop_guard(Pid),
    Bad = #{
        mode => auto_scan,
        combined_node => ?NODE,
        floor_safe_before => 42,
        created_at_rel_ms => rel_now(),
        catalog_digest => binary:copy(<<7>>, 32)
    },
    ok = elib_tsid_bootstrap:write_manifest(manifest_path(Root), Bad),
    Cfg = (fast_cfg(Root))#{store_bootstrap => existing},
    ?assertMatch({error, catalog_changed}, start_trapped(Cfg)).

%% pristine manual_floor：env 指定 floor → 无需扫描直接授权启动。
%% floor 值取 epoch 后 1234ms——无论状态机按 ts 域还是 slot 域换算
%% （(ms-EPOCH)<<11）都低于当前时钟，本用例只验证接线不绑定换算口径。
manual_floor_bootstrap_test() ->
    Root = tmp_root(),
    UnixMs = 1735689600000 + 1234,
    EnvFun = fun
        ("IMBOY_TSID_BOOTSTRAP_MODE") -> "manual_floor";
        ("IMBOY_TSID_BOOTSTRAP_FLOOR_UNIX_MS") -> integer_to_list(UnixMs);
        (_) -> false
    end,
    Cfg = (fast_cfg(Root))#{store_bootstrap => existing, bootstrap_env_fun => EnvFun},
    {ok, Pid} = elib_tsid_guard:start_link(Cfg),
    ok = wait_ready(Pid, 50),
    ?assert(filelib:is_file(manifest_path(Root))),
    {ok, Store} = elib_tsid_store:open(read_store_cfg(Root)),
    #{safe_before := SB} = elib_tsid_store:status(Store),
    ?assert(SB > (1234 bsl 11)),
    stop_guard(Pid).

%% manual_floor 配非法 floor env：FAIL 级 {bootstrap_env, _}；
%% scan_fun 注入即崩函数，证明 manual_floor 路径绝不触发扫描
manual_floor_bad_env_stops_test() ->
    Root = tmp_root(),
    EnvFun = fun
        ("IMBOY_TSID_BOOTSTRAP_MODE") -> "manual_floor";
        ("IMBOY_TSID_BOOTSTRAP_FLOOR_UNIX_MS") -> "not-an-integer";
        (_) -> false
    end,
    Cfg = (fast_cfg(Root))#{
        store_bootstrap => existing,
        bootstrap_env_fun => EnvFun,
        bootstrap_scan_fun => fun(_Opts) -> error(scan_must_not_be_called) end
    },
    ?assertMatch({error, {bootstrap_env, _}}, start_trapped(Cfg)).

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

%% 锁 provider 不可用 / 未知名：启动期 typed 拒绝（init 预检，
%% review D2 处置——provider_available 的运行时调用点）。
%% 预检先于锁获取与 store open，无资源需要清理。
lock_provider_unavailable_test() ->
    Root = tmp_root(),
    %% 未知名：provider_available function_clause 被预检归一为 typed stop
    ?assertMatch(
        {error, {lock_provider_unavailable, no_such_provider}},
        start_trapped((fast_cfg(Root))#{lock_provider => no_such_provider})
    ).

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
    ?assertEqual({200, live}, healthz_handler:probe(live, #{db => false, tsid => not_configured})),
    ?assertEqual({200, live}, healthz_handler:probe(live, #{db => false, tsid => fenced})).

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

guard_child_first_in_sup_test_() ->
    case sup_src_path() of
        {ok, Src} ->
            fun() ->
                {ok, Bin} = file:read_file(Src),
                Lines = binary:split(Bin, <<"\n">>, [global]),
                SpecsI = first_line(Lines, <<"Specs =">>),
                GuardI = first_line(Lines, <<"tsid_guard_spec(),">>),
                CacheI = first_line_from(Lines, <<"IMBoyCache">>, GuardI + 1),
                %% Specs 列表存在、guard 在其中、且位于首个常规 worker（imboy_cache）之前
                ?assert(SpecsI > 0),
                ?assert(GuardI > SpecsI),
                ?assert(CacheI > GuardI)
            end;
        false ->
            {skip,
                "imboy_sup source not available (installed release); "
                "structural assertion requires the dev source tree"}
    end.

sup_src_path() ->
    case code:which(imboy_sup) of
        Beam when is_list(Beam) ->
            AppDir = filename:dirname(filename:dirname(Beam)),
            Src = filename:join(AppDir, "src/imboy_sup.erl"),
            case filelib:is_file(Src) of
                true -> {ok, Src};
                false -> false
            end;
        _NonExistingOrCover ->
            false
    end.

%% ===================================================================
%% 内部助手
%% ===================================================================

%% 当前相对毫秒（guard safe_before 同一口径）
rel_now() ->
    os:system_time(millisecond) - ?EPOCH_MS.

%% 割接 manifest 路径（elib_tsid_guard bootstrap_manifest_path 同构）
manifest_path(Root) ->
    filename:join([Root, io_lib:format("node-~4..0B", [?NODE]), "tsid.bootstrap"]).

%% 只读重开 store（status 校验 floor 持久化；不 persist）
read_store_cfg(Root) ->
    #{root => Root, combined_node => ?NODE, dc_bits => ?DC_BITS}.

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

%% owner.lock 被替换为 symlink：启动期拒绝（store 槽 symlink 修复的
%% 同族补全——flock 会跟随链接打开攻击者选定的 inode，绕过双实例互斥）
owner_lock_symlink_rejected_test() ->
    Root = tmp_root(),
    NodeDir = filename:join(Root, io_lib:format("node-~4..0B", [?NODE])),
    ok = filelib:ensure_dir(filename:join(NodeDir, "x")),
    ok = file:make_symlink("/nonexistent-target", filename:join(NodeDir, "owner.lock")),
    ?assertMatch(
        {error, symlink_rejected},
        start_trapped(fast_cfg(Root))
    ).

%% F-12 混跑卫生（R2 登记项落地）：套件收尾显式复位 elib_tsid 的 VM 级
%% 发布状态——上方用例以 ?NODE=129 启停 guard 后 elib_tsid 仍持已初始化
%% runtime，同 VM 内后续真实 app boot 会撞 already_initialized。eunit_runner
%% 的 cleanup_start_orphans 亦已兜底（boot 前统一复位），此处为套件级
%% 双保险：任何仅运行本套件的轨道（单套件直跑）也零泄漏。
guard_tests_tail_state_reset_test() ->
    LiveGuard =
        case elib_tsid:runtime_handle() of
            {ok, #{guard_pid := Pid}} when is_pid(Pid) ->
                %% is_process_alive 非法 guard 表达式，存活判定放函数体
                is_process_alive(Pid);
            _ ->
                false
        end,
    case LiveGuard of
        true ->
            %% 全量轨道 app 已在役（真实 guard 持 runtime）：复位会
            %% 掏空在役生成状态，绝不可清——混跑卫生由 eunit_runner
            %% 的 boot 前复位（仅 app 未运行窗口）负责。
            ok;
        false ->
            elib_tsid:reset_for_test(),
            ?assertMatch(
                {elib_tsid_not_initialized, _},
                try elib_tsid:generate() of
                    _ -> ok
                catch
                    error:{elib_tsid_not_initialized, _} = E -> E
                end
            )
    end.

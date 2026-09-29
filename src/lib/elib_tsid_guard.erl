%%% elib_tsid_guard — TSID durable future fence guard（TSID-06）
%%%
%%% 生命周期：acquire lifetime lock → open store（先锁后读）→ 校验
%%% 时钟与恢复 floor → 持久化新 fence → 一次性发布完整 runtime
%%% （cursor 初始化为 (start_ts << 11) - 1）→ READY。
%%%
%%% 状态机（发布进 runtime handle 的 guard_ref atomics，热路径只读）：
%%%   slot1 status：0=FENCED / 1=READY / 2=STOPPING
%%%   slot2 safe_before：durable horizon（只在 persist 成功后写入，
%%%   机械上不存在 publish-before-durable 路径，AC-05D/AC-06C）
%%%
%%% 续租：周期定时器 + 调用方触发（generate 逼近 fence 时同步请求），
%%% 并发请求由 gen_server 天然合并（重复 horizon 直接复用新 fence）。
%%%
%%% 故障策略（计划 §6）：
%%%   持久化失败 → FENCED（当前 fence 余量内调用方继续，到 margin 后
%%%   全部拒绝）；恢复后成功提交更高 fence 才回 READY。
%%%   锁丢失（port/registry 退出）→ FENCED 并停止——绝不换 NodeId 偷跑。
%%%   双实例 → 后到者锁获取失败拒绝启动（AC-06A）。
-module(elib_tsid_guard).

-behaviour(gen_server).

%% 生产启动入口（sup child）与测试入口
-export([start_link/1, stop/1]).
%% 观测
-export([status/1, store_dir/1, probe/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% guard 状态常量（与 elib_tsid 的 guard_ref 布局一致）
-define(STATUS_FENCED, 0).
-define(STATUS_READY, 1).
-define(STATUS_STOPPING, 2).

-define(EPOCH_MS, 1735689600000).
-define(MAX_REL_TS, 4398046511103).

-record(state, {
    root :: file:filename_all(),
    combined_node :: 0..1023,
    dc_bits :: 0..10,
    lock_provider :: elib_tsid_lock:provider(),
    lock :: term(),
    store :: elib_tsid_store:store() | undefined,
    guard_ref :: atomics:atomics_ref() | undefined,
    guard_pid :: pid(),
    fence_window_ms :: pos_integer(),
    fence_renew_margin_ms :: pos_integer(),
    lock_path :: file:filename_all(),
    %% 等待续租回复的调用方（fence 更新后统一应答）
    waiting = [] :: [{pid(), reference(), integer()}]
}).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 启动 guard。Config（见 elib_tsid_guard_tests:fast_cfg/1 与
%% imboy_sup 的生产装配）：
%%   root / combined_node / dc_bits / names / limits（lead 等）
%%   lock_provider（flock | registry，默认 flock）
%%   store_bootstrap（fresh | existing，默认 existing）
%%   fence_window_ms（默认 1000）/ fence_renew_margin_ms（默认 100）
%%   startup_clock_wait_timeout_ms（默认 5000）
%%   capacity_wait_timeout_ms（默认 100）
%%   wall_clock_ms（可选测试 seam 覆盖）
%%   max_initial_lead_ms（默认 60000：恢复 floor 允许领先时钟的量）
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid, normal, 5000).

%% @doc guard 状态（进程查询，诊断/测试用）
-spec status(pid()) -> ready | fenced | stopping | starting.
status(Pid) ->
    try
        gen_server:call(Pid, status, 5000)
    catch
        exit:_ -> unreachable
    end.

%% @doc store 目录（诊断）
-spec store_dir(pid()) -> file:filename_all() | undefined.
store_dir(Pid) ->
    gen_server:call(Pid, store_dir, 5000).

%% @doc 无 pid 探测：从 runtime handle 读 guard 状态
%% （healthz readiness 用；不暴露路径等敏感信息）
-spec probe() -> ready | fenced | stopping | not_configured.
probe() ->
    case elib_tsid:runtime_handle() of
        {ok, #{guard_ref := GRef}} ->
            case atomics:get(GRef, 1) of
                ?STATUS_READY -> ready;
                ?STATUS_STOPPING -> stopping;
                _ -> fenced
            end;
        _ ->
            not_configured
    end.

%% ===================================================================
%% gen_server
%% ===================================================================

init(Config) ->
    process_flag(trap_exit, true),
    Root = maps:get(root, Config),
    CombinedNode = maps:get(combined_node, Config),
    Provider = maps:get(lock_provider, Config, flock),
    Window = maps:get(fence_window_ms, Config, 1000),
    Margin = maps:get(fence_renew_margin_ms, Config, 100),
    %% TSID-10 定标：缺省与 elib_tsid init 一致（512）
    Lead = maps:get(max_logical_lead_ms, Config, 512),
    %% §5.6 不变量校验（fail-closed）
    Valid =
        is_integer(Window) andalso Window > Lead andalso
            is_integer(Margin) andalso Margin > 0 andalso Margin < Window - Lead,
    case Valid of
        true ->
            init_with_lock(Config, Root, CombinedNode, Provider);
        false ->
            {stop,
                {elib_tsid_invalid_config, #{
                    fence_window_ms => Window,
                    fence_renew_margin_ms => Margin,
                    max_logical_lead_ms => Lead
                }}}
    end.

init_with_lock(Config, Root, CombinedNode, Provider) ->
    LockPath = filename:join([Root, node_dir(CombinedNode), "owner.lock"]),
    _ = filelib:ensure_dir(LockPath),
    TimeoutMs = maps:get(lock_timeout_ms, Config, 5000),
    case elib_tsid_lock:acquire(LockPath, TimeoutMs, Provider) of
        {ok, Lock} ->
            case boot_after_lock(Config, Lock) of
                {ok, State} ->
                    {ok, State};
                {error, Reason} ->
                    _ = elib_tsid_lock:release(Lock),
                    {stop, Reason}
            end;
        {error, Reason} ->
            %% 双实例/锁不可用：拒绝启动（AC-06A）
            {stop, {lock_unavailable, Reason}}
    end.

%% 先锁后读（计划：恢复时先锁后读）
boot_after_lock(Config, Lock) ->
    Root = maps:get(root, Config),
    CombinedNode = maps:get(combined_node, Config),
    StoreCfg = #{
        root => Root,
        combined_node => CombinedNode,
        dc_bits => maps:get(dc_bits, Config),
        store_bootstrap => maps:get(store_bootstrap, Config, existing)
    },
    case elib_tsid_store:open(StoreCfg) of
        {error, Reason} ->
            {error, {store_open, Reason}};
        {ok, Store0} ->
            %% 恢复 floor：fresh 空 store 从当前时钟起步
            #{safe_before := PersistedFloor} = elib_tsid_store:status(Store0),
            boot_with_floor(Config, Lock, Store0, PersistedFloor)
    end.

boot_with_floor(Config, Lock, Store0, PersistedFloor) ->
    WallF = maps:get(wall_clock_ms, Config, fun erlang:system_time/1),
    Now = WallF(millisecond) - ?EPOCH_MS,
    StartupWait = maps:get(startup_clock_wait_timeout_ms, Config, 5000),
    MaxInitialLead = maps:get(max_initial_lead_ms, Config, 60000),
    case Now < 0 of
        true ->
            {error, {clock_before_epoch, #{now_rel => Now}}};
        false ->
            %% floor 远超时钟（重启+时钟回拨）：bounded 等待追平
            case PersistedFloor - Now > MaxInitialLead of
                true ->
                    case wait_clock_catchup(WallF, PersistedFloor - MaxInitialLead, StartupWait) of
                        ok ->
                            boot_with_floor(Config, Lock, Store0, PersistedFloor);
                        timeout ->
                            {error, {clock_behind, #{floor => PersistedFloor, now_rel => Now}}}
                    end;
                false ->
                    boot_ready(Config, Lock, Store0, PersistedFloor)
            end
    end.

boot_ready(Config, Lock, Store0, PersistedFloor) ->
    %% 新 fence：max(时钟, floor) + window；持久化成功才发布（AC-05D）
    WallF = maps:get(wall_clock_ms, Config, fun erlang:system_time/1),
    Now = max(WallF(millisecond) - ?EPOCH_MS, PersistedFloor),
    Window = maps:get(fence_window_ms, Config, 1000),
    NewSafeBefore = min(Now + Window, ?MAX_REL_TS + 1),
    case NewSafeBefore =< Now of
        true ->
            {error, exhausted};
        false ->
            case elib_tsid_store:persist(Store0, NewSafeBefore) of
                {ok, Store1} ->
                    publish_and_ready(Config, Lock, Store1, Now, NewSafeBefore);
                {error, Reason} ->
                    {error, {initial_persist, Reason}}
            end
    end.

publish_and_ready(Config, Lock, Store1, StartTs, SafeBefore) ->
    GRef = atomics:new(2, [{signed, false}]),
    ok = atomics:put(GRef, 1, ?STATUS_FENCED),
    ok = atomics:put(GRef, 2, SafeBefore),
    Publish = #{
        combined_node => maps:get(combined_node, Config),
        dc_bits => maps:get(dc_bits, Config),
        names => maps:get(names, Config, []),
        max_logical_lead_ms => maps:get(max_logical_lead_ms, Config, 512),
        capacity_wait_timeout_ms => maps:get(capacity_wait_timeout_ms, Config, 100),
        %% max_batch_chunk 仅显式配置时透传；缺省由 elib_tsid init 按
        %% §5.6 不变量派生 (lead+1)*2048，避免硬编码与 lead 脱钩
        max_batch_chunk =>
            case maps:find(max_batch_chunk, Config) of
                {ok, V} -> V;
                error -> (maps:get(max_logical_lead_ms, Config, 512) + 1) * 2048
            end,
        %% cursor 初始化为 (start_ts << 11) - 1：首个可分配 slot 恰在
        %% start_ts 毫秒 seq=0，绝不低于 durable floor
        cursor_floor_ts => StartTs,
        guard_ref => GRef,
        guard_pid => self()
    },
    case elib_tsid:guarded_publish(Publish) of
        ok ->
            ok = atomics:put(GRef, 1, ?STATUS_READY),
            Margin = maps:get(fence_renew_margin_ms, Config, 100),
            erlang:send_after(max(Margin div 2, 10), self(), renew_tick),
            State = #state{
                root = maps:get(root, Config),
                combined_node = maps:get(combined_node, Config),
                dc_bits = maps:get(dc_bits, Config),
                lock_provider = maps:get(lock_provider, Config, flock),
                lock = Lock,
                store = Store1,
                guard_ref = GRef,
                guard_pid = self(),
                fence_window_ms = maps:get(fence_window_ms, Config, 1000),
                fence_renew_margin_ms = Margin,
                lock_path = filename:join([
                    maps:get(root, Config), node_dir(maps:get(combined_node, Config)), "owner.lock"
                ])
            },
            {ok, State};
        %% 通配（dialyzer 从调用点可证 ok，但保留防御分支覆盖未来重构）
        Other ->
            {error, {publish, Other}}
    end.

%% -------------------------------------------------------------------
%% calls
%% -------------------------------------------------------------------

handle_call(status, _From, #state{guard_ref = GRef} = State) ->
    Reply =
        case GRef of
            undefined -> starting;
            _ -> status_of(atomics:get(GRef, 1))
        end,
    {reply, Reply, State};
handle_call(store_dir, _From, #state{store = undefined} = State) ->
    {reply, undefined, State};
handle_call(store_dir, _From, #state{store = Store} = State) ->
    {reply, elib_tsid_store:dir(Store), State};
handle_call({renew_fence, Horizon}, From, #state{guard_ref = GRef} = State) ->
    case atomics:get(GRef, 1) of
        ?STATUS_READY ->
            SafeBefore = atomics:get(GRef, 2),
            case Horizon < SafeBefore of
                true ->
                    %% 已有 fence 覆盖：立即应答（合并去重）
                    {reply, {ok, SafeBefore}, State};
                false ->
                    %% 挂起等待统一续租（合并并发请求）
                    {noreply, enqueue_renewal(Horizon, From, State)}
            end;
        Other ->
            {reply, {error, {elib_tsid_fenced, #{status => status_of(Other)}}}, State}
    end;
handle_call(_Other, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------
%% info：续租定时器、锁丢失、port 退出
%% -------------------------------------------------------------------

handle_info(renew_tick, State) ->
    {noreply, maybe_renew(State)};
handle_info({Port, {exit_status, _N}}, #state{lock = Lock} = State) when Port =:= Lock ->
    %% flock port 死亡 = 锁丢失：STOPPING 并停止（绝不换 NodeId 偷跑）
    {stop, {lock_lost, port_exit}, mark_stopping(State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{guard_ref = GRef, lock = Lock} = _State) ->
    case GRef of
        undefined -> ok;
        _ -> ok = atomics:put(GRef, 1, ?STATUS_STOPPING)
    end,
    _ = elib_tsid_lock:release(Lock),
    ok.

%% -------------------------------------------------------------------
%% 续租
%% -------------------------------------------------------------------

enqueue_renewal(Horizon, {Pid, Ref} = _From, #state{waiting = W} = State) ->
    State#state{waiting = [{Pid, Ref, Horizon} | W]}.

maybe_renew(#state{waiting = [], guard_ref = GRef, store = Store} = State) when
    Store =/= undefined
->
    %% 周期检查：fence 相对【时钟】老化（无负载时 cursor 不动，但新
    %% reservation 可从当前时钟起借 lead），触发条件取
    %% max(now, cursor) + margin >= safe_before
    SafeBefore = atomics:get(GRef, 2),
    Horizon = max(now_rel(State), cursor_horizon(State)),
    case Horizon + State#state.fence_renew_margin_ms >= SafeBefore of
        true ->
            do_renew(State);
        false ->
            schedule_tick(State),
            State
    end;
maybe_renew(#state{waiting = []} = State) ->
    schedule_tick(State),
    State;
maybe_renew(#state{} = State) ->
    %% 有等待调用方：立即续租
    do_renew(State).

do_renew(#state{guard_ref = GRef, store = Store} = State) ->
    Window = State#state.fence_window_ms,
    Base = max(now_rel(State), cursor_horizon(State)),
    NewSafeBefore = min(Base + Window, ?MAX_REL_TS + 1),
    Current = atomics:get(GRef, 2),
    case NewSafeBefore =< Current of
        true ->
            State1 = reply_waiting(State, {ok, Current}),
            _ = schedule_tick(State1),
            State1;
        false ->
            case elib_tsid_store:persist(Store, NewSafeBefore) of
                {ok, Store1} ->
                    %% 持久化成功后才发布新 horizon（AC-05D）
                    ok = atomics:put(GRef, 2, NewSafeBefore),
                    case atomics:get(GRef, 1) of
                        ?STATUS_FENCED ->
                            ok = atomics:put(GRef, 1, ?STATUS_READY);
                        _ ->
                            ok
                    end,
                    State2 = reply_waiting(State, {ok, NewSafeBefore}),
                    _ = schedule_tick(State2),
                    State2#state{store = Store1};
                {error, _Reason} ->
                    %% 持久化失败：FENCED（余量内调用方可继续，margin 后拒绝）
                    ok = atomics:put(GRef, 1, ?STATUS_FENCED),
                    State3 =
                        reply_waiting(
                            State, {error, {elib_tsid_fenced, #{phase => renew_persist}}}
                        ),
                    _ = schedule_tick(State3),
                    State3
            end
    end.

reply_waiting(#state{waiting = W} = State, Reply) ->
    lists:foreach(
        fun({Pid, Ref, _Horizon}) ->
            Pid ! {Ref, Reply}
        end,
        W
    ),
    State#state{waiting = []}.

schedule_tick(#state{fence_renew_margin_ms = Margin}) ->
    erlang:send_after(max(Margin div 2, 10), self(), renew_tick),
    ok.

%% -------------------------------------------------------------------
%% 内部
%% -------------------------------------------------------------------

now_rel(#state{} = _State) ->
    erlang:system_time(millisecond) - ?EPOCH_MS.

cursor_horizon(_State) ->
    case elib_tsid:runtime_handle() of
        {ok, #{cursor := Cursor}} ->
            (atomics:get(Cursor, 1) bsr 11) + 1;
        _ ->
            0
    end.

mark_stopping(#state{guard_ref = GRef} = State) ->
    _ = atomics:put(GRef, 1, ?STATUS_STOPPING),
    State.

status_of(?STATUS_READY) -> ready;
status_of(?STATUS_STOPPING) -> stopping;
status_of(_) -> fenced.

node_dir(CombinedNode) ->
    lists:flatten(io_lib:format("node-~4..0B", [CombinedNode])).

wait_clock_catchup(_WallF, _TargetNow, 0) ->
    timeout;
wait_clock_catchup(WallF, TargetNow, BudgetMs) ->
    Now = WallF(millisecond) - ?EPOCH_MS,
    case Now >= TargetNow of
        true ->
            ok;
        false ->
            ok = timer:sleep(min(100, BudgetMs)),
            wait_clock_catchup(WallF, TargetNow, BudgetMs - min(100, BudgetMs))
    end.

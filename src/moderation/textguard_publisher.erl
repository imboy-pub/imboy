%%% @doc TextGuard 指定发布者：完整 state_etag CAS，冲突重读真源，有界重试。
-module(textguard_publisher).

-behaviour(gen_server).

%% 管理 API
-export([start_link/0, start_link/1, stop/0]).
%% 触发 API
-export([mark_dirty/0, reconcile/0]).
%% 观察 API
-export([status/0]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export_type([publisher_status/0, state_name/0]).

-define(SERVER, ?MODULE).
-define(DEFAULT_TRANSPORT, moderation_detector_http).
-define(DEFAULT_RECONCILE_INTERVAL_MS, 60000).
-define(DEFAULT_CAS_MAX_RETRIES, 1).
-define(DEFAULT_CAS_BACKOFF_MS, 200).
-define(DEFAULT_PUBLISH_TIMEOUT_MS, 10000).

-type state_name() ::
    idle
    | exporting
    | publishing
    | published
    | failed
    | state_unknown.

-type publisher_status() :: #{
    state := state_name(),
    pending := boolean(),
    last_published =>
        undefined
        | #{
            ruleset_revision := binary(),
            generation := binary(),
            state_etag := binary(),
            instance_id := binary(),
            digest := binary(),
            base_digest := binary(),
            published_at := integer()
        },
    last_error => undefined | term(),
    transport := module()
}.

-record(state, {
    transport :: module(),
    interval_ms :: pos_integer(),
    cas_max_retries :: non_neg_integer(),
    backoff_ms :: non_neg_integer(),
    ctx :: map(),
    timer_ref :: reference() | undefined,
    state_name :: state_name(),
    dirty :: boolean(),
    last_published :: map() | undefined,
    last_error :: term()
}).

%% ===================================================================
%% 管理 API
%% ===================================================================

%% @doc 按配置启动（supervision 接线归 T19/T20；当前显式启动）。
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc 带 Opts 覆盖启动（测试注入 transport/间隔/退避）。
%%%
%%% Opts：`transport` / `reconcile_interval_ms` / `cas_max_retries` /
%%% `cas_retry_backoff_ms` / `publish_timeout_ms`。
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) when is_map(Opts) ->
    Conf = config_ds:env(textguard_publisher, []),
    Merged = maps:merge(proplist_to_map(Conf), Opts),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Merged, []).

-spec stop() -> ok.
stop() ->
    case whereis(?SERVER) of
        undefined ->
            ok;
        Pid ->
            gen_server:stop(Pid)
    end.

%% ===================================================================
%% 触发 API
%% ===================================================================

%% @doc 脏通知（cast，fire-and-forget）：丢失由周期对账兜底。
-spec mark_dirty() -> ok.
mark_dirty() ->
    case whereis(?SERVER) of
        undefined ->
            ok;
        Pid ->
            gen_server:cast(Pid, mark_dirty),
            ok
    end.

%% @doc 手动对账（同步 call）：跑一次完整发布循环后返回状态。
-spec reconcile() -> {ok, publisher_status()} | {error, not_running}.
reconcile() ->
    case whereis(?SERVER) of
        undefined ->
            {error, not_running};
        Pid ->
            try
                gen_server:call(Pid, reconcile, infinity)
            catch
                exit:_Reason ->
                    {error, not_running}
            end
    end.

%% @doc 当前状态快照（saved/pending 与 published/failed 可区分）。
-spec status() -> {ok, publisher_status()} | {error, not_running}.
status() ->
    case whereis(?SERVER) of
        undefined ->
            {error, not_running};
        Pid ->
            try
                gen_server:call(Pid, status, infinity)
            catch
                exit:_Reason ->
                    {error, not_running}
            end
    end.

%% ===================================================================
%% gen_server 回调
%% ===================================================================

-spec init(map()) -> {ok, #state{}} | {stop, term()}.
init(Opts) ->
    case transport_module(maps:get(transport, Opts, ?DEFAULT_TRANSPORT)) of
        undefined ->
            {stop, {invalid_transport, maps:get(transport, Opts)}};
        Transport ->
            process_label(),
            Interval = pos_int(
                maps:get(reconcile_interval_ms, Opts, ?DEFAULT_RECONCILE_INTERVAL_MS),
                ?DEFAULT_RECONCILE_INTERVAL_MS
            ),
            CasMax = non_neg_int(maps:get(cas_max_retries, Opts, ?DEFAULT_CAS_MAX_RETRIES)),
            Backoff = non_neg_int(
                maps:get(cas_retry_backoff_ms, Opts, ?DEFAULT_CAS_BACKOFF_MS)
            ),
            TimeoutMs = pos_int(
                maps:get(publish_timeout_ms, Opts, ?DEFAULT_PUBLISH_TIMEOUT_MS),
                ?DEFAULT_PUBLISH_TIMEOUT_MS
            ),
            State =
                #state{
                    transport = Transport,
                    interval_ms = Interval,
                    cas_max_retries = CasMax,
                    backoff_ms = Backoff,
                    ctx = #{deadline_ms => TimeoutMs},
                    timer_ref = undefined,
                    state_name = idle,
                    dirty = false,
                    last_published = undefined,
                    last_error = undefined
                },
            %% 启动即排一次立即对账（重启后无需等满一个周期才开始收敛；
            %% CAS + digest 比对保证幂等，不会重复制造版本）
            {ok, schedule_reconcile(0, State)}
    end.

-spec handle_call(term(), {pid(), reference()}, #state{}) ->
    {reply, term(), #state{}}.
handle_call(status, _From, State) ->
    {reply, {ok, status_map(State)}, State};
handle_call(reconcile, _From, State) ->
    State1 = run_cycle(State),
    {reply, {ok, status_map(State1)}, State1};
handle_call(_Other, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(mark_dirty, State) ->
    %% 脏通知：置位；若当前无更近的对账在途，立即触发（不等满周期）
    {noreply, schedule_reconcile(0, State#state{dirty = true})};
handle_cast(_Other, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(reconcile, State) ->
    State1 = run_cycle(State#state{timer_ref = undefined}),
    %% 周期对账常开（丢失通知/重启恢复都靠它），无论本轮结果如何
    {noreply, schedule_reconcile(State1#state.interval_ms, State1)};
handle_info(_Other, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, #state{timer_ref = Ref}) ->
    cancel_timer(Ref),
    ok.

%% ===================================================================
%% 对账循环（单槽串行）
%% ===================================================================

-spec run_cycle(#state{}) -> #state{}.
run_cycle(State0) ->
    run_cycle(State0, State0#state.cas_max_retries).

-spec run_cycle(#state{}, non_neg_integer()) -> #state{}.
run_cycle(State0, CasRetriesLeft) ->
    State1 = State0#state{state_name = exporting, dirty = true},
    case textguard_export:export_package() of
        {ok, Export} ->
            State2 = State1#state{state_name = publishing},
            attempt_publish(State2, Export, CasRetriesLeft);
        {error, Reason} ->
            %% DB 读失败/增强文件损坏：绝不发布旧包冒充（T16 禁止条件：
            %% DB读取失败不能导出空库）——failed 可见，dirty 保留待下轮
            State1#state{state_name = failed, last_error = {export, Reason}}
    end.

%% 单次尝试：GET → ruleset_revision 比对 → （需要时）PUT If-Match。
%% 冲突重试从**全新导出**重来（run_cycle 重跑），有限次后退 failed。
-spec attempt_publish(#state{}, map(), non_neg_integer()) -> #state{}.
attempt_publish(State, Export, RetriesLeft) ->
    Transport = State#state.transport,
    Ctx = State#state.ctx,
    case safe_call(Transport, get_active_ruleset, [Ctx]) of
        {ok, Active} ->
            maybe_put(State, Export, RetriesLeft, Active);
        {error, Reason} ->
            %% GET 失败：远端状态未知，绝不盲目回滚/重发旧包
            State#state{state_name = state_unknown, last_error = {get_active, Reason}}
    end.

-spec maybe_put(#state{}, map(), non_neg_integer(), map()) -> #state{}.
maybe_put(State, Export, RetriesLeft, Active) ->
    PackageSha = maps:get(package_sha256, Export),
    case Active of
        #{ready := true, ruleset_revision := PackageSha} = Conv ->
            %% 已收敛：免 PUT（digest 相同 = 同一字节包），吸收远端
            %% ruleset_revision/generation 作为本地发布事实
            record_published(State, Conv, Export, converged);
        _ ->
            put_package(State, Export, RetriesLeft, Active)
    end.

-spec put_package(#state{}, map(), non_neg_integer(), map()) -> #state{}.
put_package(State, Export, RetriesLeft, Active) ->
    Transport = State#state.transport,
    Ctx = State#state.ctx,
    Package = maps:get(package, Export),
    IfMatch = maps:get(state_etag, Active),
    case safe_call(Transport, publish_package, [Package, #{if_match => IfMatch, ctx => Ctx}]) of
        {ok, Published} ->
            record_published(State, Published, Export, put);
        {error, cas_conflict} when RetriesLeft > 0 ->
            %% 退避后从全新导出重试（远端已前进，本地包必须重建）；
            %% 重试预算递减，耗尽 → failed 可见
            timer:sleep(State#state.backoff_ms),
            run_cycle(State#state{state_name = idle}, RetriesLeft - 1);
        {error, cas_conflict} ->
            State#state{state_name = failed, last_error = {cas_conflict, exhausted}};
        {error, Reason} when
            Reason =:= publish_busy;
            Reason =:= retired_snapshot_busy;
            Reason =:= invalid_ruleset;
            Reason =:= unsupported_ruleset
        ->
            State#state{state_name = failed, last_error = {publish, Reason}};
        {error, Reason} ->
            State#state{state_name = state_unknown, last_error = {publish, Reason}}
    end.

-spec record_published(#state{}, map(), map(), converged | put) -> #state{}.
record_published(State, Remote, Export, _Via) ->
    case maps:get(ruleset_revision, Remote) =:= maps:get(package_sha256, Export) of
        true ->
            Published = Remote#{
                digest => maps:get(package_sha256, Export),
                base_digest => maps:get(base_digest, Export),
                published_at => os:system_time(millisecond)
            },
            State#state{
                state_name = published,
                dirty = false,
                last_published = Published,
                last_error = undefined
            };
        false ->
            State#state{state_name = state_unknown, last_error = {publish, protocol_error}}
    end.

%% 传输调用防崩溃包装：mock/适配器异常不带走发布者进程。
-spec safe_call(module(), atom(), [term()]) -> term().
safe_call(M, F, Args) ->
    try
        erlang:apply(M, F, Args)
    catch
        _:_Crash ->
            {error, transport_crash}
    end.

%% ===================================================================
%% 定时与状态投影
%% ===================================================================

%% 只在当前没有更近的待触发对账时重排（mark_dirty 的立即触发不被
%% 周期 tick 无意义覆盖；反之立即触发重置周期间隔是可接受的）。
-spec schedule_reconcile(non_neg_integer(), #state{}) -> #state{}.
schedule_reconcile(DelayMs, #state{timer_ref = Ref} = State) ->
    cancel_timer(Ref),
    NewRef = erlang:send_after(DelayMs, self(), reconcile),
    State#state{timer_ref = NewRef}.

-spec cancel_timer(reference() | undefined) -> ok.
cancel_timer(undefined) ->
    ok;
cancel_timer(Ref) ->
    erlang:cancel_timer(Ref),
    ok.

-spec status_map(#state{}) -> publisher_status().
status_map(#state{} = S) ->
    #{
        state => S#state.state_name,
        pending => S#state.dirty,
        last_published => S#state.last_published,
        last_error => S#state.last_error,
        transport => S#state.transport
    }.

%% ===================================================================
%% 配置归一
%% ===================================================================

-spec proplist_to_map(proplists:proplist()) -> map().
proplist_to_map(Conf) when is_list(Conf) ->
    maps:from_list([{K, V} || {K, V} <- Conf, not is_integer(K)]);
proplist_to_map(_) ->
    #{}.

-spec transport_module(term()) -> module() | undefined.
transport_module(M) when is_atom(M) ->
    case code:ensure_loaded(M) of
        {module, M} -> M;
        _ -> undefined
    end;
transport_module(_) ->
    undefined.

-spec pos_int(term(), pos_integer()) -> pos_integer().
pos_int(N, _Default) when is_integer(N), N > 0 -> N;
pos_int(_, Default) -> Default.

-spec non_neg_int(term()) -> non_neg_integer().
non_neg_int(N) when is_integer(N), N >= 0 -> N;
non_neg_int(_) -> 0.

%% OTP 28 进程标签（观测性：日志/崩溃报告中可辨识发布者进程）。
-spec process_label() -> ok.
process_label() ->
    try
        proc_lib:set_label(textguard_publisher)
    catch
        _:_ -> ok
    end.

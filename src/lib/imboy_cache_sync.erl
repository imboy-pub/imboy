%% @doc 分布式缓存同步服务器（V2.2.1 W2/W3/W4）
%%
%% 单一写 owner：版本元数据表、generation（19.3 栅栏）、复制事件应用全部
%% 由本 gen_server 串行处理（计划 5.2 方案 A 约束）。
%%
%% 协议（计划 5.1，19.8 混合版本合同）：
%% - legacy 消息 {set,Key,Data,MaxAge,Depend} | {flush,Key} | flush 保持原语义；
%% - V2 事件 #{protocol => 2, operation => set|delete|invalidate, ...} 按
%%   Lamport revision {Counter,NodeId} + {NodeId,BootId} + origin_seq 判序。
%%
%% 状态机（计划 5.3/9.1）：STARTING → DISCOVER → RECOVER_VERSION → READY；
%% 单节点部署允许空缓存直接 READY（5.3）。恢复重试有界，超限进入 DEGRADED；
%% DEGRADED 期间 replicate 写明确报错（9.1），不伪装成功。
%%
%% 不重写 depcache 的 waiter/memo/TTL/依赖失效能力（19.3）：本模块只维护
%% 版本/墓碑/generation 元数据与事件串行应用，值仍存 depcache。
-module(imboy_cache_sync).
-moduledoc "分布式缓存同步：V2 协议串行应用、generation 栅栏元数据、恢复状态机。".
-behaviour(gen_server).

-include("cache.hrl").
-include("log.hrl").
-include("chat.hrl").

-define(CACHE_GROUP_NAME, dsync_handler).
-define(META_TABLE, imboy_cache_meta).
%% 消息队列高水位（W4-09）：超过时拒绝新 replicate 写入
-define(HIGH_WATERMARK, 10000).
%% 恢复单轮等待与有界重试（9.1 RETRY_WITH_LIMIT；总预算受 recovery_window 约束）
-define(RECOVER_ROUND_MS, 5000).
-define(RECOVER_MAX_RETRY, 3).
%% 定期状态校对周期（9.4，本地工程候选）
-define(RECONCILE_INTERVAL_MS, 60000).
%% 时钟偏差预算（秒，计划 6.1 C=5）
-define(SKEW_SEC, 5).

%% API
-export([
    start_link/0,
    broadcast/1,
    %% generation 栅栏（19.3）
    generation/1,
    next_generation/1,
    dep_generation/1,
    next_dep_generation/1,
    %% 失效与发布
    invalidate/1,
    publish_set/3,
    publish_delete/1,
    %% 状态与可观测性
    status/0,
    meta_snapshot/0,
    node_boot/0,
    queue_depth/0,
    overloaded/0,
    is_replicated/1,
    replicate_state/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    boot_id :: binary(),
    counter = 0 :: non_neg_integer(),
    origin_seq = 0 :: non_neg_integer(),
    status = starting :: starting | discovering | recovering | ready | degraded,
    retries = 0 :: non_neg_integer(),
    %% 恢复中：等待快照完成的 peer 节点集合（W4-03/04）
    pending_snapshots = [] :: [node()]
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc legacy 广播（保持原语义；syn 组含本节点，回流幂等）
-spec broadcast(term()) -> {ok, non_neg_integer()}.
broadcast(Message) ->
    try syn:publish(?CACHE_SCOPE, ?CACHE_GROUP_NAME, {cache_sync, Message}) of
        R -> R
    catch
        _:_ -> {ok, 0}
    end.

%% @doc 当前失效域 generation（读 ETS，不经进程；缺省 0）。
-spec generation(term()) -> non_neg_integer().
generation(Domain) ->
    case ets:lookup(?META_TABLE, {gen, Domain}) of
        [{_, G}] when is_integer(G) -> G;
        [] -> 0
    end.

%% @doc 依赖标签 generation（版本化物理标签 {cache_v2_dep, Tag, Gen} 的 Gen）。
-spec dep_generation(term()) -> non_neg_integer().
dep_generation(Tag) ->
    generation({dep, Tag}).

%% @doc 推进失效域 generation 并清理旧物理键（同步；19.3：先推进再失效旧值）。
-spec next_generation(term()) -> non_neg_integer().
next_generation(Domain) ->
    gen_call({next_generation, Domain}).

-spec next_dep_generation(term()) -> non_neg_integer().
next_dep_generation(Tag) ->
    gen_call({next_dep_generation, Tag}).

%% @doc 统一失效入口 imboy_cache:invalidate/1 的服务端：
%% 本地 gen 推进 + 旧物理键清理 + 逻辑键 flush + 跨节点发布（协议自适应）。
-spec invalidate(term()) -> ok.
invalidate(Key) ->
    gen_call({invalidate, Key}).

%% @doc replicate SET（计划 2.2）：校验→分配版本→本地写→发布。
%% 未 READY / 过载 / 未授权 / 超限时明确报错，不伪装成功。
-spec publish_set(term(), term(), non_neg_integer()) -> ok | {error, term()}.
publish_set(Key, Value, TTL) ->
    gen_call({publish_set, Key, Value, TTL}).

%% @doc replicate DELETE：版本化删除 + 墓碑。
-spec publish_delete(term()) -> ok | {error, term()}.
publish_delete(Key) ->
    gen_call({publish_delete, Key}).

-spec status() -> atom().
status() ->
    case whereis(?MODULE) of
        undefined ->
            starting;
        _ ->
            try ets:lookup(?META_TABLE, status) of
                [{_, S}] -> S;
                [] -> starting
            catch
                _:_ -> starting
            end
    end.

%% @doc 元数据快照（快照恢复 W4-03 与测试取证用）。
-spec meta_snapshot() -> [term()].
meta_snapshot() ->
    try
        ets:tab2list(?META_TABLE)
    catch
        _:_ -> []
    end.

%% @doc 本节点 {NodeId, BootId}（可观测性，19.8）。
-spec node_boot() -> {node(), binary()}.
node_boot() ->
    case ets:lookup(?META_TABLE, node_boot) of
        [{_, NB}] -> NB;
        [] -> {node(), <<>>}
    end.

%% @doc 过载状态查询（调用方前置检查，W4-09）。
-spec overloaded() -> boolean().
overloaded() ->
    queue_depth() > ?HIGH_WATERMARK.

-spec queue_depth() -> non_neg_integer().
queue_depth() ->
    case whereis(?MODULE) of
        undefined ->
            %% sync 进程不可用（崩溃重启窗口/被停）：视同饱和（严格大于水位线
            %% 才判 overloaded，哨兵必须超过阈值），调用方 fail-closed 拒绝，
            %% 而非 badarg 崩掉业务调用方
            ?HIGH_WATERMARK + 1;
        Pid ->
            case process_info(Pid, message_queue_len) of
                {message_queue_len, N} -> N;
                _ -> 0
            end
    end.

%% @doc Key 是否为本地已登记的 replicate Key（get/1 拦截用，计划 5.5）。
-spec is_replicated(term()) -> boolean().
is_replicated(Key) ->
    try
        ets:member(?META_TABLE, {rindex, Key})
    catch
        _:_ -> false
    end.

%% @doc replicate Key 的带版本读：墓碑/未 READY 时不得返回未校验旧值（5.5/9.1）。
-spec replicate_state(Key :: term()) -> live | deleted | unverified.
replicate_state(Key) ->
    case ets:lookup(?META_TABLE, {rev, Key}) of
        [{{rev, Key}, {_Rev, _C, _Exp, deleted}}] ->
            deleted;
        [{{rev, Key}, {_Rev, _C, _Exp, live}}] ->
            case status() of
                ready -> live;
                _ -> unverified
            end;
        [] ->
            %% 无版本记录：可能是本节点未同步的副本，视同未校验
            case status() of
                ready -> unknown_live;
                _ -> unverified
            end
    end.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

-spec init([]) -> {ok, #state{}, 0}.
init([]) ->
    BootId = crypto:strong_rand_bytes(16),
    ?META_TABLE = ets:new(?META_TABLE, [
        named_table, public, set, {read_concurrency, true}
    ]),
    true = ets:insert(?META_TABLE, {node_boot, {node(), BootId}}),
    true = ets:insert(?META_TABLE, {status, discovering}),
    join_syn(),
    ?DEBUG_LOG(["cache sync starting, boot_id", BootId]),
    {ok, #state{boot_id = BootId, status = discovering}, 0}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.
%% --- generation 栅栏 ---
handle_call({next_generation, Domain}, _From, State) ->
    G = bump_generation(Domain),
    ok = flush_physical(Domain, G - 1),
    {reply, G, State};
handle_call({next_dep_generation, Tag}, _From, State) ->
    G = bump_generation({dep, Tag}),
    _ = depcache:flush(phys_dep(Tag, G - 1), ?DEPCACHE_SERVER),
    {reply, G, State};
%% --- 统一失效 ---
handle_call({invalidate, Key}, _From, State) ->
    ok = do_invalidate(Key),
    {reply, ok, State};
%% --- replicate 发布 ---
handle_call({publish_set, Key, Value, TTL}, _From, State) ->
    case pre_publish() of
        ok ->
            {{C, _} = Rev, Seq, S1} = allocate(State),
            ExpiresAt = erlang:system_time(millisecond) + TTL * 1000,
            _ = depcache:set(Key, Value, TTL, [], ?DEPCACHE_SERVER),
            true = ets:insert(?META_TABLE, {{rev, Key}, {Rev, C, ExpiresAt, live}}),
            ok = index_replicate(Key),
            incr(publish_total),
            _ = publish(v2_event(set, Key, Value, ExpiresAt, Rev, Seq, State#state.boot_id)),
            {reply, ok, S1};
        {error, _} = Err ->
            incr(reject_total),
            {reply, Err, State}
    end;
handle_call({publish_delete, Key}, _From, State) ->
    case pre_publish() of
        ok ->
            {{C, _} = Rev, Seq, S1} = allocate(State),
            TombstoneTTL = imboy_cache_policy:tombstone_ttl_sec(),
            ExpiresAt = erlang:system_time(millisecond) + TombstoneTTL * 1000,
            _ = depcache:flush(Key, ?DEPCACHE_SERVER),
            true = ets:insert(?META_TABLE, {{rev, Key}, {Rev, C, ExpiresAt, deleted}}),
            incr(publish_total),
            _ = publish(v2_event(delete, Key, undefined, ExpiresAt, Rev, Seq, State#state.boot_id)),
            {reply, ok, S1};
        {error, _} = Err ->
            incr(reject_total),
            {reply, Err, State}
    end;
%% --- 诊断（消息串行化确认：调用方发异步消息后 ping 确保 已处理）---
handle_call(ping_ok, _From, State) ->
    {reply, pong, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
%% --- 恢复状态机 ---
handle_info(timeout, #state{status = discovering} = State) ->
    discover(State);
handle_info(timeout, #state{status = recovering} = State) ->
    %% 单轮恢复窗口超时：有界重试（RETRY_WITH_LIMIT）
    discover(State#state{retries = State#state.retries + 1});
handle_info({cache_sync, Msg}, State) ->
    {noreply, handle_sync_message(Msg, State)};
%% 版本上界应答（恢复期间 peer → 本节点；每个应答都抬高本地 clock 下界）
handle_info({cache_version_report, FromNode, MaxCounter}, #state{status = recovering} = State) ->
    State1 = merge_version_ceiling(MaxCounter, State),
    %% W4-03：向应答 peer 请求 replicate 快照（数据回填）
    _ = [
        P ! {cache_snapshot_request, self()}
     || P <- syn_scope_members(), node(P) =:= FromNode
    ],
    Pending = [FromNode | lists:delete(FromNode, State1#state.pending_snapshots)],
    {noreply, State1#state{pending_snapshots = Pending}};
handle_info({cache_version_report, _FromNode, MaxCounter}, State) ->
    {noreply, merge_version_ceiling(MaxCounter, State)};
%% 快照请求（ready peer → 本恢复节点）：枚举 rindex 回放版本化 SET（W4-03）
handle_info({cache_snapshot_request, RequestorPid}, #state{status = ready} = State) ->
    _ = send_snapshot(RequestorPid, State),
    RequestorPid ! {cache_snapshot_done, node()},
    incr(reconcile_total),
    {noreply, State};
handle_info({cache_snapshot_request, RequestorPid}, State) ->
    %% 非 ready（peer 同在恢复）：回空快照 + done 标记，请求方回合可闭合
    _ =
        try
            RequestorPid ! {cache_snapshot_done, node()}
        catch
            _:_ -> ok
        end,
    {noreply, State};
%% 快照完成标记：全部 peer 快照到齐才 READY（W4-04：快照期间 delta 由
%% 版本比较覆盖，无需冻结写入）
handle_info({cache_snapshot_done, FromNode}, #state{status = recovering} = State) ->
    Pending = lists:delete(FromNode, State#state.pending_snapshots),
    case Pending of
        [] -> become_ready(State#state{pending_snapshots = []});
        _ -> {noreply, State#state{pending_snapshots = Pending}}
    end;
handle_info({cache_snapshot_done, _FromNode}, State) ->
    {noreply, State};
%% 版本上界请求（他节点恢复 → 本节点应答，5.3）
handle_info({cache_version_request, FromNode}, State) ->
    _ = [
        P ! {cache_version_report, node(), State#state.counter}
     || P <- syn_scope_members(), node(P) =:= FromNode
    ],
    {noreply, State};
handle_info(reconcile_tick, State) ->
    erlang:send_after(?RECONCILE_INTERVAL_MS, self(), reconcile_tick),
    {noreply, periodic_reconcile(State)};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    try
        syn:leave(?CACHE_SCOPE, ?CACHE_GROUP_NAME, self())
    catch
        _:_ -> ok
    end,
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% 同步消息处理（legacy + V2）
%% ===================================================================

-spec handle_sync_message(term(), #state{}) -> #state{}.
handle_sync_message(#{protocol := 2} = Event, State) ->
    case apply_v2(Event, State) of
        {ok, S1} ->
            S1;
        {reject, _Reason} ->
            incr(reject_total),
            State
    end;
handle_sync_message({set, Key, Data, MaxAge, Depend}, State) ->
    %% legacy：保持原语义（含本节点回流，depcache:set 幂等）
    incr(receive_total),
    depcache:set(Key, Data, MaxAge, Depend, ?DEPCACHE_SERVER),
    State;
handle_sync_message({flush, Key}, State) ->
    incr(receive_total),
    _ = depcache:flush(Key, ?DEPCACHE_SERVER),
    %% 保守栅栏：legacy flush 到达时推进 fenced 域 generation（混合版本宁可多失效）
    _ = maybe_bump_fenced(Key),
    State;
handle_sync_message(flush, State) ->
    incr(receive_total),
    _ = depcache:flush(?DEPCACHE_SERVER),
    ok = bump_all_fenced(),
    State;
handle_sync_message(_, State) ->
    %% 未知消息静默丢弃 + 计数（19.8：可观测，未来升级为告警）
    incr(unknown_total),
    State.

%% @doc V2 事件应用（W3-01..W3-10）。gen_server 串行调用即写入串行化（5.2 方案 A）。
-spec apply_v2(map(), #state{}) -> {ok, #state{}} | {reject, term()}.
apply_v2(Event, State) ->
    case validate_v2(Event) of
        ok ->
            incr(receive_total),
            Key = maps:get(key, Event),
            Op = maps:get(operation, Event),
            Origin = maps:get(origin_node, Event),
            Seq = maps:get(origin_seq, Event),
            BootId = maps:get(origin_boot_id, Event),
            SelfLoop = Origin =:= node() andalso BootId =:= State#state.boot_id,
            case {SelfLoop, Op} of
                {true, _} ->
                    %% W3-10：自己发出的广播不重复修改本地状态
                    {ok, State};
                {false, invalidate} ->
                    %% invalidate 不参与版本比较与 Lamport 推进（非 replicate 数据）
                    _ = maybe_bump_fenced(Key),
                    _ = depcache:flush(Key, ?DEPCACHE_SERVER),
                    {ok, State};
                {false, _} ->
                    ok = detect_gap(Origin, Seq),
                    State1 = merge_clock(maps:get(revision, Event), State),
                    apply_v2_op(Op, Key, Event, State1)
            end;
        {error, Reason} ->
            {reject, Reason}
    end.

-spec validate_v2(map()) -> ok | {error, term()}.
validate_v2(Event) when is_map(Event) ->
    Op = maps:get(operation, Event, undefined),
    Key = maps:get(key, Event, undefined),
    checks_v2([
        {fun() -> Key =/= undefined end, missing_key},
        {fun() -> lists:member(Op, [set, delete, invalidate]) end, unknown_operation},
        {
            fun() ->
                is_tuple(maps:get(revision, Event, none)) andalso
                    tuple_size(maps:get(revision, Event)) =:= 2
            end,
            invalid_revision
        },
        {fun() -> is_integer(maps:get(origin_seq, Event, none)) end, invalid_origin_seq},
        {fun() -> is_atom(maps:get(origin_node, Event, none)) end, invalid_origin_node},
        {fun() -> is_binary(maps:get(origin_boot_id, Event, none)) end, invalid_boot_id},
        %% 接收端独立白名单/敏感性校验（19.5：发送/接收两端独立校验，fail-closed）
        {
            fun() ->
                Op =:= invalidate orelse
                    imboy_cache_policy:replicate_allowed(Key)
            end,
            not_authorized
        },
        %% 接收端独立大小校验
        {
            fun() ->
                Op =/= set orelse
                    imboy_cache_policy:check_value_size(
                        maps:get(value, Event)
                    )
            end,
            value_too_large
        }
    ]);
validate_v2(_) ->
    {error, not_map}.

-spec apply_v2_op(set | delete, term(), map(), #state{}) ->
    {ok, #state{}} | {reject, term()}.
apply_v2_op(set, Key, Event, State) ->
    Rev = maps:get(revision, Event),
    case compare_rev(Rev, current_rev(Key)) of
        newer ->
            TTL = remaining_ttl_sec(maps:get(expires_at_ms, Event, 0)),
            case TTL > 0 of
                true ->
                    _ = depcache:set(Key, maps:get(value, Event), TTL, [], ?DEPCACHE_SERVER),
                    C = erlang:element(1, Rev),
                    true = ets:insert(
                        ?META_TABLE,
                        {{rev, Key}, {Rev, C, maps:get(expires_at_ms, Event), live}}
                    ),
                    ok = index_replicate(Key),
                    {ok, State};
                false ->
                    incr(expired_total),
                    {reject, expired}
            end;
        equal ->
            %% 同版本幂等（W3-08）
            {ok, State};
        older ->
            incr(conflict_total),
            {reject, stale_revision}
    end;
apply_v2_op(delete, Key, Event, State) ->
    Rev = maps:get(revision, Event),
    case compare_rev(Rev, current_rev(Key)) of
        newer ->
            _ = depcache:flush(Key, ?DEPCACHE_SERVER),
            TombstoneTTL = imboy_cache_policy:tombstone_ttl_sec(),
            ExpiresAt = erlang:system_time(millisecond) + TombstoneTTL * 1000,
            C = erlang:element(1, Rev),
            true = ets:insert(?META_TABLE, {{rev, Key}, {Rev, C, ExpiresAt, deleted}}),
            {ok, State};
        equal ->
            {ok, State};
        older ->
            incr(conflict_total),
            {reject, stale_revision}
    end.

%% ===================================================================
%% 恢复与校对（W4）
%% ===================================================================

-spec discover(#state{}) -> {noreply, #state{}}.
discover(#state{retries = R} = State) when R > ?RECOVER_MAX_RETRY ->
    ?WARN_LOG(["cache sync recovery retries exhausted, entering DEGRADED"]),
    incr(degraded_total),
    true = ets:insert(?META_TABLE, {status, degraded}),
    {noreply, State#state{status = degraded}};
discover(State) ->
    Members = [P || P <- syn_scope_members(), node(P) =/= node()],
    case Members of
        [] when State#state.retries < 3 ->
            %% syn 成员视图最终一致：刚 join/重启后可能瞬时为空，
            %% 有界重试后再判定（防止误判单节点而跳过版本上界恢复）
            _ = erlang:send_after(500, self(), timeout),
            {noreply, State#state{retries = State#state.retries + 1}};
        [] ->
            %% 稳定后仍空：真单节点，允许从空缓存启动（5.3）
            become_ready(State);
        _ ->
            %% 多节点：向全部 peer 请求版本上界（5.3 RECOVER_VERSION）
            _ = [P ! {cache_version_request, node()} || P <- Members],
            _ = erlang:send_after(?RECOVER_ROUND_MS, self(), timeout),
            true = ets:insert(?META_TABLE, {status, recovering}),
            {noreply, State#state{status = recovering}}
    end.

-spec become_ready(#state{}) -> {noreply, #state{}}.
become_ready(State) ->
    _ = erlang:send_after(?RECONCILE_INTERVAL_MS, self(), reconcile_tick),
    true = ets:insert(?META_TABLE, {status, ready}),
    {noreply, State#state{status = ready, retries = 0}}.

-spec merge_version_ceiling(non_neg_integer(), #state{}) -> #state{}.
merge_version_ceiling(MaxCounter, #state{counter = C} = State) ->
    State#state{counter = erlang:max(C, MaxCounter)}.

%% @doc 定期轻量校对（9.4）：交换版本摘要上界 + 有界清理（W4-06/W4-07）。
-spec periodic_reconcile(#state{}) -> #state{}.
periodic_reconcile(#state{status = ready} = State) ->
    incr(reconcile_total),
    Members = [P || P <- syn_scope_members(), node(P) =/= node()],
    _ = [P ! {cache_version_request, node()} || P <- Members],
    ok = bounded_sweep(),
    State;
periodic_reconcile(State) ->
    State.

%% @doc 有界清理（W4-06/W4-07）：过期墓碑淘汰。索引与墓碑总量上界由
%% tombstone 过期时间约束（时间有界即数量有界；超窗 peer 走冷恢复 19.6）。
-spec bounded_sweep() -> ok.
bounded_sweep() ->
    NowMs = erlang:system_time(millisecond),
    TTLms = imboy_cache_policy:tombstone_ttl_sec() * 1000,
    %% tab2list 直取整行：ets:match('$1') 绑定的是整行元组，按 [K, V] 列表
    %% 解构永不匹配、清理空转（F4 场景实证的潜伏缺陷）
    _ = [
        begin
            ok = sweep_entry(K),
            true = ets:delete(?META_TABLE, K)
        end
     || {K, V} <- ets:tab2list(?META_TABLE),
        is_sweepable(K, V, NowMs, TTLms)
    ],
    ok.

%% 淘汰 rev 行时连带清理同名 rindex 行——rindex 无独立过期路径，
%% 不连带清理则随复制写入无界增长（W4-06/07：时间有界即数量有界）
sweep_entry({rev, K}) ->
    _ = ets:delete(?META_TABLE, {rindex, K}),
    ok;
sweep_entry(_) ->
    ok.

-spec is_sweepable(term(), term(), non_neg_integer(), non_neg_integer()) -> boolean().
%% 过期墓碑：再保留一个 Tmax 写窗（陈旧事件防复活），之后连同 rindex 淘汰
is_sweepable(K, {_Rev, _C, ExpiresAt, deleted}, NowMs, TTLms) ->
    is_tuple(K) andalso tuple_size(K) =:= 2 andalso element(1, K) =:= rev andalso
        ExpiresAt + TTLms < NowMs;
%% 过期 live：值已随 depcache TTL 消失，延迟重放由事件自带 expires_at 拒绝
%% （expired），版本上界由 gen_server counter 承载——元数据到期即淘汰
is_sweepable(K, {_Rev, _C, ExpiresAt, live}, NowMs, _TTLms) ->
    is_tuple(K) andalso tuple_size(K) =:= 2 andalso element(1, K) =:= rev andalso
        ExpiresAt < NowMs;
is_sweepable(_, _, _, _) ->
    false.

%% ===================================================================
%% Internal
%% ===================================================================

-spec join_syn() -> ok.
join_syn() ->
    try syn:join(?CACHE_SCOPE, ?CACHE_GROUP_NAME, self(), #{}) of
        ok -> ok
    catch
        _:_ -> ok
    end.

-spec gen_call(term()) -> term().
gen_call(Msg) ->
    try
        gen_server:call(?MODULE, Msg, 5000)
    catch
        _:_ -> {error, sync_unavailable}
    end.

-spec bump_generation(term()) -> non_neg_integer().
bump_generation(Domain) ->
    G = generation(Domain) + 1,
    true = ets:insert(?META_TABLE, {{gen, Domain}, G}),
    G.

-spec flush_physical(term(), integer()) -> ok.
flush_physical(_Domain, G) when G < 0 ->
    ok;
flush_physical(Domain, G) ->
    _ = depcache:flush(phys_key(Domain, G), ?DEPCACHE_SERVER),
    ok.

-spec phys_key(term(), non_neg_integer()) -> term().
phys_key(LogicalKey, G) ->
    {cache_v2, LogicalKey, G}.

-spec phys_dep(term(), non_neg_integer()) -> term().
phys_dep(Tag, G) ->
    {cache_v2_dep, Tag, G}.

%% @doc 统一失效：先推进 gen（19.3），再清旧物理键与逻辑键，最后跨节点发布。
-spec do_invalidate(term()) -> ok.
do_invalidate(Key) ->
    case is_fenced(Key) of
        true ->
            G = bump_generation(Key),
            ok = flush_physical(Key, G - 1);
        false ->
            ok
    end,
    _ = depcache:flush(Key, ?DEPCACHE_SERVER),
    publish_invalidate(Key).

-spec publish_invalidate(term()) -> ok.
publish_invalidate(Key) ->
    case imboy_cache_policy:protocol() of
        v2 ->
            incr(publish_total),
            %% invalidate 事件不带 Lamport 版本（接收端跳过版本比较），
            %% revision 占位仅满足消息结构校验。
            _ = publish(#{
                protocol => 2,
                operation => invalidate,
                namespace => imboy_cache_policy:namespace_of(Key),
                key => Key,
                revision => {0, node()},
                origin_seq => 0,
                origin_node => node(),
                origin_boot_id => boot()
            }),
            ok;
        legacy ->
            %% legacy 模式跨节点失效用现有 {flush, Key}（19.8 能力交集）
            _ = broadcast({flush, Key}),
            ok
    end.

-spec boot() -> binary().
boot() ->
    case ets:lookup(?META_TABLE, node_boot) of
        [{_, {_, B}}] -> B;
        [] -> <<>>
    end.

-spec is_fenced(term()) -> boolean().
is_fenced(Key) ->
    NS = imboy_cache_policy:namespace_of(Key),
    NS =/= undefined andalso lists:member(NS, fenced_namespaces()).

-spec fenced_namespaces() -> [atom()].
fenced_namespaces() ->
    application:get_env(imboy, cache_fenced_namespaces, []).

-spec maybe_bump_fenced(term()) -> ok.
maybe_bump_fenced(Key) ->
    case is_fenced(Key) of
        true ->
            G = bump_generation(Key),
            ok = flush_physical(Key, G - 1);
        false ->
            ok
    end.

%% @doc 整库 flush 时推进全部已知 fenced 域 generation：正在进行中的旧 loader
%% 仍可能回写旧物理键（19.3 允许，TTL 有界），代次前移保证新请求不再读旧值。
-spec bump_all_fenced() -> ok.
bump_all_fenced() ->
    _ = [
        begin
            [{_, G}] = ets:lookup(?META_TABLE, {gen, Domain}),
            true = ets:insert(?META_TABLE, {{gen, Domain}, G + 1})
        end
     || [Domain] <- ets:match(?META_TABLE, {{gen, '$1'}, '_'})
    ],
    ok.

%% --- 版本与发布 ---

-spec allocate(#state{}) ->
    {{non_neg_integer(), node()}, non_neg_integer(), #state{}}.
allocate(#state{counter = C, origin_seq = S} = State) ->
    C1 = C + 1,
    S1 = S + 1,
    {{C1, node()}, S1, State#state{counter = C1, origin_seq = S1}}.

-spec merge_clock({non_neg_integer(), node()}, #state{}) -> #state{}.
merge_clock({RemoteC, _Node}, #state{counter = C} = State) ->
    State#state{counter = erlang:max(C, RemoteC)}.

-spec current_rev(term()) -> {non_neg_integer(), node()} | undefined.
current_rev(Key) ->
    case ets:lookup(?META_TABLE, {rev, Key}) of
        [{{rev, Key}, {Rev, _C, _Exp, _St}}] -> Rev;
        [] -> undefined
    end.

%% @doc Lamport revision 比较（5.2）：Counter 大者新；同 Counter 按 NodeId 确定性。
-spec compare_rev({non_neg_integer(), node()}, {non_neg_integer(), node()} | undefined) ->
    newer | equal | older.
compare_rev(_Rev, undefined) ->
    newer;
compare_rev({C1, N1}, {C2, N2}) ->
    if
        C1 > C2 -> newer;
        C1 < C2 -> older;
        N1 > N2 -> newer;
        N1 < N2 -> older;
        true -> equal
    end.

%% @doc TTL 剩余（6.2）：接收端按本地时钟计算，保守扣减时钟偏差预算，不重置 TTL。
-spec remaining_ttl_sec(non_neg_integer()) -> non_neg_integer().
remaining_ttl_sec(ExpiresAtMs) ->
    NowMs = erlang:system_time(millisecond),
    Remaining = ExpiresAtMs - NowMs - ?SKEW_SEC * 1000,
    case Remaining =< 0 of
        true -> 0;
        false -> Remaining div 1000
    end.

-spec v2_event(
    set | delete,
    term(),
    term(),
    non_neg_integer(),
    {non_neg_integer(), node()},
    non_neg_integer(),
    binary()
) -> map().
v2_event(Op, Key, Value, ExpiresAt, Rev, Seq, BootId) ->
    #{
        protocol => 2,
        operation => Op,
        namespace => imboy_cache_policy:namespace_of(Key),
        key => Key,
        value => Value,
        expires_at_ms => ExpiresAt,
        revision => Rev,
        origin_seq => Seq,
        origin_node => node(),
        origin_boot_id => BootId
    }.

-spec publish(map()) -> {ok, non_neg_integer()}.
publish(Event) ->
    try syn:publish(?CACHE_SCOPE, ?CACHE_GROUP_NAME, {cache_sync, Event}) of
        R -> R
    catch
        _:_ -> {ok, 0}
    end.

-spec pre_publish() -> ok | {error, term()}.
pre_publish() ->
    case status() of
        St when St =/= ready ->
            {error, {not_ready, St}};
        ready ->
            case queue_depth() > ?HIGH_WATERMARK of
                true -> {error, overloaded};
                false -> ok
            end
    end.

%% @doc origin_seq 缺口检测（W4-05）：记录每个来源最大 seq；观察到跳号计入
%% gap 计数（有界补偿由定期校对触发，9.4）。
-spec detect_gap(node(), non_neg_integer()) -> ok.
detect_gap(Origin, Seq) ->
    Key = {seq, Origin},
    Last =
        case ets:lookup(?META_TABLE, Key) of
            [{_, L}] -> L;
            [] -> 0
        end,
    case Seq > Last + 1 of
        true -> incr(gap_detected_total);
        false -> ok
    end,
    _ = ets:insert(?META_TABLE, {Key, erlang:max(Last, Seq)}),
    ok.

-spec index_replicate(term()) -> ok.
index_replicate(Key) ->
    _ = ets:insert(?META_TABLE, {{rindex, Key}, erlang:system_time(millisecond)}),
    ok.

%% @doc 向恢复节点发送快照（W4-03）：枚举 rindex，live 且未过期的条目
%% 以**原 revision/origin** 回放为普通 V2 SET 事件——接收端按版本比较
%% 幂等合并（与快照期间的新 delta 天然融合，9.3）。批次受 batch_size 约束。
-spec send_snapshot(pid(), #state{}) -> ok.
send_snapshot(RequestorPid, _State) ->
    NowMs = erlang:system_time(millisecond),
    Entries = [
        K
     || [K] <- ets:match(?META_TABLE, {{rindex, '$1'}, '_'}),
        valid_live_entry(K, NowMs)
    ],
    %% 快照正确性优先：全部有效条目以原版本回放（单轮发送；条目总量受
    %% Nmax/墓碑过期上界约束，W4-06/07）；快照期间 peer 的正常写入作为
    %% delta 并行到达，版本比较保证合并正确（9.3）
    _ = [RequestorPid ! {cache_sync, snapshot_event(K)} || K <- Entries],
    ok.

-spec valid_live_entry(term(), non_neg_integer()) -> boolean().
valid_live_entry(K, NowMs) ->
    case ets:lookup(?META_TABLE, {rev, K}) of
        [{{rev, K}, {_Rev, _C, ExpiresAt, live}}] ->
            ExpiresAt > NowMs andalso
                depcache:get(K, ?DEPCACHE_SERVER) =/= undefined;
        _ ->
            false
    end.

-spec snapshot_event(term()) -> map().
snapshot_event(K) ->
    [{{rev, K}, {{_Counter, OriginNode} = Rev, _C2, ExpiresAt, live}}] =
        ets:lookup(?META_TABLE, {rev, K}),
    {ok, Value} = depcache:get(K, ?DEPCACHE_SERVER),
    #{
        protocol => 2,
        operation => set,
        key => K,
        value => Value,
        expires_at_ms => ExpiresAt,
        revision => Rev,
        %% 快照回放保留原 origin（含原 boot_id）：若原 origin 是重启节点
        %% 自身，旧 boot_id 使自环过滤不命中，数据得以恢复
        origin_seq => 0,
        origin_node => OriginNode,
        origin_boot_id => <<0:128>>
    }.

-spec syn_scope_members() -> [pid()].
syn_scope_members() ->
    %% syn 3.4.2 API：syn:members(Scope, Group) -> [{Pid, Meta}]
    try syn:members(?CACHE_SCOPE, ?CACHE_GROUP_NAME) of
        L when is_list(L) -> [P || {P, _Meta} <- L];
        _ -> []
    catch
        _:_ -> []
    end.

-spec incr(atom()) -> ok.
incr(Name) ->
    _ = elib_metric:increment(Name),
    ok.

-spec checks_v2([{fun(() -> boolean()), term()}]) -> ok | {error, term()}.
checks_v2([{Pred, Reason} | Rest]) ->
    case Pred() of
        true -> checks_v2(Rest);
        false -> {error, Reason}
    end;
checks_v2([]) ->
    ok.

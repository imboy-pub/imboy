%% @doc 分布式缓存同步服务器（V2.2.1 W2/W3/W4 + 收口 C02～C05）
%%
%% 单一写 owner：版本元数据表、generation（19.3 栅栏）、复制事件应用全部
%% 由本 gen_server 串行处理（计划 5.2 方案 A）。
%%
%% 协议（计划 5.1 + C01 冻结增量）：
%% - legacy 消息 {set,Key,Data,MaxAge,Depend} | {flush,Key} | flush 保持原语义；
%% - V2 事件 #{protocol => 2, operation, ..., epoch, config_hash, registry_hash}
%%   按 Lamport revision + {NodeId,BootId} + origin_seq 判序；结构校验与
%%   epoch/incarnation/hash 双端一致性由 imboy_cache_sync_proto 承担（C02）；
%% - 快照回合（round/token/watermark/page digest）由
%%   imboy_cache_sync_snapshot 承担（C03/C04）。
%%
%% 状态机（计划 5.3/9.1 + C01 第 5 节）：
%% STARTING → DISCOVERING → RECOVERING → READY；重试有界（单轮 5s × 3 +
%% 快照回合 Rmax_rounds），总预算受 Dmax 硬顶；超限 DEGRADED（读
%% unverified、写拒绝 fail-closed），退出手段=节点级重启。
%%
%% 资源预算（C01 第 6/7 节）：Nmax rindex 上限（满则拒绝 index_full）、
%% Tmax 墓碑上限（满则先过期淘汰，仍满推进 epoch 冷恢复）、HighWater
%% 队列高水位（满则拒绝 overloaded）；不得删除 tombstone 后接受旧事件。
%%
%% invalidate 缺口补偿（C05）：origin_seq 跳号或 peer 化身变化 → 受管
%% fenced namespace 推进 generation（有界清理，旧物理键不可读，新请求
%% 回权威），不依赖旧 TTL 自然到期。
-module(imboy_cache_sync).
-moduledoc "分布式缓存同步：V2 协议串行应用、generation 栅栏、恢复状态机、资源预算与缺口补偿。".
-behaviour(gen_server).

-include("cache.hrl").
-include("log.hrl").
-include("chat.hrl").

-define(CACHE_GROUP_NAME, dsync_handler).
-define(META_TABLE, imboy_cache_meta).
%% 恢复单轮等待与有界重试（9.1 RETRY_WITH_LIMIT；总预算受 Dmax 约束）
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
    replicate_state/1,
    %% 供快照模块复用的纯函数/ETS 读（A1 内部合同，先冻结后消费）
    compare_rev/2,
    current_rev/1,
    remaining_ttl_sec/1
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
    %% 恢复总 deadline（Dmax，C01 第 6 节）
    recover_deadline_ms :: non_neg_integer() | undefined,
    %% 快照回合状态（imboy_cache_sync_snapshot 的 plain map；undefined=非恢复中）
    snapshot_round = undefined :: map() | undefined
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc legacy 广播（保持原语义；syn 组含本节点，回流幂等）。
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
%% 未 READY / 过载 / 未授权 / 超限（Nmax/大小/TTL）时明确报错，不伪装成功。
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

%% @doc 元数据快照（测试取证用）。
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
    queue_depth() > imboy_cache_policy:high_watermark().

-spec queue_depth() -> non_neg_integer().
queue_depth() ->
    case whereis(?MODULE) of
        undefined ->
            %% sync 进程不可用（崩溃重启窗口/被停）：视同饱和（哨兵超过阈值），
            %% 调用方 fail-closed 拒绝，而非 badarg 崩掉业务调用方
            imboy_cache_policy:high_watermark() + 1;
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
    true = ets:insert(?META_TABLE, {sync_epoch, imboy_cache_sync_proto:epoch()}),
    true = ets:insert(?META_TABLE, {sync_counter, 0}),
    join_syn(),
    ?DEBUG_LOG(["cache sync starting, boot_id", BootId]),
    Deadline = erlang:system_time(millisecond) + imboy_cache_policy:d_max_ms(),
    {ok,
        #state{
            boot_id = BootId,
            status = discovering,
            recover_deadline_ms = Deadline
        },
        0}.

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
%% --- replicate 发布（C03 预算执行）---
handle_call({publish_set, Key, Value, TTL}, _From, State) ->
    case pre_publish(Key) of
        ok ->
            {{C, _} = Rev, Seq, S1} = allocate(State),
            ExpiresAt = erlang:system_time(millisecond) + TTL * 1000,
            _ = depcache:set(Key, Value, TTL, [], ?DEPCACHE_SERVER),
            true = ets:insert(?META_TABLE, {{rev, Key}, {Rev, C, ExpiresAt, live}}),
            ok = index_replicate(Key),
            incr(publish_total),
            _ = publish(
                imboy_cache_sync_proto:event(
                    set, Key, Value, ExpiresAt, Rev, Seq, State#state.boot_id
                )
            ),
            {reply, ok, S1};
        {error, _} = Err ->
            incr(reject_total),
            {reply, Err, State}
    end;
handle_call({publish_delete, Key}, _From, State) ->
    case pre_publish(Key) of
        ok ->
            case tombstone_budget_exceeded() of
                true ->
                    ok = controlled_cold_recovery(),
                    incr(reject_total),
                    {reply, {error, tombstone_budget_exceeded}, State};
                false ->
                    {{C, _} = Rev, Seq, S1} = allocate(State),
                    TombstoneTTL = imboy_cache_policy:tombstone_ttl_sec(),
                    ExpiresAt = erlang:system_time(millisecond) + TombstoneTTL * 1000,
                    _ = depcache:flush(Key, ?DEPCACHE_SERVER),
                    true = ets:insert(
                        ?META_TABLE,
                        {{rev, Key}, {Rev, C, ExpiresAt, deleted}}
                    ),
                    incr(publish_total),
                    _ = publish(
                        imboy_cache_sync_proto:event(
                            delete, Key, undefined, ExpiresAt, Rev, Seq, State#state.boot_id
                        )
                    ),
                    {reply, ok, S1}
            end;
        {error, _} = Err ->
            incr(reject_total),
            {reply, Err, State}
    end;
%% --- 诊断（消息串行化确认）---
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
    %% 单轮恢复窗口超时：有界重试（RETRY_WITH_LIMIT），受 Dmax 硬顶。
    %% 回合卡死检查（C04）：回合 deadline 过期仍未闭合 → abort（有界重试）
    case round_expired(State) of
        true ->
            {noreply, abort_round(deadline_expired, State)};
        false ->
            discover(State#state{retries = State#state.retries + 1})
    end;
%% --- 同步消息（legacy + V2）---
handle_info({cache_sync, Msg}, State) ->
    {noreply, handle_sync_message(Msg, State)};
%% --- 版本上界握手（新协议帧：report 携带 epoch/hash/boot/ready）---
handle_info(
    {cache_version_report, FromNode, MaxCounter, Epoch, ConfigHash, RegistryHash, BootId,
        ReadyFlag},
    State
) ->
    State1 = handle_version_report(
        FromNode,
        MaxCounter,
        Epoch,
        ConfigHash,
        RegistryHash,
        BootId,
        ReadyFlag,
        State
    ),
    {noreply, State1};
%% 旧格式 report（真实旧版本节点，C10 混合矩阵）：hash 不可校验，
%% 不作为 authority；不请求数据
handle_info({cache_version_report, _FromNode, _MaxCounter}, State) ->
    {noreply, State};
%% --- 快照请求（新协议帧：带 Round）---
handle_info(
    {cache_snapshot_request, RequestorPid, Round},
    #state{status = ready} = State
) ->
    _ = imboy_cache_sync_snapshot:send_snapshot(RequestorPid, Round),
    incr(reconcile_total),
    {noreply, State};
handle_info({cache_snapshot_request, RequestorPid, _Round}, State) ->
    %% 非 ready（peer 同在恢复）：回 done（无数据页），请求方回合不闭合
    _ =
        try
            RequestorPid ! {cache_snapshot_done, node(), _Round, 0, 0}
        catch
            _:_ -> ok
        end,
    {noreply, State};
%% --- 快照页/终帧（恢复节点接收）---
handle_info(
    {cache_snapshot_page, FromNode, Round, PageNo, Entries, Digest, HasMore},
    #state{status = recovering, snapshot_round = RoundSt} = State
) when
    is_map(RoundSt)
->
    case
        imboy_cache_sync_snapshot:handle_page(
            FromNode, Round, PageNo, Entries, Digest, HasMore, RoundSt
        )
    of
        {ok, RoundSt1} ->
            {noreply, maybe_complete_round(State#state{snapshot_round = RoundSt1})};
        {abort, Reason, RoundSt1} ->
            {noreply, abort_round(Reason, State#state{snapshot_round = RoundSt1})}
    end;
handle_info(
    {cache_snapshot_page, _FromNode, _Round, _PageNo, _Entries, _Digest, _HasMore},
    State
) ->
    %% 非 recovering 或无回合：迟到页丢弃（有界）
    incr(reject_total),
    {noreply, State};
handle_info(
    {cache_snapshot_done, FromNode, Round, Watermark, _Total},
    #state{status = recovering, snapshot_round = RoundSt} = State
) when
    is_map(RoundSt)
->
    case imboy_cache_sync_snapshot:handle_done(FromNode, Round, Watermark, RoundSt) of
        {ok, RoundSt1} ->
            {noreply, maybe_complete_round(State#state{snapshot_round = RoundSt1})};
        {abort, Reason, RoundSt1} ->
            {noreply, abort_round(Reason, State#state{snapshot_round = RoundSt1})}
    end;
handle_info({cache_snapshot_done, _FromNode, _Round, _Watermark, _Total}, State) ->
    %% 旧格式 done（3 元组）或非恢复期 done：不推进任何回合（C01 第 4 节）
    {noreply, State};
%% --- 版本上界请求（他节点恢复 → 本节点应答，新协议帧）---
handle_info({cache_version_request, FromNode}, State) ->
    _ = [
        P !
            {cache_version_report, node(), State#state.counter, imboy_cache_sync_proto:epoch(),
                imboy_cache_sync_proto:config_hash(), imboy_cache_sync_proto:registry_hash(),
                State#state.boot_id,
                State#state.status =:= ready}
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
        {reject, Reason} ->
            incr(reject_total),
            ?DEBUG_LOG(["cache sync v2 reject", Reason]),
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

%% @doc V2 事件应用（W3 + C02 双端一致性）。gen_server 串行调用即写入串行化。
-spec apply_v2(map(), #state{}) -> {ok, #state{}} | {reject, term()}.
apply_v2(Event, State) ->
    case imboy_cache_sync_proto:validate(Event) of
        ok ->
            Op0 = maps:get(operation, Event),
            case imboy_cache_sync_proto:check_consistency(Event, State#state.boot_id) of
                {error, self_loop} ->
                    %% W3-10：自己发出的广播不重复修改本地状态
                    {ok, State};
                %% C01 第 1 节：invalidate 不参与 epoch 比较（宁多失效）；
                %% 其余双端一致性失败（hash）仍拒绝
                {error, stale_epoch} when Op0 =:= invalidate ->
                    apply_v2_validated(Event, State);
                {error, Reason} ->
                    {reject, Reason};
                {ok, changed} ->
                    %% peer 化身变化（重启）：C05 key 级有界补偿
                    ok = gap_compensation_key(maps:get(key, Event)),
                    apply_v2_validated(Event, State);
                ok ->
                    apply_v2_validated(Event, State)
            end;
        {error, Reason} ->
            {reject, Reason}
    end.

%% @doc 双端一致性通过后的事件应用（C05 化身变化补偿 + op 路由）。
-spec apply_v2_validated(map(), #state{}) -> {ok, #state{}} | {reject, term()}.
apply_v2_validated(Event, State) ->
    incr(receive_total),
    Key = maps:get(key, Event),
    Op = maps:get(operation, Event),
    case Op of
        invalidate ->
            %% invalidate 不参与版本/epoch 比较（宁多失效）
            _ = maybe_bump_fenced(Key),
            _ = depcache:flush(Key, ?DEPCACHE_SERVER),
            {ok, State};
        _ ->
            Seq = maps:get(origin_seq, Event),
            ok = detect_gap(maps:get(origin_node, Event), Seq, Key),
            State1 = merge_clock(maps:get(revision, Event), State),
            apply_v2_op(Op, Key, Event, State1)
    end.

-spec apply_v2_op(set | delete, term(), map(), #state{}) ->
    {ok, #state{}} | {reject, term()}.
apply_v2_op(set, Key, Event, State) ->
    Rev = maps:get(revision, Event),
    case compare_rev(Rev, current_rev(Key)) of
        newer ->
            TTL = remaining_ttl_sec(maps:get(expires_at_ms, Event, 0)),
            case TTL > 0 of
                true ->
                    case index_budget_ok(Key) of
                        true ->
                            _ = depcache:set(
                                Key, maps:get(value, Event), TTL, [], ?DEPCACHE_SERVER
                            ),
                            C = erlang:element(1, Rev),
                            true = ets:insert(
                                ?META_TABLE,
                                {{rev, Key}, {Rev, C, maps:get(expires_at_ms, Event), live}}
                            ),
                            ok = index_replicate(Key),
                            {ok, State};
                        false ->
                            incr(reject_total),
                            {ok, State}
                    end;
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
            case tombstone_budget_exceeded() of
                true ->
                    %% 远端删除在本节点预算满：受控清理后拒绝本条（fail-closed，
                    %% 不部分应用；对端重试/校对补齐）
                    ok = controlled_cold_recovery(),
                    {reject, tombstone_budget_exceeded};
                false ->
                    _ = depcache:flush(Key, ?DEPCACHE_SERVER),
                    TombstoneTTL = imboy_cache_policy:tombstone_ttl_sec(),
                    ExpiresAt = erlang:system_time(millisecond) + TombstoneTTL * 1000,
                    C = erlang:element(1, Rev),
                    true = ets:insert(
                        ?META_TABLE,
                        {{rev, Key}, {Rev, C, ExpiresAt, deleted}}
                    ),
                    {ok, State}
            end;
        equal ->
            {ok, State};
        older ->
            incr(conflict_total),
            {reject, stale_revision}
    end.

%% ===================================================================
%% 恢复与校对（W4 + C04 快照回合）
%% ===================================================================

-spec discover(#state{}) -> {noreply, #state{}}.
discover(#state{retries = R, recover_deadline_ms = Deadline} = State) when
    R > ?RECOVER_MAX_RETRY
->
    case erlang:system_time(millisecond) >= Deadline of
        true ->
            enter_degraded(State);
        false ->
            %% Dmax 未到：继续有界重试（重置轮计数，保持 5s 节奏）
            discover(State#state{retries = 0})
    end;
discover(#state{recover_deadline_ms = Deadline} = State) ->
    case
        is_integer(Deadline) andalso
            erlang:system_time(millisecond) >= Deadline
    of
        true ->
            enter_degraded(State);
        false ->
            discover_peers(State)
    end;
discover(State) ->
    discover_peers(State).

-spec discover_peers(#state{}) -> {noreply, #state{}}.
discover_peers(State) ->
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

-spec enter_degraded(#state{}) -> {noreply, #state{}}.
enter_degraded(State) ->
    ?WARN_LOG(["cache sync recovery retries exhausted, entering DEGRADED"]),
    incr(degraded_total),
    true = ets:insert(?META_TABLE, {status, degraded}),
    {noreply, State#state{status = degraded, snapshot_round = undefined}}.

%% @doc 处理版本上界应答（新协议帧）：
%% 1. 登记 peer 化身（变化 → C05 全量 fenced 补偿）；
%% 2. hash 一致才作为快照源（divergent 排除 authority）；
%% 3. 版本上界并入（既有 5.3）；
%% 4. recovering 时向 ready peer 请求快照（round 回合，C04）。
-spec handle_version_report(
    node(),
    non_neg_integer(),
    non_neg_integer(),
    binary(),
    binary(),
    binary(),
    boolean(),
    #state{}
) ->
    #state{}.
handle_version_report(
    FromNode,
    MaxCounter,
    Epoch,
    ConfigHash,
    RegistryHash,
    BootId,
    _ReadyFlag,
    #state{status = recovering} = State
) ->
    Changed = imboy_cache_sync_proto:register_incarnation(FromNode, BootId),
    changed_compensation(Changed, {peer_epoch, FromNode}),
    true = ets:insert(?META_TABLE, {{peer_epoch, FromNode}, Epoch}),
    State1 = merge_version_ceiling(MaxCounter, State),
    HashOk =
        ConfigHash =:= imboy_cache_sync_proto:config_hash() andalso
            RegistryHash =:= imboy_cache_sync_proto:registry_hash(),
    case HashOk of
        true ->
            %% hash 一致的 peer（ready 发数据快照；同在恢复的 peer 回空
            %% done——三节点同时冷启动时全部空快照闭合是合法路径：
            %% "全部 peer done + 水位合并"三条件仍需满足，C01 第 4 节）
            RoundSt0 = current_or_new_round(State1),
            RoundSt1 = imboy_cache_sync_snapshot:expect_peer(RoundSt0, FromNode),
            _ = [
                P ! {cache_snapshot_request, self(), maps:get(round, RoundSt1)}
             || P <- syn_scope_members(), node(P) =:= FromNode
            ],
            State1#state{snapshot_round = RoundSt1};
        false ->
            %% divergent peer：不请求数据、不闭合回合（G05：不假装同步正常；
            %% 全部 peer divergent → 回合空 → 轮超时重试 → Dmax 后 DEGRADED）
            State1
    end;
handle_version_report(
    FromNode,
    MaxCounter,
    _Epoch,
    _ConfigHash,
    _RegistryHash,
    BootId,
    _ReadyFlag,
    State
) ->
    %% 非 recovering（ready/degraded）：仅并入版本上界与登记化身
    Changed = imboy_cache_sync_proto:register_incarnation(FromNode, BootId),
    changed_compensation(Changed, {peer_epoch, FromNode}),
    merge_version_ceiling(MaxCounter, State).

-spec current_or_new_round(#state{}) -> map().
current_or_new_round(#state{snapshot_round = undefined, recover_deadline_ms = Dl}) ->
    imboy_cache_sync_snapshot:new_round(0, Dl);
current_or_new_round(#state{snapshot_round = RoundSt}) when is_map(RoundSt) ->
    RoundSt;
current_or_new_round(#state{recover_deadline_ms = Dl}) ->
    imboy_cache_sync_snapshot:new_round(0, Dl).

-spec changed_compensation(term(), term()) -> ok.
changed_compensation(changed, Key) ->
    ok = gap_compensation_key(Key);
changed_compensation(registered, _Key) ->
    ok.

%% @doc 回合闭合检查（C04 三条件）：闭合 → 并入全部水位 → READY。
%% 页序悬挂但 done 的 peer 视为失败：有界重试新回合。
-spec maybe_complete_round(#state{}) -> #state{}.
maybe_complete_round(#state{snapshot_round = RoundSt} = State) when
    is_map(RoundSt)
->
    case imboy_cache_sync_snapshot:round_complete(RoundSt) of
        true ->
            State1 = merge_all_watermarks(RoundSt, State),
            become_ready_state(State1#state{snapshot_round = undefined});
        false ->
            State
    end;
maybe_complete_round(State) ->
    State.

-spec merge_all_watermarks(map(), #state{}) -> #state{}.
merge_all_watermarks(#{peers := Peers}, #state{counter = C} = State) ->
    Watermarks = [maps:get(watermark, St, 0) || St <- maps:values(Peers)],
    MaxWm = lists:max([C | Watermarks]),
    true = ets:insert(?META_TABLE, {sync_counter, MaxWm}),
    State#state{counter = MaxWm}.

%% @doc 回合失败：Rmax_rounds 内重试新回合，超限 DEGRADED。
-spec abort_round(term(), #state{}) -> #state{}.
abort_round(Reason, #state{snapshot_round = RoundSt} = State) when is_map(RoundSt) ->
    RoundsUsed = maps:get(rounds_used, RoundSt, 0) + 1,
    ?WARN_LOG(["cache sync snapshot round aborted", Reason, RoundsUsed]),
    incr(snapshot_abort_total),
    case RoundsUsed >= imboy_cache_policy:r_max_rounds() of
        true ->
            {noreply, S1} = enter_degraded(State),
            S1;
        false ->
            Deadline = recover_deadline_of(State),
            State#state{
                snapshot_round =
                    imboy_cache_sync_snapshot:new_round(RoundsUsed, Deadline)
            }
    end;
abort_round(_Reason, State) ->
    State.

-spec recover_deadline_of(#state{}) -> non_neg_integer().
recover_deadline_of(#state{recover_deadline_ms = Dl}) when is_integer(Dl) ->
    Dl;
recover_deadline_of(_) ->
    erlang:system_time(millisecond) + imboy_cache_policy:d_max_ms().

%% 回合是否过期（无回合 = 未过期，走常规恢复轮）。
-spec round_expired(#state{}) -> boolean().
round_expired(#state{snapshot_round = RoundSt}) when is_map(RoundSt) ->
    Deadline = maps:get(deadline_ms, RoundSt, 0),
    erlang:system_time(millisecond) >= Deadline;
round_expired(_) ->
    false.

-spec become_ready(#state{}) -> {noreply, #state{}}.
become_ready(State) ->
    {noreply, become_ready_state(State)}.

-spec become_ready_state(#state{}) -> #state{}.
become_ready_state(State) ->
    _ = erlang:send_after(?RECONCILE_INTERVAL_MS, self(), reconcile_tick),
    true = ets:insert(?META_TABLE, {status, ready}),
    %% 冷恢复完成：epoch 取 max(本 epoch, 全部已知 peer epoch)（C01 第 3 节）
    PeerEpochs = [E || {{peer_epoch, _N}, E} <- ets:tab2list(?META_TABLE), is_integer(E)],
    MaxEpoch = lists:max([imboy_cache_sync_proto:epoch() | PeerEpochs]),
    case MaxEpoch > imboy_cache_sync_proto:epoch() of
        true -> _ = imboy_cache_sync_proto:bump_epoch();
        false -> ok
    end,
    State#state{status = ready, retries = 0, snapshot_round = undefined}.

-spec merge_version_ceiling(non_neg_integer(), #state{}) -> #state{}.
merge_version_ceiling(MaxCounter, #state{counter = C} = State) ->
    State#state{counter = erlang:max(C, MaxCounter)}.

%% @doc 定期轻量校对（9.4）：交换版本摘要上界 + 有界清理（W4-06/W4-07）
%% + 宽限化身淘汰（C02）+ rindex 计数校准（C03）。
-spec periodic_reconcile(#state{}) -> #state{}.
periodic_reconcile(#state{status = ready} = State) ->
    incr(reconcile_total),
    Members = [P || P <- syn_scope_members(), node(P) =/= node()],
    _ = [P ! {cache_version_request, node()} || P <- Members],
    ok = bounded_sweep(),
    ok = imboy_cache_sync_proto:prune_grace(erlang:system_time(millisecond)),
    ok = recalibrate_rindex_count(),
    State;
periodic_reconcile(State) ->
    State.

%% @doc 有界清理（W4-06/W4-07）：过期墓碑/过期 live 淘汰 + rindex 连带。
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
%% 不连带清理则随复制写入无界增长（W4-06/07）
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
%% C03 资源预算执行
%% ===================================================================

%% @doc rindex 计数（近似 + 周期校准，写路径 O(1)）。
-spec index_budget_ok(term()) -> boolean().
index_budget_ok(Key) ->
    case ets:member(?META_TABLE, {rindex, Key}) of
        true ->
            true;
        false ->
            rindex_count() < imboy_cache_policy:n_max()
    end.

-spec rindex_count() -> non_neg_integer().
rindex_count() ->
    case ets:lookup(?META_TABLE, rindex_count) of
        [{_, N}] when is_integer(N) -> N;
        [] -> 0
    end.

-spec recalibrate_rindex_count() -> ok.
recalibrate_rindex_count() ->
    N = ets:select_count(
        ?META_TABLE,
        [{{{'$1', '$2'}, '_'}, [{'==', '$1', rindex}], [true]}]
    ),
    true = ets:insert(?META_TABLE, {rindex_count, N}),
    ok.

%% @doc 墓碑预算：精确统计 deleted rev 行（删除为低频路径，select_count
%% 可接受）；满 → 先过期淘汰，仍满 → 受控冷恢复（C01 第 7 节，不删
%% tombstone 后接受旧事件）。
-spec tombstone_budget_exceeded() -> boolean().
tombstone_budget_exceeded() ->
    tombstone_count() >= imboy_cache_policy:t_max() andalso
        begin
            ok = bounded_sweep(),
            tombstone_count() >= imboy_cache_policy:t_max()
        end.

-spec tombstone_count() -> non_neg_integer().
tombstone_count() ->
    ets:select_count(
        ?META_TABLE,
        [{{{rev, '_'}, {'_', '_', '_', deleted}}, [], [true]}]
    ).

%% @doc 受控冷恢复（C01 第 3/7 节）：推进 epoch、丢弃全部受管复制副本
%% （rindex/rev），后续走新快照回合从 authority 回填。
-spec controlled_cold_recovery() -> ok.
controlled_cold_recovery() ->
    _ = imboy_cache_sync_proto:bump_epoch(),
    _ = [
        ets:delete(?META_TABLE, K)
     || {K, _V} <- ets:tab2list(?META_TABLE),
        (is_tuple(K) andalso tuple_size(K) =:= 2 andalso
            (element(1, K) =:= rev orelse element(1, K) =:= rindex))
    ],
    true = ets:insert(?META_TABLE, {rindex_count, 0}),
    incr(cold_recovery_total),
    ok.

%% ===================================================================
%% C05 invalidate 缺口补偿
%% ===================================================================

%% @doc origin_seq 缺口检测（W4-05 + C05 增强）：跳号计数 + 该 namespace
%% 若为受管 fenced 域，立即推进 generation（有界清理，旧物理键不可读，
%% 新请求回权威，不依赖旧 TTL 自然到期）。
-spec detect_gap(node(), non_neg_integer(), term()) -> ok.
detect_gap(Origin, Seq, Key) ->
    SeqKey = {seq, Origin},
    Last =
        case ets:lookup(?META_TABLE, SeqKey) of
            [{_, L}] -> L;
            [] -> 0
        end,
    case Seq > Last + 1 of
        true ->
            incr(gap_detected_total),
            ok = gap_compensation(Key);
        false ->
            ok
    end,
    _ = ets:insert(?META_TABLE, {SeqKey, erlang:max(Last, Seq)}),
    ok.

%% @doc 单 namespace 补偿：fenced 域推进 generation（非 fenced 保持 legacy
%% 合同，TTL 兜底——受管 = 显式配置的 fenced namespace）。
-spec gap_compensation(term()) -> ok.
gap_compensation(Key) ->
    case is_fenced(Key) of
        true ->
            G = bump_generation(Key),
            ok = flush_physical(Key, G - 1);
        false ->
            ok
    end.

%% @doc 化身变化/重连补偿（C05，key 级落地）：对化身变化事件携带的
%% Key 推进 key 级 generation（19.3 物理键读路径即 key 级——补偿必须
%% 同粒度才可被读取路径感知）。
%% 局限（记录）：无法枚举未观察到的同源 Key，未覆盖 Key 依赖 TTL
%% 兜底或权威回源；namespace 级扫荡需 fenced 枚举机制（后续演进），
%% 本轮按"补偿检测到的受影响 Key"交付（C05 oracle 场景成立）。
-spec gap_compensation_key(term()) -> ok.
gap_compensation_key(Key) ->
    case is_fenced(Key) of
        true ->
            G = bump_generation(Key),
            ok = flush_physical(Key, G - 1);
        false ->
            ok
    end.

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
            %% revision/seq 占位仅满足消息结构校验。
            _ = publish(#{
                protocol => 2,
                operation => invalidate,
                namespace => imboy_cache_policy:namespace_of(Key),
                key => Key,
                revision => {0, node()},
                origin_seq => 0,
                origin_node => node(),
                origin_boot_id => boot(),
                epoch => imboy_cache_sync_proto:epoch(),
                config_hash => imboy_cache_sync_proto:config_hash(),
                registry_hash => imboy_cache_sync_proto:registry_hash()
            }),
            %% 19.8 能力交集：旧节点静默丢弃 v2 信封，混合期失效必须同时
            %% 走 legacy {flush, Key} 通道；v2 接收端按保守栅栏处理该消息，
            %% 回流自环幂等，重复失效是安全方向（宁多失效）。
            _ = broadcast({flush, Key}),
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
    imboy_cache_policy:is_fenced(Key).

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
    true = ets:insert(?META_TABLE, {sync_counter, C1}),
    {{C1, node()}, S1, State#state{counter = C1, origin_seq = S1}}.

-spec merge_clock({non_neg_integer(), node()}, #state{}) -> #state{}.
merge_clock({RemoteC, _Node}, #state{counter = C} = State) ->
    true = ets:insert(?META_TABLE, {sync_counter, erlang:max(C, RemoteC)}),
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

-spec publish(map()) -> {ok, non_neg_integer()}.
publish(Event) ->
    try syn:publish(?CACHE_SCOPE, ?CACHE_GROUP_NAME, {cache_sync, Event}) of
        R -> R
    catch
        _:_ -> {ok, 0}
    end.

%% @doc 发布前置检查：READY + 队列高水位 + Nmax 预算（fail-closed）。
-spec pre_publish(term()) -> ok | {error, term()}.
pre_publish(Key) ->
    case status() of
        St when St =/= ready ->
            {error, {not_ready, St}};
        ready ->
            case queue_depth() > imboy_cache_policy:high_watermark() of
                true ->
                    {error, overloaded};
                false ->
                    case index_budget_ok(Key) of
                        true -> ok;
                        false -> {error, index_full}
                    end
            end
    end.

-spec index_replicate(term()) -> ok.
index_replicate(Key) ->
    New = not ets:member(?META_TABLE, {rindex, Key}),
    _ = ets:insert(?META_TABLE, {{rindex, Key}, erlang:system_time(millisecond)}),
    case New of
        true ->
            N = rindex_count(),
            true = ets:insert(?META_TABLE, {rindex_count, N + 1});
        false ->
            ok
    end,
    ok.

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

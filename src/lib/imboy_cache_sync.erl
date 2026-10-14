%% @doc 分布式缓存单一写入者：串行处理版本、generation、复制和恢复。
%% 协议/预算/恢复合同见 docs/architecture/cache-v2.2.1/closeout-v1/protocol-budget-frozen.md。
-module(imboy_cache_sync).
-moduledoc "分布式缓存同步：V2 协议串行应用、generation 栅栏、恢复状态机、资源预算与缺口补偿。".
-behaviour(gen_server).
-include("cache.hrl").
-include("log.hrl").
-include("chat.hrl").
-include("imboy_cache_sync_state.hrl").
-define(CACHE_GROUP_NAME, dsync_handler).
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
%% 同步族内部合同（imboy_cache_sync_apply / imboy_cache_sync_recover 消费；
%% 例外 4 拆分引入，非公共 API，命名与语义随同步族整体演进）
-export([
    incr/1,
    is_fenced/1,
    maybe_bump_fenced/1,
    bump_generation/1,
    bump_all_fenced/0,
    flush_physical/2,
    merge_clock/2,
    index_replicate/1,
    syn_scope_members/0,
    replication_capable/0,
    send_version_report/2
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
%% ===================================================================
%% API
%% ===================================================================
-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%% @doc legacy SET 发布与接收共用策略边界；flush 保留混合版本兼容。
-spec broadcast(term()) -> {ok, non_neg_integer()} | {error, policy_denied | incompatible_peers}.
broadcast({set, Key, _, _, _} = Message) ->
    case legacy_set_allowed(Key) of
        true ->
            publish(Message);
        false ->
            incr(reject_total),
            {error, policy_denied}
    end;
broadcast(#{protocol := 2, operation := Op} = Message) when Op =:= set; Op =:= delete ->
    case replication_capable() of
        true -> publish(Message);
        false -> {error, incompatible_peers}
    end;
broadcast(Message) ->
    publish(Message).
legacy_set_allowed(Key) ->
    imboy_cache_policy:policy(Key) =:= legacy andalso not is_replicated(Key).
%% @doc 当前失效域 generation（读 ETS，不经进程；缺省 0）。
-spec generation(term()) -> non_neg_integer().
generation(Domain) ->
    Local =
        case ets:lookup(?META_TABLE, {gen, Domain}) of
            [{_, G}] when is_integer(G) -> G;
            [] -> 0
        end,
    Floor =
        case ets:lookup(?META_TABLE, fence_floor) of
            [{_, F}] -> F;
            [] -> 0
        end,
    erlang:max(Local, Floor).
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
        imboy_cache_sync_apply:metadata_snapshot()
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

%% @doc 本地受管 Key；DELETE-only 版本行也必须保持读写边界。
-spec is_replicated(term()) -> boolean().
is_replicated(Key) ->
    try
        ets:member(?META_TABLE, {rindex, Key}) orelse ets:member(?META_TABLE, {rev, Key})
    catch
        _:_ -> false
    end.

%% @doc replicate Key 的带版本读：墓碑/未 READY 时不得返回未校验旧值（5.5/9.1）。
-spec replicate_state(Key :: term()) -> live | deleted | unverified.
replicate_state(Key) ->
    try ets:lookup(?META_TABLE, {rev, Key}) of
        [{{rev, Key}, {_Rev, _C, _Exp, deleted, _Epoch}}] ->
            deleted;
        [{{rev, Key}, {_Rev, _C, Exp, live, Epoch}}] ->
            case
                is_integer(Exp) andalso Epoch =:= imboy_cache_sync_proto:epoch() andalso
                    status() =:= ready andalso
                    Exp > erlang:system_time(millisecond)
            of
                true -> live;
                false -> unverified
            end;
        [] ->
            %% 无版本记录：可能是本节点未同步的副本，视同未校验
            unverified
    catch
        error:badarg -> unverified
    end.

%% gen_server callbacks

-spec init([]) -> {ok, #state{}}.
init([]) ->
    BootId = crypto:strong_rand_bytes(16),
    ?META_TABLE = ets:new(?META_TABLE, [
        named_table, public, set, {read_concurrency, true}
    ]),
    true = ets:insert(?META_TABLE, {node_boot, {node(), BootId}}),
    true = ets:insert(?META_TABLE, {status, discovering}),
    true = ets:insert(?META_TABLE, {sync_epoch, imboy_cache_sync_proto:epoch()}),
    true = ets:insert(?META_TABLE, [{sync_counter, 0}, {sync_origin_seq, 0}]),
    _ = net_kernel:monitor_nodes(true),
    join_syn(),
    ?DEBUG_LOG(["cache sync starting, boot_id", BootId]),
    Deadline = erlang:system_time(millisecond) + imboy_cache_policy:d_max_ms(),
    {ok,
        imboy_cache_sync_recover:schedule_recovery(
            #state{
                boot_id = BootId,
                status = discovering,
                recover_deadline_ms = Deadline
            },
            0
        )}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.
%% --- generation 栅栏 ---
handle_call({next_generation, Domain}, _From, State) ->
    Old = generation(Domain),
    G = bump_generation(Domain),
    ok = flush_physical(Domain, Old),
    {reply, G, State};
handle_call({next_dep_generation, Tag}, _From, State) ->
    {reply, bump_dependency(Tag), State};
%% --- 统一失效 ---
handle_call({invalidate, Key}, _From, State) ->
    ok = do_invalidate(Key),
    {reply, ok,
        imboy_cache_sync_recover:abort_round(snapshot_invalidated, publish_invalidate(Key, State))};
%% --- replicate 发布（C03 预算执行）---
handle_call({publish_set, Key, Value, TTL}, _From, State) ->
    case imboy_cache_sync_apply:pre_publish(set, Key, Value, TTL, State) of
        ok ->
            {Rev, Seq, S1} = allocate(State),
            ExpiresAt = erlang:system_time(millisecond) + TTL * 1000,
            _ = depcache:set(Key, Value, TTL, [], ?DEPCACHE_SERVER),
            ok = imboy_cache_sync_apply:store_revision(
                Key,
                Rev,
                ExpiresAt,
                live,
                imboy_cache_sync_proto:epoch(),
                {node(), State#state.boot_id, Seq}
            ),
            ok = index_replicate(Key),
            incr(publish_total),
            _ = publish(
                imboy_cache_sync_proto:event(
                    set, Key, Value, ExpiresAt, Rev, Seq, State#state.boot_id
                )
            ),
            {reply, ok, S1};
        {error, Reason} = Err ->
            _ = imboy_cache_sync_proto:record_reject(Reason),
            {reply, Err, State}
    end;
handle_call({publish_delete, Key}, _From, State) ->
    case imboy_cache_sync_apply:pre_publish(delete, Key, undefined, 0, State) of
        ok ->
            case
                imboy_cache_sync_apply:tombstone_budget_exceeded(
                    Key,
                    {State#state.counter + 1, node()},
                    erlang:system_time(millisecond) + imboy_cache_policy:tombstone_ttl_sec() * 1000,
                    imboy_cache_sync_proto:epoch(),
                    {node(), State#state.boot_id, State#state.origin_seq + 1}
                )
            of
                true ->
                    S1 = imboy_cache_sync_recover:start_cold_recovery(State),
                    _ = imboy_cache_sync_proto:record_reject(tombstone_budget_exceeded),
                    {reply, {error, tombstone_budget_exceeded}, S1};
                false ->
                    {Rev, Seq, S1} = allocate(State),
                    TombstoneTTL = imboy_cache_policy:tombstone_ttl_sec(),
                    ExpiresAt = erlang:system_time(millisecond) + TombstoneTTL * 1000,
                    _ = depcache:flush(Key, ?DEPCACHE_SERVER),
                    ok = imboy_cache_sync_apply:store_revision(
                        Key,
                        Rev,
                        ExpiresAt,
                        deleted,
                        imboy_cache_sync_proto:epoch(),
                        {node(), State#state.boot_id, Seq}
                    ),
                    incr(publish_total),
                    _ = publish(
                        imboy_cache_sync_proto:event(
                            delete, Key, undefined, ExpiresAt, Rev, Seq, State#state.boot_id
                        )
                    ),
                    {reply, ok, S1}
            end;
        {error, Reason} = Err ->
            _ = imboy_cache_sync_proto:record_reject(Reason),
            {reply, Err, State}
    end;
%% --- 诊断（消息串行化确认）---
handle_call(ping_ok, _From, State) ->
    {reply, pong, State};
handle_call(benchmark_reset_snapshot_page_peak, _From, State) ->
    true = ets:delete(?META_TABLE, snapshot_page_peak),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
%% --- 恢复状态机 ---
handle_info({timeout, Ref, cache_recovery}, #state{recovery_timer = Ref} = State) ->
    imboy_cache_sync_recover:handle_timeout(State#state{recovery_timer = undefined});
handle_info({timeout, _Ref, cache_recovery}, State) ->
    {noreply, State};
%% --- 同步消息（legacy + V2）---
handle_info({cache_sync, Msg}, State) ->
    {noreply, handle_sync_message(Msg, State)};
handle_info({nodedown, Node}, State) ->
    case imboy_cache_sync_coverage:observe_node_down(Node) of
        cold -> {noreply, imboy_cache_sync_recover:start_cold_recovery(State)};
        continue -> {noreply, State};
        invalid -> imboy_cache_sync_recover:reject_membership(State)
    end;
%% 周期握手附带来源序列水位；末尾事件丢失也能在校对时补偿。
handle_info(
    {cache_invalidation_watermark, _, _, _, _, _} = Frame, State
) ->
    {noreply, imboy_cache_sync_recover:handle_watermark(Frame, State)};
handle_info({cache_snapshot_cut, _, _, _, _, _, _, _, _, _} = Frame, State) ->
    {noreply, imboy_cache_sync_recover:handle_snapshot_cut(Frame, State)};
handle_info({cache_snapshot_cut, _, _, _, _, _, _, _, _} = Frame, State) ->
    {noreply, imboy_cache_sync_recover:handle_snapshot_cut(Frame, State)};
handle_info({cache_snapshot_cut, _, _, _, _, _, _, _} = Frame, State) ->
    {noreply, imboy_cache_sync_recover:handle_snapshot_cut(Frame, State)};
%% --- 版本上界握手（新协议帧：report 携带 epoch/hash/boot/ready + 发送者 pid）---
handle_info(Report, #state{status = Status, snapshot_round = R} = State) when
    is_tuple(Report),
    tuple_size(Report) >= 9,
    tuple_size(Report) =< 12,
    element(1, Report) =:= cache_version_report
->
    {cache_version_report, FromNode, _, _, _, _, _, _, FromPid} =
        imboy_cache_policy:version_report_base(Report),
    case
        imboy_cache_sync_proto:valid_version_report(Report) andalso
            imboy_cache_policy:version_report_round_matches(Report, Status, R) andalso
            version_report_peer(FromNode, FromPid, State)
    of
        false ->
            incr(reject_total),
            {noreply, State};
        true ->
            State1 = imboy_cache_sync_proto:handle_valid_report(Report, State),
            {noreply, State1}
    end;
%% 旧格式 report（真实旧版本节点，C10 混合矩阵）：hash 不可校验，
%% 不作为 authority；不请求数据
handle_info({cache_version_report, _FromNode, _MaxCounter}, State) ->
    {noreply, State};
%% --- 快照请求（新协议帧：带 Round）---
handle_info(
    {cache_snapshot_request, RequestorPid, Round},
    #state{status = ready} = State
) when is_pid(RequestorPid), is_binary(Round), byte_size(Round) =:= 12 ->
    _ = imboy_cache_sync_snapshot:send_snapshot(RequestorPid, Round),
    incr(reconcile_total),
    {noreply, State};
handle_info({cache_snapshot_request, RequestorPid, Round}, State) when
    is_pid(RequestorPid), is_binary(Round), byte_size(Round) =:= 12
->
    %% 握手后源可能已离开 READY；显式中止，不能以空 done 充当权威。
    _ =
        try
            RequestorPid ! {cache_snapshot_abort, node(), Round, source_not_ready}
        catch
            _:_ -> ok
        end,
    {noreply, State};
handle_info({cache_snapshot_request, _Requestor, _Round}, State) ->
    incr(reject_total),
    {noreply, State};
handle_info({cache_snapshot_pull, Pid, Round, PageNo}, #state{status = ready} = State) when
    is_pid(Pid),
    is_binary(Round),
    byte_size(Round) =:= 12,
    is_integer(PageNo),
    PageNo > 1
->
    case imboy_cache_sync_proto:within_meta_budget({Pid, Round, PageNo}) of
        true -> imboy_cache_sync_snapshot:continue_snapshot(Pid, Round, PageNo);
        false -> incr(reject_total)
    end,
    {noreply, State};
handle_info({cache_snapshot_pull, _, _, _}, State) ->
    incr(reject_total),
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
        {ok, RoundSt} ->
            %% 未知来源、旧回合或已完成来源的迟到页没有接受新版本。
            {noreply, State};
        {ok, RoundSt1} ->
            %% 已接受的页版本必须立即并入时钟；随后回合中止也不得回退。
            ok = imboy_cache_sync_recover:pull_next_page(FromNode, PageNo, RoundSt1),
            Counter = imboy_cache_sync_snapshot:peer_revision_ceiling(RoundSt1, FromNode),
            State1 = merge_clock({Counter, FromNode}, State),
            {noreply,
                imboy_cache_sync_recover:maybe_complete_round(State1#state{
                    snapshot_round = RoundSt1
                })};
        {abort, Reason, RoundSt1} ->
            Counter = imboy_cache_sync_snapshot:peer_revision_ceiling(RoundSt1, FromNode),
            State1 = merge_clock({Counter, FromNode}, State),
            {noreply,
                imboy_cache_sync_recover:abort_round(Reason, State1#state{snapshot_round = RoundSt1})}
    end;
handle_info(
    {cache_snapshot_page, _FromNode, _Round, _PageNo, _Entries, _Digest, _HasMore},
    State
) ->
    %% 非 recovering 或无回合：迟到页丢弃（有界）
    incr(reject_total),
    {noreply, State};
handle_info(
    {cache_snapshot_done, FromNode, Round, Watermark, Total},
    #state{status = recovering, snapshot_round = RoundSt} = State
) when
    is_map(RoundSt)
->
    case imboy_cache_sync_snapshot:handle_done(FromNode, Round, Watermark, Total, RoundSt) of
        {ok, RoundSt1} ->
            {noreply,
                imboy_cache_sync_recover:maybe_complete_round(State#state{snapshot_round = RoundSt1})};
        {abort, Reason, RoundSt1} ->
            {noreply,
                imboy_cache_sync_recover:abort_round(Reason, State#state{snapshot_round = RoundSt1})}
    end;
handle_info(
    {cache_snapshot_abort, FromNode, Round, Reason},
    #state{status = recovering, snapshot_round = RoundSt} = State
) when is_map(RoundSt) ->
    case
        maps:get(round, RoundSt) =:= Round andalso
            maps:is_key(FromNode, maps:get(peers, RoundSt))
    of
        true ->
            {noreply, imboy_cache_sync_recover:abort_round(Reason, State)};
        false ->
            incr(reject_total),
            {noreply, State}
    end;
handle_info({cache_snapshot_done, _FromNode, _Round, _Watermark, _Total}, State) ->
    %% 旧格式 done（3 元组）或非恢复期 done：不推进任何回合（C01 第 4 节）
    {noreply, State};
%% --- 版本上界请求（他节点恢复 → 本节点应答）---
%% 新协议帧（r2）：请求自带请求者 pid，应答直达该 pid。恢复节点刚 restart_sync
%% 换 pid 时，syn 视图传播滞后会让视图反查漏发或把应答发向陈旧死 pid，
%% 恢复轮被系统性饿死（C09 F5 三节点实证）；pid 直达消除该竞态。
handle_info({cache_version_request, FromPid, _FromNode, Round}, State) when
    is_pid(FromPid), is_binary(Round), byte_size(Round) =:= 12
->
    send_version_report(FromPid, State, Round),
    {noreply, State};
handle_info({cache_version_request, FromPid, _FromNode}, State) when is_pid(FromPid) ->
    send_version_report(FromPid, State),
    {noreply, State};
%% 旧格式请求（无请求者 pid）：保持视图反查应答（C10 混合矩阵旧节点）
handle_info({cache_version_request, FromNode}, State) ->
    _ = [
        send_version_report(P, State)
     || P <- syn_scope_members(), node(P) =:= FromNode
    ],
    {noreply, State};
handle_info(reconcile_tick, State) ->
    {noreply, imboy_cache_sync_recover:reconcile_tick(reconcile_tick, State)};
handle_info({timeout, Ref, reconcile_tick}, State) ->
    {noreply, imboy_cache_sync_recover:reconcile_tick(Ref, State)};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    try
        _ = syn:leave(?CACHE_SCOPE, ?CACHE_GROUP_NAME, self()),
        ok
    catch
        _:_ -> ok
    end.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec handle_sync_message(term(), #state{}) -> #state{}.
handle_sync_message(#{protocol := 2} = Event, State) ->
    case imboy_cache_sync_apply:apply_v2(Event, State) of
        {ok, S1} ->
            S1;
        {reject, Reason, S1} ->
            _ = imboy_cache_sync_proto:record_reject(Reason),
            ?DEBUG_LOG(["cache sync v2 reject", Reason]),
            S1;
        {reject, Reason} ->
            _ = imboy_cache_sync_proto:record_reject(Reason),
            ?DEBUG_LOG(["cache sync v2 reject", Reason]),
            State
    end;
handle_sync_message(#{protocol := _}, State) ->
    _ = imboy_cache_sync_proto:record_reject(invalid_protocol),
    State;
handle_sync_message({legacy_invalidation_seq, _, _, _, _, _, _} = Frame, State) ->
    imboy_cache_sync_apply:apply_legacy_sequence(Frame, State);
handle_sync_message({set, Key, Data, MaxAge, Depend}, State) ->
    incr(cache_sync_legacy_receive_total),
    %% 旧 SET 仅兼容未分类键，不能绕过本地策略或版本化写入边界。
    _ =
        case legacy_set_allowed(Key) of
            true ->
                incr(receive_total),
                depcache:set(Key, Data, MaxAge, Depend, ?DEPCACHE_SERVER);
            false ->
                imboy_cache_sync_proto:record_reject(legacy_policy)
        end,
    State;
handle_sync_message({flush, Key}, State) ->
    incr(cache_sync_legacy_receive_total),
    incr(receive_total),
    ok = imboy_cache_sync_apply:abort_snapshot_senders(snapshot_invalidated),
    _ = depcache:flush(Key, ?DEPCACHE_SERVER),
    %% 保守栅栏：legacy flush 到达时推进 fenced 域 generation（混合版本宁可多失效）
    _ = maybe_bump_fenced(Key),
    imboy_cache_sync_recover:abort_round(snapshot_invalidated, State);
handle_sync_message(flush, State) ->
    incr(cache_sync_legacy_receive_total),
    incr(receive_total),
    ok = imboy_cache_sync_apply:abort_snapshot_senders(snapshot_invalidated),
    _ = depcache:flush(?DEPCACHE_SERVER),
    ok = bump_all_fenced(),
    imboy_cache_sync_recover:abort_round(snapshot_invalidated, State);
handle_sync_message(_, State) ->
    %% 未知消息静默丢弃 + 计数（19.8：可观测，未来升级为告警）
    incr(unknown_total),
    State.

version_report_peer(FromNode, FromPid, #state{
    status = recovering, snapshot_round = #{peers := Peers}
}) ->
    maps:is_key(FromNode, Peers) andalso lists:member(FromPid, syn_scope_members());
version_report_peer(_FromNode, _FromPid, #state{status = recovering}) ->
    false;
version_report_peer(FromNode, FromPid, _State) ->
    FromNode =/= node() andalso
        lists:member(FromPid, syn_scope_members()).

%% 周期报告无回合；恢复应答回显请求回合和实际来源序列。
send_version_report(Pid, State) -> send_version_report(Pid, State, undefined).
send_version_report(Pid, State, Round) ->
    imboy_cache_sync_proto:send_version_report(
        Pid,
        State#state.counter,
        State#state.boot_id,
        State#state.origin_seq,
        State#state.status =:= ready,
        Round
    ).

join_syn() ->
    try
        syn:join(?CACHE_SCOPE, ?CACHE_GROUP_NAME, self(), #{
            cache_protocol => 2, cache_boot => element(2, node_boot())
        })
    of
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
    G = next_fence_clock(),
    Row = {{gen, Domain}, G},
    Added =
        case ets:lookup(?META_TABLE, {gen, Domain}) of
            [Previous] -> erlang:external_size(Row) - erlang:external_size(Previous);
            [] -> erlang:external_size(Row)
        end,
    New = not ets:member(?META_TABLE, {gen, Domain}),
    Count =
        metadata_counter(gen_count) +
            case New of
                true -> 1;
                false -> 0
            end,
    Bytes = metadata_counter(gen_bytes) + Added,
    case Count =< imboy_cache_policy:n_max() andalso Bytes =< imboy_cache_policy:gen_max_bytes() of
        true ->
            ets:insert(?META_TABLE, [Row, {gen_count, Count}, {gen_bytes, Bytes}]);
        false ->
            %% 新底座大于所有旧代次，先使旧值不可读，再删除索引；不得复用旧代次。
            true = ets:insert(?META_TABLE, {fence_floor, G}),
            ets:match_delete(?META_TABLE, {{gen, '_'}, '_'}),
            ets:insert(?META_TABLE, [{gen_count, 0}, {gen_bytes, 0}])
    end,
    G.

metadata_counter(Key) ->
    case ets:lookup(?META_TABLE, Key) of
        [{_, N}] -> N;
        [] -> 0
    end.

next_fence_clock() ->
    ets:update_counter(?META_TABLE, fence_clock, 1, {fence_clock, 0}).

flush_physical(_Domain, G) when G < 0 ->
    ok;
flush_physical(Domain, G) ->
    _ = depcache:flush(phys_key(Domain, G), ?DEPCACHE_SERVER),
    ok.

-spec phys_key(term(), non_neg_integer()) -> term().
phys_key(LogicalKey, G) ->
    {cache_v2, LogicalKey, {node_boot(), G}}.

-spec phys_dep(term(), non_neg_integer()) -> term().
phys_dep(Tag, G) ->
    {cache_v2_dep, Tag, {node_boot(), G}}.

%% @doc 统一失效：先推进 gen（19.3），再清旧物理键与逻辑键，最后跨节点发布。
do_invalidate(Key) ->
    ok = imboy_cache_sync_apply:abort_snapshot_senders(snapshot_invalidated),
    ok = maybe_bump_fenced(Key),
    depcache:flush(Key, ?DEPCACHE_SERVER).

-spec publish_invalidate(term(), #state{}) -> #state{}.
publish_invalidate(Key, State) ->
    {Rev, Seq, NextState} = allocate(State),
    case imboy_cache_policy:protocol() of
        v2 ->
            incr(publish_total),
            %% 三种事件共享来源序列；invalidate 不比较值版本，但不能漏记缺口。
            _ = publish(
                imboy_cache_sync_proto:event(
                    invalidate, Key, undefined, 0, Rev, Seq, State#state.boot_id
                )
            ),
            %% 旧节点静默丢弃 v2 信封；兼容帧只发给未声明 V2 的成员，
            %% 避免新节点重复推进 generation，并避免本节点自回流触发重读。
            _ = publish_legacy_flush(Key, v2),
            NextState;
        legacy ->
            %% legacy 模式跨节点失效用现有 {flush, Key}（19.8 能力交集）
            _ = publish_legacy_flush(Key, legacy),
            _ = broadcast(
                {legacy_invalidation_seq, node(), State#state.boot_id, Seq,
                    imboy_cache_sync_proto:config_hash(), imboy_cache_sync_proto:registry_hash(),
                    Key}
            ),
            NextState
    end.

is_fenced(Key) ->
    imboy_cache_policy:is_fenced(Key).

maybe_bump_fenced(Key) ->
    _ = bump_dependency(Key),
    case is_fenced(Key) of
        true ->
            Old = generation(Key),
            _ = bump_generation(Key),
            ok = flush_physical(Key, Old);
        false ->
            ok
    end.

bump_dependency(Tag) ->
    Old = dep_generation(Tag),
    G = bump_generation({dep, Tag}),
    _ = depcache:flush(phys_dep(Tag, Old), ?DEPCACHE_SERVER),
    G.

%% @doc 缺口/整库 flush 推进统一栅栏底座，O(1) 使全部受管物理键不可读。
%% 包括从未发生单键失效、尚在首次加载中的 Key；旧物理键由 depcache TTL 回收。
-spec bump_all_fenced() -> ok.
bump_all_fenced() ->
    true = ets:insert(?META_TABLE, {fence_floor, next_fence_clock()}),
    ok.

%% --- 版本与发布 ---

-spec allocate(#state{}) ->
    {{non_neg_integer(), node()}, non_neg_integer(), #state{}}.
allocate(#state{counter = C, origin_seq = S} = State) ->
    C1 = C + 1,
    S1 = S + 1,
    true = ets:insert(?META_TABLE, [{sync_counter, C1}, {sync_origin_seq, S1}]),
    {{C1, node()}, S1, State#state{counter = C1, origin_seq = S1}}.

merge_clock({RemoteC, _Node}, #state{counter = C} = State) ->
    true = ets:insert(?META_TABLE, {sync_counter, erlang:max(C, RemoteC)}),
    State#state{counter = erlang:max(C, RemoteC)}.
-spec current_rev(term()) -> {non_neg_integer(), node()} | undefined.
current_rev(Key) ->
    case ets:lookup(?META_TABLE, {rev, Key}) of
        [{{rev, Key}, {Rev, _C, _Exp, _St, _Epoch}}] -> Rev;
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
remaining_ttl_sec(ExpiresAtMs) -> imboy_cache_policy:remaining_ttl_sec(ExpiresAtMs).

publish(Event) ->
    try syn:publish(?CACHE_SCOPE, ?CACHE_GROUP_NAME, {cache_sync, Event}) of
        R -> R
    catch
        _:_ -> {ok, 0}
    end.

publish_legacy_flush(Key, Protocol) ->
    try syn:members(?CACHE_SCOPE, ?CACHE_GROUP_NAME) of
        Members when is_list(Members) ->
            Targets = [
                Pid
             || {Pid, Meta} <- Members,
                Pid =/= self(),
                Protocol =:= legacy orelse not member_uses_v2(Meta)
            ],
            lists:foreach(fun(Pid) -> Pid ! {cache_sync, {flush, Key}} end, Targets),
            {ok, length(Targets)}
    catch
        _:_ -> {ok, 0}
    end.

member_uses_v2(Meta) when is_map(Meta) ->
    maps:get(cache_protocol, Meta, undefined) =:= 2;
member_uses_v2(_) ->
    false.

-spec index_replicate(term()) -> ok.
index_replicate(Key) -> imboy_cache_sync_apply:index_replicate(Key).

syn_scope_members() ->
    try syn:members(?CACHE_SCOPE, ?CACHE_GROUP_NAME) of
        L when is_list(L) -> [P || {P, _Meta} <- L]
    catch
        _:_ -> []
    end.

replication_capable() ->
    try
        Members = syn:members(?CACHE_SCOPE, ?CACHE_GROUP_NAME),
        Nodes = [node(Pid) || {Pid, _} <- Members],
        Expected = application:get_env(imboy, cluster_nodes, []),
        lists:any(
            fun({P, M}) ->
                P =:= whereis(?MODULE) andalso maps:get(cache_protocol, M, undefined) =:= 2
            end,
            Members
        ) andalso
            length(Members) =< imboy_cache_policy:peer_max() + 1 andalso
            length(Expected) =< imboy_cache_policy:peer_max() + 1 andalso
            lists:all(fun(N) -> is_atom(N) andalso lists:member(N, Nodes) end, Expected) andalso
            lists:all(fun({_, M}) -> maps:get(cache_protocol, M, undefined) =:= 2 end, Members)
    catch
        _:_ -> false
    end.

-spec incr(atom()) -> ok.
incr(reject_total) ->
    imboy_cache_sync_proto:record_reject(other);
incr(Name) ->
    _ = elib_metric:increment(Name),
    ok.

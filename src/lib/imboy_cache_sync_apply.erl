%% @doc V2 同步事件应用（C02 双端一致性后的 op 路由）+ C03 资源预算 + C05
%% 缺口/化身补偿。由 imboy_cache_sync 串行调用（gen_server 即写入串行化），
%% 函数体自 imboy_cache_sync.erl 拆分迁移（closeout 例外 4），零行为变更。
-module(imboy_cache_sync_apply).

-include("cache.hrl").
-include("log.hrl").
-include("imboy_cache_sync_state.hrl").

-export([
    apply_v2/2,
    detect_gap/3,
    gap_compensation_key/1,
    index_budget_ok/1,
    rindex_count/0,
    recalibrate_rindex_count/0,
    tombstone_budget_exceeded/0,
    controlled_cold_recovery/0
]).

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
    imboy_cache_sync:incr(receive_total),
    Key = maps:get(key, Event),
    Op = maps:get(operation, Event),
    case Op of
        invalidate ->
            %% invalidate 不参与版本/epoch 比较（宁多失效）
            _ = imboy_cache_sync:maybe_bump_fenced(Key),
            _ = depcache:flush(Key, ?DEPCACHE_SERVER),
            {ok, State};
        _ ->
            Seq = maps:get(origin_seq, Event),
            ok = detect_gap(maps:get(origin_node, Event), Seq, Key),
            State1 = imboy_cache_sync:merge_clock(maps:get(revision, Event), State),
            apply_v2_op(Op, Key, Event, State1)
    end.

-spec apply_v2_op(set | delete, term(), map(), #state{}) ->
    {ok, #state{}} | {reject, term()}.
apply_v2_op(set, Key, Event, State) ->
    Rev = maps:get(revision, Event),
    case imboy_cache_sync:compare_rev(Rev, imboy_cache_sync:current_rev(Key)) of
        newer ->
            TTL = imboy_cache_sync:remaining_ttl_sec(maps:get(expires_at_ms, Event, 0)),
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
                            ok = imboy_cache_sync:index_replicate(Key),
                            {ok, State};
                        false ->
                            imboy_cache_sync:incr(reject_total),
                            {ok, State}
                    end;
                false ->
                    imboy_cache_sync:incr(expired_total),
                    {reject, expired}
            end;
        equal ->
            %% 同版本幂等（W3-08）
            {ok, State};
        older ->
            imboy_cache_sync:incr(conflict_total),
            {reject, stale_revision}
    end;
apply_v2_op(delete, Key, Event, State) ->
    Rev = maps:get(revision, Event),
    case imboy_cache_sync:compare_rev(Rev, imboy_cache_sync:current_rev(Key)) of
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
            imboy_cache_sync:incr(conflict_total),
            {reject, stale_revision}
    end.

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
            ok = imboy_cache_sync_recover:bounded_sweep(),
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
    imboy_cache_sync:incr(cold_recovery_total),
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
            imboy_cache_sync:incr(gap_detected_total),
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
    case imboy_cache_sync:is_fenced(Key) of
        true ->
            G = imboy_cache_sync:bump_generation(Key),
            ok = imboy_cache_sync:flush_physical(Key, G - 1);
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
    case imboy_cache_sync:is_fenced(Key) of
        true ->
            G = imboy_cache_sync:bump_generation(Key),
            ok = imboy_cache_sync:flush_physical(Key, G - 1);
        false ->
            ok
    end.

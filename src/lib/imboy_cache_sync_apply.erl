%% @doc 同步事件应用、统一 metadata 预算/记账及缺口补偿。
%% 由 imboy_cache_sync gen_server 串行调用，保持单写者。
-module(imboy_cache_sync_apply).
-include("cache.hrl").
-include("log.hrl").
-include("imboy_cache_sync_state.hrl").
-export([
    apply_v2/2,
    detect_gap/3,
    reconcile_sequence/3,
    apply_legacy_sequence/2,
    gap_compensation_key/1,
    recalibrate_rindex_count/0,
    tombstone_budget_exceeded/3,
    tombstone_budget_exceeded/4,
    tombstone_budget_exceeded/5,
    metadata_fits/1,
    tombstones_fit/1,
    pre_publish/5,
    index_replicate/1,
    prospective_rows/4,
    prospective_rows/5,
    prospective_rows/6,
    compare_entry/3,
    required_epoch/0,
    store_revision/4,
    store_revision/5,
    store_revision/6,
    store_metadata/1,
    delete_metadata/1,
    controlled_cold_recovery/0,
    controlled_cold_recovery/1,
    snapshot_budget_error/2,
    apply_expired_snapshot_entry/4,
    apply_expired_snapshot_entry/5,
    journal_position/0,
    journal_refresh/0,
    abort_snapshot_senders/1,
    abort_application_coverage/1,
    journal_after/1,
    journal_keys_after/1,
    journal_covers/1,
    metadata_snapshot/0,
    adopt_epoch/1,
    flush_replicas/0
]).
%% @doc V2 事件应用（W3 + C02 双端一致性）。gen_server 串行调用即写入串行化。
-spec apply_v2(map(), #state{}) -> {ok, #state{}} | {reject, term()} | {reject, term(), #state{}}.
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
                {error, unknown_incarnation} ->
                    {reject, unknown_incarnation, recover_unknown_incarnation(State)};
                {error, Reason} ->
                    {reject, Reason};
                ok ->
                    apply_v2_validated(Event, State)
            end;
        {error, expired} ->
            imboy_cache_sync:incr(expired_total),
            {reject, expired};
        {error, Reason} ->
            {reject, Reason}
    end.
%% @doc 双端一致性通过后的事件应用（C05 化身变化补偿 + op 路由）。
-spec apply_v2_validated(map(), #state{}) ->
    {ok, #state{}} | {reject, term()} | {reject, term(), #state{}}.
apply_v2_validated(Event, State) ->
    imboy_cache_sync:incr(receive_total),
    Key = maps:get(key, Event),
    Op = maps:get(operation, Event),
    Origin = maps:get(origin_node, Event),
    Boot = maps:get(origin_boot_id, Event),
    EntryRound = round_id_of(State#state.snapshot_round),
    BarrierState = imboy_cache_sync_recover:guard_data_epoch(Event, State),
    SourceState = imboy_cache_sync_recover:check_authority_event(Event, BarrierState),
    %% 宽限旧化身的重放不得污染当前化身序列。
    Gap =
        case ets:lookup(?META_TABLE, {peer_incarnation, Origin}) of
            [{_, Boot}] -> detect_gap({Origin, Boot}, maps:get(origin_seq, Event), Key);
            _ -> ok
        end,
    State0 =
        case Gap of
            gap when SourceState =/= State -> SourceState;
            gap -> imboy_cache_sync_recover:start_gap_recovery(SourceState);
            ok -> SourceState
        end,
    FinalResult =
        case Op of
            invalidate ->
                %% invalidate 不参与版本/epoch 比较（宁多失效）
                ok = abort_snapshot_senders(snapshot_invalidated),
                _ = imboy_cache_sync:maybe_bump_fenced(Key),
                _ = depcache:flush(Key, ?DEPCACHE_SERVER),
                {ok, State0};
            _ ->
                State1 = imboy_cache_sync:merge_clock(maps:get(revision, Event), State0),
                case apply_v2_op(Op, Key, Event, State1) of
                    {reject, Reason} -> {reject, Reason, State1};
                    OpResult -> OpResult
                end
        end,
    ok = imboy_cache_sync_proto:note_applied_event(Event, FinalResult),
    case FinalResult of
        {ok, Next} ->
            case
                imboy_cache_sync_coverage:record_delta(
                    Event,
                    Next,
                    imboy_cache_policy:snapshot_manifest_limits(),
                    imboy_cache_policy:b_max()
                )
            of
                {ok, _} = Recorded ->
                    Recorded;
                {error, Reason0} ->
                    case round_id_of(Next#state.snapshot_round) of
                        EntryRound ->
                            {ok, imboy_cache_sync_recover:abort_round(Reason0, Next)};
                        _ ->
                            %% 同一事件已中止过进入时的回合；由它产生的新回合
                            %% 尚无快照状态，重复计费会双倍消耗 Rmax 预算。
                            {ok, Next}
                    end
            end;
        _ ->
            FinalResult
    end.

round_id_of(#{round := R}) ->
    R;
round_id_of(_) ->
    undefined.
-spec apply_v2_op(set | delete, term(), map(), #state{}) ->
    {ok, #state{}} | {reject, term()} | {reject, term(), #state{}}.
apply_v2_op(set, Key, Event, State) ->
    Rev = maps:get(revision, Event),
    case compare_entry(Key, Rev, maps:get(epoch, Event)) of
        newer ->
            TTL = imboy_cache_sync:remaining_ttl_sec(maps:get(expires_at_ms, Event, 0)),
            case TTL > 0 of
                true ->
                    case metadata_fits(event_rows(set, Key, Event)) of
                        true ->
                            _ = depcache:set(
                                Key, maps:get(value, Event), TTL, [], ?DEPCACHE_SERVER
                            ),
                            ok = store_revision(
                                Key,
                                Rev,
                                maps:get(expires_at_ms, Event),
                                live,
                                maps:get(epoch, Event),
                                event_origin(Event)
                            ),
                            ok = imboy_cache_sync:index_replicate(Key),
                            {ok, State};
                        false ->
                            imboy_cache_sync:incr(reject_total),
                            {reject, index_full,
                                imboy_cache_sync_recover:start_cold_recovery(
                                    State, maps:get(epoch, Event) + 1
                                )}
                    end;
                false ->
                    imboy_cache_sync:incr(expired_total),
                    apply_expired_delta(Key, Event, State)
            end;
        equal ->
            %% 同版本幂等（W3-08）
            case imboy_cache_sync_coverage:idempotent_event(Event) of
                true -> {ok, State};
                false -> {reject, revision_content_conflict}
            end;
        older ->
            imboy_cache_sync:incr(conflict_total),
            {reject, stale_revision}
    end;
apply_v2_op(delete, Key, Event, State) ->
    Rev = maps:get(revision, Event),
    case compare_entry(Key, Rev, maps:get(epoch, Event)) of
        newer ->
            ExpiresAt = maps:get(expires_at_ms, Event),
            case metadata_fits(event_rows(delete, Key, Event)) of
                false ->
                    {reject, index_full,
                        imboy_cache_sync_recover:start_cold_recovery(
                            State, maps:get(epoch, Event) + 1
                        )};
                true ->
                    apply_delete(Key, Rev, ExpiresAt, maps:get(epoch, Event), State, Event)
            end;
        equal ->
            case imboy_cache_sync_coverage:idempotent_event(Event) of
                true -> {ok, State};
                false -> {reject, revision_content_conflict}
            end;
        older ->
            imboy_cache_sync:incr(conflict_total),
            {reject, stale_revision}
    end.
apply_expired_delta(Key, Event, State) ->
    Rev = maps:get(revision, Event),
    Exp = maps:get(expires_at_ms, Event),
    Epoch = maps:get(epoch, Event),
    InWindow =
        Exp + imboy_cache_policy:tombstone_ttl_sec() * 1000 >
            erlang:system_time(millisecond),
    case {InWindow, metadata_fits(event_rows(set, Key, Event))} of
        {false, _} ->
            {reject, expired};
        {true, true} ->
            ok = apply_expired_snapshot_entry(Key, Exp, Rev, Epoch, event_origin(Event)),
            {ok, State};
        {true, false} ->
            imboy_cache_sync:incr(reject_total),
            {reject, index_full, imboy_cache_sync_recover:start_cold_recovery(State, Epoch + 1)}
    end.
apply_delete(Key, Rev, ExpiresAt, Epoch, State, Event) ->
    case tombstone_budget_exceeded(Key, Rev, ExpiresAt, Epoch, event_origin(Event)) of
        true ->
            %% 远端删除超预算时受控清理并拒绝，不部分应用。
            S1 = imboy_cache_sync_recover:start_cold_recovery(State, Epoch + 1),
            {reject, tombstone_budget_exceeded, S1};
        false ->
            _ = depcache:flush(Key, ?DEPCACHE_SERVER),
            ok = store_revision(Key, Rev, ExpiresAt, deleted, Epoch, event_origin(Event)),
            {ok, State}
    end.
%% ===================================================================
%% C03 资源预算执行
%% ===================================================================
%% @doc 统一校准索引数量、版本行字节和墓碑预算，写路径复用同一 usage。
-spec recalibrate_rindex_count() -> ok.
recalibrate_rindex_count() ->
    Usage = scan_usage(),
    true = ets:insert(?META_TABLE, [{rindex_count, element(1, Usage)}, {metadata_usage, Usage}]),
    ok.
%% @doc 墓碑预算按覆盖后的数量与序列化字节投影；超限先淘汰过期行，
%% 仍超限由调用方受控冷恢复（C01 第 7 节），不删单条墓碑接受旧事件。
-spec tombstone_budget_exceeded(term(), term(), integer()) -> boolean().
tombstone_budget_exceeded(Key, Rev, ExpiresAt) ->
    tombstone_budget_exceeded(Key, Rev, ExpiresAt, imboy_cache_sync_proto:epoch()).
tombstone_budget_exceeded(Key, Rev, ExpiresAt, Epoch) ->
    tombstone_budget_exceeded(Key, Rev, ExpiresAt, Epoch, unknown).
tombstone_budget_exceeded(Key, Rev, ExpiresAt, Epoch, Origin) ->
    Rows = prospective_rows(delete, Key, Rev, ExpiresAt, Epoch, Origin),
    case tombstones_fit(Rows) of
        true ->
            false;
        false ->
            ok = imboy_cache_sync_recover:bounded_sweep(),
            not tombstones_fit(Rows)
    end.
tombstones_fit(Rows) ->
    {_, _, Count, Bytes} = projected_usage(Rows),
    Count =< imboy_cache_policy:t_max() andalso Bytes =< imboy_cache_policy:t_max_bytes().
pre_publish(Op, Key, Value, TTL, State) ->
    Rev = {State#state.counter + 1, node()},
    case
        imboy_cache_sync_proto:validate_publish(
            Op,
            Key,
            Value,
            TTL,
            Rev,
            State#state.origin_seq + 1,
            State#state.boot_id
        )
    of
        ok ->
            Seconds =
                case Op of
                    set -> TTL;
                    delete -> imboy_cache_policy:tombstone_ttl_sec()
                end,
            Rows = prospective_rows(
                Op,
                Key,
                Rev,
                erlang:system_time(millisecond) + Seconds * 1000,
                imboy_cache_sync_proto:epoch(),
                {node(), State#state.boot_id, State#state.origin_seq + 1}
            ),
            case compare_entry(Key, Rev, imboy_cache_sync_proto:epoch()) of
                older -> {error, stale_epoch};
                _ -> publish_capacity(Rows)
            end;
        Error ->
            Error
    end.
publish_capacity(Rows) ->
    case {imboy_cache_sync:status(), imboy_cache_sync:replication_capable()} of
        {ready, false} ->
            {error, incompatible_peers};
        {ready, true} ->
            case imboy_cache_sync:queue_depth() > imboy_cache_policy:high_watermark() of
                true ->
                    {error, overloaded};
                false ->
                    case metadata_fits(Rows) of
                        true -> ok;
                        false -> {error, index_full}
                    end
            end;
        {Status, _} ->
            {error, {not_ready, Status}}
    end.
index_replicate(Key) ->
    store_metadata({{rindex, Key}, erlang:system_time(millisecond)}).
%% 所有受管 metadata 写入复用同一计数；平常写路径不扫描整表。
prospective_rows(Op, Key, Rev, ExpiresAt) ->
    prospective_rows(Op, Key, Rev, ExpiresAt, imboy_cache_sync_proto:epoch()).
prospective_rows(Op, Key, Rev, ExpiresAt, Epoch) ->
    prospective_rows(Op, Key, Rev, ExpiresAt, Epoch, unknown).
prospective_rows(Op, Key, Rev, ExpiresAt, Epoch, Origin) ->
    imboy_cache_policy:prospective_revision_rows(Op, Key, Rev, ExpiresAt, Epoch, Origin).
compare_entry(Key, Rev, Epoch) ->
    case Epoch < required_epoch() of
        true -> older;
        false -> compare_stored_entry(Key, Rev, Epoch)
    end.
compare_stored_entry(Key, Rev, Epoch) ->
    case ets:lookup(?META_TABLE, {rev, Key}) of
        [] ->
            newer;
        [{_, {OldRev, _, _, _, OldEpoch}}] when Epoch =:= OldEpoch ->
            imboy_cache_sync:compare_rev(Rev, OldRev);
        [{_, {_, _, _, _, OldEpoch}}] when Epoch > OldEpoch -> newer;
        [_] ->
            older
    end.
metadata_fits(Rows) ->
    {Count, Bytes, _, _} = projected_usage(Rows),
    Count =< imboy_cache_policy:n_max() andalso Bytes =< imboy_cache_policy:n_max_bytes().
%% 重复行按各预算维度取最大值，覆盖 live/delete 混合及中间写入峰值。
projected_usage(Rows) ->
    Unique = lists:foldl(
        fun({K, _} = Row, Acc) ->
            Usage = row_usage(Row),
            case maps:find(K, Acc) of
                {ok, Existing} ->
                    Acc#{K => maximum_usage(Usage, Existing)};
                error ->
                    Acc#{K => Usage}
            end
        end,
        #{},
        Rows
    ),
    maps:fold(
        fun(K, Usage, Acc) ->
            Growth = subtract_usage(Usage, existing_usage(K)),
            %% 预检不借用可能被版本比较跳过的缩减，也覆盖页内瞬时峰值。
            add_usage(Acc, maximum_usage(Growth, {0, 0, 0, 0}))
        end,
        metadata_usage(),
        Unique
    ).
store_revision(Key, Rev, ExpiresAt, Kind) ->
    store_revision(Key, Rev, ExpiresAt, Kind, imboy_cache_sync_proto:epoch()).
store_revision(Key, Rev, ExpiresAt, Kind, Epoch) ->
    store_revision(Key, Rev, ExpiresAt, Kind, Epoch, unknown).
store_revision(Key, Rev, ExpiresAt, Kind, Epoch, Origin) ->
    note_epoch(Epoch),
    ok = store_metadata(
        imboy_cache_policy:revision_metadata_row(Key, Rev, ExpiresAt, Kind, Epoch, Origin)
    ),
    journal_record(Key, Rev, ExpiresAt, Kind, Epoch, Origin).

journal_position() ->
    case ets:lookup(?META_TABLE, journal_position) of
        [{_, Position}] -> Position;
        [] -> 0
    end.
%% 在 ETS 内排除全值 journal/发送游标，避免先将大对象复制到调用进程再丢弃。
metadata_snapshot() ->
    Keep =
        {'orelse', {'not', {'is_tuple', '$1'}},
            {'orelse', {'=/=', {'tuple_size', '$1'}, 2},
                {'andalso', {'=/=', {'element', 1, '$1'}, journal},
                    {'=/=', {'element', 1, '$1'}, snapshot_sender}}}},
    ets:select(?META_TABLE, [{{'$1', '_'}, [Keep], ['$_']}]).

journal_refresh() ->
    Active = ets:select_count(?META_TABLE, [{{{snapshot_sender, '_'}, '_'}, [], [true]}]),
    ets:insert(?META_TABLE, {journal_active, Active > 0}),
    case Active of
        0 ->
            ets:match_delete(?META_TABLE, {{journal, '_'}, '_'}),
            P = journal_position(),
            ets:insert(?META_TABLE, {journal_window, {P, P, 0, 0}});
        _ ->
            journal_trim(journal_window())
    end,
    ok.

journal_window() ->
    case ets:lookup(?META_TABLE, journal_window) of
        [{_, Window}] ->
            Window;
        [] ->
            P = journal_position(),
            {P, P, 0, 0}
    end.

journal_after(Start) when is_integer(Start), Start >= 0 ->
    {Floor, Last, _, _} = journal_window(),
    case Start >= Floor andalso Start =< Last of
        true ->
            Entries = [
                V
             || P <- lists:seq(Start + 1, Last), {_, V} <- ets:lookup(?META_TABLE, {journal, P})
            ],
            case length(Entries) =:= Last - Start of
                true -> {ok, Entries};
                false -> {error, journal_gap}
            end;
        false ->
            {error, journal_window_exceeded}
    end.

journal_covers(Start) ->
    {Floor, Last, _, _} = journal_window(),
    is_integer(Start) andalso Start >= Floor andalso Start =< Last.
%% 同步单写者内取连续窗口；作为普通快照页重放，保留版本和绝对期限。
journal_keys_after(Start) ->
    case journal_after(Start) of
        {ok, Entries} ->
            Epoch = imboy_cache_sync_proto:epoch(),
            case lists:all(fun({_, E, _, _}) -> E =:= Epoch end, Entries) of
                true ->
                    {
                        [
                            {journal_entry, self(),
                                imboy_cache_policy:snapshot_entry_with_origin(E, O)}
                         || {_, _, E, O} <- Entries
                        ],
                        journal_position()
                    };
                false ->
                    error(journal_epoch_changed)
            end;
        {error, Reason} ->
            error(Reason)
    end.

journal_record(Key, Rev, ExpiresAt, Kind, Epoch, Origin) ->
    case ets:lookup(?META_TABLE, journal_active) of
        [{_, true}] ->
            case checked_journal_entry(Key, Rev, ExpiresAt, Kind, Origin) of
                missing ->
                    journal_abort_senders();
                Entry ->
                    {Floor, _, Count, Bytes} = journal_window(),
                    P = ets:update_counter(
                        ?META_TABLE, journal_position, {2, 1}, {journal_position, 0}
                    ),
                    Row = {{journal, P}, {erlang:system_time(millisecond), Epoch, Entry, Origin}},
                    ets:insert(?META_TABLE, Row),
                    journal_trim({Floor, P, Count + 1, Bytes + erlang:external_size(Row)})
            end;
        _ ->
            ok
    end.

checked_journal_entry(Key, Rev, Exp, Kind, Origin) ->
    Entry = journal_entry(Key, Rev, Exp, Kind),
    case Origin =:= unknown orelse imboy_cache_sync_proto:valid_snapshot_origin(Origin, Entry) of
        true -> Entry;
        false -> missing
    end.

journal_entry(Key, Rev, ExpiresAt, Kind) when Kind =:= deleted; Kind =:= unavailable ->
    {Kind, Key, undefined, ExpiresAt, Rev};
journal_entry(Key, Rev, ExpiresAt, live) ->
    case imboy_cache_sync:remaining_ttl_sec(ExpiresAt) =:= 0 of
        true -> {expired, Key, undefined, ExpiresAt, Rev};
        false -> journal_live_entry(Key, Rev, ExpiresAt)
    end.

journal_live_entry(Key, Rev, ExpiresAt) ->
    try imboy_cache:get(Key) of
        {ok, Value} -> {Key, Value, ExpiresAt, Rev};
        _ -> missing
    catch
        _:_ ->
            imboy_cache_sync:incr(journal_capture_failed),
            missing
    end.

journal_trim({Floor, Last, Count, Bytes} = Window) ->
    Expired =
        case ets:lookup(?META_TABLE, {journal, Floor + 1}) of
            [{_, {At, _, _, _}}] ->
                At + imboy_cache_policy:tombstone_ttl_sec() * 1000 =<
                    erlang:system_time(millisecond);
            [] ->
                false
        end,
    case
        Count > 0 andalso
            (Count > imboy_cache_policy:j_max() orelse
                Bytes > imboy_cache_policy:j_max_bytes() orelse Expired)
    of
        true ->
            [Row] = ets:lookup(?META_TABLE, {journal, Floor + 1}),
            ets:delete(?META_TABLE, {journal, Floor + 1}),
            journal_trim({Floor + 1, Last, Count - 1, Bytes - erlang:external_size(Row)});
        false ->
            ets:insert(?META_TABLE, {journal_window, Window}),
            ok
    end.

journal_abort_senders() ->
    abort_snapshot_senders(journal_entry_unavailable).
%% ETS 内只投影成功候选的 PID/Round，避免复制游标预读值；失败候选不声明覆盖。
abort_application_coverage(Reason) ->
    Captured = ets:select(?META_TABLE, [
        {{{snapshot_sender, '$1'}, #{application_coverage => {ok, '_'}, round => '$2'}}, [], [
            {{'$1', '$2'}}
        ]}
    ]),
    case Captured of
        [] ->
            ok;
        _ ->
            [
                begin
                    P ! {cache_snapshot_abort, node(), R, Reason},
                    ets:delete(?META_TABLE, {snapshot_sender, P})
                end
             || {P, R} <- Captured
            ],
            journal_refresh()
    end.

abort_snapshot_senders(Reason) ->
    [
        begin
            P ! {cache_snapshot_abort, node(), maps:get(round, C), Reason},
            ets:delete(?META_TABLE, K)
        end
     || {K = {snapshot_sender, P}, C} <-
            ets:match_object(?META_TABLE, {{snapshot_sender, '_'}, '_'})
    ],
    journal_refresh().

required_epoch() ->
    case ets:lookup(?META_TABLE, data_epoch_ceiling) of
        [{_, Epoch}] -> erlang:max(Epoch, imboy_cache_sync_proto:epoch());
        [] -> imboy_cache_sync_proto:epoch()
    end.

note_epoch(Epoch) ->
    ets:insert(?META_TABLE, {data_epoch_ceiling, erlang:max(Epoch, required_epoch())}),
    ok.

store_metadata({K, _} = Row) ->
    Usage = add_usage(metadata_usage(), subtract_usage(row_usage(Row), existing_usage(K))),
    true = ets:insert(?META_TABLE, [Row, {metadata_usage, Usage}, {rindex_count, element(1, Usage)}]),
    ok.

delete_metadata(K) ->
    Usage = subtract_usage(metadata_usage(), existing_usage(K)),
    true = ets:delete(?META_TABLE, K),
    true = ets:insert(?META_TABLE, [{metadata_usage, Usage}, {rindex_count, element(1, Usage)}]),
    ok.

metadata_usage() ->
    case ets:lookup(?META_TABLE, metadata_usage) of
        [{_, Usage}] -> Usage;
        [] -> scan_usage()
    end.

scan_usage() ->
    lists:foldl(
        fun(Row, Acc) -> add_usage(Acc, row_usage(Row)) end,
        {0, 0, 0, 0},
        ets:tab2list(?META_TABLE)
    ).

existing_usage(K) ->
    case ets:lookup(?META_TABLE, K) of
        [Row] -> row_usage(Row);
        [] -> {0, 0, 0, 0}
    end.

row_usage({{rindex, _}, _} = Row) ->
    {1, erlang:external_size(Row), 0, 0};
row_usage({{rev, _}, {_, _, _, deleted, _}} = Row) ->
    Bytes = erlang:external_size(Row),
    {0, Bytes, 1, Bytes};
row_usage({{rev, _}, _} = Row) ->
    {0, erlang:external_size(Row), 0, 0};
row_usage(_) ->
    {0, 0, 0, 0}.

add_usage({A, B, C, D}, {E, F, G, H}) -> {A + E, B + F, C + G, D + H}.
subtract_usage({A, B, C, D}, {E, F, G, H}) -> {A - E, B - F, C - G, D - H}.
maximum_usage({A, B, C, D}, {E, F, G, H}) ->
    {erlang:max(A, E), erlang:max(B, F), erlang:max(C, G), erlang:max(D, H)}.
%% @doc 推进 epoch，清理低代副本，保留已接收的当前或未来代版本。
-spec controlled_cold_recovery() -> ok.
controlled_cold_recovery() ->
    controlled_cold_recovery(0).

controlled_cold_recovery(MinimumEpoch) when is_integer(MinimumEpoch), MinimumEpoch >= 0 ->
    Target = lists:max([MinimumEpoch, imboy_cache_sync_proto:epoch() + 1, required_epoch()]),
    ok = adopt_epoch(Target),
    imboy_cache_sync:incr(cold_recovery_total),
    ok.
%% 仅在恢复读写屏障内调用；代次不得回退或低于已观察数据下限。
adopt_epoch(Target) ->
    true = Target >= required_epoch(),
    ok = imboy_cache_sync_proto:reset_sequence_coverage(),
    ok = journal_abort_senders(),
    ok = imboy_cache_sync:bump_all_fenced(),
    true = ets:insert(?META_TABLE, {sync_epoch, Target}),
    Keys = [
        Key
     || {{rev, Key}, {_, _, _, _, Epoch}} <-
            ets:match_object(?META_TABLE, {{rev, '_'}, '_'}),
        Epoch < Target
    ],
    [
        begin
            depcache:flush(Key, ?DEPCACHE_SERVER),
            delete_metadata({rev, Key}),
            delete_metadata({rindex, Key})
        end
     || Key <- Keys
    ],
    ok = recalibrate_rindex_count(),
    %% reset 的重建停在旧代零行；采纳新代后按登记化身重建当前代零前缀，
    %% 保证 ready 时 census（application_coverage_candidate）不因错代整份拒绝。
    ok = imboy_cache_sync_proto:rebase_sequence_coverage(),
    ok.
%% 仅清理受管 live 副本值；恢复中保留 revision/tombstone 抵御旧事件。
flush_replicas() ->
    ok = imboy_cache_sync_proto:reset_sequence_coverage(),
    Keys = ets:select(?META_TABLE, [
        {{{rev, '$1'}, {'_', '_', '_', live, '_'}}, [], ['$1']}
    ]),
    lists:foreach(fun(K) -> depcache:flush(K, ?DEPCACHE_SERVER) end, Keys),
    ok.
%% C05 invalidate 缺口补偿

%% legacy flush 保留旧帧；随后同一发送者发布序列收据，新节点记账，旧节点忽略。
apply_legacy_sequence(
    {legacy_invalidation_seq, Origin, Boot, Seq, Hash, Registry, Key} = Frame, State
) ->
    Valid =
        imboy_cache_sync_proto:valid_watermark(
            {cache_invalidation_watermark, Origin, Boot, Seq, Hash, Registry}
        ) andalso
            imboy_cache_policy:check_value_size(Key) andalso
            imboy_cache_policy:check_value_type(Key) andalso
            erlang:external_size(Frame) =<
                imboy_cache_policy:max_value_bytes() +
                    imboy_cache_policy:protocol_meta_max_bytes() + 32 andalso
            Hash =:= imboy_cache_sync_proto:config_hash() andalso
            Registry =:= imboy_cache_sync_proto:registry_hash(),
    case {Valid, Origin =:= node()} of
        {false, _} ->
            imboy_cache_sync:incr(reject_total),
            State;
        {true, true} ->
            State;
        {true, false} ->
            State0 =
                case imboy_cache_sync_proto:check_incarnation(Origin, Boot) of
                    {error, unknown_incarnation} ->
                        recover_unknown_incarnation(State);
                    ok ->
                        State
                end,
            State1 = imboy_cache_sync_recover:check_authority_incarnation(Origin, State0),
            case ets:lookup(?META_TABLE, {peer_incarnation, Origin}) of
                [{_, Boot}] ->
                    %% 收据自身完成失效，不能依赖两次 publish 拥有相同接收集合。
                    ok = abort_snapshot_senders(snapshot_invalidated),
                    _ = depcache:flush(Key, ?DEPCACHE_SERVER),
                    _ = imboy_cache_sync:maybe_bump_fenced(Key),
                    ok = imboy_cache_sync_proto:note_applied_sequence(Origin, Boot, Seq),
                    case detect_gap({Origin, Boot}, Seq, legacy_sequence) of
                        gap when State1 =/= State -> State1;
                        gap -> imboy_cache_sync_recover:start_gap_recovery(State1);
                        ok -> imboy_cache_sync_recover:abort_round(snapshot_invalidated, State1)
                    end;
                _ ->
                    State1
            end
    end.
%% 序列协议状态仍由同步 actor 单写，协议模块集中维护。
recover_unknown_incarnation(#state{status = ready} = State) ->
    imboy_cache_sync_recover:start_gap_recovery(State#state{identity_recovery = true});
recover_unknown_incarnation(State) ->
    imboy_cache_sync_recover:start_gap_recovery(State).

reconcile_sequence(Origin, Boot, Seq) ->
    imboy_cache_sync_proto:reconcile_sequence(Origin, Boot, Seq).
detect_gap(Origin, Seq, Key) ->
    imboy_cache_sync_proto:detect_gap(Origin, Seq, Key).
%% 缺口内容未知时推进统一栅栏，普通本地键不受影响。
-spec gap_compensation_key(term()) -> ok.
gap_compensation_key(_Key) ->
    imboy_cache_sync:bump_all_fenced().

snapshot_budget_error(WireEntries, Epoch) ->
    Entries = [imboy_cache_policy:snapshot_entry_body(E) || E <- WireEntries],
    case
        lists:any(fun(E) -> imboy_cache_sync_coverage:snapshot_conflict(E, Epoch) end, WireEntries)
    of
        true -> snapshot_content_conflict;
        false -> snapshot_expiry_budget_error(WireEntries, Entries, Epoch)
    end.

snapshot_expiry_budget_error(WireEntries, Entries, Epoch) ->
    case lists:any(fun expired_live_outside_window/1, Entries) of
        true ->
            expired_floor_window_exceeded;
        false ->
            case lists:any(fun(E) -> expired_floor_conflict(E, Epoch, Entries) end, Entries) of
                true -> expired_floor_conflict;
                false -> snapshot_rows_budget_error(WireEntries, Epoch)
            end
    end.

expired_live_outside_window({_, _, Exp, _}) ->
    imboy_cache_sync:remaining_ttl_sec(Exp) =:= 0 andalso
        Exp + imboy_cache_policy:tombstone_ttl_sec() * 1000 =< erlang:system_time(millisecond);
expired_live_outside_window(_) ->
    false.

snapshot_rows_budget_error(Entries, Epoch) ->
    Rows = lists:append([imboy_cache_policy:snapshot_metadata_rows(E, Epoch) || E <- Entries]),
    case imboy_cache_sync_apply:metadata_fits(Rows) of
        false ->
            index_full;
        true ->
            case imboy_cache_sync_apply:tombstones_fit(Rows) of
                true -> ok;
                false -> tombstone_budget_exceeded
            end
    end.

expired_floor_conflict({Kind, K, undefined, Exp, Rev}, Epoch, Entries) when
    Kind =:= expired; Kind =:= unavailable
->
    Stored =
        case ets:lookup(?META_TABLE, {rev, K}) of
            [{_, {Rev, _, OtherExp, live, Epoch}}] when OtherExp =/= Exp -> true;
            _ -> false
        end,
    Stored orelse
        lists:any(
            fun
                ({K0, _, E0, R0}) ->
                    K0 =:= K andalso R0 =:= Rev andalso E0 =/= Exp;
                ({Kind0, K0, undefined, E0, R0}) when Kind0 =:= expired; Kind0 =:= unavailable ->
                    K0 =:= K andalso R0 =:= Rev andalso E0 =/= Exp;
                (_) ->
                    false
            end,
            Entries
        );
expired_floor_conflict({K, _, Exp, Rev}, Epoch, _Entries) ->
    case ets:lookup(?META_TABLE, {rev, K}) of
        [{_, {Rev, _, OtherExp, live, Epoch}}] when OtherExp =/= Exp -> true;
        _ -> false
    end;
expired_floor_conflict(_, _, _) ->
    false.

event_rows(Op, Key, Event) ->
    prospective_rows(
        Op,
        Key,
        maps:get(revision, Event),
        maps:get(expires_at_ms, Event),
        maps:get(epoch, Event),
        event_origin(Event)
    ).

event_origin(Event) ->
    {maps:get(origin_node, Event), maps:get(origin_boot_id, Event), maps:get(origin_seq, Event)}.

apply_expired_snapshot_entry(K, Exp, Rev, Epoch) ->
    apply_expired_snapshot_entry(K, Exp, Rev, Epoch, unknown).
apply_expired_snapshot_entry(K, Exp, Rev, Epoch, Origin) ->
    case compare_entry(K, Rev, Epoch) of
        newer ->
            _ = depcache:flush(K, ?DEPCACHE_SERVER),
            ok = store_revision(K, Rev, Exp, live, Epoch, Origin),
            index_replicate(K);
        equal ->
            case ets:lookup(?META_TABLE, {rev, K}) of
                [{_, {Rev, _, Exp, live, Epoch}}] ->
                    depcache:flush(K, ?DEPCACHE_SERVER),
                    ok;
                _ ->
                    ok
            end;
        older ->
            ok
    end.

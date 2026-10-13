%% @doc 缓存同步快照层（V2.2.1 收口 C03/C04）
-module(imboy_cache_sync_snapshot).
-moduledoc "分页快照：回合、页摘要、缓冲预算与 READY 闭合判定。".
-include("cache.hrl").
-define(META_TABLE, imboy_cache_meta).
%% 非空 tuple 条目列表的 ETF 开销：version + LIST_EXT + length + NIL_EXT。
-define(PAGE_LIST_BYTES, 7).
-export([
    new_round/2,
    recovery_round/2,
    round_id/0,
    expect_peer/2,
    authorize_peer/2,
    mark_requested/2,
    exclude_peer/2,
    %% 发送端
    send_snapshot/2,
    continue_snapshot/3,
    sweep_senders/0,
    %% 接收端（返回 {ok, RoundSt} | {abort, Reason, RoundSt}）
    handle_page/7,
    handle_done/5,
    round_complete/1,
    round_failed_peers/1,
    applied_count/1,
    peer_revision_ceiling/2,
    page_digest/3
]).
%% @doc 新建快照回合：Round token；deadline 由调用方传入（恢复总预算
%% Dmax 的剩余值，回合自身不另立预算——总恢复期限唯一）。
-spec new_round(non_neg_integer(), non_neg_integer()) -> map().
new_round(RoundsUsed, DeadlineMs) ->
    #{
        round => round_id(),
        deadline_ms => DeadlineMs,
        peers => #{},
        applied_items => 0,
        applied_bytes => 0,
        retained_obligations => unverified,
        rounds_used => RoundsUsed
    }.
%% 只由运行中的恢复 actor 调用，冻结本节点已知键；空 done 不能抹掉它们。
recovery_round(RoundsUsed, Deadline) ->
    Round = new_round(RoundsUsed, Deadline),
    Rows =
        try
            Now = erlang:system_time(millisecond),
            W = imboy_cache_policy:tombstone_ttl_sec() * 1000,
            [
                R
             || R = {{rev, _}, {_, _, Exp, _, _}} <- imboy_cache_sync_coverage:retained_rows(),
                Exp + W > Now
            ]
        catch
            error:Reason -> {error, Reason}
        end,
    Round#{retained_obligations := Rows}.
-spec round_id() -> binary().
round_id() ->
    crypto:strong_rand_bytes(12).
%% @doc 登记预期应答 peer（恢复节点在收到 version_report 后调用）。
-spec expect_peer(map(), node()) -> map().
expect_peer(RoundSt, Peer) ->
    Peers = maps:get(peers, RoundSt),
    case maps:is_key(Peer, Peers) of
        true ->
            RoundSt;
        false ->
            PeerSt = #{
                pages => 0,
                applied => 0,
                done => false,
                watermark => 0,
                max_revision_counter => 0,
                expected_more => false,
                authority => false,
                reported => false,
                requested => false
            },
            RoundSt#{peers := Peers#{Peer => PeerSt}}
    end.
authorize_peer(RoundSt, Peer) ->
    State1 = record_peer_report(RoundSt, Peer, true),
    Peers = maps:get(peers, State1),
    case maps:find(Peer, Peers) of
        {ok, PeerSt} ->
            Boot =
                case ets:lookup(?META_TABLE, {peer_incarnation, Peer}) of
                    [{_, Id}] -> Id;
                    [] -> undefined
                end,
            Epoch =
                case ets:lookup(?META_TABLE, {peer_epoch, Peer}) of
                    [{_, Value}] -> Value;
                    [] -> imboy_cache_sync_proto:epoch()
                end,
            State1#{peers := Peers#{Peer := PeerSt#{boot => Boot, source_epoch => Epoch}}};
        error ->
            State1
    end.
mark_requested(RoundSt, Peer) ->
    Peers = maps:get(peers, RoundSt),
    PeerSt = maps:get(Peer, Peers),
    RoundSt#{peers := Peers#{Peer := PeerSt#{requested := true}}}.

exclude_peer(RoundSt, Peer) ->
    record_peer_report(RoundSt, Peer, false).

record_peer_report(RoundSt, Peer, Authority) ->
    Peers = maps:get(peers, RoundSt),
    case maps:find(Peer, Peers) of
        {ok, #{authority := true}} when Authority =:= false -> RoundSt;
        {ok, PeerSt} ->
            RoundSt#{peers := Peers#{Peer := PeerSt#{authority := Authority, reported := true}}};
        error ->
            RoundSt
    end.

deadline_expired(#{deadline_ms := DeadlineMs}) ->
    erlang:system_time(millisecond) >= DeadlineMs.
%% 发送端（ready peer 应答快照请求）
%% @doc 按项数/字节双预算分页；值在组装时读取，墓碑保留绝对期限。
-spec send_snapshot(pid(), binary()) -> ok.
send_snapshot(Requestor, Round) ->
    sweep_senders(),
    %% 一个接收进程只保留一个回合；新回合撤销其旧游标。
    ets:delete(?META_TABLE, {snapshot_sender, Requestor}),
    Cursor = #{
        round => Round,
        next => 1,
        watermark => imboy_cache_sync_proto:snapshot_watermark_counter(),
        journal_start => imboy_cache_sync_apply:journal_position(),
        boot => imboy_cache_sync:node_boot(),
        epoch => imboy_cache_sync_proto:epoch(),
        config_hash => imboy_cache_policy:config_hash(),
        registry_hash => imboy_cache_sync_proto:registry_hash(),
        application_coverage => imboy_cache_sync_proto:application_coverage_candidate(),
        keys => snapshot_keys(erlang:system_time(millisecond)),
        deadline => erlang:system_time(millisecond) + imboy_cache_policy:d_max_ms()
    },
    send_cursor(Requestor, Cursor).

continue_snapshot(Requestor, Round, PageNo) ->
    sweep_senders(),
    case ets:lookup(?META_TABLE, {snapshot_sender, Requestor}) of
        [{_, #{round := Round, next := PageNo} = Cursor}] ->
            case
                maps:get(epoch, Cursor) =:= imboy_cache_sync_proto:epoch() andalso
                    ets:lookup(?META_TABLE, status) =:= [{status, ready}] andalso
                    maps:get(boot, Cursor) =:= imboy_cache_sync:node_boot() andalso
                    maps:get(config_hash, Cursor) =:= imboy_cache_policy:config_hash() andalso
                    maps:get(registry_hash, Cursor) =:= imboy_cache_sync_proto:registry_hash() andalso
                    imboy_cache_sync_apply:journal_covers(maps:get(journal_start, Cursor))
            of
                true ->
                    send_cursor(Requestor, Cursor);
                false ->
                    ets:delete(?META_TABLE, {snapshot_sender, Requestor}),
                    ok = imboy_cache_sync_apply:journal_refresh(),
                    Reason =
                        case
                            imboy_cache_sync_apply:journal_covers(maps:get(journal_start, Cursor))
                        of
                            true -> source_changed;
                            false -> journal_window_exceeded
                        end,
                    Requestor ! {cache_snapshot_abort, node(), Round, Reason},
                    ok
            end;
        _ ->
            ok
    end.

send_cursor(Requestor, #{round := Round, next := PageNo, keys := Keys} = Cursor) ->
    try
        Pmax = erlang:min(imboy_cache_policy:p_max(), imboy_cache_policy:b_max()),
        Pbytes = erlang:min(imboy_cache_policy:p_max_bytes(), imboy_cache_policy:b_max_bytes()),
        Prepared = maps:get(prepared, Cursor, undefined),
        {Page, Rest} =
            case Prepared of
                undefined -> take_page(Keys, Pmax, Pbytes, []);
                _ -> Prepared
            end,
        {NextKeys, JournalStart} =
            case Rest of
                [] -> imboy_cache_sync_apply:journal_keys_after(maps:get(journal_start, Cursor));
                _ -> {Rest, maps:get(journal_start, Cursor)}
            end,
        Next = take_page(NextKeys, Pmax, Pbytes, []),
        {NextPage, _} = Next,
        More = NextPage =/= [],
        Cursor1 = Cursor#{
            keys := [],
            prepared => Next,
            journal_start := JournalStart,
            next := PageNo + 1,
            watermark := erlang:max(
                maps:get(watermark, Cursor), imboy_cache_sync_proto:snapshot_watermark_counter()
            )
        },
        ok = retain_sender(Requestor, Cursor1, More),
        emit_page(Requestor, Round, PageNo, Page, More, Cursor1)
    catch
        error:Reason ->
            ets:delete(?META_TABLE, {snapshot_sender, Requestor}),
            ok = imboy_cache_sync_apply:journal_refresh(),
            Requestor ! {cache_snapshot_abort, node(), Round, sender_error(Reason)},
            ok
    end.

emit_page(Requestor, Round, PageNo, Page, More, Cursor) ->
    ok = ensure_unexpired_page(Page),
    case Page of
        [] ->
            ok;
        _ ->
            Requestor !
                {cache_snapshot_page, node(), Round, PageNo, Page, page_digest(Round, PageNo, Page),
                    More}
    end,
    case More of
        true ->
            ok;
        false ->
            imboy_cache_sync_proto:send_snapshot_completion(Requestor, Round, PageNo, Page, Cursor)
    end.

sender_error(snapshot_entry_too_large) -> oversized_entry;
sender_error(Reason) -> Reason.

ensure_unexpired_page(Page) ->
    ensure_page_deadline(Page, fun imboy_cache_sync_proto:snapshot_entry_deadline/1).

ensure_page_deadline(Page, Deadline) ->
    Now = erlang:system_time(millisecond),
    lists:foreach(
        fun(E) ->
            Exp = Deadline(E),
            case Exp > Now of
                true -> ok;
                false -> error(prepared_page_expired)
            end
        end,
        Page
    ).

retain_sender(Requestor, _Cursor, false) ->
    ets:delete(?META_TABLE, {snapshot_sender, Requestor}),
    ok = imboy_cache_sync_apply:journal_refresh(),
    ok;
retain_sender(Requestor, Cursor, true) ->
    Others = ets:match_object(?META_TABLE, {{snapshot_sender, '_'}, '_'}),
    OtherRows = [R || R = {{_, Pid}, _} <- Others, Pid =/= Requestor],
    Row = {{snapshot_sender, Requestor}, Cursor},
    Bytes = lists:sum([erlang:external_size(R) || R <- [Row | OtherRows]]),
    case length(OtherRows) < 8 andalso Bytes =< imboy_cache_policy:b_max_bytes() of
        true ->
            ets:insert(?META_TABLE, Row),
            ok = imboy_cache_sync_apply:journal_refresh(),
            ok;
        false ->
            error(sender_budget_exceeded)
    end.

sweep_senders() ->
    Now = erlang:system_time(millisecond),
    [
        ets:delete(?META_TABLE, K)
     || {K, #{deadline := Deadline}} <-
            ets:match_object(?META_TABLE, {{snapshot_sender, '_'}, '_'}),
        Deadline =< Now
    ],
    ok = imboy_cache_sync_apply:journal_refresh(),
    ok.
%% @doc 取一页：项数与完整条目字节双预算；单条超限显式中止回合。
-spec take_page([term()], pos_integer(), pos_integer(), [term()]) ->
    {[term()], [term()]}.
take_page(Keys, Pmax, PmaxBytes, []) ->
    take_page(Keys, Pmax, PmaxBytes, [], 0, 0).

take_page(Keys, Pmax, _PmaxBytes, Acc, Count, _Bytes) when Count >= Pmax ->
    {lists:reverse(Acc), Keys};
take_page([K | Rest], Pmax, PmaxBytes, Acc, Count, Bytes) ->
    case page_entry(K) of
        skip ->
            take_page(Rest, Pmax, PmaxBytes, Acc, Count, Bytes);
        Entry ->
            %% 每条 external_size 含独立 version 字节，嵌入列表时去掉该字节。
            EntryBytes = entry_bytes(Entry) - 1,
            case EntryBytes + ?PAGE_LIST_BYTES =< PmaxBytes of
                true -> ok;
                false -> error(snapshot_entry_too_large)
            end,
            NextBytes = Bytes + EntryBytes,
            case Count > 0 andalso NextBytes + ?PAGE_LIST_BYTES > PmaxBytes of
                true ->
                    {lists:reverse(Acc), [K | Rest]};
                false ->
                    take_page(Rest, Pmax, PmaxBytes, [Entry | Acc], Count + 1, NextBytes)
            end
    end;
take_page([], _Pmax, _PmaxBytes, Acc, _Count, _Bytes) ->
    {lists:reverse(Acc), []}.

%% @doc live 条目 {Key, Value, ExpiresAtMs, Rev}；墓碑
%% {deleted, Key, undefined, ExpiresAtMs, Rev} 保留原期限并在保护窗内传递删除下限；
%% 过期 live 在保护窗内发送 expired 下限。
%% 尚有效但物理值缺失的 live 发送 unavailable 下限；已知来源保留 Boot/seq。
-spec page_entry(term()) -> tuple() | skip.
page_entry({journal_entry, Owner, Entry}) when is_pid(Owner) ->
    Exp = imboy_cache_sync_proto:snapshot_entry_deadline(Entry),
    case
        Exp > erlang:system_time(millisecond) andalso
            imboy_cache_sync_proto:valid_snapshot_entry(Entry)
    of
        true -> Entry;
        false -> error(journal_entry_expired_or_invalid)
    end;
page_entry(K) ->
    case ets:lookup(?META_TABLE, {rev, K}) of
        [{_, Row = {Rev, Slot, _, _, _}}] ->
            case base_page_entry(K, Row) of
                skip ->
                    skip;
                Entry ->
                    imboy_cache_policy:snapshot_entry_with_origin(
                        Entry, imboy_cache_policy:revision_metadata_origin(Rev, Slot)
                    )
            end;
        _ ->
            skip
    end.

base_page_entry(K, {Rev, _, Exp, deleted, Epoch}) ->
    current_epoch_entry({deleted, K, undefined, Exp, Rev}, Epoch);
base_page_entry(K, {Rev, _, Exp, live, Epoch}) ->
    case imboy_cache_sync:remaining_ttl_sec(Exp) =:= 0 of
        true -> floor_page_entry(expired, K, Rev, Exp, Epoch);
        false -> live_page_entry(K, Rev, Exp, Epoch)
    end;
base_page_entry(_, _) ->
    skip.

live_page_entry(K, Rev, ExpiresAt, Epoch) ->
    case depcache:get(K, ?DEPCACHE_SERVER) of
        {ok, Value} ->
            Entry = {K, Value, ExpiresAt, Rev},
            current_epoch_entry(Entry, Epoch);
        undefined ->
            floor_page_entry(unavailable, K, Rev, ExpiresAt, Epoch)
    end.

floor_page_entry(Kind, K, Rev, Exp, Epoch) ->
    Entry = {Kind, K, undefined, Exp, Rev},
    current_epoch_entry(Entry, Epoch).

current_epoch_entry(Entry, Epoch) ->
    case
        Epoch =:= imboy_cache_sync_proto:epoch() andalso
            imboy_cache_sync_proto:valid_snapshot_entry(Entry)
    of
        true -> Entry;
        false -> skip
    end.

-spec snapshot_keys(non_neg_integer()) -> [term()].
snapshot_keys(NowMs) ->
    [
        K
     || {{rev, K}, {_, _, Exp, Kind, Epoch}} <- ets:match_object(?META_TABLE, {{rev, '_'}, '_'}),
        (Kind =:= live orelse Kind =:= deleted),
        Exp + imboy_cache_policy:tombstone_ttl_sec() * 1000 > NowMs,
        Epoch =:= imboy_cache_sync_proto:epoch()
    ].

%% @doc 页摘要绑定 Round、页号及完整原始条目。
page_digest(Round, PageNo, Page) ->
    imboy_cache_sync_proto:snapshot_page_digest(Round, PageNo, Page).

%% 接收端

%% @doc 处理快照页（恢复节点）：Round/连续性/digest/预算校验后应用条目。
%% 返回 {ok, RoundSt} 或 {abort, Reason, RoundSt}（调用方有界重试新回合）。
-spec handle_page(
    node(),
    binary(),
    pos_integer(),
    [term()],
    integer(),
    boolean(),
    map()
) ->
    {ok, map()} | {abort, term(), map()}.
handle_page(FromNode, Round, PageNo, Entries, Digest, HasMore, RoundSt) ->
    case maps:get(round, RoundSt, undefined) of
        Round ->
            case maps:find(FromNode, maps:get(peers, RoundSt)) of
                error ->
                    %% 未 expect 的 peer 发页：忽略（未知 peer 不推进回合）
                    {ok, RoundSt};
                {ok, PeerSt} ->
                    handle_known_page(
                        FromNode,
                        Round,
                        PageNo,
                        Entries,
                        Digest,
                        HasMore,
                        RoundSt,
                        PeerSt
                    )
            end;
        _ ->
            %% 旧回合/未知回合的页：丢弃（有界，C01 第 4 节）
            {ok, RoundSt}
    end.

handle_known_page(FromNode, Round, PageNo, Entries, Digest, HasMore, RoundSt, PeerSt) ->
    case {maps:get(authority, PeerSt), maps:get(requested, PeerSt, false)} of
        {false, _} ->
            {abort, invalid_authority, RoundSt};
        {true, false} ->
            {ok, RoundSt};
        {true, true} ->
            handle_requested_page(
                FromNode, Round, PageNo, Entries, Digest, HasMore, RoundSt, PeerSt
            )
    end.

handle_requested_page(FromNode, Round, PageNo, Entries, Digest, HasMore, RoundSt, PeerSt) ->
    case {maps:get(authority, PeerSt), maps:get(done, PeerSt)} of
        {false, _} ->
            {abort, invalid_authority, RoundSt};
        {true, true} ->
            %% done 之后的迟到页：丢弃（幂等安全）
            {ok, RoundSt};
        {true, false} ->
            checks_page(Round, PageNo, Entries, Digest, HasMore, RoundSt, FromNode, PeerSt)
    end.

checks_page(Round, PageNo, Entries, Digest, HasMore, RoundSt, FromNode, PeerSt) ->
    case deadline_expired(RoundSt) of
        true ->
            {abort, deadline_expired, RoundSt};
        false ->
            case page_shape(PageNo, Entries, Digest, HasMore) of
                {error, Reason} ->
                    {abort, Reason, RoundSt};
                {ok, Count} ->
                    case
                        imboy_cache_sync_proto:within_meta_budget(
                            {FromNode, Round, PageNo, Digest, HasMore}
                        )
                    of
                        false ->
                            {abort, metadata_too_large, RoundSt};
                        true ->
                            check_page_limits(
                                Round,
                                PageNo,
                                Entries,
                                Digest,
                                HasMore,
                                RoundSt,
                                FromNode,
                                PeerSt,
                                Count
                            )
                    end
            end
    end.

page_shape(PageNo, Entries, Digest, HasMore) ->
    case
        is_integer(PageNo) andalso PageNo > 0 andalso is_binary(Digest) andalso
            byte_size(Digest) =:= 32 andalso is_boolean(HasMore)
    of
        false ->
            {error, invalid_snapshot_page};
        true ->
            case page_count(Entries, 0, imboy_cache_policy:p_max()) of
                overflow -> {error, page_overflow};
                Count when is_integer(Count), Count > 0 -> {ok, Count};
                _ -> {error, invalid_snapshot_page}
            end
    end.

page_count([_ | _], Count, Limit) when Count >= Limit -> overflow;
page_count([_ | Tail], Count, Limit) -> page_count(Tail, Count + 1, Limit);
page_count([], Count, _) -> Count;
page_count(_, _, _) -> invalid.

check_page_limits(Round, PageNo, Entries, Digest, HasMore, RoundSt, FromNode, PeerSt, Count) ->
    SeenPages = maps:get(pages, PeerSt),
    ExpectedNo = SeenPages + 1,
    Bmax = imboy_cache_policy:b_max(),
    Bytes = page_bytes(Entries),
    Failed = [
        R
     || {C, R} <- [
            {not deadline_expired(RoundSt), deadline_expired},
            {PageNo =:= ExpectedNo, page_gap},
            {Count =< imboy_cache_policy:p_max(), page_overflow},
            {Bytes =< imboy_cache_policy:p_max_bytes(), page_bytes_overflow},
            {Count =< Bmax, buffer_overflow},
            {Bytes =< imboy_cache_policy:b_max_bytes(), buffer_bytes_overflow}
        ],
        not C
    ],
    case Failed of
        [Reason | _] ->
            {abort, Reason, RoundSt};
        [] ->
            validate_and_apply_page(
                Round,
                PageNo,
                Entries,
                Digest,
                HasMore,
                RoundSt,
                FromNode,
                PeerSt
            )
    end.

validate_and_apply_page(
    Round,
    PageNo,
    Entries,
    Digest,
    HasMore,
    RoundSt,
    FromNode,
    PeerSt
) ->
    case page_digest(Round, PageNo, Entries) =:= Digest of
        false ->
            {abort, page_digest_mismatch, RoundSt};
        true ->
            case
                lists:all(
                    fun(E) ->
                        imboy_cache_sync_proto:valid_authorized_snapshot_entry(E, FromNode, PeerSt)
                    end,
                    Entries
                )
            of
                false ->
                    {abort, invalid_snapshot_entry, RoundSt};
                true ->
                    case
                        imboy_cache_sync_apply:snapshot_budget_error(
                            Entries, maps:get(source_epoch, PeerSt)
                        )
                    of
                        Reason when Reason =/= ok ->
                            {abort, Reason, RoundSt};
                        ok ->
                            apply_manifest_page(
                                RoundSt,
                                FromNode,
                                PeerSt,
                                Entries,
                                PageNo,
                                HasMore
                            )
                    end
            end
    end.

%% 全回合共享有界清单；预算预检在缓存写入前，只有成功页可进入清单。
apply_manifest_page(Round, Source, Peer, Entries, PageNo, More) ->
    Rows = [imboy_cache_policy:snapshot_manifest_entry(E) || E <- Entries],
    Items = maps:get(manifest_items, Round, 0) + length(Rows),
    Bytes = maps:get(manifest_bytes, Round, 0) + 7 + erlang:external_size({PageNo, Rows}),
    case
        {
            Items =< imboy_cache_policy:n_max() + imboy_cache_policy:t_max(),
            Bytes =< imboy_cache_policy:n_max_bytes() + imboy_cache_policy:t_max_bytes()
        }
    of
        {false, _} ->
            {abort, snapshot_manifest_items_overflow, Round};
        {_, false} ->
            {abort, snapshot_manifest_bytes_overflow, Round};
        {true, true} ->
            case
                imboy_cache_policy:snapshot_page_result(
                    apply_page_entries(Entries, maps:get(source_epoch, Peer)),
                    Round,
                    Source,
                    Peer,
                    Entries,
                    PageNo,
                    More
                )
            of
                {ok, Next} ->
                    Peers = maps:get(peers, Next),
                    P = maps:get(Source, Peers),
                    Manifest = [{PageNo, Rows} | maps:get(manifest, P, [])],
                    {ok, Next#{
                        manifest_items => Items,
                        manifest_bytes => Bytes,
                        peers := Peers#{
                            Source := P#{
                                manifest => Manifest,
                                manifest_own_seq => imboy_cache_policy:snapshot_manifest_own_seq(
                                    Rows,
                                    {Source, maps:get(boot, P)},
                                    maps:get(manifest_own_seq, P, 0)
                                )
                            }
                        }
                    }};
                Abort ->
                    Abort
            end
    end.

peer_revision_ceiling(RoundSt, Peer) ->
    PeerSt = maps:get(Peer, maps:get(peers, RoundSt), #{}),
    maps:get(max_revision_counter, PeerSt, 0).

%% @doc 页条目应用：逐条按版本比较合并（等价 V2 set，与快照期间 delta
%% 天然融合；同版本幂等，低版本拒绝——不崩接收进程）。
-spec apply_page_entries([term()], non_neg_integer()) -> ok | {error, snapshot_entry_expired}.
apply_page_entries(Entries, Epoch) ->
    try
        lists:foreach(
            fun(E) ->
                ok = ensure_page_deadline(
                    [E], fun imboy_cache_policy:snapshot_entry_retention_deadline/1
                ),
                apply_snapshot_entry(E, Epoch)
            end,
            Entries
        ),
        ensure_page_deadline(Entries, fun imboy_cache_policy:snapshot_entry_retention_deadline/1)
    catch
        error:prepared_page_expired -> {error, snapshot_entry_expired}
    end.

%% 来源保存为逐键注记并转交日志，不提交 foreign floor 或连续覆盖。
apply_snapshot_entry({with_origin, Origin, Entry}, Epoch) ->
    apply_snapshot_entry(Entry, Epoch, Origin);
apply_snapshot_entry(Entry, Epoch) ->
    apply_snapshot_entry(Entry, Epoch, unknown).

apply_snapshot_entry({K, Value, ExpiresAt, Rev}, Epoch, Origin) ->
    apply_entry(K, Value, ExpiresAt, Rev, Epoch, Origin);
apply_snapshot_entry({unavailable, K, undefined, ExpiresAt, Rev}, Epoch, Origin) ->
    %% 缺值不等于删除：同版可用副本保留；只有更高版本下限清旧值。
    case imboy_cache_sync_apply:compare_entry(K, Rev, Epoch) of
        newer ->
            depcache:flush(K, ?DEPCACHE_SERVER),
            imboy_cache_sync_apply:store_revision(K, Rev, ExpiresAt, unavailable, Epoch, Origin),
            imboy_cache_sync:index_replicate(K);
        _ ->
            ok
    end;
apply_snapshot_entry({expired, K, undefined, ExpiresAt, Rev}, Epoch, Origin) ->
    imboy_cache_sync_apply:apply_expired_snapshot_entry(K, ExpiresAt, Rev, Epoch, Origin);
apply_snapshot_entry({deleted, K, undefined, ExpiresAt, Rev}, Epoch, Origin) ->
    case imboy_cache_sync_apply:compare_entry(K, Rev, Epoch) of
        newer -> write_snapshot_tombstone(K, ExpiresAt, Rev, Epoch, Origin);
        _ -> ok
    end.

write_snapshot_tombstone(K, ExpiresAt, Rev, Epoch, Origin) ->
    case
        ExpiresAt + imboy_cache_policy:tombstone_ttl_sec() * 1000 > erlang:system_time(millisecond)
    of
        true ->
            _ = depcache:flush(K, ?DEPCACHE_SERVER),
            imboy_cache_sync_apply:store_revision(K, Rev, ExpiresAt, deleted, Epoch, Origin);
        false ->
            ok
    end.

apply_entry(K, Value, ExpiresAt, Rev, Epoch, Origin) ->
    case imboy_cache_sync_apply:compare_entry(K, Rev, Epoch) of
        newer ->
            write_snapshot_entry(K, Value, ExpiresAt, Rev, Epoch, Origin);
        equal ->
            %% 失败回合清理值但保留版本；同版本合法快照可补回缺失 live 值。
            case ets:lookup(?META_TABLE, {rev, K}) of
                [{_, {Rev, _, ExpiresAt, live, Epoch}}] ->
                    case depcache:get(K, ?DEPCACHE_SERVER) of
                        undefined -> write_snapshot_entry(K, Value, ExpiresAt, Rev, Epoch, Origin);
                        _ -> ok
                    end;
                _ ->
                    ok
            end;
        _ ->
            %% 低版本跳过；缺口补偿是否完整须由恢复/增量闭环另行证明。
            ok
    end.

write_snapshot_entry(K, Value, ExpiresAt, Rev, Epoch, Origin) ->
    Remaining = imboy_cache_sync:remaining_ttl_sec(ExpiresAt),
    case Remaining > 0 of
        true ->
            _ = depcache:set(K, Value, Remaining, [], ?DEPCACHE_SERVER),
            ok = imboy_cache_sync_apply:store_revision(K, Rev, ExpiresAt, live, Epoch, Origin),
            ok = imboy_cache_sync:index_replicate(K);
        false ->
            imboy_cache_sync_apply:apply_expired_snapshot_entry(K, ExpiresAt, Rev, Epoch, Origin)
    end.

%% @doc 处理 peer 终帧：done + 版本水位。Round 不匹配的 done 不推进闭合
%% （C01 第 4 节：旧 round/未知 peer 的 done 不推进 READY）。
-spec handle_done(node(), binary(), non_neg_integer(), non_neg_integer(), map()) ->
    {ok, map()} | {abort, term(), map()}.
handle_done(FromNode, Round, Watermark, Total, RoundSt) ->
    case maps:get(round, RoundSt, undefined) of
        Round ->
            Peers = maps:get(peers, RoundSt),
            case maps:find(FromNode, Peers) of
                error ->
                    {ok, RoundSt};
                {ok, #{authority := false}} ->
                    {abort, invalid_authority, RoundSt};
                {ok, P} when not is_map_key(requested, P) ->
                    {ok, RoundSt};
                {ok, #{requested := false}} ->
                    {ok, RoundSt};
                {ok, _PeerSt} when not is_integer(Watermark); Watermark < 0 ->
                    {abort, invalid_watermark, RoundSt};
                {ok, PeerSt} ->
                    case done_error(FromNode, Round, PeerSt, Watermark, Total, RoundSt) of
                        none ->
                            PeerSt1 = PeerSt#{done := true, watermark := Watermark, total => Total},
                            {ok, RoundSt#{peers := Peers#{FromNode := PeerSt1}}};
                        Reason ->
                            {abort, Reason, RoundSt}
                    end
            end;
        _ ->
            {ok, RoundSt}
    end.

done_error(FromNode, Round, PeerSt, Watermark, Total, RoundSt) ->
    case deadline_expired(RoundSt) of
        true ->
            deadline_expired;
        false ->
            case imboy_cache_sync_proto:within_meta_budget({FromNode, Round, Watermark, Total}) of
                true -> done_count_error(PeerSt, Watermark, Total);
                false -> metadata_too_large
            end
    end.

done_count_error(#{authority := false}, _Watermark, _Total) ->
    invalid_authority;
done_count_error(_PeerSt, _Watermark, Total) when not is_integer(Total); Total < 0 ->
    invalid_page_total;
done_count_error(#{max_revision_counter := Max}, Watermark, _Total) when Watermark < Max ->
    invalid_version_watermark;
done_count_error(#{done := true, watermark := Watermark, total := Total}, Watermark, Total) ->
    none;
done_count_error(#{done := true}, _Watermark, _Total) ->
    conflicting_done;
done_count_error(#{pages := Pages, expected_more := More} = P, _Watermark, Total) ->
    case {Total =:= Pages andalso not More, maps:is_key(source_cut, P)} of
        {true, true} ->
            case imboy_cache_policy:final_snapshot_manifest(P) of
                {ok, _} -> none;
                {error, _} -> snapshot_content_conflict
            end;
        {true, false} ->
            missing_source_cut;
        _ ->
            incomplete_snapshot
    end.

%% @doc 回合是否完整闭合（C04 三条件）：
%% 全部发现应答、至少一个权威、全部权威快照完整、deadline 未超。
-spec round_complete(map()) -> boolean().
round_complete(Round) ->
    imboy_cache_sync_coverage:transport_complete(Round, erlang:system_time(millisecond)).
peer_complete(Peer) -> imboy_cache_sync_coverage:peer_complete(Peer).

%% @doc 失败 peer（done 但页序悬挂，或未 done）。
-spec round_failed_peers(map()) -> [node()].
round_failed_peers(#{peers := Peers}) ->
    [N || {N, St} <- maps:to_list(Peers), not peer_complete({N, St})].

%% @doc 已应用条目总数（缓冲预算监控）。
-spec applied_count(map()) -> non_neg_integer().
applied_count(#{peers := Peers}) ->
    lists:sum([maps:get(applied, St) || St <- maps:values(Peers)]).

entry_bytes(Entry) ->
    erlang:external_size(Entry).

page_bytes(Page) ->
    erlang:external_size(Page).

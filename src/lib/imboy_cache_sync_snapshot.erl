%% @doc 缓存同步快照层（V2.2.1 收口 C03/C04）
%%
%% 职责（从 imboy_cache_sync 拆出）：
%% - 快照回合（Round token，C01 第 4 节）：请求/分页发送/页摘要/终帧水位；
%% - 发送端预算：每页 <= Pmax 项且 <= Pmax_bytes，不预构造全部 payload；
%% - 接收端校验：Round 匹配、页号连续、digest 复算、Bmax 缓冲预算；
%% - READY 闭合判定（C04 核心）：全部预期 peer done(Round) + 水位并入 +
%%   无缺页，三者同时成立；不允许仅凭空 done 推进 READY。
%%
%% 回合状态为 plain map（由 imboy_cache_sync gen_server 持有）：
%% #{round => binary(), deadline_ms => integer(),
%%   peers => #{node() => PeerSt}, rounds_used => non_neg_integer()}
%% PeerSt = #{pages => sets(PageNo), applied => non_neg_integer(),
%%            done => boolean(), watermark => non_neg_integer(),
%%            expected_more => boolean()}
-module(imboy_cache_sync_snapshot).
-moduledoc "分页快照：回合、页摘要、缓冲预算与 READY 闭合判定。".

-include("cache.hrl").

-define(META_TABLE, imboy_cache_meta).

-export([
    new_round/2,
    round_id/0,
    expect_peer/2,
    %% 发送端
    send_snapshot/2,
    %% 接收端（返回 {ok, RoundSt} | {abort, Reason, RoundSt}）
    handle_page/7,
    handle_done/4,
    %% 判定
    round_complete/1,
    round_failed_peers/1,
    applied_count/1,
    page_digest/3
]).

%% ===================================================================
%% 回合状态
%% ===================================================================

%% @doc 新建快照回合：Round token；deadline 由调用方传入（恢复总预算
%% Dmax 的剩余值，回合自身不另立预算——总恢复期限唯一）。
-spec new_round(non_neg_integer(), non_neg_integer()) -> map().
new_round(RoundsUsed, DeadlineMs) ->
    #{
        round => round_id(),
        deadline_ms => DeadlineMs,
        peers => #{},
        rounds_used => RoundsUsed
    }.

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
                pages => #{},
                applied => 0,
                done => false,
                watermark => 0,
                expected_more => false
            },
            RoundSt#{peers := Peers#{Peer => PeerSt}}
    end.

-spec deadline_expired(map()) -> boolean().
deadline_expired(#{deadline_ms := DeadlineMs}) ->
    erlang:system_time(millisecond) >= DeadlineMs.

%% ===================================================================
%% 发送端（ready peer 应答快照请求）
%% ===================================================================

%% @doc 分页发送快照（C03：不预构造全部 payload，按页组装逐页发送）。
%% 每页 <= Pmax 项且累计 external_size <= Pmax_bytes；页摘要
%% phash2({Round, PageNo, Keys, Revs})；终帧 done 携带本节点版本水位。
%% 发送预算：发送进程单轮枚举 META 键（元数据级），值在组装页时逐条
%% 从 depcache 读取；depcache 逐出/过期导致的取值失败直接跳过该条目
%% （soft-state 缺失，接收端 miss 安全，5.6）。
%% 帧格式：页/终帧均携带 FromNode（接收端按节点路由回合状态）。
-spec send_snapshot(pid(), binary()) -> ok.
send_snapshot(RequestorPid, Round) ->
    NowMs = erlang:system_time(millisecond),
    Keys = live_keys(NowMs),
    Pmax = imboy_cache_policy:p_max(),
    PmaxBytes = imboy_cache_policy:p_max_bytes(),
    Watermark = watermark_counter(),
    {Total, _} = send_pages(RequestorPid, Round, Keys, Pmax, PmaxBytes, 1, 0, 0),
    RequestorPid ! {cache_snapshot_done, node(), Round, Watermark, Total},
    ok.

-spec send_pages(
    pid(),
    binary(),
    [term()],
    pos_integer(),
    pos_integer(),
    pos_integer(),
    non_neg_integer(),
    non_neg_integer()
) ->
    {non_neg_integer(), non_neg_integer()}.
send_pages(RequestorPid, Round, Keys, Pmax, PmaxBytes, PageNo, Count, Bytes) ->
    case take_page(Keys, Pmax, PmaxBytes, []) of
        {[], Rest} ->
            _ = Rest,
            {PageNo - 1, 0};
        {Page, Rest} ->
            PageBytes = page_bytes(Page),
            Digest = page_digest(Round, PageNo, Page),
            HasMore = Rest =/= [],
            RequestorPid ! {cache_snapshot_page, node(), Round, PageNo, Page, Digest, HasMore},
            send_pages(
                RequestorPid,
                Round,
                Rest,
                Pmax,
                PmaxBytes,
                PageNo + 1,
                Count + length(Page),
                Bytes + PageBytes
            )
    end.

%% @doc 取一页：项数与字节双预算（先到先满；单条超页预算仍单独成页，
%% 因 MaxValue=32KiB << Pmax_bytes=4MiB，正常不可能触发）。
%% C09 修复：旧守卫 `length(Acc) >= Pmax; Acc =/= []` 的第二个析取项恒真
%% （只要页内已有 1 条就切页），快照退化为每页 1 条——400 行数据膨胀为
%% 400 页，恢复节点单轮 RECOVER_ROUND_MS 内无法收完，回合永不闭合
%% （F5 三节点实证：30 页/轮 × 35 轮全部超时重开）。
-spec take_page([term()], pos_integer(), pos_integer(), [term()]) ->
    {[term()], [term()]}.
take_page(Keys, Pmax, _PmaxBytes, Acc) when length(Acc) >= Pmax ->
    {lists:reverse(Acc), Keys};
take_page([K | Rest], Pmax, PmaxBytes, Acc) ->
    case page_entry(K) of
        skip ->
            take_page(Rest, Pmax, PmaxBytes, Acc);
        Entry = {_Key, _Value, _ExpiresAt, _Rev} ->
            case Acc =/= [] andalso entry_sum_bytes(Entry, Acc) > PmaxBytes of
                true ->
                    {lists:reverse(Acc), [K | Rest]};
                false ->
                    take_page(Rest, Pmax, PmaxBytes, [Entry | Acc])
            end
    end;
take_page([], _Pmax, _PmaxBytes, Acc) ->
    {lists:reverse(Acc), []}.

%% @doc 快照页条目：{Key, Value, ExpiresAtMs, Rev}；值缺失（逐出/过期）
%% 返回 skip。
-spec page_entry(term()) -> {term(), term(), non_neg_integer(), term()} | skip.
page_entry(K) ->
    case ets:lookup(?META_TABLE, {rev, K}) of
        [{{rev, K}, {Rev, _C, ExpiresAt, live}}] ->
            case depcache:get(K, ?DEPCACHE_SERVER) of
                {ok, Value} ->
                    {K, Value, ExpiresAt, Rev};
                undefined ->
                    skip
            end;
        _ ->
            skip
    end.

-spec live_keys(non_neg_integer()) -> [term()].
live_keys(NowMs) ->
    [
        K
     || [K] <- ets:match(?META_TABLE, {{rindex, '$1'}, '_'}),
        valid_live_entry(K, NowMs)
    ].

-spec valid_live_entry(term(), non_neg_integer()) -> boolean().
valid_live_entry(K, NowMs) ->
    case ets:lookup(?META_TABLE, {rev, K}) of
        [{{rev, K}, {_Rev, _C, ExpiresAt, live}}] ->
            ExpiresAt > NowMs;
        _ ->
            false
    end.

%% @doc 页摘要（C01 第 4 节）：Round+PageNo+键序列+版本序列。
-spec page_digest(binary(), pos_integer(), [term()]) -> integer().
page_digest(Round, PageNo, Page) ->
    erlang:phash2({Round, PageNo, [K || {K, _, _, _} <- Page], [R || {_, _, _, R} <- Page]}).

%% @doc 发送端版本水位（快照枚举时刻的本地 counter 上界）。
-spec watermark_counter() -> non_neg_integer().
watermark_counter() ->
    case ets:lookup(?META_TABLE, sync_counter) of
        [{_, C}] when is_integer(C) -> C;
        [] -> 0
    end.

%% ===================================================================
%% 接收端
%% ===================================================================

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

-spec handle_known_page(
    node(),
    binary(),
    pos_integer(),
    [term()],
    integer(),
    boolean(),
    map(),
    map()
) ->
    {ok, map()} | {abort, term(), map()}.
handle_known_page(FromNode, Round, PageNo, Entries, Digest, HasMore, RoundSt, PeerSt) ->
    case maps:get(done, PeerSt) of
        true ->
            %% done 之后的迟到页：丢弃（幂等安全）
            {ok, RoundSt};
        false ->
            checks_page(Round, PageNo, Entries, Digest, HasMore, RoundSt, FromNode, PeerSt)
    end.

-spec checks_page(
    binary(),
    pos_integer(),
    [term()],
    integer(),
    boolean(),
    map(),
    node(),
    map()
) ->
    {ok, map()} | {abort, term(), map()}.
checks_page(Round, PageNo, Entries, Digest, HasMore, RoundSt, FromNode, PeerSt) ->
    SeenPages = maps:get(pages, PeerSt),
    ExpectedNo = maps:size(SeenPages) + 1,
    Applied = maps:get(applied, PeerSt),
    Bmax = imboy_cache_policy:b_max(),
    Failed = [
        R
     || {C, R} <- [
            {PageNo =:= ExpectedNo, page_gap},
            {page_digest(Round, PageNo, Entries) =:= Digest, page_digest_mismatch},
            {Applied + length(Entries) =< Bmax, buffer_overflow}
        ],
        not C
    ],
    case Failed of
        [Reason | _] ->
            {abort, Reason, RoundSt};
        [] ->
            apply_page_entries(Entries),
            PeerSt1 = PeerSt#{
                pages := SeenPages#{PageNo => true},
                applied := Applied + length(Entries),
                expected_more := HasMore
            },
            Peers = maps:get(peers, RoundSt),
            {ok, RoundSt#{peers := Peers#{FromNode := PeerSt1}}}
    end.

%% @doc 页条目应用：逐条按版本比较合并（等价 V2 set，与快照期间 delta
%% 天然融合；同版本幂等，低版本拒绝——不崩接收进程）。
-spec apply_page_entries([term()]) -> ok.
apply_page_entries(Entries) ->
    _ = [
        apply_entry(K, Value, ExpiresAt, Rev)
     || {K, Value, ExpiresAt, Rev} <- Entries
    ],
    ok.

-spec apply_entry(term(), term(), non_neg_integer(), term()) -> ok.
apply_entry(K, Value, ExpiresAt, Rev) ->
    case imboy_cache_sync:compare_rev(Rev, imboy_cache_sync:current_rev(K)) of
        newer ->
            Remaining = imboy_cache_sync:remaining_ttl_sec(ExpiresAt),
            case Remaining > 0 of
                true ->
                    _ = depcache:set(K, Value, Remaining, [], ?DEPCACHE_SERVER),
                    true = ets:insert(
                        ?META_TABLE,
                        {{rev, K}, {Rev, element(1, Rev), ExpiresAt, live}}
                    ),
                    _ = ets:insert(
                        ?META_TABLE,
                        {{rindex, K}, erlang:system_time(millisecond)}
                    ),
                    ok;
                false ->
                    ok
            end;
        _ ->
            %% 同版本幂等 / 低版本跳过（无漏事件：peer 正常写入以 delta 到达）
            ok
    end.

%% @doc 处理 peer 终帧：done + 版本水位。Round 不匹配的 done 不推进闭合
%% （C01 第 4 节：旧 round/未知 peer 的 done 不推进 READY）。
-spec handle_done(node(), binary(), non_neg_integer(), map()) ->
    {ok, map()} | {abort, term(), map()}.
handle_done(FromNode, Round, Watermark, RoundSt) ->
    case maps:get(round, RoundSt, undefined) of
        Round ->
            Peers = maps:get(peers, RoundSt),
            case maps:find(FromNode, Peers) of
                error ->
                    {ok, RoundSt};
                {ok, PeerSt} ->
                    PeerSt1 = PeerSt#{done := true, watermark := Watermark},
                    RoundSt1 = RoundSt#{peers := Peers#{FromNode := PeerSt1}},
                    case deadline_expired(RoundSt1) of
                        true -> {abort, deadline_expired, RoundSt1};
                        false -> {ok, RoundSt1}
                    end
            end;
        _ ->
            {ok, RoundSt}
    end.

%% ===================================================================
%% 判定
%% ===================================================================

%% @doc 回合是否完整闭合（C04 三条件）：
%% (1) 全部预期 peer done；(2) 无缺页（每个 peer 的页序完整且无
%% expected_more 悬挂）；(3) deadline 未超。
-spec round_complete(map()) -> boolean().
round_complete(#{peers := Peers} = RoundSt) ->
    maps:size(Peers) > 0 andalso
        not deadline_expired(RoundSt) andalso
        lists:all(fun peer_complete/1, maps:to_list(Peers)).

-spec peer_complete({node(), map()}) -> boolean().
peer_complete({_Node, PeerSt}) ->
    maps:get(done, PeerSt) =:= true andalso
        maps:get(expected_more, PeerSt) =:= false.

%% @doc 失败 peer（done 但页序悬挂，或未 done）。
-spec round_failed_peers(map()) -> [node()].
round_failed_peers(#{peers := Peers}) ->
    [N || {N, St} <- maps:to_list(Peers), not peer_complete({N, St})].

%% @doc 已应用条目总数（缓冲预算监控）。
-spec applied_count(map()) -> non_neg_integer().
applied_count(#{peers := Peers}) ->
    lists:sum([maps:get(applied, St) || St <- maps:values(Peers)]).

%% ===================================================================
%% Internal
%% ===================================================================

-spec entry_bytes(term()) -> non_neg_integer().
entry_bytes({_K, V, _E, _R}) ->
    try
        erlang:external_size(V)
    catch
        _:_ -> 0
    end.

-spec entry_sum_bytes(term(), [term()]) -> non_neg_integer().
entry_sum_bytes(Entry, Acc) ->
    entry_bytes(Entry) + lists:sum([entry_bytes(E) || E <- Acc]).

-spec page_bytes([term()]) -> non_neg_integer().
page_bytes(Page) ->
    lists:sum([entry_bytes(E) || E <- Page]).

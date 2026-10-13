%% @doc 完整页集与来源覆盖校验；actor 内的有界元数据取证不安装 floor。
-module(imboy_cache_sync_coverage).
-include("imboy_cache_sync_state.hrl").
-include("chat.hrl").
-export([
    manifest/3,
    verify/3,
    record_delta/4,
    transport_complete/2,
    peer_complete/1,
    source_certificate/2,
    manifest_certificate/1,
    retained_rows/0,
    content_census/0,
    snapshot_conflict/2,
    idempotent_event/1,
    certified_prefixes/1,
    verify_current_prefixes/1,
    prefix_rows/4,
    record_origin_target/2,
    sensitive_members_current/0,
    observe_member_loss/1,
    observe_node_down/1
]).

%% 完整来源页集按页号、键和版本归并；不从序列向量推断缺席或安装 floor。
manifest(#{peers := Peers} = Round, Limits, Now) ->
    try
        {Final, _} = maps:fold(
            fun(_, Peer, Acc) ->
                case maps:get(authority, Peer) of
                    false ->
                        Acc;
                    true ->
                        {ok, Rows} = manifest(Peer, Limits, Now),
                        maps:fold(
                            fun(_, Row, A) -> merge_manifest_row(Row, A, Limits) end, Acc, Rows
                        )
                end
            end,
            {#{}, 0},
            Peers
        ),
        true = Now < maps:get(deadline_ms, Round),
        {ok, Final}
    catch
        error:Reason -> {error, Reason}
    end;
manifest(Peer, Limits, _Now) ->
    try
        Pages = lists:keysort(1, maps:get(manifest, Peer, [])),
        true = [N || {N, _} <- Pages] =:= lists:seq(1, maps:get(pages, Peer)),
        {Final, _} = lists:foldl(
            fun({_, Rows}, Acc) ->
                lists:foldl(fun(Row, Acc0) -> merge_manifest_row(Row, Acc0, Limits) end, Acc, Rows)
            end,
            {#{}, 0},
            Pages
        ),
        {ok, Final}
    catch
        error:Reason -> {error, Reason}
    end.

merge_manifest_row(Row, {Rows, Bytes}, {ByteLimit, ItemLimit}) ->
    true = tuple_size(Row) =:= 7,
    Key = element(2, Row),
    Old = maps:get(Key, Rows, undefined),
    Chosen =
        case Old of
            undefined -> Row;
            _ -> choose_manifest_row(Row, Old)
        end,
    Previous =
        case Old of
            undefined -> 0;
            _ -> erlang:external_size({Key, Old}) + 8
        end,
    NextBytes = Bytes - Previous + erlang:external_size({Key, Chosen}) + 8,
    true = NextBytes =< ByteLimit,
    true = map_size(Rows) < ItemLimit orelse Old =/= undefined,
    {Rows#{Key => Chosen}, NextBytes}.

choose_manifest_row(New, Old) ->
    case compare_rev(element(3, New), element(3, Old)) of
        newer ->
            New;
        older ->
            Old;
        equal ->
            true = element(4, New) =:= element(4, Old),
            Origins = [element(5, New), element(5, Old)],
            true = hd(Origins) =:= lists:last(Origins) orelse lists:member(unknown, Origins),
            case {element(1, New), element(1, Old)} of
                {live, live} ->
                    true = element(7, New) =:= element(7, Old),
                    reliable_row(New, Old);
                {deleted, deleted} ->
                    reliable_row(New, Old);
                {deleted, _} ->
                    error(snapshot_kind_conflict);
                {_, deleted} ->
                    error(snapshot_kind_conflict);
                {live, _} ->
                    New;
                {_, live} ->
                    Old;
                {expired, _} ->
                    New;
                _ ->
                    Old
            end
    end.

reliable_row(New, Old) ->
    case element(5, New) of
        unknown -> Old;
        _ -> New
    end.

compare_rev(A, B) when A > B -> newer;
compare_rev(A, B) when A < B -> older;
compare_rev(_, _) -> equal.

%% 缺席不等于删除：声明覆盖某条来源序列时，必须有同版或新版的最终效果。
verify(#{peers := Peers, cohort_epoch := Epoch} = Round, Limits, Now) ->
    try
        {ok, Snapshot} = manifest(Round, Limits, Now),
        DeltaRows = maps:get(delta_manifest, Round, #{}),
        DeltaBytes = maps:get(delta_manifest_bytes, Round, 0),
        {Final, _} = maps:fold(
            fun(_, Row, Acc) -> merge_manifest_row(Row, Acc, Limits) end,
            {DeltaRows, DeltaBytes},
            Snapshot
        ),
        verify_retained(maps:get(retained_obligations, Round, unverified), Final, Epoch),
        Sources = [{N, P} || {N, P = #{authority := true}} <- maps:to_list(Peers)],
        true = Sources =/= [],
        lists:foreach(
            fun({Node, P}) -> verify_source(Node, P, Final, Epoch, Limits, Now) end, Sources
        ),
        lists:foreach(
            fun({Node, P}) -> verify_target(Node, P, Sources, Epoch) end, maps:to_list(Peers)
        ),
        {ok, Final}
    catch
        error:Reason -> {error, Reason}
    end.

verify_source(Node, P, Final, Epoch, Limits, Now) ->
    Prefixes = source_prefixes(Node, P, Epoch),
    {ok, Rows} = manifest(P, Limits, Now),
    case maps:get(source_cut, P) of
        {cache_snapshot_cut, _, _, _, _, _, _, _, _, Certificate} ->
            true = Certificate =:= manifest_certificate(Rows);
        _ ->
            error(source_content_certificate_missing)
    end,
    maps:foreach(
        fun(Key, Row) ->
            case element(5, Row) of
                {Origin, Boot, Seq} ->
                    case maps:get({Origin, Boot}, Prefixes, -1) >= Seq of
                        false ->
                            ok;
                        true ->
                            case maps:find(Key, Rows) of
                                {ok, Seen} ->
                                    case compare_rev(element(3, Seen), element(3, Row)) of
                                        older -> error(snapshot_covered_content_regressed);
                                        _ -> ok
                                    end;
                                error ->
                                    error(snapshot_absence_unproven)
                            end
                    end;
                unknown ->
                    error(snapshot_provenance_unproven)
            end
        end,
        Final
    ).

source_prefixes(Node, #{source_cut := Cut}, Epoch) ->
    {cache_snapshot_cut, Node, _, Boot, Epoch, Seq, _, _} = imboy_cache_policy:snapshot_cut_base(
        Cut
    ),
    {ok, Vector} = imboy_cache_policy:snapshot_cut_coverage(Cut),
    Prefixes = maps:from_list([{{O, B}, S} || {O, B, E, S} <- Vector, E =:= Epoch]),
    Prefixes#{{Node, Boot} => Seq}.

%% 只拼接逐条成功应用的收据；最大已见序列和逐键最终版本不代替中间序列。
certified_prefixes(#{peers := Peers, cohort_epoch := Epoch} = Round) ->
    Base = lists:foldl(
        fun({N, P}, Acc) ->
            maps:fold(
                fun(K, Seq, A) -> A#{K => erlang:max(Seq, maps:get(K, A, 0))} end,
                Acc,
                source_prefixes(N, P, Epoch)
            )
        end,
        #{},
        [{N, P} || {N, P = #{authority := true}} <- maps:to_list(Peers)]
    ),
    Receipts = maps:get(delta_receipts, Round, #{}),
    maps:map(
        fun({Origin, Boot}, Seq) -> stitch_prefix(Origin, Boot, Epoch, Seq, Receipts) end, Base
    ).

stitch_prefix(Origin, Boot, Epoch, Seq, Receipts) ->
    case maps:is_key({Origin, Boot, Epoch, Seq + 1}, Receipts) of
        true -> stitch_prefix(Origin, Boot, Epoch, Seq + 1, Receipts);
        false -> Seq
    end.

verify_current_prefixes(Round) ->
    Prefixes = certified_prefixes(Round),
    Epoch = maps:get(cohort_epoch, Round),
    Max = imboy_cache_policy:peer_max(),
    Match = [{{{peer_incarnation, '$1'}, '$2'}, [], [{{'$1', '$2'}}]}],
    Peers =
        case ets:select(imboy_cache_meta, Match, Max + 1) of
            '$end_of_table' -> [];
            {Rows, '$end_of_table'} when length(Rows) =< Max -> Rows;
            _ -> error(coverage_peer_budget)
        end,
    lists:foreach(
        fun({Origin, Boot}) ->
            Seen =
                case ets:lookup(imboy_cache_meta, {seq, {Origin, Boot}}) of
                    [{_, N}] -> N;
                    [] -> 0
                end,
            AppliedEpoch =
                case ets:lookup(imboy_cache_meta, {covered_seq, {Origin, Boot}}) of
                    [{_, {E, _}}] -> E;
                    [] -> 0
                end,
            case
                erlang:max(AppliedEpoch, imboy_cache_sync_proto:peer_epoch(Origin)) < Epoch orelse
                    maps:get({Origin, Boot}, Prefixes, 0) >= Seen
            of
                true -> ok;
                false -> error(incremental_content_gap)
            end
        end,
        Peers
    ),
    ok.

registered_peers() ->
    Max = imboy_cache_policy:peer_max(),
    Match = [{{{peer_incarnation, '$1'}, '$2'}, [], [{{'$1', '$2'}}]}],
    case ets:select(imboy_cache_meta, Match, Max + 1) of
        '$end_of_table' -> [];
        {Rows, '$end_of_table'} when length(Rows) =< Max -> Rows;
        _ -> error(coverage_peer_budget)
    end.

sensitive_members_current() ->
    try
        Members = syn:members(?CACHE_SCOPE, dsync_handler),
        Connected = nodes(),
        lists:all(
            fun({Origin, Boot}) ->
                Origin =:= node() orelse
                    (lists:member(Origin, Connected) andalso
                        lists:any(
                            fun({Pid, Meta}) ->
                                node(Pid) =:= Origin andalso
                                    maps:get(cache_boot, Meta, unknown) =:= Boot
                            end,
                            Members
                        ))
            end,
            registered_peers()
        )
    catch
        _:_ -> false
    end.

expected_peer_nodes() ->
    Config = application:get_env(imboy, cluster_nodes, []),
    true = length(Config) =< imboy_cache_policy:peer_max() + 1,
    true = lists:all(fun erlang:is_atom/1, Config),
    lists:usort([N || N <- Config ++ [O || {O, _} <- registered_peers()], N =/= node()]).

observe_node_down(Node) ->
    try
        case lists:member(Node, expected_peer_nodes()) of
            true -> observe_member_loss(imboy_cache_sync:syn_scope_members());
            false -> continue
        end
    catch
        _:_ -> invalid
    end.

%% 一个有界槽保留首次失联时间，部分断连也不得跨 W 继续持有旧 epoch。
observe_member_loss(Members) ->
    try
        observe_valid_members(Members)
    catch
        _:_ -> invalid
    end.

observe_valid_members(Members) ->
    Expected = expected_peer_nodes(),
    Live = [node(P) || P <- Members, node(P) =/= node(), lists:member(node(P), nodes())],
    Missing = lists:any(fun(N) -> not lists:member(N, Live) end, Expected),
    Island = Missing andalso Live =:= [],
    Now = erlang:monotonic_time(millisecond),
    Previous =
        case ets:lookup(imboy_cache_meta, cache_peer_loss) of
            [{_, Pair}] -> Pair;
            [] -> {undefined, undefined}
        end,
    case observe_outage(Missing, Island, Now, Previous) of
        cold -> cold;
        pending -> update_outage(Missing, Island, Now, Previous)
    end.

observe_outage(_, _, Now, {Any, _}) when is_integer(Any), Now - Any > 0 ->
    case Now - Any > imboy_cache_policy:grace_window_ms() of
        true ->
            ets:delete(imboy_cache_meta, cache_peer_loss),
            cold;
        false ->
            pending
    end;
observe_outage(_, _, _, _) ->
    pending.

update_outage(false, _, _, {undefined, _}) ->
    continue;
update_outage(false, _, _, _) ->
    ok = imboy_cache_sync:bump_all_fenced(),
    ets:delete(imboy_cache_meta, cache_peer_loss),
    continue;
update_outage(true, Island, Now, {Any, PreviousIsland}) ->
    First =
        case Any of
            undefined ->
                ok = imboy_cache_sync:bump_all_fenced(),
                Now;
            _ ->
                Any
        end,
    Since =
        case {Island, PreviousIsland} of
            {true, undefined} -> Now;
            {true, S} -> S;
            {false, _} -> undefined
        end,
    ets:insert(imboy_cache_meta, {cache_peer_loss, {First, Since}}),
    continue.

%% 同一发送 actor 在尾部切点枚举当前保留元数据；证书不从已发送页面倒推。
source_certificate(Epoch, Now) ->
    Rows = retained_rows(),
    W = imboy_cache_policy:tombstone_ttl_sec() * 1000,
    certificate([
        {Key, Rev, Exp, Kind, imboy_cache_policy:revision_metadata_origin(Rev, Slot)}
     || {{rev, Key}, {Rev, Slot, Exp, Kind, E}} <- Rows,
        E =:= Epoch,
        Exp + W > Now,
        Kind =:= live orelse Kind =:= deleted
    ]).

retained_rows() ->
    Limit = imboy_cache_policy:n_max() + imboy_cache_policy:t_max(),
    Rows =
        case ets:select(imboy_cache_meta, [{{{rev, '_'}, '_'}, [], ['$_']}], Limit + 1) of
            '$end_of_table' -> [];
            {Found, '$end_of_table'} when length(Found) =< Limit -> Found;
            _ -> error(source_census_overflow)
        end,
    case erlang:external_size(Rows) =< element(1, imboy_cache_policy:snapshot_manifest_limits()) of
        true -> ok;
        false -> error(source_census_byte_budget)
    end,
    Rows.

%% 空报告覆盖全部元数据 epoch，不能隐藏恢复节点保留的更高代数据。
content_census() ->
    try
        certificate(retained_rows())
    catch
        error:Reason -> {error, Reason}
    end.

manifest_certificate(Rows) ->
    certificate([
        {
            element(2, R),
            element(3, R),
            element(4, R),
            case element(1, R) of
                deleted -> deleted;
                _ -> live
            end,
            element(5, R)
        }
     || R <- maps:values(Rows)
    ]).

certificate(Rows) ->
    {length(Rows), crypto:hash(sha256, term_to_binary(lists:sort(Rows)))}.

snapshot_conflict(Wire, Epoch) ->
    Row = imboy_cache_policy:snapshot_manifest_entry(Wire),
    Key = element(2, Row),
    Rev = element(3, Row),
    case ets:lookup(imboy_cache_meta, {rev, Key}) of
        [{_, {Rev, Slot, Exp, Kind, Epoch}}] ->
            Origin = imboy_cache_policy:revision_metadata_origin(Rev, Slot),
            not (Exp =:= element(4, Row) andalso
                (Kind =:= deleted) =:= (element(1, Row) =:= deleted) andalso
                (Origin =:= unknown orelse element(5, Row) =:= unknown orelse
                    Origin =:= element(5, Row)) andalso
                snapshot_value_matches(Row));
        _ ->
            false
    end.

snapshot_value_matches({live, Key, _, Exp, _, _, Hash}) ->
    case stored_value(Key) of
        undefined ->
            true;
        {ok, Value} ->
            Exp =< erlang:system_time(millisecond) orelse
                crypto:hash(sha256, term_to_binary(Value)) =:= Hash;
        _ ->
            false
    end;
snapshot_value_matches(_) ->
    true.

idempotent_event(Event) ->
    Key = maps:get(key, Event),
    Exp = maps:get(expires_at_ms, Event),
    Rev = maps:get(revision, Event),
    Wire =
        case maps:get(operation, Event) of
            set -> {Key, maps:get(value, Event), Exp, Rev};
            delete -> {deleted, Key, undefined, Exp, Rev}
        end,
    Origin = {
        maps:get(origin_node, Event), maps:get(origin_boot_id, Event), maps:get(origin_seq, Event)
    },
    not snapshot_conflict({with_origin, Origin, Wire}, maps:get(epoch, Event)) andalso
        (maps:get(operation, Event) =:= delete orelse Exp =< erlang:system_time(millisecond) orelse
            stored_value(Key) =:= {ok, maps:get(value, Event)}).

stored_value(Key) ->
    try
        gen_server:call(imboy_cache, {get, Key}, erlang:min(5000, imboy_cache_policy:d_max_ms()))
    catch
        exit:_ -> {error, cache_unavailable}
    end.

verify_target(Node, P, Sources, Epoch) ->
    StaleEpoch =
        is_integer(maps:get(reported_epoch, P, unknown)) andalso
            maps:get(reported_epoch, P) < Epoch,
    EmptyTarget = empty_target(Node, P, Epoch),
    case maps:get(authority, P) orelse EmptyTarget orelse StaleEpoch of
        true -> ok;
        false -> error(peer_content_unproven)
    end,
    case maps:get(origin_target, P, unknown) of
        {Node, _, Epoch, 0} when EmptyTarget -> ok;
        {_, _, Older, _} when Older < Epoch -> ok;
        {Origin, Boot, Epoch, Target} ->
            true = lists:any(
                fun({N, Source}) ->
                    maps:get({Origin, Boot}, source_prefixes(N, Source, Epoch), -1) >= Target
                end,
                Sources
            );
        _ ->
            error(snapshot_target_unproven)
    end.

empty_target(Node, P, Epoch) ->
    case maps:get(origin_target, P, unknown) of
        {Node, Boot, Epoch, 0} ->
            %% 只有同一启动化身零序列的显式空 census 才能证明无需来源前缀。
            Pid = maps:get(reported_pid, P, undefined),
            Census = {0, crypto:hash(sha256, term_to_binary([]))},
            maps:get(reported_empty, P, false) andalso
                maps:get(reported_boot, P, undefined) =:= Boot andalso
                maps:get(reported_epoch, P, unknown) =:= Epoch andalso
                is_pid(Pid) andalso maps:get(content_census, P, undefined) =:= {Census, Pid};
        _ ->
            false
    end.

%% 调用方只在已应用成功后登记；保留最终效果摘要，不复制 payload。
record_delta(_, #state{snapshot_round = undefined} = State, _, _) ->
    {ok, State};
record_delta(#{operation := invalidate}, _, _, _) ->
    {error, snapshot_invalidated};
record_delta(Event, #state{snapshot_round = Round} = State, Limits, DeltaMax) ->
    try
        Key = maps:get(key, Event),
        Exp = maps:get(expires_at_ms, Event),
        Rev = maps:get(revision, Event),
        Body =
            case maps:get(operation, Event) of
                set -> {Key, maps:get(value, Event), Exp, Rev};
                delete -> {deleted, Key, undefined, Exp, Rev}
            end,
        Origin = {
            maps:get(origin_node, Event),
            maps:get(origin_boot_id, Event),
            maps:get(origin_seq, Event)
        },
        Row = imboy_cache_policy:snapshot_manifest_entry({with_origin, Origin, Body}),
        {Rows, Bytes} = merge_manifest_row(
            Row,
            {maps:get(delta_manifest, Round, #{}), maps:get(delta_manifest_bytes, Round, 0)},
            Limits
        ),
        true = map_size(Rows) =< DeltaMax,
        Receipt = {
            maps:get(origin_node, Event),
            maps:get(origin_boot_id, Event),
            maps:get(epoch, Event),
            maps:get(origin_seq, Event)
        },
        Receipts = (maps:get(delta_receipts, Round, #{}))#{Receipt => true},
        true = map_size(Receipts) =< DeltaMax,
        true =
            maps:get(applied_bytes, Round, 0) + Bytes + erlang:external_size(Receipts) =<
                element(1, Limits) * 2,
        {ok, State#state{
            snapshot_round = Round#{
                delta_manifest => Rows,
                delta_manifest_bytes => Bytes,
                delta_receipts => Receipts
            }
        }}
    catch
        error:Reason -> {error, Reason}
    end.

verify_retained(unverified, _, _) ->
    error(receiver_census_unproven);
verify_retained({error, Reason}, _, _) ->
    error(Reason);
verify_retained(Rows, Final, Epoch) when is_list(Rows) ->
    lists:foreach(
        fun
            ({{rev, _}, {_, _, _, _, Older}}) when Older < Epoch -> ok;
            ({{rev, Key}, {Rev, Slot, Exp, Kind, Epoch0}}) when Epoch0 =:= Epoch ->
                case maps:find(Key, Final) of
                    error ->
                        error(retained_content_missing);
                    {ok, Row} ->
                        case compare_rev(element(3, Row), Rev) of
                            older ->
                                error(retained_content_regressed);
                            newer ->
                                ok;
                            equal ->
                                true = element(4, Row) =:= Exp,
                                Origin = imboy_cache_policy:revision_metadata_origin(Rev, Slot),
                                true = Origin =:= unknown orelse Origin =:= element(5, Row),
                                true = (Kind =:= deleted) =:= (element(1, Row) =:= deleted)
                        end
                end;
            (_) ->
                error(retained_epoch_unproven)
        end,
        Rows
    ).

transport_complete(#{peers := Peers, deadline_ms := Deadline}, Now) ->
    map_size(Peers) > 0 andalso Now < Deadline andalso
        lists:any(fun(P) -> maps:get(authority, P) end, maps:values(Peers)) andalso
        lists:all(fun peer_complete/1, maps:to_list(Peers)).
peer_complete({_Node, P}) ->
    maps:get(reported, P) andalso
        (not maps:get(authority, P) orelse
            (maps:get(done, P) andalso maps:is_key(source_cut, P) andalso
                not maps:get(expected_more, P))).

prefix_rows(Origin, Boot, Seq, Epoch) ->
    case
        ets:lookup(imboy_cache_meta, {peer_incarnation, Origin}) =:=
            [{{peer_incarnation, Origin}, Boot}] andalso
            imboy_cache_sync_proto:peer_epoch(Origin) =:= Epoch
    of
        false ->
            [];
        true ->
            Keys = [{snapshot_seq_floor, {Origin, Boot}}, {seq, {Origin, Boot}}],
            Rows = [
                {K,
                    case ets:lookup(imboy_cache_meta, K) of
                        [{_, N}] -> erlang:max(N, Seq);
                        [] -> Seq
                    end}
             || K <- Keys
            ],
            Old =
                case ets:lookup(imboy_cache_meta, {covered_seq, {Origin, Boot}}) of
                    [{_, {Epoch, N0}}] -> N0;
                    _ -> 0
                end,
            [{{covered_seq, {Origin, Boot}}, {Epoch, erlang:max(Old, Seq)}} | Rows]
    end.

record_census_target({Node, Boot, Epoch, Seq, Ref, Census, Pid}, Round) ->
    case record_origin_target({Node, Boot, Epoch, Seq, Ref}, Round) of
        {ok, #{peers := Peers} = Next} ->
            case maps:find(Node, Peers) of
                error ->
                    {ok, Next};
                {ok, P} ->
                    case maps:get(content_census, P, {Census, Pid}) of
                        {Census, Pid} ->
                            Empty = Census =:= {0, crypto:hash(sha256, term_to_binary([]))},
                            {ok, Next#{
                                peers := Peers#{
                                    Node := P#{
                                        content_census => {Census, Pid},
                                        reported_empty => Empty,
                                        reported_pid => Pid,
                                        reported_boot => Boot,
                                        reported_epoch => Epoch
                                    }
                                }
                            }};
                        _ ->
                            {abort, peer_census_changed, Round}
                    end
            end;
        Abort ->
            Abort
    end.

record_origin_target({_, _, _, _, _, _, _} = Target, Round) ->
    record_census_target(Target, Round);
record_origin_target({Node, Boot, Epoch, unknown}, #{round := Ref} = Round) ->
    record_origin_target({Node, Boot, Epoch, unknown, Ref}, Round);
record_origin_target({_, _, _, unknown}, Round) ->
    {ok, Round};
record_origin_target(unknown, Round) ->
    {ok, Round};
record_origin_target({Node, Boot, Epoch, Seq, Ref}, #{round := Ref, peers := Peers} = Round) ->
    case maps:find(Node, Peers) of
        {ok, Peer} ->
            case maps:get(origin_target, Peer, unknown) of
                unknown when Seq =:= unknown -> {ok, Round};
                unknown ->
                    {ok, Round#{
                        peers := Peers#{Node := Peer#{origin_target => {Node, Boot, Epoch, Seq}}}
                    }};
                {Node, Boot, Epoch, _} ->
                    {ok, Round};
                _ ->
                    {abort, origin_target_identity_changed, Round}
            end;
        error ->
            {ok, Round}
    end;
record_origin_target(_, Round) ->
    {abort, origin_target_round_mismatch, Round}.

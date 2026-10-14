%% @doc 缓存同步协议层（V2.2.1 收口 C02）
%%
%% 集中处理信封/条目校验、epoch/hash、化身登记和覆盖证据。
%% 本模块为纯函数 + 共享 META ETS 表操作，不持有进程状态；
%% 写路径仍由 imboy_cache_sync gen_server 串行调用（计划 5.2 方案 A）。
-module(imboy_cache_sync_proto).
-moduledoc "缓存同步 V2 协议：信封校验、epoch/incarnation/hash 双端一致性。".
-include("cache.hrl").
-define(META_TABLE, imboy_cache_meta).
-export([
    %% 信封
    event/7,
    validate/1,
    validate_publish/7,
    valid_version_report/1,
    valid_watermark/1,
    valid_snapshot_cut/1,
    valid_snapshot_entry/1,
    valid_snapshot_origin/2,
    valid_received_snapshot_entry/1,
    valid_authorized_snapshot_entry/3,
    snapshot_page_digest/3,
    snapshot_revision_counter/1,
    snapshot_watermark_counter/0,
    snapshot_page_revision_ceiling/2,
    snapshot_entry_deadline/1,
    send_snapshot_cut/2,
    send_snapshot_cut/3,
    send_snapshot_completion/5,
    within_meta_budget/1,
    %% 双端一致性（对照本节点 META 状态）
    check_consistency/2,
    detect_gap/3,
    reconcile_sequence/3,
    note_applied_event/2,
    note_applied_sequence/3,
    reset_sequence_coverage/0,
    rebase_sequence_coverage/0,
    application_coverage_candidate/0,
    commit_snapshot_floors/1,
    %% epoch
    epoch/0,
    bump_epoch/0,
    peer_epoch/1,
    %% incarnation
    register_incarnation/2,
    check_incarnation/2,
    incarnation_changed/2,
    prune_grace/1,
    %% hash
    config_hash/0,
    registry_hash/0,
    hash_divergent/1,
    %% 快照握手帧构造（新协议帧）
    version_request_frame/1,
    version_request_frame/2,
    send_version_report/6,
    record_origin_target/2,
    handle_valid_report/2,
    record_reject/1,
    record_recovery_abort/1,
    record_recovery_timeout/1
]).

record_reject({not_ready, _}) ->
    record_reject(not_ready);
record_reject(Reason) ->
    Allowed = [
        invalid_protocol,
        unknown_protocol_field,
        hash_mismatch,
        stale_epoch,
        expired,
        stale_revision,
        stale_sequence,
        unknown_incarnation,
        incompatible_peers,
        overloaded,
        not_ready,
        index_full,
        tombstone_budget_exceeded,
        event_too_large,
        revision_content_conflict,
        legacy_policy,
        snapshot,
        other
    ],
    Label =
        case lists:member(Reason, Allowed) of
            true -> Reason;
            false -> other
        end,
    _ = elib_metric:increment(reject_total),
    elib_metric:increment(cache_sync_reject_reason_total, 1, #{reason => Label}).

record_recovery_abort(Reason) ->
    Allowed = [round_timeout, snapshot_invalidated, authority_epoch_changed],
    Label =
        case lists:member(Reason, Allowed) of
            true -> Reason;
            false -> other
        end,
    _ = elib_metric:increment(snapshot_abort_total),
    elib_metric:increment(cache_sync_snapshot_abort_reason_total, 1, #{reason => Label}).

record_recovery_timeout(Reason) ->
    Label =
        case lists:member(Reason, [round_timeout, deadline_timeout]) of
            true -> Reason;
            false -> other
        end,
    elib_metric:increment(cache_sync_recovery_timeout_total, 1, #{reason => Label}).
%% 信封构造与校验
%% @doc V2 事件信封（C01 第 1 节：新增 epoch/config_hash/registry_hash）。
-spec event(
    set | delete | invalidate,
    term(),
    term(),
    non_neg_integer(),
    {non_neg_integer(), node()},
    non_neg_integer(),
    binary()
) -> map().
event(Op, Key, Value, ExpiresAt, Rev, Seq, BootId) ->
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
        origin_boot_id => BootId,
        epoch => epoch(),
        config_hash => config_hash(),
        registry_hash => registry_hash()
    }.
%% @doc 事件结构校验（W3 既有规则 + C01 新增字段，接收端独立执行）。
-spec validate(map()) -> ok | {error, term()}.
validate(Event) when is_map(Event) ->
    Op = maps:get(operation, Event, undefined),
    Key = maps:get(key, Event, undefined),
    checks(
        envelope_checks(Event) ++ identity_checks(Event, Key, Op) ++ origin_checks(Event) ++
            contract_checks(Event) ++ payload_checks(Event, Key, Op)
    );
validate(_) ->
    {error, not_map}.
%% 发布前构造预览；不得先分配时钟或写入缓存再拒绝信封。
validate_publish(set, Key, Value, TTL, Rev, Seq, BootId) ->
    case imboy_cache_policy:validate_replicate(Key, Value, TTL) of
        ok ->
            Event = event(
                set,
                Key,
                Value,
                erlang:system_time(millisecond) + TTL * 1000,
                Rev,
                Seq,
                BootId
            ),
            checks(envelope_checks(Event));
        Error ->
            Error
    end;
validate_publish(delete, Key, _Value, _TTL, Rev, Seq, BootId) ->
    Expiry = erlang:system_time(millisecond) + imboy_cache_policy:tombstone_ttl_sec() * 1000,
    validate(event(delete, Key, undefined, Expiry, Rev, Seq, BootId)).
envelope_checks(Event) ->
    Fields = [
        protocol,
        operation,
        namespace,
        key,
        value,
        expires_at_ms,
        revision,
        origin_seq,
        origin_node,
        origin_boot_id,
        epoch,
        config_hash,
        registry_hash
    ],
    [
        {fun() -> maps:get(protocol, Event, undefined) =:= 2 end, invalid_protocol},
        {
            fun() ->
                map_size(Event) =< length(Fields) andalso
                    lists:all(fun(K) -> lists:member(K, Fields) end, maps:keys(Event))
            end,
            unknown_protocol_field
        },
        %% Key、Value 各自最多 max_value_bytes；其余固定字段共享 4 KiB。
        {
            fun() ->
                erlang:external_size(Event) =<
                    2 * imboy_cache_policy:max_value_bytes() +
                        imboy_cache_policy:protocol_meta_max_bytes()
            end,
            event_too_large
        },
        {fun() -> within_meta_budget(maps:without([key, value], Event)) end, metadata_too_large}
    ].
valid_version_report(Report) -> imboy_cache_policy:valid_version_report(Report).
valid_watermark(Frame) -> imboy_cache_policy:valid_watermark(Frame).
valid_snapshot_cut(Frame) -> imboy_cache_policy:valid_snapshot_cut(Frame).
send_snapshot_cut(Requestor, Round) ->
    send_snapshot_cut(Requestor, Round, legacy).
send_snapshot_cut(Requestor, Round, Candidate) ->
    {Origin, Boot} = imboy_cache_sync:node_boot(),
    [{_, Seq}] = ets:lookup(?META_TABLE, sync_origin_seq),
    Hash = config_hash(),
    Registry = registry_hash(),
    Cut = {cache_snapshot_cut, Origin, Round, Boot, epoch(), Seq, Hash, Registry},
    Frame =
        case Candidate of
            legacy ->
                Cut;
            _ ->
                erlang:append_element(
                    erlang:append_element(Cut, Candidate),
                    imboy_cache_sync_coverage:source_certificate(
                        epoch(), erlang:system_time(millisecond)
                    )
                )
        end,
    case valid_snapshot_cut(Frame) of
        true -> ok;
        false -> error(snapshot_cut_invalid_or_over_budget)
    end,
    Requestor ! {cache_invalidation_watermark, Origin, Boot, Seq, Hash, Registry},
    Requestor ! Frame,
    ok.
%% 最后一页、尾部切点与 done 仍在同一同步写入者回调内闭合。
send_snapshot_completion(Requestor, Round, PageNo, Page, Cursor) ->
    ok = send_snapshot_cut(Requestor, Round, maps:get(application_coverage, Cursor)),
    Total =
        case Page of
            [] -> PageNo - 1;
            _ -> PageNo
        end,
    Requestor ! {cache_snapshot_done, node(), Round, maps:get(watermark, Cursor), Total},
    ok.
within_meta_budget(Term) -> imboy_cache_policy:within_meta_budget(Term).
identity_checks(Event, Key, Op) ->
    [
        {fun() -> Key =/= undefined end, missing_key},
        {fun() -> imboy_cache_policy:check_value_size(Key) end, key_too_large},
        {fun() -> imboy_cache_policy:check_value_type(Key) end, key_type_forbidden},
        {
            fun() ->
                maps:is_key(namespace, Event) andalso
                    maps:get(namespace, Event) =:= imboy_cache_policy:namespace_of(Key)
            end,
            namespace_mismatch
        },
        {fun() -> lists:member(Op, [set, delete, invalidate]) end, unknown_operation}
    ].
origin_checks(Event) -> imboy_cache_policy:event_origin_checks(Event).
contract_checks(Event) ->
    [
        %% C01 第 1 节：新增字段结构性校验（缺字段拒绝，不静默降级）
        {
            fun() ->
                is_integer(maps:get(epoch, Event, none)) andalso
                    maps:get(epoch, Event) >= 0
            end,
            missing_protocol_field
        },
        {
            fun() ->
                is_binary(maps:get(config_hash, Event, none)) andalso
                    byte_size(maps:get(config_hash, Event)) =:= 16
            end,
            missing_protocol_field
        },
        {
            fun() ->
                is_binary(maps:get(registry_hash, Event, none)) andalso
                    byte_size(maps:get(registry_hash, Event)) =:= 16
            end,
            missing_protocol_field
        }
    ].
payload_checks(Event, Key, Op) ->
    [
        %% 接收端独立白名单/敏感性校验（19.5：两端独立，fail-closed）
        {
            fun() ->
                Op =:= invalidate orelse
                    imboy_cache_policy:replicate_allowed(Key)
            end,
            not_authorized
        },
        %% 接收端独立大小校验
        {
            fun() -> Op =/= set orelse maps:get(value, Event, undefined) =/= undefined end,
            invalid_value
        }
    ] ++ expiry_checks(Event, Op) ++ value_checks(Event, Op).
expiry_checks(Event, Op) ->
    [
        {
            fun() ->
                Op =:= invalidate orelse
                    (is_integer(maps:get(expires_at_ms, Event, none)) andalso
                        maps:get(expires_at_ms, Event) > 0)
            end,
            invalid_expiry
        },
        {
            fun() ->
                Op =:= invalidate orelse
                    maps:get(expires_at_ms, Event) =<
                        erlang:system_time(millisecond) +
                            (expiry_limit_sec(Op) + 5) * 1000
            end,
            ttl_too_large
        },
        {
            fun() ->
                Op =:= invalidate orelse
                    imboy_cache_sync:remaining_ttl_sec(maps:get(expires_at_ms, Event)) > 0 orelse
                    (Op =:= set andalso
                        maps:get(expires_at_ms, Event) +
                            imboy_cache_policy:tombstone_ttl_sec() * 1000 >
                            erlang:system_time(millisecond))
            end,
            expired
        }
    ].
value_checks(Event, Op) ->
    [
        {
            fun() ->
                Op =/= set orelse
                    imboy_cache_policy:check_value_type(maps:get(value, Event, undefined))
            end,
            value_type_forbidden
        },
        {
            fun() ->
                Op =/= set orelse
                    imboy_cache_policy:check_value_size(maps:get(value, Event, undefined))
            end,
            value_too_large
        }
    ].
expiry_limit_sec(delete) -> imboy_cache_policy:tombstone_ttl_sec();
expiry_limit_sec(set) -> imboy_cache_policy:max_ttl_sec().
%% 双端一致性（C01 第 1/2/3 节）
%% @doc 对照本节点状态校验远端事件的一致性。
%% 依次：旧化身拒绝 → epoch 推移 → hash 匹配；任一失败返回 {error, Reason}。
%% hash 不一致同时标记 peer hash_divergent（19.8 可观测 + 排除 authority）。
-spec check_consistency(map(), binary()) ->
    ok | {ok, changed} | {error, term()}.
check_consistency(Event, SelfBootId) ->
    Origin = maps:get(origin_node, Event),
    BootId = maps:get(origin_boot_id, Event),
    RemoteEpoch = maps:get(epoch, Event),
    case self_loop(Origin, BootId, SelfBootId) of
        true ->
            {error, self_loop};
        false ->
            %% hash/epoch 拒绝不得先登记新化身或删除当前来源序列。
            %% hash 优先，旧 epoch 的 invalidate 例外也不能绕过配置校验。
            case check_hashes(Origin, Event) of
                ok ->
                    case check_epoch(RemoteEpoch) of
                        ok -> check_snapshot_replay(Event, Origin, BootId);
                        EpochError -> EpochError
                    end;
                HashError ->
                    HashError
            end
    end.
-spec self_loop(node(), binary(), binary()) -> boolean().
self_loop(Origin, BootId, SelfBootId) ->
    Origin =:= node() andalso BootId =:= SelfBootId.
%% 事件只能使用已确认或有效宽限化身，不能凭旧帧自行替换当前 Boot。
%% 新化身由成员握手登记；未知事件触发有界恢复，不保留待确认 payload。
-spec check_incarnation(node(), binary()) ->
    ok | {error, unknown_incarnation}.
check_incarnation(Origin, BootId) ->
    case ets:lookup(?META_TABLE, {peer_incarnation, Origin}) of
        [{_, BootId}] ->
            ok;
        [{_, _KnownOther}] ->
            case grace_member(Origin, BootId) of
                true ->
                    ok;
                false ->
                    {error, unknown_incarnation}
            end;
        [] ->
            {error, unknown_incarnation}
    end.
check_snapshot_replay(Event, Origin, Boot) ->
    Seq = maps:get(origin_seq, Event),
    case
        {maps:get(operation, Event), ets:lookup(?META_TABLE, {snapshot_seq_floor, {Origin, Boot}})}
    of
        {Op, [{_, Floor}]} when Op =/= invalidate, Seq =< Floor ->
            imboy_cache_sync:incr(conflict_total),
            {error, stale_sequence};
        _ ->
            check_incarnation(Origin, Boot)
    end.
commit_snapshot_floors(undefined) ->
    ok;
commit_snapshot_floors(Round) ->
    try
        commit_verified_floors(Round)
    catch
        error:Reason -> {error, Reason}
    end.
commit_verified_floors(#{peers := Peers} = Round) ->
    true = imboy_cache_sync_snapshot:round_complete(Round),
    {ok, _} = imboy_cache_policy:verify_snapshot_coverage(Round),
    ok = imboy_cache_sync_coverage:verify_current_prefixes(Round),
    Rows = [
        begin
            RoundId = maps:get(round, Round),
            {cache_snapshot_cut, Origin, RoundId, Boot, Epoch, Seq, Hash, Registry} = imboy_cache_policy:snapshot_cut_base(
                maps:get(source_cut, P)
            ),
            true = valid_snapshot_cut(maps:get(source_cut, P)),
            Epoch = maps:get(source_epoch, P),
            Hash = config_hash(),
            Registry = registry_hash(),
            [{{peer_incarnation, Origin}, Boot}] = ets:lookup(
                ?META_TABLE, {peer_incarnation, Origin}
            ),
            Key = {snapshot_seq_floor, {Origin, Boot}},
            Old =
                case ets:lookup(?META_TABLE, Key) of
                    [{_, N}] -> N;
                    [] -> 0
                end,
            {Key, erlang:max(Old, Seq)}
        end
     || {Origin, P = #{authority := true}} <- maps:to_list(Peers)
    ],
    PrefixRows = lists:append([
        imboy_cache_sync_coverage:prefix_rows(Origin, Boot, Seq, maps:get(cohort_epoch, Round))
     || {{Origin, Boot}, Seq} <- maps:to_list(imboy_cache_sync_coverage:certified_prefixes(Round))
    ]),
    case
        imboy_cache_sync_recover:sources_current(Round) andalso
            erlang:system_time(millisecond) < maps:get(deadline_ms, Round)
    of
        true ->
            FinalRows = maps:to_list(maps:from_list(Rows ++ PrefixRows)),
            ets:insert(?META_TABLE, [{status, ready} | FinalRows]),
            ok;
        false ->
            {error, snapshot_commit_barrier_changed}
    end.

-spec grace_member(node(), binary()) -> boolean().
grace_member(Origin, BootId) ->
    case ets:lookup(?META_TABLE, {grace_incarnation, {Origin, BootId}}) of
        [{_, RegisteredAtMs}] ->
            RegisteredAtMs + imboy_cache_policy:grace_window_ms() >
                erlang:system_time(millisecond);
        [] ->
            false
    end.
%% @doc 登记 peer 当前化身；若与已知化身不同，旧化身转入宽限集合
%% （W=720s），并返回 changed=true（调用方对受管 namespace 执行
%% 有界补偿，C05）。
-spec register_incarnation(node(), binary()) ->
    changed | registered | {error, incarnation_budget_exceeded}.
register_incarnation(Origin, BootId) ->
    case ets:lookup(?META_TABLE, {peer_incarnation, Origin}) of
        [{_, BootId}] ->
            registered;
        [{_, OldBootId}] ->
            register_changed_incarnation(Origin, BootId, OldBootId);
        [] ->
            register_new_incarnation(Origin, BootId)
    end.
register_new_incarnation(Origin, BootId) ->
    Count = ets:select_count(?META_TABLE, [{{{peer_incarnation, '_'}, '_'}, [], [true]}]),
    case Count < imboy_cache_policy:peer_max() of
        true ->
            ok = imboy_cache_sync_apply:abort_application_coverage(coverage_identity_changed),
            true = ets:insert(?META_TABLE, {{peer_incarnation, Origin}, BootId}),
            ensure_sequence_floor(Origin, BootId),
            registered;
        false ->
            {error, incarnation_budget_exceeded}
    end.
register_changed_incarnation(Origin, BootId, OldBootId) ->
    NowMs = erlang:system_time(millisecond),
    ok = prune_grace(NowMs),
    Count = ets:select_count(?META_TABLE, [{{{grace_incarnation, '_'}, '_'}, [], [true]}]),
    case Count < imboy_cache_policy:grace_max() of
        false ->
            {error, incarnation_budget_exceeded};
        true ->
            ok = imboy_cache_sync_apply:abort_application_coverage(coverage_identity_changed),
            true = ets:insert(
                ?META_TABLE,
                {{grace_incarnation, {Origin, OldBootId}}, NowMs}
            ),
            true = ets:insert(?META_TABLE, {{peer_incarnation, Origin}, BootId}),
            %% 每个 peer 仅保留当前化身的序列，防止重启后旧高水位遮蔽缺口。
            true = ets:delete(?META_TABLE, {seq, {Origin, OldBootId}}),
            true = ets:delete(?META_TABLE, {covered_seq, {Origin, OldBootId}}),
            ensure_sequence_floor(Origin, BootId),
            changed
    end.
%% @doc 化身是否发生变化（握手帧路径用：变化即触发 C05 补偿）。
-spec incarnation_changed(node(), binary()) -> boolean().
incarnation_changed(Origin, BootId) ->
    case ets:lookup(?META_TABLE, {peer_incarnation, Origin}) of
        [{_, BootId}] -> false;
        [{_, _Other}] -> true;
        [] -> false
    end.
%% @doc 淘汰超窗宽限化身；ETS 内删除，不复制 journal/快照 payload。
-spec prune_grace(non_neg_integer()) -> ok.
prune_grace(NowMs) ->
    WindowMs = imboy_cache_policy:grace_window_ms(),
    _ = ets:select_delete(?META_TABLE, [
        {{{grace_incarnation, '_'}, '$1'}, [{'=<', {'+', '$1', WindowMs}, NowMs}], [true]}
    ]),
    FloorKeys = ets:select(?META_TABLE, [
        {{{snapshot_seq_floor, {'$1', '$2'}}, '_'}, [], [{{'$1', '$2'}}]}
    ]),
    [
        ets:delete(?META_TABLE, {snapshot_seq_floor, {Origin, Boot}})
     || {Origin, Boot} <- FloorKeys,
        ets:lookup(?META_TABLE, {peer_incarnation, Origin}) =/=
            [{{peer_incarnation, Origin}, Boot}],
        not grace_member(Origin, Boot)
    ],
    ok.
%% 水位超过已接收序列时，丢失的可能就是最后一条事件，无需等下一次业务写。
%% 仅当前已登记化身可推进水位，重复/迟到报告不反复失效。
-spec reconcile_sequence(node(), binary(), non_neg_integer()) ->
    ok | gap | {error, unknown_incarnation}.
reconcile_sequence(Origin, Boot, Seq) ->
    case ets:lookup(?META_TABLE, {peer_incarnation, Origin}) of
        [{_, Boot}] ->
            SeqKey = {seq, {Origin, Boot}},
            Last =
                case ets:lookup(?META_TABLE, SeqKey) of
                    [{_, N}] -> N;
                    [] -> 0
                end,
            case Seq > Last of
                true ->
                    imboy_cache_sync:incr(gap_detected_total),
                    ok = imboy_cache_sync:bump_all_fenced(),
                    true = ets:insert(?META_TABLE, {SeqKey, Seq}),
                    gap;
                false ->
                    ok
            end;
        _ ->
            {error, unknown_incarnation}
    end.
%% @doc 按节点/化身检测来源序列缺口；跳号时保守失效全部受管物理键。
%% 同一来源缺口内容未知，不能从随后事件的 Key 推断受影响 namespace。
-spec detect_gap({node(), binary()}, non_neg_integer(), term()) -> ok | gap.
detect_gap(Origin, Seq, _Key) ->
    SeqKey = {seq, Origin},
    Last =
        case ets:lookup(?META_TABLE, SeqKey) of
            [{_, L}] -> L;
            [] -> 0
        end,
    Result =
        case Seq > Last + 1 of
            true ->
                imboy_cache_sync:incr(gap_detected_total),
                ok = imboy_cache_sync:bump_all_fenced(),
                gap;
            false ->
                ok
        end,
    _ = ets:insert(?META_TABLE, {SeqKey, erlang:max(Last, Seq)}),
    Result.
%% 连续应用下限独立于最大已见序列；缺口与应用失败不能充当覆盖证明。
note_applied_event(Event, Result) ->
    case Result of
        {ok, _} ->
            note_applied_sequence(
                maps:get(origin_node, Event),
                maps:get(origin_boot_id, Event),
                maps:get(origin_seq, Event),
                maps:get(epoch, Event)
            );
        {reject, stale_revision, _} ->
            note_applied_sequence(
                maps:get(origin_node, Event),
                maps:get(origin_boot_id, Event),
                maps:get(origin_seq, Event),
                maps:get(epoch, Event)
            );
        _ ->
            ok
    end.
note_applied_sequence(Origin, Boot, Seq) ->
    note_applied_sequence(Origin, Boot, Seq, epoch()).
note_applied_sequence(Origin, Boot, Seq, Epoch) ->
    Key = {covered_seq, {Origin, Boot}},
    {LastEpoch, Last} =
        case ets:lookup(?META_TABLE, Key) of
            [{_, {E, N}}] -> {E, N};
            _ -> {Epoch, 0}
        end,
    case
        ets:lookup(?META_TABLE, {peer_incarnation, Origin}) =:=
            [{{peer_incarnation, Origin}, Boot}] andalso Epoch >= epoch() andalso
            Epoch >= LastEpoch
    of
        true ->
            case Epoch > LastEpoch of
                true -> imboy_cache_sync_apply:abort_application_coverage(coverage_epoch_changed);
                false -> ok
            end,
            Base =
                case Epoch =:= LastEpoch of
                    true -> Last;
                    false -> 0
                end,
            Prefix =
                case Seq =:= Base + 1 of
                    true -> Seq;
                    false -> Base
                end,
            ets:insert(?META_TABLE, {Key, {Epoch, Prefix}});
        false ->
            false
    end,
    ok.
reset_sequence_coverage() ->
    ok = imboy_cache_sync_apply:abort_application_coverage(coverage_reset),
    ets:match_delete(?META_TABLE, {{covered_seq, '_'}, '_'}),
    ok.
%% 冻结合同（covered_seq 节）：每个已登记来源必有无缺的当前代下限行。
%% 高代切换重置前缀为零、不继承旧代计数；缺席不等于零，缺项由候选整份拒绝。
ensure_sequence_floor(Origin, Boot) ->
    Epoch = epoch(),
    case ets:lookup(?META_TABLE, {covered_seq, {Origin, Boot}}) of
        [{_, {E, _}}] when E >= Epoch -> ok;
        _ -> true = ets:insert(?META_TABLE, {{covered_seq, {Origin, Boot}}, {Epoch, 0}})
    end.
-spec rebase_sequence_coverage() -> ok.
rebase_sequence_coverage() ->
    Peers = ets:select(?META_TABLE, [{{{peer_incarnation, '$1'}, '$2'}, [], [{{'$1', '$2'}}]}]),
    lists:foreach(fun({Origin, Boot}) -> ensure_sequence_floor(Origin, Boot) end, Peers),
    ok.
%% 仅声明当前代连续应用证据；不是快照枚举/缺席证明，也不安装 foreign floor。
application_coverage_candidate() ->
    Max = imboy_cache_policy:peer_max(),
    Match = [{{{peer_incarnation, '$1'}, '$2'}, [], [{{'$1', '$2'}}]}],
    case ets:select(?META_TABLE, Match, Max + 1) of
        '$end_of_table' -> {ok, []};
        {Peers, '$end_of_table'} when length(Peers) =< Max -> coverage_candidate(Peers);
        _ -> {error, coverage_peer_budget}
    end.
coverage_candidate(Peers) ->
    try
        Epoch = epoch(),
        Vector = [
            begin
                true = is_atom(Origin) andalso is_binary(Boot) andalso byte_size(Boot) =:= 16,
                %% 冻结合同：每个当前登记来源须有同本地 epoch 的连续前缀；
                %% 缺项、旧格式、错代整份拒绝，不造零前缀候选。
                [{{covered_seq, {Origin, Boot}}, {Epoch, Seq}}] =
                    ets:lookup(?META_TABLE, {covered_seq, {Origin, Boot}}),
                true = is_integer(Seq) andalso Seq >= 0,
                {Origin, Boot, Epoch, Seq}
            end
         || {Origin, Boot} <- Peers
        ],
        case within_meta_budget(Vector) of
            true -> {ok, lists:sort(Vector)};
            false -> {error, coverage_byte_budget}
        end
    catch
        error:{badmatch, _} -> {error, incomplete_application_coverage};
        error:{case_clause, _} -> {error, incomplete_application_coverage}
    end.
%% @doc epoch 推移校验（C01 第 1 节）：远端 epoch 小于本节点 → 拒绝
%% （set/delete）；invalidate 不经此路径（宁多失效语义，见 sync 路由）。
-spec check_epoch(non_neg_integer()) -> ok | {error, stale_epoch}.
check_epoch(RemoteEpoch) ->
    case RemoteEpoch < epoch() of
        true -> {error, stale_epoch};
        false -> ok
    end.
%% @doc hash 双端校验：不一致 → 标记 divergent 并拒绝整事件
%% （不部分应用；C10 混合矩阵的 hash 不一致场景由此闭合）。
-spec check_hashes(node(), map()) -> ok | {error, hash_mismatch}.
check_hashes(Origin, Event) ->
    case
        maps:get(config_hash, Event) =:= config_hash() andalso
            maps:get(registry_hash, Event) =:= registry_hash()
    of
        true ->
            _ = ets:delete(?META_TABLE, {hash_divergent, Origin}),
            ok;
        false ->
            %% 未登记来源本就不具权威资格，不能因拒绝信封而无限增加诊断行。
            case ets:member(?META_TABLE, {peer_incarnation, Origin}) of
                true -> ets:insert(?META_TABLE, {{hash_divergent, Origin}, true});
                false -> false
            end,
            {error, hash_mismatch}
    end.

%% @doc peer 是否 hash 不一致（排除 ready authority 用，C01 第 8 节）。
-spec hash_divergent(node()) -> boolean().
hash_divergent(Node) ->
    ets:member(?META_TABLE, {hash_divergent, Node}).

-spec epoch() -> non_neg_integer().
epoch() ->
    case ets:lookup(?META_TABLE, sync_epoch) of
        [{_, E}] when is_integer(E) -> E;
        [] -> 0
    end.

%% @doc 推进本节点 epoch（C01 第 3 节：冷恢复完成或受控清理时）。
-spec bump_epoch() -> non_neg_integer().
bump_epoch() ->
    E = epoch() + 1,
    true = ets:insert(?META_TABLE, {sync_epoch, E}),
    ok = rebase_sequence_coverage(),
    E.

%% @doc 观察 peer epoch（握手帧路径记录；authority 判定用）。
-spec peer_epoch(node()) -> non_neg_integer().
peer_epoch(Node) ->
    case ets:lookup(?META_TABLE, {peer_epoch, Node}) of
        [{_, E}] when is_integer(E) -> E;
        [] -> 0
    end.

-spec config_hash() -> binary().
config_hash() -> imboy_cache_policy:config_hash().

-spec registry_hash() -> binary().
registry_hash() -> imboy_cache_policy:registry_hash().

%% 周期请求直达请求者 pid；恢复请求另带 Round 防止迟到报告跨回合。
-spec version_request_frame(pid()) -> term().
version_request_frame(FromPid) when is_pid(FromPid) ->
    {cache_version_request, FromPid, node()}.

-spec checks([{fun(() -> boolean()), term()}]) -> ok | {error, term()}.
checks([{Pred, Reason} | Rest]) ->
    case Pred() of
        true -> checks(Rest);
        false -> {error, Reason}
    end;
checks([]) ->
    ok.

snapshot_entry_deadline(Entry) -> imboy_cache_policy:snapshot_entry_deadline(Entry).
valid_snapshot_entry(Entry) -> imboy_cache_policy:valid_snapshot_entry(Entry).
valid_snapshot_origin(Origin, Entry) -> imboy_cache_policy:valid_snapshot_origin(Origin, Entry).

valid_received_snapshot_entry({with_origin, {Origin, Boot, _}, _} = Entry) ->
    valid_snapshot_entry(Entry) andalso
        (imboy_cache_sync:node_boot() =:= {Origin, Boot} orelse
            check_incarnation(Origin, Boot) =:= ok);
valid_received_snapshot_entry(Entry) ->
    valid_snapshot_entry(Entry).

%% 只有当前已授权快照来源可携带历史注记；不激活历史 Origin。
valid_authorized_snapshot_entry({with_origin, _, _} = Entry, Source, PeerSt) ->
    Boot = maps:get(boot, PeerSt, undefined),
    valid_snapshot_entry(Entry) andalso is_binary(Boot) andalso byte_size(Boot) =:= 16 andalso
        maps:get(authority, PeerSt, false) andalso maps:get(requested, PeerSt, false) andalso
        ets:lookup(?META_TABLE, {peer_incarnation, Source}) =:=
            [{{peer_incarnation, Source}, Boot}] andalso
        peer_epoch(Source) =:= maps:get(source_epoch, PeerSt, undefined);
valid_authorized_snapshot_entry(Entry, _, _) ->
    valid_received_snapshot_entry(Entry).

snapshot_page_digest(Round, PageNo, Page) ->
    imboy_cache_policy:snapshot_page_digest(Round, PageNo, Page).
snapshot_revision_counter(Entry) -> imboy_cache_policy:snapshot_revision_counter(Entry).

snapshot_watermark_counter() ->
    case ets:lookup(?META_TABLE, sync_counter) of
        [{_, C}] when is_integer(C) -> C;
        [] -> 0
    end.

snapshot_page_revision_ceiling(Entries, PeerSt) ->
    imboy_cache_policy:snapshot_page_revision_ceiling(Entries, PeerSt).

version_request_frame(FromPid, Round) when
    is_pid(FromPid), is_binary(Round), byte_size(Round) =:= 12
->
    {cache_version_request, FromPid, node(), Round}.

send_version_report(Pid, Counter, Boot, Seq, Ready, Round) ->
    Hash = config_hash(),
    Registry = registry_hash(),
    Base =
        {cache_version_report, node(), Counter, epoch(), Hash, Registry, Boot, Ready, self(), Seq},
    Report =
        case Round of
            undefined ->
                Base;
            _ ->
                erlang:append_element(
                    erlang:append_element(Base, Round),
                    imboy_cache_sync_coverage:content_census()
                )
        end,
    true = valid_version_report(Report),
    Pid ! Report,
    Pid ! {cache_invalidation_watermark, node(), Boot, Seq, Hash, Registry},
    ok.

%% 仅由成功登记的报告调用；本轮首次目标不能被后续报告升降。
record_origin_target(Target, Round) ->
    imboy_cache_sync_coverage:record_origin_target(Target, Round).
handle_valid_report(Report, State) ->
    {cache_version_report, N, C, E, H, R, B, Ready, Pid} = imboy_cache_policy:version_report_base(
        Report
    ),
    Target = imboy_cache_policy:version_report_target(Report),
    imboy_cache_sync_recover:handle_version_report(N, C, E, H, R, B, Ready, Pid, Target, State).

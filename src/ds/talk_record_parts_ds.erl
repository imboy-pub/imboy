-module(talk_record_parts_ds).
-moduledoc "Extracted talk-record service responsibility; public entry remains talk_record_ds.".
-export([register_part/6, confirm_part/5]).
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% @doc POST /:id/captures/:cid/audio-parts（§4/§10.3/§11.1）：幂等键事务
%% （仅注册分段声明）+ 事务后签发稳定 V5 上传引用；确认后才绑定资产。
-spec register_part(integer(), integer(), integer(), map(), binary(), binary()) ->
    {ok, created | replay, map()} | {error, term()}.
register_part(Uid, RecordId, CaptureId, Validated, IdemKey, Digest) ->
    case talk_record_capture_input:valid_part_codec(maps:get(codec, Validated, undefined)) of
        true -> register_valid_part(Uid, RecordId, CaptureId, Validated, IdemKey, Digest);
        false -> {error, invalid_codec}
    end.

register_valid_part(Uid, RecordId, CaptureId, Validated, IdemKey, Digest) ->
    ExpiresAt = elib_dt:add(elib_dt:now(), {talk_record_view_ds:idempotency_ttl_seconds(), second}),
    Resource = parts_resource(RecordId, CaptureId),
    case
        elib_pg:with_tx(
            fun(Conn) ->
                part_register_tx(
                    Conn, Uid, RecordId, CaptureId, Validated, IdemKey, Digest, Resource, ExpiresAt
                )
            end,
            [{reraise, false}]
        )
    of
        {ok, Mark, Payload} ->
            part_upload_payload(Uid, RecordId, CaptureId, Mark, Payload);
        {rollback, {talk_record_rollback, {error, Reason}}} ->
            {error, Reason};
        {rollback, Reason} ->
            ?ERROR_LOG([talk_record_part_register_rolled_back, CaptureId, Reason]),
            {error, db_error};
        {error, Reason} ->
            {error, Reason}
    end.

part_upload_payload(Uid, RecordId, CaptureId, Mark, Payload) ->
    %% Idempotency snapshots identify the part, but its mutable upload projection
    %% must be reread; confirmed retries must not return stale null attachment_id.
    Current = elib_pg:with_conn(fun(Conn) ->
        talk_record_capture_repo:find_part_tx(
            Conn,
            RecordId,
            CaptureId,
            maps:get(<<"part_id">>, Payload)
        )
    end),
    case Current of
        {ok, Row} ->
            Part = talk_record_capture_support_ds:part_payload(Row),
            case talk_record_part_upload:prepare(Uid, RecordId, Part) of
                {ok, Upload} -> {ok, Mark, Part#{<<"upload_ref">> => Upload}};
                Error -> Error
            end;
        Error ->
            Error
    end.

-spec parts_resource(integer(), integer()) -> binary().
parts_resource(RecordId, CaptureId) ->
    <<"/api/v1/talk-records/", (integer_to_binary(RecordId))/binary, "/captures/",
        (integer_to_binary(CaptureId))/binary, "/audio-parts">>.

-spec part_register_tx(
    any(), integer(), integer(), integer(), map(), binary(), binary(), binary(), binary()
) -> {ok, created | replay, map()} | {error, term()}.
part_register_tx(Conn, Uid, RecordId, CaptureId, Validated, IdemKey, Digest, Resource, ExpiresAt) ->
    case
        talk_record_idempotency_repo:record_tx(
            Conn, Uid, <<"POST">>, Resource, IdemKey, Digest, ExpiresAt
        )
    of
        {ok, inserted} ->
            part_register_fresh(Conn, Uid, RecordId, CaptureId, Validated, IdemKey, Resource);
        {ok, existing, Row} ->
            talk_record_capture_support_ds:replay_idem_row(Row, Digest);
        {error, Reason} ->
            ?ERROR_LOG([talk_record_part_idem_failed, Uid, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

-spec part_register_fresh(any(), integer(), integer(), integer(), map(), binary(), binary()) ->
    {ok, created | replay, map()} | {error, term()}.
part_register_fresh(Conn, Uid, RecordId, CaptureId, Validated, IdemKey, Resource) ->
    case part_for_upload_tx(Conn, Uid, RecordId, CaptureId) of
        {ok, _Capture} ->
            part_gate_and_insert(Conn, Uid, RecordId, CaptureId, Validated, IdemKey, Resource);
        {error, Reason} ->
            throw({talk_record_rollback, {error, Reason}})
    end.

-spec part_gate_and_insert(any(), integer(), integer(), integer(), map(), binary(), binary()) ->
    {ok, created | replay, map()} | {error, term()}.
part_gate_and_insert(Conn, Uid, RecordId, CaptureId, Validated, IdemKey, Resource) ->
    Seq = maps:get(seq, Validated),
    StartMs = maps:get(start_ms, Validated),
    EndMs = maps:get(end_ms, Validated),
    case talk_record_capture_repo:overlap_tx(Conn, CaptureId, Seq, StartMs, EndMs) of
        {ok, 0} ->
            part_attach_and_insert(Conn, Uid, RecordId, CaptureId, Validated, IdemKey, Resource);
        {ok, _N} ->
            throw({talk_record_rollback, {error, part_time_overlap}});
        {error, Reason} ->
            ?ERROR_LOG([talk_record_part_overlap_check_failed, CaptureId, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

-spec part_attach_and_insert(any(), integer(), integer(), integer(), map(), binary(), binary()) ->
    {ok, created | replay, map()} | {error, term()}.
part_attach_and_insert(Conn, Uid, RecordId, CaptureId, Validated, IdemKey, Resource) ->
    %% 注册声明不创建资产；attachment.id 只来自 V5 真实上传确认。
    part_insert_tx(Conn, Uid, RecordId, CaptureId, Validated, IdemKey, Resource, null).

-spec part_insert_tx(
    any(), integer(), integer(), integer(), map(), binary(), binary(), integer() | null
) ->
    {ok, created | replay, map()} | {error, term()}.
part_insert_tx(Conn, Uid, RecordId, CaptureId, Validated, IdemKey, Resource, AttachmentId) ->
    case
        talk_record_capture_repo:insert_part_tx(Conn, #{
            <<"id">> => elib_tsid:generate(talk_record),
            <<"record_id">> => RecordId,
            <<"capture_id">> => CaptureId,
            <<"seq">> => maps:get(seq, Validated),
            <<"start_ms">> => maps:get(start_ms, Validated),
            <<"end_ms">> => maps:get(end_ms, Validated),
            <<"capture_offset_ms">> => maps:get(capture_offset_ms, Validated),
            <<"attachment_id">> => AttachmentId,
            <<"sha256">> => maps:get(sha256, Validated),
            <<"byte_size">> => maps:get(byte_size, Validated),
            <<"codec">> => maps:get(codec, Validated, null),
            <<"sample_rate">> => maps:get(sample_rate, Validated, null),
            <<"channels">> => maps:get(channels, Validated, null),
            <<"upload_status">> => <<"pending">>
        })
    of
        {ok, existing, ExistingRow} ->
            part_replay_tx(Conn, CaptureId, ExistingRow);
        {ok, inserted, _} ->
            finish_part_register(
                Conn, Uid, RecordId, CaptureId, maps:get(seq, Validated), IdemKey, Resource
            );
        {error, part_seq_conflict} ->
            %% 同 seq 异 sha256（§2）
            throw({talk_record_rollback, {error, part_seq_conflict}});
        {error, Reason} ->
            ?ERROR_LOG([talk_record_part_insert_failed, CaptureId, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

%% @private 同 seq 同 sha256 幂等重放：返回既有分段（上传引用随响应重签）。
-spec part_replay_tx(any(), integer(), map()) -> {ok, replay, map()}.
part_replay_tx(Conn, CaptureId, ExistingRow) ->
    PartId = maps:get(<<"id">>, ExistingRow),
    case
        talk_record_capture_repo:find_part_tx(
            Conn, maps:get(<<"record_id">>, ExistingRow), CaptureId, PartId
        )
    of
        {ok, Row} -> {ok, replay, talk_record_capture_support_ds:part_payload(Row)};
        {error, _} -> {ok, replay, talk_record_capture_support_ds:part_payload(ExistingRow)}
    end.

-spec finish_part_register(any(), integer(), integer(), integer(), integer(), binary(), binary()) ->
    {ok, created, map()} | {error, term()}.
finish_part_register(Conn, Uid, _RecordId, CaptureId, Seq, IdemKey, Resource) ->
    %% 分段写入即视为设备活跃：续租
    _ = talk_record_capture_repo:touch_lease_tx(Conn, CaptureId),
    case talk_record_capture_repo:find_part_by_seq_tx(Conn, CaptureId, Seq) of
        {ok, Row} ->
            Payload = talk_record_capture_support_ds:part_payload(Row),
            Snapshot = jsone:encode(#{<<"payload">> => Payload}),
            case
                talk_record_idempotency_repo:complete_tx(
                    Conn, Uid, <<"POST">>, Resource, IdemKey, Snapshot
                )
            of
                ok -> {ok, created, Payload};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            ?ERROR_LOG([talk_record_part_reload_failed, CaptureId, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

%% @doc POST .../audio-parts/:pid/confirm（§4/§10.3）：回执校验 →
%% 已 confirmed 幂等 200；真实 V5 会话/字节与首绑均成功才 200 confirmed；
%% 会话登记尚未可见时 202 verifying，未 PUT/错字节/墓碑均拒绝。
-spec confirm_part(integer(), integer(), integer(), integer(), map()) ->
    {ok, confirmed | verifying | replay, map()} | {error, term()}.
confirm_part(Uid, RecordId, CaptureId, PartId, Receipt) ->
    %% Object I/O is outside SQL transactions: avoid holding record/asset locks
    %% during slow storage reads. The second transaction rechecks all authority.
    Read = fun(Conn) ->
        case part_for_upload_tx(Conn, Uid, RecordId, CaptureId) of
            {ok, _} ->
                case talk_record_capture_repo:find_part_tx(Conn, RecordId, CaptureId, PartId) of
                    {ok, Part} ->
                        confirm_receipt_check(Part, Receipt),
                        {ok, Part};
                    {error, not_found} ->
                        {error, part_not_found};
                    Error ->
                        Error
                end;
            Error ->
                Error
        end
    end,
    case part_transaction(Read) of
        {ok, Part} ->
            case talk_record_part_upload:verify_upload(Uid, RecordId, Part) of
                ok ->
                    part_transaction(fun(Conn) ->
                        confirm_part_tx(Conn, Uid, RecordId, CaptureId, PartId, Receipt)
                    end);
                {error, upload_not_confirmed} ->
                    {ok, verifying, (talk_record_capture_support_ds:part_payload(Part))#{
                        <<"status">> => <<"verifying">>
                    }};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

part_transaction(Fun) ->
    case elib_pg:with_tx(Fun, [{reraise, false}]) of
        {rollback, {talk_record_rollback, Error}} -> Error;
        {rollback, _} -> {error, db_error};
        Result -> Result
    end.

-spec confirm_part_tx(any(), integer(), integer(), integer(), integer(), map()) ->
    {ok, confirmed | verifying | replay, map()} | {error, term()}.
confirm_part_tx(Conn, Uid, RecordId, CaptureId, PartId, Receipt) ->
    case part_for_upload_tx(Conn, Uid, RecordId, CaptureId) of
        {ok, _Capture} ->
            confirm_part_gated(Conn, Uid, RecordId, CaptureId, PartId, Receipt);
        {error, Reason} ->
            {error, Reason}
    end.

-spec confirm_part_gated(any(), integer(), integer(), integer(), integer(), map()) ->
    {ok, confirmed | verifying | replay, map()} | {error, term()}.
confirm_part_gated(Conn, Uid, RecordId, CaptureId, PartId, Receipt) ->
    case talk_record_capture_repo:find_part_tx(Conn, RecordId, CaptureId, PartId) of
        {ok, Part} ->
            confirm_receipt_check(Part, Receipt),
            confirm_part_state(Conn, Uid, RecordId, CaptureId, Part);
        {error, not_found} ->
            {error, part_not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 回执与注册事实比对：同分段异 hash/size → 409（§4 同 seq 异 hash 拒绝）。
-spec confirm_receipt_check(map(), map()) -> ok.
confirm_receipt_check(Part, Receipt) ->
    Sha = maps:get(sha256, Receipt, maps:get(<<"sha256">>, Part)),
    Size = maps:get(byte_size, Receipt, maps:get(<<"byte_size">>, Part)),
    case {Sha =:= maps:get(<<"sha256">>, Part), Size =:= maps:get(<<"byte_size">>, Part)} of
        {true, true} ->
            ok;
        _ ->
            throw({talk_record_rollback, {error, audio_part_hash_conflict}})
    end.

confirm_part_state(Conn, Uid, RecordId, CaptureId, Part) ->
    case talk_record_part_upload:confirmed_asset_tx(Conn, Uid, RecordId, Part) of
        {ok, AttachmentId} ->
            PartId = maps:get(<<"id">>, Part),
            %% Assign and bind in one transaction. Any binding failure rolls back
            %% the part state as well; a bare metadata row is never sufficient.
            case talk_record_capture_repo:confirm_part_asset_tx(Conn, PartId, AttachmentId) of
                {ok, Mark, Row} ->
                    case
                        talk_record_attachment_adapter:bind_audio_part_tx(
                            Conn, Uid, RecordId, CaptureId, PartId, AttachmentId
                        )
                    of
                        {ok, _, _} -> {ok, Mark, talk_record_capture_support_ds:part_payload(Row)};
                        {error, Reason} -> throw({talk_record_rollback, {error, Reason}})
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% A normally ended capture accepts delayed parts; abandon is fenced forever.
%% Lock the parent before capture/part/object locks, matching record cleanup.
part_for_upload_tx(Conn, Uid, RecordId, CaptureId) ->
    case talk_record_attachment_adapter:lock_upload_tx(Conn, Uid, RecordId) of
        ok ->
            part_capture_gate_tx(Conn, Uid, RecordId, CaptureId);
        {error, forbidden} ->
            case talk_record_repo:find_active_grant(RecordId, Uid) of
                {ok, _} -> {error, forbidden};
                _ -> {error, not_found}
            end;
        Error ->
            Error
    end.

part_capture_gate_tx(Conn, Uid, RecordId, CaptureId) ->
    Sql = <<
        "SELECT c.started_by,c.lease_epoch,c.status FROM talk_record_capture c"
        " WHERE c.id=$1 AND c.record_id=$2 FOR UPDATE"
    >>,
    case elib_pg:query(Conn, Sql, [CaptureId, RecordId]) of
        {ok, [#{<<"started_by">> := Uid, <<"lease_epoch">> := Epoch}]} when Epoch > 0 ->
            {error, capture_fenced};
        {ok, [#{<<"started_by">> := Uid} = Row]} ->
            {ok, Row};
        {ok, _} ->
            {error, capture_not_found};
        Error ->
            Error
    end.

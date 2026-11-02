-module(attachment_derived_artifact).
-moduledoc "服务端派生附件：真实作业来源、先登记后写入、未知结果隔离。".
-export([
    reserve_tx/3,
    write/2,
    begin_write/1,
    finish_write/2,
    copy_staging/2,
    read/1,
    complete_tx/2,
    reconcile/0,
    read_live/2,
    with_spool/2,
    with_live_spool/3,
    metadata/1,
    with_range/4
]).

%% The caller already holds its business-parent/job locks. Ordinary metadata,
%% immutable storage identity and the write-intent commit before any storage I/O.
reserve_tx(Conn, Source, Meta) ->
    #{job_id := JobId, attempt := Attempt, actor_id := Uid} = Source,
    Key = maps:get(<<"path">>, Meta),
    {ok, Location} = attachment_storage:registration_location(
        maps:get(<<"scope">>, Meta),
        elib_oss:get_bucket(<<"talk_record">>),
        Key
    ),
    Attach = maps:merge(Meta, storage_columns(Location)),
    case attachment_repo:confirm_if_absent_tx(Conn, elib_dt:now(), Uid, Attach) of
        {ok, Aid, true} ->
            Sql = <<
                "INSERT INTO attachment_derived_write(attachment_id,source_job_id,source_attempt)"
                " VALUES($1,$2,$3)"
            >>,
            case elib_pg:execute(Conn, Sql, [Aid, JobId, Attempt]) of
                {ok, 1} -> {ok, Aid};
                Error -> Error
            end;
        {ok, _, false} ->
            {error, artifact_identity_reused};
        Error ->
            Error
    end.

write(Aid, Bytes) ->
    case elib_pg:with_tx(fun(C) -> begin_write_tx(C, Aid) end, [{reraise, false}]) of
        {ok, I} ->
            Result = put_bytes(I, Bytes),
            %% A failure is not proof that no bytes reached object storage.
            State =
                case Result of
                    ok -> <<"complete">>;
                    _ -> <<"unknown">>
                end,
            case
                elib_pg:execute(
                    <<
                        "UPDATE attachment_derived_write SET state=$2,updated_at=now()"
                        " WHERE attachment_id=$1 AND state='writing'"
                    >>,
                    [Aid, State]
                )
            of
                {ok, 1} -> Result;
                _ -> {error, artifact_write_quarantined}
            end;
        Error ->
            Error
    end.

%% ===== V3-18 P2：Worker 写链拆分（begin → Worker staging upload → Copy → finish） =====

%% begin_write/1：reserved→writing + 返回冻结位置（含凭据快照给 Worker）。
begin_write(Aid) ->
    elib_pg:with_tx(fun(C) -> begin_write_tx(C, Aid) end, [{reraise, false}]).

%% finish_write/2：Worker 结果收口（complete/unknown——与 write/2 同语义：
%% 上传失败不是"无字节到达"的证明）。
finish_write(Aid, Result) ->
    State =
        case Result of
            {ok, _} -> <<"complete">>;
            _ -> <<"unknown">>
        end,
    case
        elib_pg:execute(
            <<
                "UPDATE attachment_derived_write SET state=$2,updated_at=now()"
                " WHERE attachment_id=$1 AND state='writing'"
            >>,
            [Aid, State]
        )
    of
        {ok, 1} -> Result;
        _ -> {error, artifact_write_quarantined}
    end.

%% copy_staging/2：服务端 CopyObject（staging 物理键 → 工件物理键），
%% 零字节过 BEAM（与 direct finalize 的 copy_dispatch 同一原语）。
copy_staging(#{namespace := Ns, container := C, key := DstKey}, StagingKey) ->
    attachment_storage_garage:copy_dispatch(Ns, C, StagingKey, DstKey).

begin_write_tx(Conn, Aid) ->
    case
        elib_pg:query(
            Conn,
            <<
                "SELECT a.* FROM attachment a JOIN attachment_derived_write w"
                " ON w.attachment_id=a.id WHERE a.id=$1 AND a.lifecycle_state='live'"
                " AND w.state='reserved' FOR UPDATE OF a,w"
            >>,
            [Aid]
        )
    of
        {ok, [Row]} ->
            case location(Row) of
                {ok, I} ->
                    {ok, 1} = elib_pg:execute(
                        Conn,
                        <<
                            "UPDATE attachment_derived_write"
                            " SET state='writing',updated_at=now() WHERE attachment_id=$1"
                        >>,
                        [Aid]
                    ),
                    {ok, I};
                Error ->
                    Error
            end;
        {ok, []} ->
            {error, artifact_write_not_reserved};
        Error ->
            Error
    end.

%% V3-20 退役：disk root fail-closed。
put_bytes(#{driver := disk}, _Bytes) ->
    {error, disk_driver_retired};
put_bytes(#{driver := garage} = I, Bytes) ->
    Now = calendar:universal_time(),
    Exp = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Now) + 30),
    attachment_storage_garage:put_snapshot(I, Bytes, maps:get(mime, I), #{
        issued_at => Now, expires_at => Exp
    }).

read(Aid) ->
    case
        elib_pg:query(
            <<
                "SELECT a.* FROM attachment a JOIN attachment_derived_write w"
                " ON w.attachment_id=a.id WHERE a.id=$1 AND a.lifecycle_state='live'"
                " AND w.state='complete'"
            >>,
            [Aid]
        )
    of
        {ok, [Row]} -> verify_read(Row, read_location(location(Row)));
        {ok, []} -> {error, artifact_unavailable};
        Error -> Error
    end.

%% Caller must authorize the business record/processing lease first. No storage
%% bytes reach the callback until frozen location, live state, size/hash all pass.
with_spool(Aid, Fun) ->
    Sql = <<
        "SELECT a.* FROM attachment a JOIN attachment_derived_write w"
        " ON w.attachment_id=a.id WHERE a.id=$1 AND a.lifecycle_state='live'"
        " AND w.state='complete' AND a.size>0 AND a.size<=134217728"
    >>,
    with_row(Sql, [Aid], 134217728, Fun).

metadata(Aid) ->
    case
        elib_pg:query(
            <<
                "SELECT a.* FROM attachment a JOIN attachment_derived_write w"
                " ON w.attachment_id=a.id WHERE a.id=$1 AND a.lifecycle_state='live'"
                " AND w.state='complete' AND a.size>0 AND a.size<=134217728"
            >>,
            [Aid]
        )
    of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, artifact_unavailable};
        Error -> Error
    end.

%% V3-18 P3 收口登记：talk_record 回放/导出下载的 HTTP 正文已切边缘承载
%% （attachment_edge_redirect）；attachment_artifact_reader:with_range/
%% range_spool 剩余消费者仅本模块 with_range（派生工件受控小段读）与
%% with_live_spool（≤Max 全量受控读）——均为有界控制面读，不再有全量
%% 正文过 BEAM 的 HTTP 入口。
with_range(Aid, Offset, Length, Fun) ->
    case metadata(Aid) of
        {ok, Row} ->
            attachment_artifact_reader:with_range(Row, Offset, Length, fun(Source) ->
                case metadata(Aid) of
                    {ok, Row} -> Fun(Source);
                    _ -> {error, artifact_unavailable}
                end
            end);
        Error ->
            Error
    end.

with_live_spool(Aid, Max, Fun) when is_integer(Max), Max > 0 ->
    Sql = <<
        "SELECT * FROM attachment WHERE id=$1 AND lifecycle_state='live'"
        " AND size>0 AND size<=$2"
    >>,
    with_row(Sql, [Aid, Max], Max, Fun).

with_row(Sql, Args, Max, Fun) ->
    case elib_pg:query(Sql, Args) of
        {ok, [Row]} ->
            attachment_artifact_reader:with_source(Row, Max, fun(Source) ->
                case elib_pg:query(Sql, Args) of
                    {ok, [Row]} -> Fun(Source);
                    {ok, _} -> {error, artifact_unavailable};
                    Error -> Error
                end
            end);
        {ok, []} ->
            {error, artifact_unavailable};
        Error ->
            Error
    end.

%% Legacy small callers may request bytes; audio processing uses with_live_spool.
read_live(Aid, Max) -> with_live_spool(Aid, Max, fun(Source) -> read_spool(Source, 0, []) end).
read_spool(Source, Offset, Acc) ->
    case attachment_upload_spool:read(Source, Offset) of
        {ok, B} -> read_spool(Source, Offset + byte_size(B), [B | Acc]);
        eof -> {ok, iolist_to_binary(lists:reverse(Acc))};
        Error -> Error
    end.

read_location({ok, #{driver := disk}}) -> {error, disk_driver_retired};
read_location({ok, #{driver := garage} = I}) -> attachment_storage_garage:read_snapshot(I);
read_location(Error) -> Error.

complete_tx(Conn, Aid) ->
    case
        elib_pg:query(
            Conn,
            <<
                "SELECT 1 FROM attachment_derived_write WHERE attachment_id=$1"
                " AND state='complete'"
            >>,
            [Aid]
        )
    of
        {ok, [_]} -> ok;
        {ok, []} -> {error, artifact_not_complete};
        Error -> Error
    end.

location(Row) ->
    attachment_storage:frozen_location(maps:get(<<"path">>, Row), Row).

storage_columns(#{driver := D, namespace := N, container := C, key := K, locator_digest := L}) ->
    #{
        <<"storage_driver">> => D,
        <<"storage_namespace">> => N,
        <<"storage_container">> => C,
        <<"storage_physical_key">> => K,
        <<"storage_locator_sha256">> => L
    }.

verify_read(Row, {ok, Bytes}) ->
    Hash = binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
    case
        byte_size(Bytes) =:= maps:get(<<"size">>, Row) andalso
            Hash =:= maps:get(<<"file_hash256">>, Row)
    of
        true -> {ok, Bytes};
        false -> {error, artifact_integrity_mismatch}
    end;
verify_read(_Row, Error) ->
    Error.

%% A stopped lease does not establish that remote I/O stopped. Uncertain writes
%% are explicitly quarantined; only never-dispatched reservations can be retired.
reconcile() ->
    elib_pg:execute(
        <<
            "UPDATE attachment_derived_write w SET state=CASE"
            " WHEN w.state='reserved' THEN 'abandoned' ELSE 'unknown' END,updated_at=now()"
            " WHERE w.state IN ('reserved','writing') AND NOT EXISTS(SELECT 1"
            " FROM talk_record_job j WHERE j.id=w.source_job_id AND j.state='running'"
            " AND j.attempt=w.source_attempt AND j.lease_until>clock_timestamp())"
        >>,
        []
    ).

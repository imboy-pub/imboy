-module(talk_record_capture_repo).
-moduledoc "面聊采集会话仓储（T07：capture/capture_event/audio_part；迁移 00000211+00000213）。".
%%%
% talk_record 采集域数据仓库层（capture / capture_event / audio_part）。
%
% 关键不变量（设计 §2/§3/§11.1，CONTRACT-FREEZE-T02 §2）：
%   * 活跃唯一：同记录/同设备各最多一条活跃 capture（部分唯一索引
%     uq_trc_record_active / uq_trc_device_active，活跃集合=未 ended）；
%   * 事件幂等与顺序：UNIQUE(capture_id,client_event_id) 去重、
%     UNIQUE(capture_id,seq) 顺序冲突；状态机由全量事件按 seq 折叠重放；
%   * 分段：UNIQUE(capture_id,seq)，同 seq 异 sha256 冲突；时间轴为有效音频
%     时间轴（start/end/capture_offset），段间不得重叠、不得为空段；
%   * 失联 fencing：lease_epoch 仅 abandon 推进；ended+epoch>0 的写一律
%     capture_fenced（§11.1）；lease 过期不证明停录，不阻止写。
%
% 上传声明不物化 attachment；真实 V5 确认后才赋 attachment_id 并首绑。
% T06 adapter 在确认事务内检查真实 V5 session + 资产/墓碑并首绑。
% 未上传时 attachment_id=NULL，禁止根据声明构造 attachment 元数据。
%%%

-export([tablename/0]).
-export([insert_capture_tx/2]).
-export([find_tx/4, find_by_record_device_tx/3, find_recent_by_record_tx/2]).
-export([active_conflict_tx/4]).
-export([insert_event_tx/2, list_events_tx/2, find_event_tx/3, find_event_by_seq_tx/3]).
-export([apply_status_tx/5]).
-export([touch_lease_tx/2]).
-export([bump_epoch_end_tx/3]).
-export([insert_part_tx/2, find_part_tx/4, find_part_by_seq_tx/3]).
-export([confirm_part_tx/3, parts_summary_tx/2, list_parts_tx/3]).
-export([overlap_tx/5]).
-export([confirm_part_asset_tx/3]).
-export([update_record_projection_tx/4]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

-define(CAPTURE_COLUMNS,
    <<"c.id,c.record_id,c.device_session_id,c.device_id,c.started_by,c.status,",
        "c.reported_started_at,c.server_started_at,c.ended_at,c.duration_ms,",
        "c.revision,c.last_seen_at,c.lease_epoch,c.lease_until,c.created_at,c.updated_at">>
).
-define(PART_COLUMNS,
    <<"p.id,p.record_id,p.capture_id,p.seq,p.start_ms,p.end_ms,p.capture_offset_ms,",
        "p.attachment_id,p.sha256,p.byte_size,p.codec,p.sample_rate,p.channels,",
        "p.upload_status,p.created_at,p.updated_at">>
).

%% ===================================================================
%% capture
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"talk_record_capture">>).

%% @doc 事务内插入 capture（id 由 DS 生成）。唯一键冲突统一收敛为
%% {error, unique_violation}（含 record+device_session 重放与活跃唯一索引）。
-spec insert_capture_tx(any(), map()) -> {ok, integer()} | {error, unique_violation | term()}.
insert_capture_tx(Conn, Data) ->
    Id = maps:get(<<"id">>, Data),
    {Sql0, Params} = elib_pg_sql:insert(tablename(), Data),
    Sql = <<Sql0/binary, " ON CONFLICT DO NOTHING">>,
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, 1} ->
            {ok, Id};
        {ok, 0} ->
            {error, unique_violation};
        {error, Reason} ->
            case sqlstate(Reason) of
                <<"23505">> -> {error, unique_violation};
                _ -> {error, Reason}
            end
    end.

%% @doc 事务内按 (record_id, capture_id) 复合键取 capture（§8：父/子 ID 同时匹配）。
-spec find_tx(any(), integer(), integer(), binary()) -> {ok, map()} | {error, not_found | term()}.
find_tx(Conn, RecordId, CaptureId, Columns) ->
    Sql =
        <<"SELECT ", Columns/binary, " FROM talk_record_capture c",
            " WHERE c.record_id = $1 AND c.id = $2">>,
    case elib_pg:query(Conn, Sql, [RecordId, CaptureId]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 业务键查找（record_id + device_session_id 离线重放，§11.1）。
-spec find_by_record_device_tx(any(), integer(), binary()) ->
    {ok, map()} | {error, not_found | term()}.
find_by_record_device_tx(Conn, RecordId, DeviceSessionId) ->
    Sql =
        <<"SELECT ", ?CAPTURE_COLUMNS/binary, " FROM talk_record_capture c",
            " WHERE c.record_id = $1 AND c.device_session_id = $2">>,
    case elib_pg:query(Conn, Sql, [RecordId, DeviceSessionId]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 记录的 capture 清单（活跃在前，其后按 created_at 倒序；§4 恢复语义）。
-spec find_recent_by_record_tx(any(), integer()) -> {ok, [map()]} | {error, term()}.
find_recent_by_record_tx(Conn, RecordId) ->
    Sql =
        <<"SELECT ", ?CAPTURE_COLUMNS/binary,
            ",a.manifest_hash AS completion_manifest_hash FROM talk_record_capture c",
            " LEFT JOIN talk_record_assembly a ON a.record_id=c.record_id AND a.capture_id=c.id",
            " WHERE c.record_id = $1", " ORDER BY (c.status <> 'draft' AND c.status <> 'recording'",
            "   AND c.status <> 'paused' AND c.status <> 'interrupted') ASC,",
            "   c.created_at DESC, c.id DESC">>,
    elib_pg:query(Conn, Sql, [RecordId]).

%% @doc 活跃唯一预检（§4 POST captures 409 判据）：同记录活跃 → {ok, record}；
%% 同设备活跃（任意记录）→ {ok, device}；无冲突 → {ok, none}。
%% ExcludeCaptureId 用于更新场景排除自身（V1 未用，传 undefined）。
-spec active_conflict_tx(any(), integer(), binary(), integer() | undefined) ->
    {ok, none | record | device} | {error, term()}.
active_conflict_tx(Conn, RecordId, DeviceId, ExcludeCaptureId) ->
    Exclude = exclude_id(ExcludeCaptureId),
    Sql =
        <<"SELECT", "  EXISTS (SELECT 1 FROM talk_record_capture r",
            "   WHERE r.record_id = $1 AND r.id <> $3",
            "     AND r.status IN ('draft','recording','paused','interrupted')) AS on_record,",
            "  EXISTS (SELECT 1 FROM talk_record_capture d",
            "   WHERE d.device_id = $2 AND d.id <> $3",
            "     AND d.status IN ('draft','recording','paused','interrupted')) AS on_device">>,
    case elib_pg:query(Conn, Sql, [RecordId, DeviceId, Exclude]) of
        {ok, [#{<<"on_record">> := true}]} -> {ok, record};
        {ok, [#{<<"on_device">> := true}]} -> {ok, device};
        {ok, [_]} -> {ok, none};
        {error, Reason} -> {error, Reason}
    end.

-spec exclude_id(integer() | undefined) -> integer().
exclude_id(undefined) -> 0;
exclude_id(Id) -> Id.

%% ===================================================================
%% capture_event
%% ===================================================================

%% @doc 事务内追加事件：
%%   * 同 (capture_id, client_event_id) → {ok, existing, Row}（幂等重放，§4）；
%%   * 同 (capture_id, seq) 异 client_event_id → {error, event_seq_conflict}；
%%   * 新事件 → {ok, inserted, Row}。
-spec insert_event_tx(any(), map()) ->
    {ok, inserted, map()} | {ok, existing, map()} | {error, event_seq_conflict | term()}.
insert_event_tx(Conn, Data) ->
    %% 事件表有两个唯一键（client_event_id 幂等 / seq 顺序），ON CONFLICT 只能
    %% 指定一个目标——命中另一键会 abort 事务使后续探测失效。故插入前先显式
    %% 双查（同事务 READ COMMITTED 下足够；残余竞态由 23505 收敛为 seq 冲突）。
    CaptureId = maps:get(<<"capture_id">>, Data),
    case find_event_tx(Conn, CaptureId, maps:get(<<"client_event_id">>, Data)) of
        {ok, Row} ->
            {ok, existing, Row};
        {error, not_found} ->
            case find_event_by_seq_tx(Conn, CaptureId, maps:get(<<"seq">>, Data)) of
                {ok, _Other} ->
                    {error, event_seq_conflict};
                {error, not_found} ->
                    insert_event_row_tx(Conn, Data);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec insert_event_row_tx(any(), map()) ->
    {ok, inserted, map()} | {error, event_seq_conflict | term()}.
insert_event_row_tx(Conn, Data) ->
    Insert =
        <<"INSERT INTO talk_record_capture_event",
            " (id, record_id, capture_id, device_session_id, client_event_id, seq,",
            "  event, position_ms, reported_at, received_at)",
            " VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10)">>,
    Params = [
        maps:get(<<"id">>, Data),
        maps:get(<<"record_id">>, Data),
        maps:get(<<"capture_id">>, Data),
        maps:get(<<"device_session_id">>, Data),
        maps:get(<<"client_event_id">>, Data),
        maps:get(<<"seq">>, Data),
        maps:get(<<"event">>, Data),
        maps:get(<<"position_ms">>, Data),
        maps:get(<<"reported_at">>, Data),
        elib_dt:now()
    ],
    case elib_pg:execute(Conn, Insert, Params) of
        {ok, 1} ->
            {ok, inserted, #{}};
        {error, Reason} ->
            case sqlstate(Reason) of
                <<"23505">> -> {error, event_seq_conflict};
                _ -> {error, Reason}
            end;
        Other ->
            {error, Other}
    end.

-spec find_event_by_seq_tx(any(), integer(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
find_event_by_seq_tx(Conn, CaptureId, Seq) ->
    Sql = <<"SELECT * FROM talk_record_capture_event", " WHERE capture_id = $1 AND seq = $2">>,
    case elib_pg:query(Conn, Sql, [CaptureId, Seq]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec find_event_tx(any(), integer(), binary()) -> {ok, map()} | {error, not_found | term()}.
find_event_tx(Conn, CaptureId, ClientEventId) ->
    Sql =
        <<"SELECT * FROM talk_record_capture_event",
            " WHERE capture_id = $1 AND client_event_id = $2">>,
    case elib_pg:query(Conn, Sql, [CaptureId, ClientEventId]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 按 seq 正序取全量事件（状态机折叠重放输入）。
-spec list_events_tx(any(), integer()) -> {ok, [map()]} | {error, term()}.
list_events_tx(Conn, CaptureId) ->
    Sql =
        <<"SELECT seq, event, position_ms FROM talk_record_capture_event",
            " WHERE capture_id = $1 ORDER BY seq ASC">>,
    elib_pg:query(Conn, Sql, [CaptureId]).

%% @doc 事件折叠后落状态投影（§3/§11.2）：status、revision+1、last_seen、
%% lease 续租、duration_ms 取 GREATEST(当前, 折叠最大 position_ms)、
%% 首次进入 ended 时补 ended_at。
-spec apply_status_tx(any(), integer(), integer(), binary(), integer()) ->
    {ok, [map()]} | {error, term()}.
apply_status_tx(Conn, RecordId, CaptureId, NewStatus, MaxPos) ->
    Now = elib_dt:now(),
    LeaseUntil = elib_dt:add(Now, {lease_seconds(), second}),
    Sql =
        <<"UPDATE talk_record_capture c SET",
            " status = $3, revision = revision + 1, last_seen_at = $4, lease_until = $5,",
            " duration_ms = GREATEST(c.duration_ms, $6), updated_at = $4,",
            " ended_at = CASE WHEN $3 = 'ended' AND c.ended_at IS NULL THEN $4 ELSE c.ended_at END",
            " WHERE c.record_id = $1 AND c.id = $2", " RETURNING ", ?CAPTURE_COLUMNS/binary>>,
    elib_pg:query(Conn, Sql, [RecordId, CaptureId, NewStatus, Now, LeaseUntil, MaxPos]).

%% @private lease 续租时长（秒；config 可覆写）。
-spec lease_seconds() -> pos_integer().
lease_seconds() ->
    case config_ds:env(imboy, talk_record_capture_lease_seconds, 300) of
        N when is_integer(N), N > 0 -> N;
        _ -> 300
    end.

%% @doc 仅续租（recover/confirm 等非事件写路径）：last_seen + lease_until + revision。
-spec touch_lease_tx(any(), integer()) -> {ok, [map()]} | {error, term()}.
touch_lease_tx(Conn, CaptureId) ->
    Now = elib_dt:now(),
    LeaseUntil = elib_dt:add(Now, {lease_seconds(), second}),
    Sql =
        <<"UPDATE talk_record_capture c SET",
            " revision = revision + 1, last_seen_at = $2, lease_until = $3, updated_at = $2",
            " WHERE c.id = $1", " RETURNING ", ?CAPTURE_COLUMNS/binary>>,
    elib_pg:query(Conn, Sql, [CaptureId, Now, LeaseUntil]).

%% @doc abandon（§11.1）：lease_epoch+1 冻结原会话 + status=ended + ended_at。
%% 旧设备后续写凭 ended + epoch>0 判 capture_fenced。
-spec bump_epoch_end_tx(any(), integer(), pos_integer()) -> {ok, map()} | {error, term()}.
bump_epoch_end_tx(Conn, CaptureId, ExpectedRevision) ->
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE talk_record_capture c SET",
            " lease_epoch = lease_epoch + 1, status = 'ended', ended_at = $3,",
            " revision = revision + 1, last_seen_at = $2, updated_at = $2",
            " WHERE c.id = $1 AND c.revision = $4", " RETURNING ", ?CAPTURE_COLUMNS/binary>>,
    case elib_pg:query(Conn, Sql, [CaptureId, Now, Now, ExpectedRevision]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% audio_part
%% ===================================================================

%% @doc 事务内注册分段：
%%   * 同 (capture_id, seq) 同 sha256 → {ok, existing, Row}（幂等重放）；
%%   * 同 seq 异 sha256 → {error, part_seq_conflict}（§2）；
%%   * 新分段 → {ok, inserted, Row}。
-spec insert_part_tx(any(), map()) ->
    {ok, inserted, map()} | {ok, existing, map()} | {error, part_seq_conflict | term()}.
insert_part_tx(Conn, Data) ->
    Insert =
        <<"INSERT INTO talk_record_audio_part",
            " (id, record_id, capture_id, seq, start_ms, end_ms, capture_offset_ms,",
            "  captured_wall_time, attachment_id, sha256, byte_size, codec,",
            "  sample_rate, channels, upload_status)",
            " VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15)",
            " ON CONFLICT (capture_id, seq) DO NOTHING">>,
    Params = [
        maps:get(<<"id">>, Data),
        maps:get(<<"record_id">>, Data),
        maps:get(<<"capture_id">>, Data),
        maps:get(<<"seq">>, Data),
        maps:get(<<"start_ms">>, Data),
        maps:get(<<"end_ms">>, Data),
        maps:get(<<"capture_offset_ms">>, Data),
        maps:get(<<"captured_wall_time">>, Data, null),
        maps:get(<<"attachment_id">>, Data),
        maps:get(<<"sha256">>, Data),
        maps:get(<<"byte_size">>, Data),
        maps:get(<<"codec">>, Data, null),
        maps:get(<<"sample_rate">>, Data, null),
        maps:get(<<"channels">>, Data, null),
        maps:get(<<"upload_status">>, Data, <<"pending">>)
    ],
    case execute_replayable_insert(Conn, Insert, Params) of
        {ok, 1} ->
            {ok, inserted, #{}};
        {ok, 0} ->
            part_replay_or_conflict(Conn, Data);
        {error, Reason} ->
            case sqlstate(Reason) of
                <<"23505">> -> part_replay_or_conflict(Conn, Data);
                _ -> {error, Reason}
            end
    end.

-spec part_replay_or_conflict(any(), map()) ->
    {ok, existing, map()} | {error, part_seq_conflict | term()}.
part_replay_or_conflict(Conn, Data) ->
    CaptureId = maps:get(<<"capture_id">>, Data),
    case find_part_by_seq_tx(Conn, CaptureId, maps:get(<<"seq">>, Data)) of
        {ok, Row} ->
            case maps:get(<<"sha256">>, Row) =:= maps:get(<<"sha256">>, Data) of
                true -> {ok, existing, Row};
                false -> {error, part_seq_conflict}
            end;
        {error, not_found} ->
            {error, part_seq_conflict};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内按复合键取分段（record+capture+part 三级同时匹配，§8）。
-spec find_part_tx(any(), integer(), integer(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
find_part_tx(Conn, RecordId, CaptureId, PartId) ->
    Sql =
        <<"SELECT ", ?PART_COLUMNS/binary, " FROM talk_record_audio_part p",
            " WHERE p.record_id = $1 AND p.capture_id = $2 AND p.id = $3">>,
    case elib_pg:query(Conn, Sql, [RecordId, CaptureId, PartId]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec find_part_by_seq_tx(any(), integer(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
find_part_by_seq_tx(Conn, CaptureId, Seq) ->
    Sql =
        <<"SELECT ", ?PART_COLUMNS/binary, " FROM talk_record_audio_part p",
            " WHERE p.capture_id = $1 AND p.seq = $2">>,
    case elib_pg:query(Conn, Sql, [CaptureId, Seq]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc confirm 回执：pending/uploading/uploaded → confirmed（CAS 单次迁移）；
%% 已 confirmed → {ok, already, Row}（重复同 hash 安全重放，§4）。
%% Real asset identity comes only from the confirmed V5 upload session.
confirm_part_asset_tx(Conn, PartId, AttachmentId) ->
    Sql = <<
        "UPDATE talk_record_audio_part p SET attachment_id=$2,"
        " upload_status='confirmed',updated_at=now() WHERE p.id=$1"
        " AND p.upload_status IN ('pending','uploading','uploaded')"
        " RETURNING ",
        ?PART_COLUMNS/binary
    >>,
    case elib_pg:query(Conn, Sql, [PartId, AttachmentId]) of
        {ok, [Row]} ->
            {ok, confirmed, Row};
        {ok, []} ->
            case find_part_by_id(Conn, PartId) of
                {ok,
                    #{
                        <<"attachment_id">> := AttachmentId,
                        <<"upload_status">> := <<"confirmed">>
                    } = Row} ->
                    {ok, replay, Row};
                {ok, _} ->
                    {error, part_state_conflict};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec confirm_part_tx(any(), integer(), binary()) ->
    {ok, map()} | {ok, already, map()} | {error, term()}.
confirm_part_tx(Conn, PartId, Sha256) ->
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE talk_record_audio_part p SET",
            " upload_status = 'confirmed', sha256 = $2, updated_at = $3",
            " WHERE p.id = $1 AND p.upload_status IN ('pending','uploading','uploaded')",
            " RETURNING ", ?PART_COLUMNS/binary>>,
    case elib_pg:query(Conn, Sql, [PartId, Sha256, Now]) of
        {ok, [Row]} ->
            {ok, Row};
        {ok, []} ->
            case find_part_by_id(Conn, PartId) of
                {ok, #{<<"upload_status">> := <<"confirmed">>} = Row} -> {ok, already, Row};
                {ok, _} -> {error, part_state_conflict};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec find_part_by_id(any(), integer()) -> {ok, map()} | {error, not_found | term()}.
find_part_by_id(Conn, PartId) ->
    Sql = <<"SELECT ", ?PART_COLUMNS/binary, " FROM talk_record_audio_part p WHERE p.id = $1">>,
    case elib_pg:query(Conn, Sql, [PartId]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 分段汇总（恢复视图：confirmed 数 / pending 数 / 最大 seq / 有效时长）。
-spec parts_summary_tx(any(), integer()) -> {ok, map()} | {error, term()}.
parts_summary_tx(Conn, CaptureId) ->
    Sql =
        <<"SELECT", " COUNT(*) FILTER (WHERE p.upload_status = 'confirmed') AS confirmed,",
            " COUNT(*) FILTER (WHERE p.upload_status <> 'confirmed') AS pending,",
            " COALESCE(MAX(p.seq), -1) AS last_seq,", " COALESCE(MAX(p.end_ms), 0) AS effective_ms",
            " FROM talk_record_audio_part p WHERE p.capture_id = $1">>,
    case elib_pg:query(Conn, Sql, [CaptureId]) of
        {ok, [Row]} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 分段清单（seq 正序；Statuses 为状态过滤列表 | all）。
-spec list_parts_tx(any(), integer(), all | [binary()]) -> {ok, [map()]} | {error, term()}.
list_parts_tx(Conn, CaptureId, all) ->
    Sql =
        <<"SELECT ", ?PART_COLUMNS/binary, " FROM talk_record_audio_part p",
            " WHERE p.capture_id = $1 ORDER BY p.seq ASC">>,
    elib_pg:query(Conn, Sql, [CaptureId]);
list_parts_tx(Conn, CaptureId, [<<>>] = _Statuses) ->
    list_parts_tx(Conn, CaptureId, all);
list_parts_tx(Conn, CaptureId, Statuses) ->
    Sql =
        <<"SELECT ", ?PART_COLUMNS/binary, " FROM talk_record_audio_part p",
            " WHERE p.capture_id = $1 AND p.upload_status = ANY($2) ORDER BY p.seq ASC">>,
    elib_pg:query(Conn, Sql, [CaptureId, Statuses]).

%% @doc 时间轴重叠检测（乱序注册安全：对既有全量分段做区间相交判断，
%% 暂停期间空段由 end>start 先行在 logic 层拒绝）。返回重叠行数。
-spec overlap_tx(any(), integer(), integer(), integer(), integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
overlap_tx(Conn, CaptureId, Seq, StartMs, EndMs) ->
    Sql =
        <<"SELECT COUNT(*) AS n FROM talk_record_audio_part p",
            " WHERE p.capture_id = $1 AND p.seq <> $2",
            "   AND p.start_ms < $4 AND p.end_ms > $3">>,
    case elib_pg:query(Conn, Sql, [CaptureId, Seq, StartMs, EndMs]) of
        {ok, [#{<<"n">> := N}]} -> {ok, N};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 记录投影（§2 active_capture_id/capture_status CAS）：
%% 不推进 record.revision（§11.2：上传进度/采集事件不得不断改 record.revision）。
-spec update_record_projection_tx(any(), integer(), integer() | null, binary()) ->
    {ok, term()} | {error, term()}.
update_record_projection_tx(Conn, RecordId, ActiveCaptureId, CaptureStatus) ->
    Sql =
        <<"UPDATE talk_record tr SET",
            " active_capture_id = $2, capture_status = $3, updated_at = $4", " WHERE tr.id = $1">>,
    elib_pg:execute(Conn, Sql, [RecordId, ActiveCaptureId, CaptureStatus, elib_dt:now()]).

%% ===================================================================
%% Internal
%% ===================================================================

%% @private epgsql 错误形态 → SQLSTATE（与 talk_record_repo:sqlstate 同构）。
-spec sqlstate(term()) -> binary() | undefined.
sqlstate({error, Inner}) when is_tuple(Inner), tuple_size(Inner) >= 3 ->
    sqlstate(Inner);
sqlstate(Error) when is_tuple(Error), tuple_size(Error) >= 3, element(1, Error) =:= error ->
    case element(3, Error) of
        Code when is_binary(Code), byte_size(Code) =:= 5 -> Code;
        _ -> undefined
    end;
sqlstate(_) ->
    undefined.

%% Only the expected unique violation is recovered before business-key reads.
%% Other errors remain aborting and must be rolled back by the transaction owner.
execute_replayable_insert(Conn, Sql, Params) ->
    Savepoint = <<"talk_replay_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    case elib_pg:query(Conn, [<<"SAVEPOINT ">>, Savepoint], []) of
        {ok, _} ->
            Result = elib_pg:execute(Conn, Sql, Params),
            case Result of
                {error, Reason} ->
                    case sqlstate(Reason) of
                        <<"23505">> ->
                            case
                                elib_pg:query(Conn, [<<"ROLLBACK TO SAVEPOINT ">>, Savepoint], [])
                            of
                                {ok, _} -> release_insert_savepoint(Conn, Savepoint, Result);
                                {error, _} = Error -> Error
                            end;
                        _ ->
                            Result
                    end;
                _ ->
                    release_insert_savepoint(Conn, Savepoint, Result)
            end;
        {error, _} = Error ->
            Error
    end.

release_insert_savepoint(Conn, Savepoint, Result) ->
    case elib_pg:query(Conn, [<<"RELEASE SAVEPOINT ">>, Savepoint], []) of
        {ok, _} -> Result;
        {error, _} = Error -> Error
    end.

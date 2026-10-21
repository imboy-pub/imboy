-module(talk_record_capture_support_ds).
-moduledoc "Extracted talk-record service responsibility; public entry remains talk_record_ds.".
-export([
    capture_for_write_tx/4,
    capture_for_owner_tx/4,
    capture_lease_seconds/0,
    capture_columns/0,
    capture_payload/2,
    part_payload/1,
    replay_idem_row/2
]).
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% -------------------------------------------------------------------
%% T07 内部：写入门禁 / DTO
%% -------------------------------------------------------------------

%% @private 采集写入门禁（events/audio-parts/confirm 共用，owner-only：
%% BACKEND-API §4 "owner/合法 capture session 才可调用"，editor 的校对权
%% 不含采集写）：Uid == record.owner_user_id 才放行；非 owner：无读权限
%% 统一 404、有读权限（活跃 grant）403 forbidden。fenced（ended+epoch>0）
%% 409 capture_fenced；ended 409 capture_ended。lease 过期不阻断（§11.1）。
-spec capture_for_write_tx(any(), integer(), integer(), integer()) ->
    {ok, map()} | {error, atom()}.
capture_for_write_tx(Conn, Uid, RecordId, CaptureId) ->
    case
        talk_record_repo:find_by_id_tx(Conn, RecordId, <<"tr.id, tr.owner_user_id, tr.deleted_at">>)
    of
        {ok, #{<<"deleted_at">> := null} = Row} ->
            capture_write_role(Row, Uid, Conn, RecordId, CaptureId);
        {ok, _} ->
            {error, not_found};
        {error, not_found} ->
            {error, not_found};
        {error, Reason} ->
            ?ERROR_LOG([talk_record_capture_gate_failed, RecordId, Reason]),
            {error, db_error}
    end.

-spec capture_write_role(map(), integer(), any(), integer(), integer()) ->
    {ok, map()} | {error, atom()}.
%% P1 修复（Q-review-T07）：首子句必须显式比对 owner——原实现绑定
%% _OwnerUid 未比较即放行，grant 检查子句成死代码（任意认证用户可写）。
capture_write_role(#{<<"owner_user_id">> := OwnerUid}, Uid, Conn, RecordId, CaptureId) when
    OwnerUid =:= Uid
->
    load_fenced_capture(Conn, RecordId, CaptureId);
capture_write_role(_Row, Uid, _Conn, RecordId, _CaptureId) ->
    %% 非 owner：可见（有活跃 grant，含 editor）→ 403；不可见 → 404
    case talk_record_repo:find_active_grant(RecordId, Uid) of
        {ok, _} -> {error, forbidden};
        _ -> {error, not_found}
    end.

%% @private recover/abandon 门禁（owner-only，不做 fencing 排除——
%% ended 后两者分别给出 capture_ended/幂等语义）。
-spec capture_for_owner_tx(any(), integer(), integer(), integer()) ->
    {ok, map()} | {error, atom()}.
capture_for_owner_tx(Conn, Uid, RecordId, CaptureId) ->
    case
        talk_record_repo:find_by_id_tx(Conn, RecordId, <<"tr.id, tr.owner_user_id, tr.deleted_at">>)
    of
        {ok, #{<<"deleted_at">> := null, <<"owner_user_id">> := Uid}} ->
            load_fenced_capture(Conn, RecordId, CaptureId);
        {ok, #{<<"deleted_at">> := null}} ->
            {error, forbidden};
        {ok, _} ->
            {error, not_found};
        {error, not_found} ->
            {error, not_found};
        {error, Reason} ->
            ?ERROR_LOG([talk_record_owner_gate_failed, RecordId, Reason]),
            {error, db_error}
    end.

-spec load_fenced_capture(any(), integer(), integer()) -> {ok, map()} | {error, atom()}.
load_fenced_capture(Conn, RecordId, CaptureId) ->
    case talk_record_capture_repo:find_tx(Conn, RecordId, CaptureId, capture_columns()) of
        {ok, #{<<"status">> := <<"ended">>, <<"lease_epoch">> := Epoch}} when Epoch > 0 ->
            {error, capture_fenced};
        {ok, #{<<"status">> := <<"ended">>}} ->
            {error, capture_ended};
        {ok, Row} ->
            {ok, Row};
        {error, not_found} ->
            {error, capture_not_found};
        {error, Reason} ->
            ?ERROR_LOG([talk_record_capture_load_failed, CaptureId, Reason]),
            {error, db_error}
    end.

%% @private lease 续租时长（秒）。repo 层私有配置读取不可跨模块，DS 统一提供。
-spec capture_lease_seconds() -> pos_integer().
capture_lease_seconds() ->
    case config_ds:env(imboy, talk_record_capture_lease_seconds, 300) of
        N when is_integer(N), N > 0 -> N;
        _ -> 300
    end.

%% @private 采集列清单（repo ?CAPTURE_COLUMNS 同步点：repo 不导出宏，
%% DS 侧重复声明，改列时两处同步）。
-spec capture_columns() -> binary().
capture_columns() ->
    <<"c.id,c.record_id,c.device_session_id,c.device_id,c.started_by,c.status,",
        "c.reported_started_at,c.server_started_at,c.ended_at,c.duration_ms,",
        "c.revision,c.last_seen_at,c.lease_epoch,c.lease_until,c.created_at,c.updated_at">>.

%% @private 采集出站 DTO（§4：capture_id/revision/状态/lease）。
-spec capture_payload(map(), map()) -> map().
capture_payload(Row, _Summary) ->
    #{
        <<"capture_id">> => maps:get(<<"id">>, Row),
        <<"record_id">> => maps:get(<<"record_id">>, Row),
        <<"device_session_id">> => maps:get(<<"device_session_id">>, Row),
        <<"status">> => maps:get(<<"status">>, Row),
        <<"revision">> => maps:get(<<"revision">>, Row),
        <<"lease_epoch">> => maps:get(<<"lease_epoch">>, Row),
        <<"lease_until">> => maps:get(<<"lease_until">>, Row),
        <<"last_seen_at">> => maps:get(<<"last_seen_at">>, Row),
        <<"started_at">> => maps:get(<<"server_started_at">>, Row),
        <<"reported_started_at">> => maps:get(<<"reported_started_at">>, Row),
        <<"ended_at">> => maps:get(<<"ended_at">>, Row),
        <<"duration_ms">> => maps:get(<<"duration_ms">>, Row),
        <<"etag">> => talk_record_view_ds:etag(maps:get(<<"revision">>, Row))
    }.

%% @private 分段出站 DTO。
-spec part_payload(map()) -> map().
part_payload(Part) ->
    #{
        <<"part_id">> => maps:get(<<"id">>, Part),
        <<"capture_id">> => maps:get(<<"capture_id">>, Part),
        <<"seq">> => maps:get(<<"seq">>, Part),
        <<"start_ms">> => maps:get(<<"start_ms">>, Part),
        <<"end_ms">> => maps:get(<<"end_ms">>, Part),
        <<"capture_offset_ms">> => maps:get(<<"capture_offset_ms">>, Part),
        <<"attachment_id">> => maps:get(<<"attachment_id">>, Part),
        <<"sha256">> => maps:get(<<"sha256">>, Part),
        <<"byte_size">> => maps:get(<<"byte_size">>, Part),
        <<"codec">> => maps:get(<<"codec">>, Part),
        <<"upload_status">> => maps:get(<<"upload_status">>, Part),
        <<"etag">> => talk_record_view_ds:etag(maps:get(<<"seq">>, Part))
    }.

-spec replay_idem_row(map(), binary()) -> {ok, replay, map()} | {error, atom()}.
replay_idem_row(Row, Digest) ->
    case maps:get(<<"payload_hash">>, Row, <<>>) of
        Digest ->
            case maps:get(<<"result_ref">>, Row, null) of
                SnapBin when is_binary(SnapBin) ->
                    #{<<"payload">> := P} = jsone:decode(SnapBin),
                    {ok, replay, P};
                _ ->
                    {error, idempotency_pending}
            end;
        _ ->
            {error, idempotency_conflict}
    end.

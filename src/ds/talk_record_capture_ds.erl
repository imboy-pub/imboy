-module(talk_record_capture_ds).
-moduledoc "Extracted talk-record service responsibility; public entry remains talk_record_ds.".
-export([
    create_capture/6,
    list_captures/2,
    append_event/4,
    recover_capture/4,
    abandon_capture/4,
    fold_events/1
]).
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% T07 采集域：capture / event / audio_part 编排（导出声明见模块头）
%% ===================================================================

%% @doc POST /:id/captures（§4/§11.1）：幂等键事务（登记→owner/If-Match→
%% 活跃唯一→业务键重放→插入→记录投影→快照回填）。
%% record.revision 不因采集推进（§11.2），If-Match 校验当前 record.revision。
-spec create_capture(integer(), integer(), pos_integer(), map(), binary(), binary()) ->
    {ok, created | replay, map()} | {error, term()}.
create_capture(Uid, RecordId, ExpectedRevision, Validated, IdemKey, Digest) ->
    ExpiresAt = elib_dt:add(elib_dt:now(), {talk_record_view_ds:idempotency_ttl_seconds(), second}),
    Resource = capture_resource(RecordId),
    case
        elib_pg:with_tx(
            fun(Conn) ->
                capture_create_tx(
                    Conn,
                    Uid,
                    RecordId,
                    ExpectedRevision,
                    Validated,
                    IdemKey,
                    Digest,
                    Resource,
                    ExpiresAt
                )
            end,
            [{reraise, false}]
        )
    of
        {ok, Mark, Payload} ->
            {ok, Mark, Payload};
        {rollback, {talk_record_rollback, {error, Reason}}} ->
            {error, Reason};
        {rollback, Reason} ->
            ?ERROR_LOG([talk_record_capture_create_rolled_back, Uid, Reason]),
            {error, db_error};
        {error, Reason} ->
            {error, Reason}
    end.

-spec capture_resource(integer()) -> binary().
capture_resource(RecordId) ->
    <<"/api/v1/talk-records/", (integer_to_binary(RecordId))/binary, "/captures">>.

-spec capture_create_tx(
    any(), integer(), integer(), pos_integer(), map(), binary(), binary(), binary(), binary()
) -> {ok, created | replay, map()} | {error, term()}.
capture_create_tx(
    Conn, Uid, RecordId, ExpectedRevision, Validated, IdemKey, Digest, Resource, ExpiresAt
) ->
    case
        talk_record_idempotency_repo:record_tx(
            Conn, Uid, <<"POST">>, Resource, IdemKey, Digest, ExpiresAt
        )
    of
        {ok, inserted} ->
            capture_create_fresh(
                Conn, Uid, RecordId, ExpectedRevision, Validated, IdemKey, Resource
            );
        {ok, existing, Row} ->
            talk_record_capture_support_ds:replay_idem_row(Row, Digest);
        {error, Reason} ->
            ?ERROR_LOG([talk_record_capture_idem_failed, Uid, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

-spec capture_create_fresh(any(), integer(), integer(), pos_integer(), map(), binary(), binary()) ->
    {ok, created | replay, map()} | {error, term()}.
capture_create_fresh(Conn, Uid, RecordId, ExpectedRevision, Validated, IdemKey, Resource) ->
    GateColumns = <<"tr.id, tr.owner_user_id, tr.revision, tr.deleted_at">>,
    case talk_record_repo:find_by_id_tx(Conn, RecordId, GateColumns) of
        {ok, Row} ->
            capture_create_checked(
                Conn, Uid, RecordId, ExpectedRevision, Validated, IdemKey, Resource, Row
            );
        {error, not_found} ->
            throw({talk_record_rollback, {error, not_found}});
        {error, Reason} ->
            ?ERROR_LOG([talk_record_capture_record_load_failed, RecordId, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

-spec capture_create_checked(
    any(), integer(), integer(), pos_integer(), map(), binary(), binary(), map()
) -> {ok, created | replay, map()} | {error, term()}.
capture_create_checked(Conn, Uid, RecordId, ExpectedRevision, Validated, IdemKey, Resource, Row) ->
    case capture_create_gate(Uid, RecordId, ExpectedRevision, Row) of
        ok ->
            capture_create_insert(Conn, Uid, RecordId, Validated, IdemKey, Resource);
        {error, Reason} ->
            throw({talk_record_rollback, {error, Reason}})
    end.

%% @private 采集创建门：软删 → 404；仅 owner 可录制（可见非 owner → 403，
%% 不可见 → 404 统一不泄露）；If-Match 对 record.revision（§4）。
-spec capture_create_gate(integer(), integer(), pos_integer(), map()) -> ok | {error, atom()}.
capture_create_gate(Uid, RecordId, ExpectedRevision, Row) ->
    case maps:get(<<"deleted_at">>, Row, null) of
        null ->
            capture_create_role_gate(Uid, RecordId, ExpectedRevision, Row);
        _ ->
            {error, not_found}
    end.

-spec capture_create_role_gate(integer(), integer(), pos_integer(), map()) -> ok | {error, atom()}.
capture_create_role_gate(Uid, RecordId, ExpectedRevision, Row) ->
    case maps:get(<<"owner_user_id">>, Row) of
        Uid ->
            case maps:get(<<"revision">>, Row) of
                ExpectedRevision -> ok;
                _ -> {error, precondition_failed}
            end;
        _ ->
            %% owner 之外仍可能持有 grant：可见 → 403，不可见 → 404
            case talk_record_repo:find_active_grant(RecordId, Uid) of
                {ok, _} -> {error, forbidden};
                _ -> {error, not_found}
            end
    end.

-spec capture_create_insert(any(), integer(), integer(), map(), binary(), binary()) ->
    {ok, created | replay, map()} | {error, term()}.
capture_create_insert(Conn, Uid, RecordId, Validated, IdemKey, Resource) ->
    DeviceSessionId = maps:get(device_session_id, Validated),
    %% 业务键重放（§11.1 离线补传：同 record+device_session 重试）
    case talk_record_capture_repo:find_by_record_device_tx(Conn, RecordId, DeviceSessionId) of
        {ok, Existing} ->
            {ok, replay, talk_record_capture_support_ds:capture_payload(Existing, #{})};
        {error, not_found} ->
            capture_active_check(Conn, Uid, RecordId, Validated, IdemKey, Resource);
        {error, Reason} ->
            ?ERROR_LOG([talk_record_capture_lookup_failed, RecordId, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

-spec capture_active_check(any(), integer(), integer(), map(), binary(), binary()) ->
    {ok, created | replay, map()} | {error, term()}.
capture_active_check(Conn, Uid, RecordId, Validated, IdemKey, Resource) ->
    DeviceId = maps:get(device_id, Validated),
    case talk_record_capture_repo:active_conflict_tx(Conn, RecordId, DeviceId, undefined) of
        {ok, none} ->
            insert_new_capture(Conn, Uid, RecordId, Validated, IdemKey, Resource);
        {ok, record} ->
            throw({talk_record_rollback, {error, capture_active_on_record}});
        {ok, device} ->
            throw({talk_record_rollback, {error, capture_active_on_device}});
        {error, Reason} ->
            ?ERROR_LOG([talk_record_capture_conflict_check_failed, RecordId, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

-spec insert_new_capture(any(), integer(), integer(), map(), binary(), binary()) ->
    {ok, created, map()} | {error, term()}.
insert_new_capture(Conn, Uid, RecordId, Validated, IdemKey, Resource) ->
    Now = elib_dt:now(),
    LeaseUntil = elib_dt:add(Now, {talk_record_capture_support_ds:capture_lease_seconds(), second}),
    Data = #{
        <<"id">> => elib_tsid:generate(talk_record),
        <<"record_id">> => RecordId,
        <<"device_session_id">> => maps:get(device_session_id, Validated),
        <<"device_id">> => maps:get(device_id, Validated),
        <<"started_by">> => Uid,
        <<"status">> => <<"draft">>,
        <<"reported_started_at">> => maps:get(reported_at, Validated, null),
        <<"server_started_at">> => Now,
        <<"last_seen_at">> => Now,
        <<"lease_epoch">> => 0,
        <<"lease_until">> => LeaseUntil,
        <<"created_at">> => Now,
        <<"updated_at">> => Now
    },
    case talk_record_capture_repo:insert_capture_tx(Conn, Data) of
        {ok, CaptureId} ->
            finish_capture_create(Conn, Uid, RecordId, CaptureId, IdemKey, Resource);
        {error, unique_violation} ->
            %% 并发撞活跃唯一索引 → 409（不重放：并发窗内语义即冲突）
            throw({talk_record_rollback, {error, capture_active_conflict}});
        {error, Reason} ->
            ?ERROR_LOG([talk_record_capture_insert_failed, RecordId, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

-spec finish_capture_create(any(), integer(), integer(), integer(), binary(), binary()) ->
    {ok, created, map()} | {error, term()}.
finish_capture_create(Conn, Uid, RecordId, CaptureId, IdemKey, Resource) ->
    _ = talk_record_capture_repo:update_record_projection_tx(
        Conn, RecordId, CaptureId, <<"draft">>
    ),
    case
        talk_record_capture_repo:find_tx(
            Conn, RecordId, CaptureId, talk_record_capture_support_ds:capture_columns()
        )
    of
        {ok, Row} ->
            Payload = talk_record_capture_support_ds:capture_payload(Row, #{}),
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
            ?ERROR_LOG([talk_record_capture_reload_failed, CaptureId, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

%% @doc GET /:id/captures（恢复视图，§4/§11.1）：活跃/最近 capture + 分段状态。
-spec list_captures(integer(), integer()) -> {ok, map()} | {error, term()}.
list_captures(Uid, RecordId) ->
    case talk_record_repo:find_visible(Uid, RecordId) of
        {ok, _Row} ->
            load_capture_list(RecordId);
        {error, Reason} ->
            {error, Reason}
    end.

-spec load_capture_list(integer()) -> {ok, map()} | {error, term()}.
load_capture_list(RecordId) ->
    case
        elib_pg:with_tx(fun(Conn) ->
            case talk_record_capture_repo:find_recent_by_record_tx(Conn, RecordId) of
                {ok, Rows} ->
                    {ok, [capture_with_parts(Conn, R) || R <- Rows]};
                {error, Reason} ->
                    {error, Reason}
            end
        end)
    of
        {ok, Items} ->
            {ok, #{<<"captures">> => Items}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec capture_with_parts(any(), map()) -> map().
capture_with_parts(Conn, Row) ->
    CaptureId = maps:get(<<"id">>, Row),
    Summary =
        case talk_record_capture_repo:parts_summary_tx(Conn, CaptureId) of
            {ok, S} -> S;
            _ -> #{}
        end,
    LastSeq =
        case talk_record_capture_repo:list_events_tx(Conn, CaptureId) of
            {ok, Events} when Events =/= [] ->
                maps:get(<<"seq">>, lists:last(Events), -1);
            _ ->
                -1
        end,
    Base = talk_record_capture_support_ds:capture_payload(Row, Summary),
    Base#{
        <<"completion_manifest_hash">> => maps:get(<<"completion_manifest_hash">>, Row, null),
        <<"last_event_seq">> => LastSeq,
        <<"parts">> => #{
            <<"confirmed">> => maps:get(<<"confirmed">>, Summary, 0),
            <<"pending">> => maps:get(<<"pending">>, Summary, 0),
            <<"last_seq">> => maps:get(<<"last_seq">>, Summary, -1),
            <<"effective_ms">> => maps:get(<<"effective_ms">>, Summary, 0)
        }
    }.

%% @doc POST /:id/captures/:cid/events（§3/§4/§11.1）：
%% 幂等（client_event_id）→ fencing（fenced/ended）→ 空录音 end 拒绝 →
%% 插入（seq 冲突 409）→ 全量事件按 seq 折叠 → 状态/revision/lease 落库 →
%% 记录投影（不动 record.revision，§11.2）。
-spec append_event(integer(), integer(), integer(), map()) -> {ok, map()} | {error, term()}.
append_event(Uid, RecordId, CaptureId, Validated) ->
    case
        elib_pg:with_tx(
            fun(Conn) -> append_event_tx(Conn, Uid, RecordId, CaptureId, Validated) end,
            [{reraise, false}]
        )
    of
        {ok, Result} ->
            {ok, Result};
        {rollback, {talk_record_rollback, {error, Reason}}} ->
            {error, Reason};
        {rollback, Reason} ->
            ?ERROR_LOG([talk_record_event_rolled_back, CaptureId, Reason]),
            {error, db_error};
        {error, Reason} ->
            {error, Reason}
    end.

-spec append_event_tx(any(), integer(), integer(), integer(), map()) ->
    {ok, map()} | {error, term()}.
append_event_tx(Conn, Uid, RecordId, CaptureId, Validated) ->
    case talk_record_capture_support_ds:capture_for_write_tx(Conn, Uid, RecordId, CaptureId) of
        {ok, Capture} ->
            event_gate_and_insert(Conn, RecordId, CaptureId, Capture, Validated);
        {error, Reason} ->
            {error, Reason}
    end.

-spec event_gate_and_insert(any(), integer(), integer(), map(), map()) ->
    {ok, map()} | {error, term()}.
event_gate_and_insert(Conn, RecordId, CaptureId, Capture, Validated) ->
    Event = maps:get(event, Validated),
    case maps:get(<<"status">>, Capture) of
        <<"draft">> when Event =:= <<"end">> ->
            %% §11.1：不生成空的“已完成录音”；空采集应 abandon
            {error, invalid_event_transition};
        Status ->
            event_insert_tx(Conn, RecordId, CaptureId, Capture, Validated, Event, Status)
    end.

-spec event_insert_tx(any(), integer(), integer(), map(), map(), binary(), binary()) ->
    {ok, map()} | {error, term()}.
event_insert_tx(Conn, RecordId, CaptureId, Capture, Validated, Event, _Status) ->
    case
        talk_record_capture_repo:insert_event_tx(Conn, #{
            <<"id">> => elib_tsid:generate(talk_record),
            <<"record_id">> => RecordId,
            <<"capture_id">> => CaptureId,
            <<"device_session_id">> => maps:get(device_session_id, Validated),
            <<"client_event_id">> => maps:get(client_event_id, Validated),
            <<"seq">> => maps:get(seq, Validated),
            <<"event">> => Event,
            <<"position_ms">> => maps:get(position_ms, Validated),
            <<"reported_at">> => maps:get(reported_at, Validated, elib_dt:now())
        })
    of
        {ok, existing, _Row} ->
            %% 幂等重放：返回当前 capture 状态与 revision（不再推进）
            Replay = talk_record_capture_support_ds:capture_payload(Capture, #{}),
            {ok, Replay#{<<"idempotent_replayed">> => true}};
        {ok, inserted, _} ->
            apply_folded_status(Conn, RecordId, CaptureId);
        {error, event_seq_conflict} ->
            {error, event_seq_conflict};
        {error, Reason} ->
            ?ERROR_LOG([talk_record_event_insert_failed, CaptureId, Reason]),
            {error, db_error}
    end.

%% @private 折叠全量事件（乱序补齐安全：状态由 seq 序全量重放决定）。
-spec apply_folded_status(any(), integer(), integer()) -> {ok, map()} | {error, term()}.
apply_folded_status(Conn, RecordId, CaptureId) ->
    case talk_record_capture_repo:list_events_tx(Conn, CaptureId) of
        {ok, Events} ->
            {NewStatus, MaxPos} = fold_events(Events),
            case
                talk_record_capture_repo:apply_status_tx(
                    Conn, RecordId, CaptureId, NewStatus, MaxPos
                )
            of
                {ok, [Row]} ->
                    _ = talk_record_capture_repo:update_record_projection_tx(
                        Conn, RecordId, CaptureId, NewStatus
                    ),
                    sync_record_duration(Conn, RecordId, maps:get(<<"duration_ms">>, Row, 0)),
                    {ok, talk_record_capture_support_ds:capture_payload(Row, #{})};
                {ok, []} ->
                    {error, not_found};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 采集状态机折叠（§3）：draft→recording↔paused→ended；interrupt→
%% interrupted（resume 回 recording）。非法定跃迁跳过保留上一有效态
%% （乱序修复场景防御，正常客户端不会触发）。
-spec fold_events([map()]) -> {binary(), integer()}.
fold_events(Events) ->
    lists:foldl(
        fun(Ev, {Status, MaxPos}) ->
            Event = maps:get(<<"event">>, Ev),
            Pos = maps:get(<<"position_ms">>, Ev, 0),
            {transition(Status, Event), max(MaxPos, Pos)}
        end,
        {<<"draft">>, 0},
        Events
    ).

-spec transition(binary(), binary()) -> binary().
transition(<<"draft">>, <<"start">>) ->
    <<"recording">>;
transition(<<"recording">>, <<"pause">>) ->
    <<"paused">>;
transition(<<"recording">>, <<"interrupt">>) ->
    <<"interrupted">>;
transition(<<"paused">>, <<"interrupt">>) ->
    <<"interrupted">>;
transition(<<"paused">>, <<"resume">>) ->
    <<"recording">>;
transition(<<"interrupted">>, <<"resume">>) ->
    <<"recording">>;
transition(S, <<"end">>) when S =:= <<"recording">>; S =:= <<"paused">>; S =:= <<"interrupted">> ->
    <<"ended">>;
transition(S, _Skip) ->
    S.

%% @private 记录 duration 投影（V1 单 capture：取 capture 有效时长；不动 revision）。
-spec sync_record_duration(any(), integer(), integer()) -> ok.
sync_record_duration(Conn, RecordId, CaptureDuration) ->
    Sql =
        <<"UPDATE talk_record tr SET duration_ms = GREATEST(tr.duration_ms, $2)",
            " WHERE tr.id = $1">>,
    _ = elib_pg:execute(Conn, Sql, [RecordId, CaptureDuration]),
    ok.

%% @doc POST /:id/captures/:cid/recover（§11.1）：原 device_session_id 恢复；
%% 不同设备不接管；返回已确认分段与状态。
-spec recover_capture(integer(), integer(), integer(), binary()) -> {ok, map()} | {error, term()}.
recover_capture(Uid, RecordId, CaptureId, DeviceSessionId) ->
    case
        elib_pg:with_tx(
            fun(Conn) -> recover_tx(Conn, Uid, RecordId, CaptureId, DeviceSessionId) end,
            [{reraise, false}]
        )
    of
        {ok, Result} ->
            {ok, Result};
        {rollback, {talk_record_rollback, {error, Reason}}} ->
            {error, Reason};
        {rollback, Reason} ->
            ?ERROR_LOG([talk_record_recover_rolled_back, CaptureId, Reason]),
            {error, db_error};
        {error, Reason} ->
            {error, Reason}
    end.

-spec recover_tx(any(), integer(), integer(), integer(), binary()) -> {ok, map()} | {error, term()}.
recover_tx(Conn, Uid, RecordId, CaptureId, DeviceSessionId) ->
    case talk_record_capture_support_ds:capture_for_owner_tx(Conn, Uid, RecordId, CaptureId) of
        {ok, Capture} ->
            recover_device_gate(Capture, DeviceSessionId, Conn, CaptureId);
        {error, Reason} ->
            {error, Reason}
    end.

-spec recover_device_gate(map(), binary(), any(), integer()) -> {ok, map()} | {error, atom()}.
recover_device_gate(#{<<"device_session_id">> := Session}, Session, Conn, CaptureId) ->
    %% 原 device_session_id：恢复（续租+返回已确认分段），§11.1
    finish_recover(Conn, CaptureId);
recover_device_gate(
    #{<<"device_session_id">> := _OtherSession}, _RequestSession, _Conn, _CaptureId
) ->
    %% 不同设备不能直接接管（§11.1）
    {error, capture_device_mismatch}.

-spec finish_recover(any(), integer()) -> {ok, map()} | {error, term()}.
finish_recover(Conn, CaptureId) ->
    case talk_record_capture_repo:touch_lease_tx(Conn, CaptureId) of
        {ok, [Row]} ->
            Summary =
                case talk_record_capture_repo:parts_summary_tx(Conn, CaptureId) of
                    {ok, S} -> S;
                    _ -> #{}
                end,
            Confirmed =
                case talk_record_capture_repo:list_parts_tx(Conn, CaptureId, [<<"confirmed">>]) of
                    {ok, Parts} -> [talk_record_capture_support_ds:part_payload(P) || P <- Parts];
                    _ -> []
                end,
            Recovered0 = talk_record_capture_support_ds:capture_payload(Row, Summary),
            Recovered1 = Recovered0#{
                <<"parts">> => #{
                    <<"confirmed">> => maps:get(<<"confirmed">>, Summary, 0),
                    <<"pending">> => maps:get(<<"pending">>, Summary, 0),
                    <<"last_seq">> => maps:get(<<"last_seq">>, Summary, -1),
                    <<"effective_ms">> => maps:get(<<"effective_ms">>, Summary, 0)
                }
            },
            {ok, Recovered1#{
                <<"recovered">> => true,
                <<"confirmed_parts">> => Confirmed
            }};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc POST /:id/captures/:cid/abandon（§11.1）：owner+If-Match（capture
%% revision）+confirm；lease_epoch+1 冻结原会话；只终止服务端会话。
-spec abandon_capture(integer(), integer(), integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
abandon_capture(Uid, RecordId, CaptureId, ExpectedRevision) ->
    case
        elib_pg:with_tx(
            fun(Conn) -> abandon_tx(Conn, Uid, RecordId, CaptureId, ExpectedRevision) end,
            [{reraise, false}]
        )
    of
        {ok, Result} ->
            {ok, Result};
        {rollback, {talk_record_rollback, {error, Reason}}} ->
            {error, Reason};
        {rollback, Reason} ->
            ?ERROR_LOG([talk_record_abandon_rolled_back, CaptureId, Reason]),
            {error, db_error};
        {error, Reason} ->
            {error, Reason}
    end.

-spec abandon_tx(any(), integer(), integer(), integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
abandon_tx(Conn, Uid, RecordId, CaptureId, ExpectedRevision) ->
    case talk_record_capture_support_ds:capture_for_owner_tx(Conn, Uid, RecordId, CaptureId) of
        {ok, Capture} ->
            case maps:get(<<"revision">>, Capture) of
                ExpectedRevision ->
                    finish_abandon(Conn, RecordId, CaptureId, ExpectedRevision);
                _ ->
                    {error, precondition_failed}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec finish_abandon(any(), integer(), integer(), pos_integer()) -> {ok, map()} | {error, term()}.
finish_abandon(Conn, RecordId, CaptureId, ExpectedRevision) ->
    case talk_record_capture_repo:bump_epoch_end_tx(Conn, CaptureId, ExpectedRevision) of
        {ok, Row} ->
            _ = talk_record_capture_repo:update_record_projection_tx(
                Conn, RecordId, CaptureId, <<"ended">>
            ),
            Abandoned = talk_record_capture_support_ds:capture_payload(Row, #{}),
            {ok, Abandoned#{<<"abandoned">> => true}};
        {error, not_found} ->
            {error, precondition_failed};
        {error, Reason} ->
            {error, Reason}
    end.

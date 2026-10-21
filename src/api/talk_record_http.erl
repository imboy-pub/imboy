-module(talk_record_http).
-moduledoc "面聊记录职责拆分；保留原端点、权限与事务语义。".
-include("error_code.hrl").
-define(ERR_TALK_NOTICE_ACK_REQUIRED, 5520).
-define(ERR_TALK_CONTEXT_CHANGED, 5521).
-define(ERR_TALK_SCOPE_INVALID, 5522).
-define(ERR_TALK_IMMUTABLE_FIELD, 5523).
-define(ERR_TALK_UNSUPPORTED_SOURCE_TYPE, 5524).
-define(ERR_TALK_PRECONDITION_REQUIRED, 5525).
-define(ERR_TALK_PRECONDITION_FAILED, 5526).
-define(ERR_TALK_LANGUAGE_LOCKED, 5527).
-define(ERR_TALK_CURSOR_INVALID, 5528).
-define(ERR_TALK_CAPTURE_ACTIVE, 5529).
-define(ERR_TALK_CAPTURE_FENCED, 5530).
-define(ERR_TALK_CAPTURE_ENDED, 5531).
-define(ERR_TALK_EVENT_SEQ_CONFLICT, 5532).
-define(ERR_TALK_EVENT_TRANSITION, 5533).
-define(ERR_TALK_PART_SEQ_CONFLICT, 5534).
-define(ERR_TALK_PART_TIME_AXIS, 5535).
-define(ERR_TALK_PART_HASH_CONFLICT, 5536).
-define(ERR_TALK_PART_VERIFY_FAILED, 5537).
-define(ERR_TALK_CAPTURE_DEVICE_MISMATCH, 5538).
-define(ERR_TALK_MARK_CONFLICT, 5539).
-define(ERR_TALK_MARK_OUT_OF_RANGE, 5540).
-define(ERR_TALK_MARK_INVALID, 5541).
-define(ERR_TALK_NO_CAPTURE, 5542).
-define(ERR_TALK_SUMMARY_SOURCE_OUTDATED, 5543).
-define(ERR_TALK_SPEAKER_CLAIM_CONFLICT, 5544).
-define(ERR_TALK_DIRECTORY_DENIED, 5545).
-define(ERR_TALK_SUMMARY_REF_INVALID, 5546).
-define(ERR_TALK_GRANT_OWNER_ROW, 5547).
-define(ERR_TALK_GRANT_DUPLICATE, 5548).
-define(ERR_TALK_PLAYBACK_MEDIA_UNAVAILABLE, 5549).
-define(ERR_TALK_PLAYBACK_TICKET_INVALID, 5550).
-define(ERR_TALK_PLAYBACK_ACL_STALE, 5551).
-define(ERR_TALK_PLAYBACK_SIGNING_UNAVAILABLE, 5552).
-define(ERR_TALK_EXPORT_NOT_READY, 5553).
-define(ERR_TALK_EXPORT_GONE, 5554).
-define(ERR_TALK_EXPORT_DISABLED, 5555).
-define(ERR_TALK_EXPORT_FORMAT, 5556).
-define(ERR_TALK_EXPORT_AUDIO_UNSUPPORTED, 5557).
-define(ERR_TALK_TRANSFER_PERSONAL, 5558).
-define(ERR_TALK_TRANSFER_TARGET_SELF, 5559).
-define(ERR_TALK_EXPORT_SIGNING_UNAVAILABLE, 5560).
-define(ERR_TALK_EXPORT_TICKET_INVALID, 5561).
-export([
    method_not_allowed/1,
    path_id/1,
    path_ids/2,
    quote_etag/1,
    reply_accepted/2,
    reply_created/2,
    respond_error/2,
    respond_ok/2
]).

%% @private HTTP ETag 头引号形态（payload 内为令牌本体，双形态同值）。
-spec quote_etag(binary()) -> binary().
quote_etag(<<"\"", _/binary>> = Quoted) ->
    Quoted;
quote_etag(Token) ->
    <<"\"", Token/binary, "\"">>.

%% ===================================================================
%% 内部：路径 ID / 201 响应 / 405 / 错误映射
%% ===================================================================

-spec path_id(cowboy_req:req()) -> {ok, integer()} | {error, invalid_param}.
path_id(Req) ->
    case elib_tsid:from_binary(cowboy_req:binding(id, Req, <<>>)) of
        {ok, Id} -> {ok, Id};
        error -> {error, invalid_param}
    end.

%% @private 多段路径 ID 解析（[id] / [id,cid] / [id,cid,pid] / [id,mark_id]）。
-spec path_ids(cowboy_req:req(), [atom()]) -> {ok, [integer()]} | {error, invalid_param}.
path_ids(Req, Keys) ->
    Bindings = cowboy_req:bindings(Req),
    Parsed = [elib_tsid:from_binary(maps:get(K, Bindings, <<>>)) || K <- Keys],
    case [V || {ok, V} <- Parsed] of
        Values when length(Values) =:= length(Keys) ->
            case lists:any(fun(E) -> E =:= error end, Parsed) of
                false -> {ok, Values};
                true -> {error, invalid_param}
            end;
        _ ->
            {error, invalid_param}
    end.

%% @private 创建成功/重放：HTTP 201 + 项目信封（elib_response 无
%% success-with-status 入口；信封字段与此处手工拼装保持同构：
%% {code=0, msg, sv_ts, payload}）。
-spec reply_created(cowboy_req:req(), map()) -> cowboy_req:req().
reply_created(Req, Payload) ->
    reply_status(201, Req, Payload).

%% @private 202 Accepted（confirm verifying；§4）。信封与 reply_created 同构。
-spec reply_accepted(cowboy_req:req(), map()) -> cowboy_req:req().
reply_accepted(Req, Payload) ->
    reply_status(202, Req, Payload).

-spec reply_status(pos_integer(), cowboy_req:req(), map()) -> cowboy_req:req().
reply_status(Status, Req, Payload) ->
    Body = #{
        <<"code">> => 0,
        <<"msg">> => <<"success">>,
        <<"sv_ts">> => elib_dt:millisecond(),
        <<"payload">> => elib_cnv:convert_at_timestamps(Payload)
    },
    cowboy_req:reply(
        Status,
        #{<<"content-type">> => <<"application/json; charset=utf-8">>},
        jsone:encode(Body, [native_utf8]),
        Req
    ).

%% @private {ok, Payload} → 200 信封；{error, Reason} → 错误映射。
-spec respond_ok(cowboy_req:req(), {ok, map()} | {error, term()}) -> cowboy_req:req().
respond_ok(Req, {ok, Payload}) ->
    elib_response:success(Req, Payload);
respond_ok(Req, {error, Reason}) ->
    respond_error(Req, Reason).

-spec method_not_allowed(cowboy_req:req()) -> cowboy_req:req().
method_not_allowed(Req) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req).

%% @doc talk_record 域错误映射：reason 原子 → {HTTP status, msg, 信封码}。
%% HTTP 语义按 BACKEND-API §4；幂等键沿用 5460/5461；其余 55xx 为本域
%% 预留段建议值（待 hrl 统一登记）。
-spec respond_error(cowboy_req:req(), atom()) -> cowboy_req:req().
respond_error(Req, {missing_sequences, Detail}) when is_map(Detail) ->
    Data = maps:with([<<"capture_id">>, <<"missing">>], Detail),
    elib_response:error_with_status(
        Req,
        409,
        <<"录音分段不完整，请补传后重试"/utf8>>,
        Data,
        ?ERR_PARAM_INVALID
    );
respond_error(Req, Reason) ->
    {Status, Msg, Code} = map_reason(Reason),
    elib_response:error_with_status(Req, Status, Msg, Code).

-spec map_reason(atom()) -> {pos_integer(), binary(), integer()}.
map_reason(not_found) ->
    {404, <<"记录不存在或无访问权限"/utf8>>, ?ERR_NOT_FOUND};
map_reason(forbidden) ->
    {403, <<"当前身份无此操作权限"/utf8>>, ?ERR_FORBIDDEN};
map_reason(forbidden_field) ->
    {403, <<"当前角色不可编辑该字段"/utf8>>, ?ERR_FORBIDDEN};
map_reason(notice_ack_required) ->
    {422, <<"缺少录音告知确认"/utf8>>, ?ERR_TALK_NOTICE_ACK_REQUIRED};
map_reason(completion_already_submitted) ->
    {409, <<"录音已提交，请刷新处理状态"/utf8>>, ?ERR_CONFLICT};
map_reason(context_changed) ->
    {409, <<"归属或策略已变化，请刷新创建上下文"/utf8>>, ?ERR_TALK_CONTEXT_CHANGED};
map_reason(policy_version_changed) ->
    {409, <<"告知策略版本已更新，请重新确认"/utf8>>, ?ERR_TALK_CONTEXT_CHANGED};
map_reason(policy_version_missing) ->
    {422, <<"缺少告知策略版本"/utf8>>, ?ERR_MISSING_PARAM};
map_reason(personal_with_org) ->
    {422, <<"个人记录不接受企业归属字段"/utf8>>, ?ERR_TALK_SCOPE_INVALID};
map_reason(invalid_participants) ->
    {422, <<"参与者清单不合规"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_scope) ->
    {422, <<"scope 取值非法"/utf8>>, ?ERR_TALK_SCOPE_INVALID};
map_reason(scope_not_enabled) ->
    {422, <<"该归属类型未开放"/utf8>>, ?ERR_TALK_SCOPE_INVALID};
map_reason(unsupported_source_type) ->
    {422, <<"不支持的录音来源类型"/utf8>>, ?ERR_TALK_UNSUPPORTED_SOURCE_TYPE};
map_reason(immutable_field) ->
    {422, <<"归属与属主不可直接修改"/utf8>>, ?ERR_TALK_IMMUTABLE_FIELD};
map_reason(precondition_required) ->
    {428, <<"缺少 If-Match 版本头"/utf8>>, ?ERR_TALK_PRECONDITION_REQUIRED};
map_reason(precondition_failed) ->
    {412, <<"版本已过期，请刷新后重试"/utf8>>, ?ERR_TALK_PRECONDITION_FAILED};
map_reason(revision_mismatch) ->
    map_reason(precondition_failed);
map_reason(invalid_if_match) ->
    {422, <<"If-Match 格式非法"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(language_locked) ->
    {422, <<"处理已开始，语言不可再修改"/utf8>>, ?ERR_TALK_LANGUAGE_LOCKED};
map_reason(cursor_invalid) ->
    {422, <<"游标无效或已过期"/utf8>>, ?ERR_TALK_CURSOR_INVALID};
map_reason(idempotency_key_required) ->
    {422, <<"缺少幂等键"/utf8>>, ?ERR_IDEMPOTENCY_KEY_REQUIRED};
map_reason(idempotency_conflict) ->
    {409, <<"请求与幂等键已绑定内容冲突"/utf8>>, ?ERR_IDEMPOTENCY_CONFLICT};
map_reason(idempotency_pending) ->
    {409, <<"同幂等键请求正在处理中，请稍后重试"/utf8>>, ?ERR_IDEMPOTENCY_CONFLICT};
map_reason(invalid_client_recording_id) ->
    {422, <<"client_recording_id 必须为 UUID"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_organization_id) ->
    {422, <<"organization_id 非法"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_workspace_id) ->
    {422, <<"workspace_id 非法"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_title) ->
    {422, <<"标题不合规"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_language) ->
    {422, <<"语言标识不合规"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_notes) ->
    {422, <<"笔记长度超限"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_processing_mode) ->
    {422, <<"processing_mode 取值非法"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_filter) ->
    {422, <<"列表筛选参数非法"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(missing_editable_field) ->
    {422, <<"缺少可编辑字段"/utf8>>, ?ERR_MISSING_PARAM};
map_reason(invalid_body) ->
    {422, <<"请求体不合规"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(purge_blocked) ->
    {409, <<"附件仍受留存或保全规则保护"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(retention_shrink_rejected) ->
    {422, <<"留存期限只能延长"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(already_purged) ->
    {409, <<"治理清理已获批准，无法恢复留存状态"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(already_deleting) ->
    {409, <<"附件已进入清理流程"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(hold_not_set) ->
    {409, <<"附件当前没有待解除的保全"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_param) ->
    {422, <<"参数非法"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(missing_param) ->
    {422, <<"缺少必填参数"/utf8>>, ?ERR_MISSING_PARAM};
%% ---- T07 采集域 ----
map_reason(capture_active_on_record) ->
    {409, <<"该记录已有进行中的采集"/utf8>>, ?ERR_TALK_CAPTURE_ACTIVE};
map_reason(capture_active_on_device) ->
    {409, <<"该设备已有进行中的采集"/utf8>>, ?ERR_TALK_CAPTURE_ACTIVE};
map_reason(capture_active_conflict) ->
    {409, <<"采集冲突，请重试"/utf8>>, ?ERR_TALK_CAPTURE_ACTIVE};
map_reason(capture_fenced) ->
    {409, <<"采集会话已被终止（fenced），不可继续写入"/utf8>>, ?ERR_TALK_CAPTURE_FENCED};
map_reason(capture_ended) ->
    {409, <<"采集已结束，不可继续写入"/utf8>>, ?ERR_TALK_CAPTURE_ENDED};
map_reason(capture_not_found) ->
    {404, <<"记录不存在或无访问权限"/utf8>>, ?ERR_NOT_FOUND};
map_reason(capture_device_mismatch) ->
    {409, <<"仅原设备会话可恢复该采集"/utf8>>, ?ERR_TALK_CAPTURE_DEVICE_MISMATCH};
map_reason(event_seq_conflict) ->
    {409, <<"事件序号冲突"/utf8>>, ?ERR_TALK_EVENT_SEQ_CONFLICT};
map_reason(invalid_event_transition) ->
    {409, <<"事件与当前采集状态不兼容"/utf8>>, ?ERR_TALK_EVENT_TRANSITION};
map_reason(part_seq_conflict) ->
    {409, <<"分段序号已绑定其他内容"/utf8>>, ?ERR_TALK_PART_SEQ_CONFLICT};
map_reason(part_state_conflict) ->
    {409, <<"分段状态冲突"/utf8>>, ?ERR_TALK_PART_SEQ_CONFLICT};
map_reason(part_time_overlap) ->
    {409, <<"分段与既有分段时间轴重叠"/utf8>>, ?ERR_TALK_PART_TIME_AXIS};
map_reason(part_not_found) ->
    {404, <<"分段不存在"/utf8>>, ?ERR_NOT_FOUND};
map_reason(audio_part_hash_conflict) ->
    {409, <<"分段内容校验值冲突"/utf8>>, ?ERR_TALK_PART_HASH_CONFLICT};
map_reason(audio_part_verification_failed) ->
    {409, <<"分段校验未通过"/utf8>>, ?ERR_TALK_PART_VERIFY_FAILED};
map_reason(invalid_device_session_id) ->
    {422, <<"device_session_id 必须为 UUID"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_client_event_id) ->
    {422, <<"client_event_id 必须为 UUID"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_device_id) ->
    {422, <<"device_id 不合规"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_reported_at) ->
    {422, <<"reported_at 不合规"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_event_seq) ->
    {422, <<"事件 seq 非法"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_event) ->
    {422, <<"event 取值非法"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_position_ms) ->
    {422, <<"position_ms 非法"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_part_seq) ->
    {422, <<"分段 seq 非法"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_part_time_axis) ->
    {422, <<"分段时间轴非法或为空段"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_capture_offset) ->
    {422, <<"capture_offset_ms 非法"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_sha256) ->
    {422, <<"sha256 必须为 64 位十六进制"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_byte_size) ->
    {422, <<"byte_size 非法"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_codec) ->
    {422, <<"codec 不合规"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_confirm_receipt) ->
    {422, <<"上传回执不合规"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(abandon_confirm_required) ->
    {422, <<"放弃采集需显式确认"/utf8>>, ?ERR_MISSING_PARAM};
%% ---- T07 marks ----
map_reason(mark_conflict) ->
    {409, <<"client_mark_id 已绑定其他内容"/utf8>>, ?ERR_TALK_MARK_CONFLICT};
map_reason(mark_position_out_of_range) ->
    {422, <<"标记位置超出有效录音范围"/utf8>>, ?ERR_TALK_MARK_OUT_OF_RANGE};
map_reason(invalid_client_mark_id) ->
    {422, <<"client_mark_id 必须为 UUID"/utf8>>, ?ERR_TALK_MARK_INVALID};
map_reason(invalid_mark_text) ->
    {422, <<"标记文本不合规"/utf8>>, ?ERR_TALK_MARK_INVALID};
map_reason(mark_not_found) ->
    {404, <<"标记不存在"/utf8>>, ?ERR_NOT_FOUND};
map_reason(mark_not_author) ->
    {403, <<"仅标记作者可编辑"/utf8>>, ?ERR_FORBIDDEN};
map_reason(mark_no_capture) ->
    {422, <<"该标记未绑定采集，不能重定位"/utf8>>, ?ERR_TALK_NO_CAPTURE};
map_reason(no_capture_for_position) ->
    {422, <<"记录暂无采集，无法定位标记"/utf8>>, ?ERR_TALK_NO_CAPTURE};
map_reason(missing_position) ->
    {422, <<"缺少 position_ms 或 capture_offset_ms"/utf8>>, ?ERR_MISSING_PARAM};
map_reason(missing_capture_offset) ->
    {422, <<"capture_id 需同时提供 capture_offset_ms"/utf8>>, ?ERR_MISSING_PARAM};
map_reason(position_projection_mismatch) ->
    {422, <<"position_ms 与 capture_offset_ms 不一致"/utf8>>, ?ERR_PARAM_INVALID};
%% ---- T15 内容域（原文/发言人/摘要）----
map_reason(segment_not_found) ->
    {404, <<"片段不存在"/utf8>>, ?ERR_NOT_FOUND};
map_reason(speaker_not_found) ->
    {404, <<"说话人不存在"/utf8>>, ?ERR_NOT_FOUND};
map_reason(summary_not_found) ->
    {404, <<"摘要不存在"/utf8>>, ?ERR_NOT_FOUND};
map_reason(summary_source_outdated) ->
    {409, <<"摘要源版本已过期，请先按最新原文校对"/utf8>>, ?ERR_TALK_SUMMARY_SOURCE_OUTDATED};
map_reason(speaker_claim_conflict) ->
    {409, <<"该说话人已被他人认领，请联系记录所有者人工纠正"/utf8>>, ?ERR_TALK_SPEAKER_CLAIM_CONFLICT};
map_reason(claim_not_participant) ->
    {403, <<"仅现场参与者可认领说话人"/utf8>>, ?ERR_FORBIDDEN};
map_reason(target_not_in_directory) ->
    {422, <<"目标用户不在可见目录内"/utf8>>, ?ERR_TALK_DIRECTORY_DENIED};
map_reason(invalid_summary_segment_ref) ->
    {422, <<"摘要引用了不属于本记录的片段"/utf8>>, ?ERR_TALK_SUMMARY_REF_INVALID};
map_reason(invalid_segment_text) ->
    {422, <<"片段文本不合规"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_speaker_name) ->
    {422, <<"说话人名称不合规"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_matched_user_id) ->
    {422, <<"用户 ID 非法"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_identity_source) ->
    {422, <<"identity_source 由服务端设置，不可指定"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_summary_overview) ->
    {422, <<"摘要概述不合规"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_summary_items) ->
    {422, <<"decisions/actions 结构不合规"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_confirm) ->
    {422, <<"confirm 取值非法"/utf8>>, ?ERR_PARAM_INVALID};
%% ---- T17 授权/分享/回放域 ----
map_reason(grant_owner_row) ->
    {422, <<"所有者权限在主表，不可出现在授权清单中"/utf8>>, ?ERR_TALK_GRANT_OWNER_ROW};
map_reason(grant_duplicate) ->
    {422, <<"授权清单内被授权人重复"/utf8>>, ?ERR_TALK_GRANT_DUPLICATE};
map_reason(invalid_grants_body) ->
    {422, <<"授权清单不合规"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_grant_role) ->
    {422, <<"授权角色取值非法"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_grantee_user_id) ->
    {422, <<"被授权人 ID 非法"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_expires_at) ->
    {422, <<"授权到期时间不合规"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(grant_not_found) ->
    {404, <<"授权不存在或已撤销"/utf8>>, ?ERR_NOT_FOUND};
map_reason(playback_media_unavailable) ->
    {409, <<"音频尚未就绪，暂不可回放"/utf8>>, ?ERR_TALK_PLAYBACK_MEDIA_UNAVAILABLE};
map_reason(playback_ticket_invalid) ->
    {403, <<"回放票据无效"/utf8>>, ?ERR_TALK_PLAYBACK_TICKET_INVALID};
map_reason(acl_stale) ->
    {403, <<"访问授权已变化，请重新获取"/utf8>>, ?ERR_TALK_PLAYBACK_ACL_STALE};
map_reason(playback_signing_key_unavailable) ->
    {503, <<"回放服务暂不可用，请稍后重试"/utf8>>, ?ERR_TALK_PLAYBACK_SIGNING_UNAVAILABLE};
map_reason(export_not_found) ->
    {404, <<"导出不存在或无访问权限"/utf8>>, ?ERR_NOT_FOUND};
map_reason(export_not_ready) ->
    {409, <<"导出尚未生成完成"/utf8>>, ?ERR_TALK_EXPORT_NOT_READY};
map_reason(export_failed) ->
    {409, <<"导出生成失败，请重新发起"/utf8>>, ?ERR_TALK_EXPORT_NOT_READY};
map_reason(export_expired) ->
    {410, <<"导出已到期，请重新生成"/utf8>>, ?ERR_TALK_EXPORT_GONE};
map_reason(export_disabled_by_policy) ->
    {403, <<"当前策略不允许导出该记录"/utf8>>, ?ERR_TALK_EXPORT_DISABLED};
map_reason(invalid_export_format) ->
    {422, <<"format 仅支持 txt/md"/utf8>>, ?ERR_TALK_EXPORT_FORMAT};
map_reason(export_audio_unsupported) ->
    {422, <<"V1 导出不包含音频"/utf8>>, ?ERR_TALK_EXPORT_AUDIO_UNSUPPORTED};
map_reason(content_too_large) ->
    {422, <<"导出内容超出大小限制"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(export_signing_key_unavailable) ->
    {503, <<"导出服务暂不可用，请稍后重试"/utf8>>, ?ERR_TALK_EXPORT_SIGNING_UNAVAILABLE};
map_reason(export_ticket_invalid) ->
    {403, <<"导出下载凭证无效"/utf8>>, ?ERR_TALK_EXPORT_TICKET_INVALID};
map_reason(personal_not_transferable) ->
    {422, <<"个人记录不支持移交管理责任"/utf8>>, ?ERR_TALK_TRANSFER_PERSONAL};
map_reason(transfer_target_self) ->
    {422, <<"移交目标不能是当前责任人自己"/utf8>>, ?ERR_TALK_TRANSFER_TARGET_SELF};
map_reason(invalid_target_user_id) ->
    {422, <<"移交目标用户 ID 非法"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_keep_access) ->
    {422, <<"keep_access 取值非法"/utf8>>, ?ERR_PARAM_INVALID};
map_reason({missing_sequences, Detail}) when is_map(Detail) ->
    {409, <<"录音分段不完整，请补传后重试"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(manifest_hash_conflict) ->
    {409, <<"录音清单校验不一致"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(capture_not_ended) ->
    {409, <<"录音尚未结束"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(job_not_retryable) ->
    {409, <<"当前任务不可重试"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(processing_unavailable) ->
    {503, <<"处理服务未配置"/utf8>>, ?ERR_ERROR};
map_reason(media_not_ready) ->
    {409, <<"录音尚未就绪"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(processing_consent_required) ->
    {422, <<"请先确认处理授权"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_capture_id) ->
    {422, <<"采集标识不正确"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_captures) ->
    {422, <<"采集清单格式不正确"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_last_seq) ->
    {422, <<"分段序号不正确"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_manifest_hash) ->
    {422, <<"清单摘要格式不正确"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(invalid_processing_consent) ->
    {422, <<"处理授权格式不正确"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(db_error) ->
    {500, <<"系统繁忙，请稍后重试"/utf8>>, ?ERR_ERROR};
map_reason(Other) when is_atom(Other) -> {500, <<"系统繁忙，请稍后重试"/utf8>>, ?ERR_ERROR};
map_reason(_NonAtom) ->
    {500, <<"系统繁忙，请稍后重试"/utf8>>, ?ERR_ERROR}.

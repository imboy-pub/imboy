-module(talk_record_handler).
-moduledoc "面聊记录 HTTP 适配层（T05+T07+T15+T17+T20）：CRUD/上下文 + 采集/marks + 原文/发言人/摘要 + 授权/分享/回放 + 删除。".
%%%
% talk_record HTTP 薄适配层：传输解析（method/qs/header/body）→
% talk_record_logic / talk_record_ds / talk_record_marks_logic /
% talk_record_content_{segments,speakers,summary} /
% talk_record_{grant_logic,grant_candidates,card_logic,grant_playback}
% → elib_response。业务与 ACL 全部下沉 Logic/DS。
%
% 路由（src/api/talk_record_routes.fragment.erl，由主控制器归并 imboy_router）：
%   GET  /api/v1/talk-records/capabilities     → capabilities
%   GET  /api/v1/talk-records/creation-contexts → creation_contexts
%   GET  /api/v1/talk-records/share-candidates  → 通用候选（T17，字面量段）
%   GET/POST /api/v1/talk-records              → list / create
%   GET/PATCH /api/v1/talk-records/:id         → detail / update
%   DELETE  /api/v1/talk-records/:id            → 删除（T20：202 即返，白名单 GET）
%   GET/POST /api/v1/talk-records/:id/captures → 采集恢复视图 / 创建采集（T07）
%   POST /api/v1/talk-records/:id/captures/:cid/events        → 采集事件（T07）
%   POST /api/v1/talk-records/:id/captures/:cid/audio-parts   → 分段注册（T07）
%   POST /api/v1/talk-records/:id/captures/:cid/audio-parts/:pid/confirm（T07）
%   POST /api/v1/talk-records/:id/captures/:cid/recover       → 失联恢复（T07）
%   POST /api/v1/talk-records/:id/captures/:cid/abandon       → 显式放弃（T07）
%   GET/POST /api/v1/talk-records/:id/marks                   → marks（T07）
%   PATCH/DELETE /api/v1/talk-records/:id/marks/:mark_id      → mark 编辑/删除（T07）
%   GET   /api/v1/talk-records/:id/segments            → 原文清单（T15）
%   PATCH /api/v1/talk-records/:id/segments/:segment_id → 人工修订（T15）
%   GET   /api/v1/talk-records/:id/speakers            → 说话人清单（T15）
%   PATCH /api/v1/talk-records/:id/speakers/:speaker_id → 人工标记（T15）
%   POST  /api/v1/talk-records/:id/speakers/:speaker_id/claim → 本人认领（T15）
%   GET/PATCH /api/v1/talk-records/:id/summary         → 摘要读取/编辑/确认（T15）
%   GET/PUT  /api/v1/talk-records/:id/grants           → 授权名单/全量提交（T17）
%   DELETE   /api/v1/talk-records/:id/grants/:grant_id → 撤销授权（T17）
%   GET      /api/v1/talk-records/:id/share-candidates → record 级候选（T17）
%   POST     /api/v1/talk-records/:id/share-card       → 分享卡片（T17）
%   POST     /api/v1/talk-records/:id/playback         → 受控回放票据（T17）
%
% HTTP 语义（BACKEND-API §4；freeze §2.6/§2.7）：404/403/409/412/422/428
% 走 elib_response:error_with_status 真实状态码（信封结构不变）；POST 创建
% 成功 201；重放附 Idempotent-Replayed: true；GET/:id 与 PATCH 响应带 ETag
% （cowboy_req:set_resp_header 预置，elib_response:reply 合并发送）。
% T15 内容域 ETag 惯例（与 T16 UI 冻结契约对齐，详见
% talk_record_content_common 模块头）：
%   * segment：`segment:<id>:<revision>`；speaker：`speaker:<id>:<revision>`；
%     summary：`summary:<revision>`——HTTP 头为 RFC 引号形态，payload etag
%     字段为令牌本体；If-Match 兼容令牌与纯 revision 两种形态。
% T17 授权域 acl ETag 惯例（与 T18 UI 冻结契约对齐，详见
% talk_record_grant_logic 模块头）：
%   * grants GET/PUT/DELETE 响应 ETag = `"acl:<acl_revision>"`（头）/
%     `acl:<acl_revision>`（payload etag 字段）；If-Match 兼容令牌与纯
%     acl_revision 两种形态。
%%%

-export([init/2]).
-export([handle_action/3]).

-include("error_code.hrl").

%% ===================================================================
%% talk_record 域错误码（5520-5528 为 T05 预留段建议值；5529-5542 为 T07
%% 续段建议值；5543-5546 为 T15 内容域建议值）。include/error_code.hrl 为
%% 共享文件（本卡不改动），此处先行定义同名宏，主控制器后续统一登记入
%% hrl 后替换（值以 hrl 登记为准）。
%% ===================================================================
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
%% ---- T15 内容域（原文/发言人/摘要）----
-define(ERR_TALK_SUMMARY_SOURCE_OUTDATED, 5543).
-define(ERR_TALK_SPEAKER_CLAIM_CONFLICT, 5544).
-define(ERR_TALK_DIRECTORY_DENIED, 5545).
-define(ERR_TALK_SUMMARY_REF_INVALID, 5546).
%% ---- T17 授权/分享/回放域 ----
-define(ERR_TALK_GRANT_OWNER_ROW, 5547).
-define(ERR_TALK_GRANT_DUPLICATE, 5548).
-define(ERR_TALK_PLAYBACK_MEDIA_UNAVAILABLE, 5549).
-define(ERR_TALK_PLAYBACK_TICKET_INVALID, 5550).
-define(ERR_TALK_PLAYBACK_ACL_STALE, 5551).
-define(ERR_TALK_PLAYBACK_SIGNING_UNAVAILABLE, 5552).

%% ---- T19 导出/责任移交域 ----
-define(ERR_TALK_EXPORT_NOT_READY, 5553).
-define(ERR_TALK_EXPORT_GONE, 5554).
-define(ERR_TALK_EXPORT_DISABLED, 5555).
-define(ERR_TALK_EXPORT_FORMAT, 5556).
-define(ERR_TALK_EXPORT_AUDIO_UNSUPPORTED, 5557).
-define(ERR_TALK_TRANSFER_PERSONAL, 5558).
-define(ERR_TALK_TRANSFER_TARGET_SELF, 5559).
-define(ERR_TALK_EXPORT_SIGNING_UNAVAILABLE, 5560).
-define(ERR_TALK_EXPORT_TICKET_INVALID, 5561).

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(collection, Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> list(Req, State);
        <<"POST">> -> create(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(capabilities, Req, _State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> elib_response:success(Req, talk_record_logic:capabilities());
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(creation_contexts, Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> creation_contexts(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(item, Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> detail(Req, State);
        <<"PATCH">> -> update(Req, State);
        <<"DELETE">> -> delete(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
%% ---- T07 采集域 ----
handle_action(captures_collection, Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> talk_record_capture_http:captures_list(Req, State);
        <<"POST">> -> talk_record_capture_http:capture_create(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(capture_events, Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> -> talk_record_capture_http:capture_event(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(capture_audio_parts, Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> -> talk_record_capture_http:part_register(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(capture_audio_part_confirm, Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> -> talk_record_capture_http:part_confirm(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(capture_recover, Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> -> talk_record_capture_http:capture_recover(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(capture_abandon, Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> -> talk_record_capture_http:capture_abandon(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
%% ---- T07 marks ----
handle_action(marks_collection, Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> talk_record_capture_http:marks_list(Req, State);
        <<"POST">> -> talk_record_capture_http:mark_create(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(marks_item, Req, State) ->
    case cowboy_req:method(Req) of
        <<"PATCH">> -> talk_record_capture_http:mark_update(Req, State);
        <<"DELETE">> -> talk_record_capture_http:mark_delete(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
%% ---- T15 内容域（原文/发言人/摘要）----
handle_action(segments_collection, Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> talk_record_content_http:segments_list(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(segments_item, Req, State) ->
    case cowboy_req:method(Req) of
        <<"PATCH">> -> talk_record_content_http:segment_update(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(speakers_collection, Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> talk_record_content_http:speakers_list(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(speakers_item, Req, State) ->
    case cowboy_req:method(Req) of
        <<"PATCH">> -> talk_record_content_http:speaker_update(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(speaker_claim, Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> -> talk_record_content_http:speaker_claim(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(summary_item, Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> talk_record_content_http:summary_get(Req, State);
        <<"PATCH">> -> talk_record_content_http:summary_update(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
%% ---- T17 授权/分享/回放域（§4 grants/share-card/playback + §8 share-candidates）----
handle_action(grants_collection, Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> talk_record_sharing_http:grants_list(Req, State);
        <<"PUT">> -> talk_record_sharing_http:grants_replace(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(grants_item, Req, State) ->
    case cowboy_req:method(Req) of
        <<"DELETE">> -> talk_record_sharing_http:grants_revoke(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(share_card, Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> -> talk_record_sharing_http:share_card(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(playback, Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> -> talk_record_sharing_http:playback(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(share_candidates, Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> talk_record_sharing_http:share_candidates_generic(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(transfer_candidates, Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> talk_record_sharing_http:transfer_candidates_for_record(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(record_share_candidates, Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> talk_record_sharing_http:share_candidates_for_record(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
%% ---- T19 导出/责任移交域（§4 exports 两端点 + exportState 轮询；
%% §9 ownership-transfers）----
handle_action(exports_collection, Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> -> talk_record_sharing_http:export_create(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(export_item, Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> talk_record_sharing_http:export_state(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(export_download, Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> talk_record_sharing_http:export_download(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(ownership_transfers, Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> -> talk_record_sharing_http:ownership_transfer(Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end;
handle_action(complete, Req, State) ->
    talk_record_processing_http:processing_action(complete, <<"POST">>, Req, State);
handle_action(jobs, Req, State) ->
    talk_record_processing_http:processing_action(jobs, <<"GET">>, Req, State);
handle_action(job_retry, Req, State) ->
    talk_record_processing_http:processing_action(job_retry, <<"POST">>, Req, State);
handle_action(processing_requests, Req, State) ->
    talk_record_processing_http:processing_action(processing_requests, <<"POST">>, Req, State);
handle_action(false, Req, _State) ->
    Req.

%% ===================================================================
%% Actions
%% ===================================================================

%% GET /api/v1/talk-records（§4：scope/org/ws/filter/q/cursor/limit）
-spec list(cowboy_req:req(), map()) -> cowboy_req:req().
list(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    Qs = cowboy_req:parse_qs(Req0),
    Raw = #{
        <<"scope">> => proplists:get_value(<<"scope">>, Qs, <<>>),
        <<"organization_id">> => proplists:get_value(<<"organization_id">>, Qs, 0),
        <<"workspace_id">> => proplists:get_value(<<"workspace_id">>, Qs, 0),
        <<"filter">> => proplists:get_value(<<"filter">>, Qs, <<"all">>),
        <<"q">> => proplists:get_value(<<"q">>, Qs, <<>>)
    },
    Cursor = proplists:get_value(<<"cursor">>, Qs, undefined),
    Limit = talk_record_logic:parse_limit(proplists:get_value(<<"limit">>, Qs, undefined)),
    case talk_record_logic:parse_filters(Raw) of
        {ok, Filters} ->
            respond_list(Req0, Uid, Filters, Cursor, Limit);
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

-spec respond_list(cowboy_req:req(), integer(), map(), binary() | undefined, pos_integer()) ->
    cowboy_req:req().
respond_list(Req0, Uid, Filters, Cursor, Limit) ->
    case talk_record_logic:list(Uid, Filters, Cursor, Limit) of
        {ok, Result} ->
            elib_response:success(Req0, Result);
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% POST /api/v1/talk-records（§4 创建 + Idempotency-Key）
-spec create(cowboy_req:req(), map()) -> cowboy_req:req().
create(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    IdemKey = cowboy_req:header(<<"idempotency-key">>, Req0),
    Body = elib_param:post(Req0),
    case talk_record_logic:create(Uid, Body, IdemKey) of
        {ok, Mark, Payload, ETag} ->
            Req1 = cowboy_req:set_resp_header(<<"etag">>, ETag, Req0),
            Req2 =
                case Mark of
                    replay ->
                        cowboy_req:set_resp_header(<<"idempotent-replayed">>, <<"true">>, Req1);
                    created ->
                        Req1
                end,
            talk_record_http:reply_created(Req2, Payload);
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% GET /api/v1/talk-records/:id（§4：无权/不存在统一 404；带 ETag）
%% 删除后 owner 分支返回清理状态白名单（§11.3；talk_record_logic:detail
%% 分流——payload 无标题/正文/参与者，capabilities_for_actor.read_content=false）
-spec detail(cowboy_req:req(), map()) -> cowboy_req:req().
detail(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case talk_record_http:path_id(Req0) of
        {ok, Id} ->
            respond_detail(Req0, Uid, Id);
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

-spec respond_detail(cowboy_req:req(), integer(), integer()) -> cowboy_req:req().
respond_detail(Req0, Uid, Id) ->
    case talk_record_logic:detail(Uid, Id) of
        {ok, #{payload := Payload, etag := ETag}} ->
            Req1 = cowboy_req:set_resp_header(<<"etag">>, ETag, Req0),
            elib_response:success(Req1, Payload);
        %% 白名单分支无 #{etag,...} 形状（清理状态不参与版本协商）
        {ok, Payload} when is_map(Payload) ->
            elib_response:success(Req0, Payload);
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% DELETE /api/v1/talk-records/:id（T20；owner + If-Match；202 即返——
%% logical_deleted=true + cleanup_status=pending；立即撤权；不在 HTTP 内
%% 物理删——字节删除归既有附件队列经 purge job 协调，§10.3/§10.4）
-spec delete(cowboy_req:req(), map()) -> cowboy_req:req().
delete(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    IfMatch = cowboy_req:header(<<"if-match">>, Req0),
    case talk_record_http:path_id(Req0) of
        {ok, Id} ->
            case talk_record_delete_logic:delete(Uid, Id, IfMatch) of
                {ok, _Mark, Payload} ->
                    talk_record_http:reply_accepted(Req0, Payload);
                {error, Reason} ->
                    talk_record_http:respond_error(Req0, Reason)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% PATCH /api/v1/talk-records/:id（title/language/notes；If-Match）
-spec update(cowboy_req:req(), map()) -> cowboy_req:req().
update(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case talk_record_http:path_id(Req0) of
        {ok, Id} ->
            IfMatch = cowboy_req:header(<<"if-match">>, Req0),
            Body = elib_param:post(Req0),
            case talk_record_logic:update(Uid, Id, IfMatch, Body) of
                {ok, #{payload := Payload, etag := ETag}} ->
                    Req1 = cowboy_req:set_resp_header(<<"etag">>, ETag, Req0),
                    elib_response:success(Req1, Payload);
                {error, Reason} ->
                    talk_record_http:respond_error(Req0, Reason)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% GET /api/v1/talk-records/creation-contexts（§8：cursor/limit 20..100）
-spec creation_contexts(cowboy_req:req(), map()) -> cowboy_req:req().
creation_contexts(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    Qs = cowboy_req:parse_qs(Req0),
    Cursor = proplists:get_value(<<"cursor">>, Qs, undefined),
    Limit = talk_record_logic:parse_limit(proplists:get_value(<<"limit">>, Qs, undefined)),
    case talk_record_logic:creation_contexts(Uid, Cursor, Limit) of
        {ok, Result} ->
            elib_response:success(Req0, Result);
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

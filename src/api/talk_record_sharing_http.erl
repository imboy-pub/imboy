-module(talk_record_sharing_http).
-moduledoc "面聊记录职责拆分；保留原端点、权限与事务语义。".
-include("error_code.hrl").
-export([
    export_create/2,
    export_download/2,
    export_state/2,
    grants_list/2,
    grants_replace/2,
    grants_revoke/2,
    ownership_transfer/2,
    playback/2,
    share_candidates_for_record/2,
    share_candidates_generic/2,
    share_card/2,
    transfer_candidates_for_record/2
]).

%% ===================================================================
%% T17 授权/分享/回放域 Actions（§4 grants/share-card/playback + §8 share-candidates）
%% ===================================================================

%% GET /api/v1/talk-records/:id/grants（§4/§11.3：owner 全量 / 非 owner 仅自己；
%% ETag = acl 令牌引号形态，payload etag 为令牌本体——T18 冻结契约）
-spec grants_list(cowboy_req:req(), map()) -> cowboy_req:req().
grants_list(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case talk_record_http:path_id(Req0) of
        {ok, Id} ->
            case talk_record_grant_logic:list(Uid, Id) of
                {ok, #{<<"etag">> := AclEtag} = Payload} ->
                    Req1 = cowboy_req:set_resp_header(
                        <<"etag">>, talk_record_http:quote_etag(AclEtag), Req0
                    ),
                    elib_response:success(Req1, Payload);
                {error, Reason} ->
                    talk_record_http:respond_error(Req0, Reason)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% PUT /api/v1/talk-records/:id/grants（owner + If-Match acl_revision；全量
%% 提交事务更新；T18 putGrants 形状 {grants: [...]}）
-spec grants_replace(cowboy_req:req(), map()) -> cowboy_req:req().
grants_replace(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    IfMatch = cowboy_req:header(<<"if-match">>, Req0),
    case talk_record_http:path_id(Req0) of
        {ok, Id} ->
            Body = elib_param:post(Req0),
            case talk_record_grant_logic:replace(Uid, Id, IfMatch, Body) of
                {ok, #{<<"etag">> := AclEtag} = Payload} ->
                    Req1 = cowboy_req:set_resp_header(
                        <<"etag">>, talk_record_http:quote_etag(AclEtag), Req0
                    ),
                    elib_response:success(Req1, Payload);
                {error, Reason} ->
                    talk_record_http:respond_error(Req0, Reason)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% DELETE /api/v1/talk-records/:id/grants/:grant_id（owner + If-Match；
%% 立即生效——票据/Range/续签同步拒绝）
-spec grants_revoke(cowboy_req:req(), map()) -> cowboy_req:req().
grants_revoke(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    IfMatch = cowboy_req:header(<<"if-match">>, Req0),
    case talk_record_http:path_ids(Req0, [id, grant_id]) of
        {ok, [Id, GrantId]} ->
            case talk_record_grant_logic:revoke(Uid, Id, GrantId, IfMatch) of
                {ok, #{<<"etag">> := AclEtag} = Payload} ->
                    Req1 = cowboy_req:set_resp_header(
                        <<"etag">>, talk_record_http:quote_etag(AclEtag), Req0
                    ),
                    elib_response:success(Req1, Payload);
                {error, Reason} ->
                    talk_record_http:respond_error(Req0, Reason)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% POST /api/v1/talk-records/:id/share-card（owner 或可读者；受控定位数据；
%% 不额外授权、不发消息；严格模式只发「面聊记录」）
-spec share_card(cowboy_req:req(), map()) -> cowboy_req:req().
share_card(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case talk_record_http:path_id(Req0) of
        {ok, Id} ->
            talk_record_http:respond_ok(Req0, talk_record_card_logic:share_card(Uid, Id));
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% POST /api/v1/talk-records/:id/playback（校验 ACL；position_ms 可选；
%% 短时受控票据 + expiry；续签 = 同端点重开重验）
-spec playback(cowboy_req:req(), map()) -> cowboy_req:req().
playback(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case talk_record_http:path_id(Req0) of
        {ok, Id} ->
            Body = elib_param:post(Req0),
            PositionMs =
                case Body of
                    #{<<"position_ms">> := P} when is_integer(P) -> P;
                    _ -> undefined
                end,
            talk_record_http:respond_ok(Req0, talk_record_grant_playback:open(Uid, Id, PositionMs));
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% GET /api/v1/talk-records/share-candidates（§8 通用候选：scope/org/ws/
%% q/cursor/limit；仅返回主体有权可见目录）
-spec share_candidates_generic(cowboy_req:req(), map()) -> cowboy_req:req().
share_candidates_generic(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    Qs = cowboy_req:parse_qs(Req0),
    Raw = #{
        scope => proplists:get_value(<<"scope">>, Qs, <<>>),
        organization_id => proplists:get_value(<<"organization_id">>, Qs, 0),
        workspace_id => proplists:get_value(<<"workspace_id">>, Qs, 0)
    },
    Q = proplists:get_value(<<"q">>, Qs, <<>>),
    Cursor = proplists:get_value(<<"cursor">>, Qs, undefined),
    Limit = talk_record_logic:parse_limit(proplists:get_value(<<"limit">>, Qs, undefined)),
    case valid_query(Q) of
        true ->
            talk_record_http:respond_ok(
                Req0, talk_record_grant_candidates:generic(Uid, Raw, Q, Cursor, Limit)
            );
        false ->
            talk_record_http:respond_error(Req0, invalid_filter)
    end.

%% GET /api/v1/talk-records/:id/share-candidates（§8 record 级：归属服务端取；
%% owner 门控；排除 owner + 已授权）
-spec share_candidates_for_record(cowboy_req:req(), map()) -> cowboy_req:req().
share_candidates_for_record(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case talk_record_http:path_id(Req0) of
        {ok, Id} ->
            Qs = cowboy_req:parse_qs(Req0),
            Q = proplists:get_value(<<"q">>, Qs, <<>>),
            Cursor = proplists:get_value(<<"cursor">>, Qs, undefined),
            Limit = talk_record_logic:parse_limit(proplists:get_value(<<"limit">>, Qs, undefined)),
            case valid_query(Q) of
                true ->
                    talk_record_http:respond_ok(
                        Req0, talk_record_grant_candidates:for_record(Uid, Id, Q, Cursor, Limit)
                    );
                false ->
                    talk_record_http:respond_error(Req0, invalid_filter)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

-spec transfer_candidates_for_record(cowboy_req:req(), map()) -> cowboy_req:req().
transfer_candidates_for_record(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case talk_record_http:path_id(Req0) of
        {ok, Id} ->
            Qs = cowboy_req:parse_qs(Req0),
            Q = proplists:get_value(<<"q">>, Qs, <<>>),
            Cursor = proplists:get_value(<<"cursor">>, Qs, undefined),
            Limit = talk_record_logic:parse_limit(proplists:get_value(<<"limit">>, Qs, undefined)),
            case valid_query(Q) of
                true ->
                    talk_record_http:respond_ok(
                        Req0, talk_record_transfer_candidates:for_record(Uid, Id, Q, Cursor, Limit)
                    );
                false ->
                    talk_record_http:respond_error(Req0, invalid_filter)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% @private q 限长（§8：q 最大 100 字，codepoint）。
-spec valid_query(term()) -> boolean().
valid_query(Q) when is_binary(Q) ->
    try
        string:length(Q) =< 100
    catch
        _:_ -> false
    end;
valid_query(undefined) ->
    true;
valid_query(_) ->
    false.

%% ===================================================================
%% T19 导出/责任移交域 Actions（§4 exports + §9 ownership-transfers）
%% ===================================================================

%% POST /api/v1/talk-records/:id/exports（format=txt/md；Idempotency-Key；
%% owner 或 grantee+allow_export 且策略允许；202 export/job——inline 同
%% 事务生成文字快照，state=ready 即返）
-spec export_create(cowboy_req:req(), map()) -> cowboy_req:req().
export_create(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    IdemKey = cowboy_req:header(<<"idempotency-key">>, Req0),
    case talk_record_http:path_id(Req0) of
        {ok, Id} ->
            Body = elib_param:post(Req0),
            case talk_record_export_logic:create(Uid, Id, IdemKey, Body) of
                {ok, created, Payload} ->
                    talk_record_http:reply_accepted(Req0, Payload);
                {ok, replay, Payload} ->
                    Req1 = cowboy_req:set_resp_header(<<"idempotent-replayed">>, <<"true">>, Req0),
                    talk_record_http:reply_accepted(Req1, Payload);
                {error, Reason} ->
                    talk_record_http:respond_error(Req0, Reason)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% GET /api/v1/talk-records/:id/exports/:export_id（exportState 轮询；
%% T21 冻结形状：{export_id, state, format, expires_at, job_id}）
-spec export_state(cowboy_req:req(), map()) -> cowboy_req:req().
export_state(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case talk_record_http:path_ids(Req0, [id, export_id]) of
        {ok, [Id, ExportId]} ->
            talk_record_http:respond_ok(Req0, talk_record_export_logic:state(Uid, Id, ExportId));
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% GET /api/v1/talk-records/:id/exports/:export_id/download（每次重验
%% ACL+requester+未过期+未删除）。无 ticket 参数 → 开票（短时票据，
%% T08/T21 exportDownloadUrl 消费 payload.url）；带 ticket → 内容流
%% （验票 + uid claim 比对，§10.3 导出对象短期受控）。
%% 已导出副本不可收回（§11.3）：本端点只控后续签发/下载。
-spec export_download(cowboy_req:req(), map()) -> cowboy_req:req().
export_download(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    Qs = cowboy_req:parse_qs(Req0),
    case talk_record_http:path_ids(Req0, [id, export_id]) of
        {ok, [Id, ExportId]} ->
            case proplists:get_value(<<"ticket">>, Qs, undefined) of
                undefined ->
                    talk_record_http:respond_ok(
                        Req0, talk_record_export_logic:download(Uid, Id, ExportId)
                    );
                Ticket ->
                    stream_export(Req0, Uid, Id, ExportId, Ticket)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% @private 内容流：验票（含 uid claim 与当前身份比对——防拿他人票据）
%% → 200 文本流（Content-Disposition 附件；不进 JSON 信封）。
-spec stream_export(cowboy_req:req(), integer(), integer(), integer(), binary()) ->
    cowboy_req:req().
stream_export(Req0, Uid, Id, ExportId, Ticket) ->
    case talk_record_export_download:verify_ticket(Ticket, Uid, Id, ExportId) of
        {ok,
            #{
                <<"claims">> := #{
                    <<"uid">> := Uid,
                    <<"rid">> := Id,
                    <<"eid">> := ExportId
                },
                <<"content">> := Content,
                <<"format">> := Format
            } = _Row} when is_binary(Content) ->
            {ContentType, Ext} =
                case Format of
                    <<"md">> -> {<<"text/markdown; charset=utf-8">>, <<"md">>};
                    _ -> {<<"text/plain; charset=utf-8">>, <<"txt">>}
                end,
            cowboy_req:reply(
                200,
                #{
                    <<"content-type">> => ContentType,
                    <<"content-disposition">> =>
                        <<"attachment; filename=\"talk-record-export.", Ext/binary, "\"">>
                },
                Content,
                Req0
            );
        {ok, _} ->
            %% 票据属他人：按无效票处理（不泄露导出存在性）
            talk_record_http:respond_error(Req0, export_ticket_invalid);
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% POST /api/v1/talk-records/:id/ownership-transfers（当前 owner 仍有权时；
%% 目标=同企业目录校验；If-Match record:<revision> + Idempotency-Key；
%% keep_access 由请求明确决定；不改 created_by/scope；推 acl_revision）
-spec ownership_transfer(cowboy_req:req(), map()) -> cowboy_req:req().
ownership_transfer(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    IfMatch = cowboy_req:header(<<"if-match">>, Req0),
    IdemKey = cowboy_req:header(<<"idempotency-key">>, Req0),
    case talk_record_http:path_id(Req0) of
        {ok, Id} ->
            Body = elib_param:post(Req0),
            case talk_record_transfer_logic:transfer(Uid, Id, IfMatch, IdemKey, Body) of
                {ok, created, Payload} ->
                    elib_response:success(Req0, Payload);
                {ok, replay, Payload} ->
                    Req1 = cowboy_req:set_resp_header(<<"idempotent-replayed">>, <<"true">>, Req0),
                    elib_response:success(Req1, Payload);
                {error, Reason} ->
                    talk_record_http:respond_error(Req0, Reason)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

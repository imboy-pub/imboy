-module(talk_record_capture_http).
-moduledoc "面聊记录职责拆分；保留原端点、权限与事务语义。".
-include("error_code.hrl").
-export([
    capture_abandon/2,
    capture_create/2,
    capture_event/2,
    capture_recover/2,
    captures_list/2,
    mark_create/2,
    mark_delete/2,
    mark_update/2,
    marks_list/2,
    part_confirm/2,
    part_register/2
]).

%% ===================================================================
%% T07 采集域 Actions
%% ===================================================================

%% GET /api/v1/talk-records/:id/captures（§4 恢复视图）
-spec captures_list(cowboy_req:req(), map()) -> cowboy_req:req().
captures_list(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case talk_record_http:path_id(Req0) of
        {ok, Id} ->
            talk_record_http:respond_ok(Req0, talk_record_ds:list_captures(Uid, Id));
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% POST /api/v1/talk-records/:id/captures（Idempotency-Key + If-Match；§4）
-spec capture_create(cowboy_req:req(), map()) -> cowboy_req:req().
capture_create(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    IdemKey = cowboy_req:header(<<"idempotency-key">>, Req0),
    IfMatch = cowboy_req:header(<<"if-match">>, Req0),
    case talk_record_http:path_id(Req0) of
        {ok, Id} ->
            capture_create_body(Req0, Uid, Id, IdemKey, IfMatch);
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

-spec capture_create_body(
    cowboy_req:req(), integer(), integer(), binary() | undefined, binary() | undefined
) ->
    cowboy_req:req().
capture_create_body(Req0, Uid, Id, IdemKey, IfMatch) ->
    case talk_record_logic:valid_idempotency_key(IdemKey) of
        false ->
            talk_record_http:respond_error(Req0, idempotency_key_required);
        true ->
            case talk_record_logic:parse_if_match(IfMatch) of
                {ok, ExpectedRevision} ->
                    capture_create_parsed(Req0, Uid, Id, IdemKey, ExpectedRevision);
                {error, Reason} ->
                    talk_record_http:respond_error(Req0, Reason)
            end
    end.

-spec capture_create_parsed(cowboy_req:req(), integer(), integer(), binary(), pos_integer()) ->
    cowboy_req:req().
capture_create_parsed(Req0, Uid, Id, IdemKey, ExpectedRevision) ->
    Body = elib_param:post(Req0),
    case talk_record_logic:parse_capture_create(Body) of
        {ok, Validated} ->
            Resource = <<"/api/v1/talk-records/", (integer_to_binary(Id))/binary, "/captures">>,
            case talk_record_logic:request_digest(<<"POST">>, Resource, Body) of
                {ok, Digest} ->
                    respond_capture_create(
                        Req0, Uid, Id, Validated, IdemKey, ExpectedRevision, Digest
                    );
                {error, non_canonical} ->
                    talk_record_http:respond_error(Req0, invalid_body)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

-spec respond_capture_create(
    cowboy_req:req(), integer(), integer(), map(), binary(), pos_integer(), binary()
) -> cowboy_req:req().
respond_capture_create(Req0, Uid, Id, Validated, IdemKey, ExpectedRevision, Digest) ->
    case talk_record_ds:create_capture(Uid, Id, ExpectedRevision, Validated, IdemKey, Digest) of
        {ok, created, Payload} ->
            talk_record_http:reply_created(Req0, Payload);
        {ok, replay, Payload} ->
            Req1 = cowboy_req:set_resp_header(<<"idempotent-replayed">>, <<"true">>, Req0),
            talk_record_http:reply_created(Req1, Payload);
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% POST /api/v1/talk-records/:id/captures/:cid/events（§4）
-spec capture_event(cowboy_req:req(), map()) -> cowboy_req:req().
capture_event(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case talk_record_http:path_ids(Req0, [id, cid]) of
        {ok, [Id, Cid]} ->
            Body = elib_param:post(Req0),
            case talk_record_logic:parse_event(Body) of
                {ok, Validated} ->
                    talk_record_http:respond_ok(
                        Req0, talk_record_ds:append_event(Uid, Id, Cid, Validated)
                    );
                {error, Reason} ->
                    talk_record_http:respond_error(Req0, Reason)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% POST /api/v1/talk-records/:id/captures/:cid/audio-parts（§4）
-spec part_register(cowboy_req:req(), map()) -> cowboy_req:req().
part_register(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    IdemKey = cowboy_req:header(<<"idempotency-key">>, Req0),
    case talk_record_http:path_ids(Req0, [id, cid]) of
        {ok, [Id, Cid]} ->
            part_register_body(Req0, Uid, Id, Cid, IdemKey);
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

-spec part_register_body(cowboy_req:req(), integer(), integer(), integer(), binary() | undefined) ->
    cowboy_req:req().
part_register_body(Req0, Uid, Id, Cid, IdemKey) ->
    case talk_record_logic:valid_idempotency_key(IdemKey) of
        false ->
            talk_record_http:respond_error(Req0, idempotency_key_required);
        true ->
            Body = elib_param:post(Req0),
            case talk_record_logic:parse_audio_part(Body) of
                {ok, Validated} ->
                    part_register_parsed(Req0, Uid, Id, Cid, IdemKey, Body, Validated);
                {error, Reason} ->
                    talk_record_http:respond_error(Req0, Reason)
            end
    end.

-spec part_register_parsed(
    cowboy_req:req(), integer(), integer(), integer(), binary(), map(), map()
) -> cowboy_req:req().
part_register_parsed(Req0, Uid, Id, Cid, IdemKey, Body, Validated) ->
    Resource =
        <<"/api/v1/talk-records/", (integer_to_binary(Id))/binary, "/captures/",
            (integer_to_binary(Cid))/binary, "/audio-parts">>,
    case talk_record_logic:request_digest(<<"POST">>, Resource, Body) of
        {ok, Digest} ->
            respond_part_register(Req0, Uid, Id, Cid, IdemKey, Validated, Digest);
        {error, non_canonical} ->
            talk_record_http:respond_error(Req0, invalid_body)
    end.

-spec respond_part_register(
    cowboy_req:req(), integer(), integer(), integer(), binary(), map(), binary()
) -> cowboy_req:req().
respond_part_register(Req0, Uid, Id, Cid, IdemKey, Validated, Digest) ->
    case talk_record_ds:register_part(Uid, Id, Cid, Validated, IdemKey, Digest) of
        {ok, created, Payload} ->
            talk_record_http:reply_created(Req0, Payload);
        {ok, replay, Payload} ->
            Req1 = cowboy_req:set_resp_header(<<"idempotent-replayed">>, <<"true">>, Req0),
            talk_record_http:reply_created(Req1, Payload);
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% POST /api/v1/talk-records/:id/captures/:cid/audio-parts/:pid/confirm（§4）
%% 202 verifying / 200 confirmed / 幂等重放 200。
-spec part_confirm(cowboy_req:req(), map()) -> cowboy_req:req().
part_confirm(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case talk_record_http:path_ids(Req0, [id, cid, pid]) of
        {ok, [Id, Cid, Pid]} ->
            Body = elib_param:post(Req0),
            case talk_record_logic:parse_confirm_receipt(Body) of
                {ok, Receipt} ->
                    respond_confirm(Req0, Uid, Id, Cid, Pid, Receipt);
                {error, Reason} ->
                    talk_record_http:respond_error(Req0, Reason)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

-spec respond_confirm(cowboy_req:req(), integer(), integer(), integer(), integer(), map()) ->
    cowboy_req:req().
respond_confirm(Req0, Uid, Id, Cid, Pid, Receipt) ->
    case talk_record_ds:confirm_part(Uid, Id, Cid, Pid, Receipt) of
        {ok, verifying, Payload} ->
            talk_record_http:reply_accepted(Req0, Payload#{<<"status">> => <<"verifying">>});
        {ok, confirmed, Payload} ->
            elib_response:success(Req0, Payload#{<<"status">> => <<"confirmed">>});
        {ok, replay, Payload} ->
            elib_response:success(Req0, Payload#{<<"status">> => <<"confirmed">>});
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% POST /api/v1/talk-records/:id/captures/:cid/recover（§11.1）
-spec capture_recover(cowboy_req:req(), map()) -> cowboy_req:req().
capture_recover(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case talk_record_http:path_ids(Req0, [id, cid]) of
        {ok, [Id, Cid]} ->
            Body = elib_param:post(Req0),
            case talk_record_logic:parse_device_session(Body) of
                {ok, #{device_session_id := Session}} ->
                    talk_record_http:respond_ok(
                        Req0, talk_record_ds:recover_capture(Uid, Id, Cid, Session)
                    );
                {error, Reason} ->
                    talk_record_http:respond_error(Req0, Reason)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% POST /api/v1/talk-records/:id/captures/:cid/abandon（owner + If-Match + confirm；§11.1）
-spec capture_abandon(cowboy_req:req(), map()) -> cowboy_req:req().
capture_abandon(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    IfMatch = cowboy_req:header(<<"if-match">>, Req0),
    case talk_record_http:path_ids(Req0, [id, cid]) of
        {ok, [Id, Cid]} ->
            abandon_body(Req0, Uid, Id, Cid, IfMatch);
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

-spec abandon_body(cowboy_req:req(), integer(), integer(), integer(), binary() | undefined) ->
    cowboy_req:req().
abandon_body(Req0, Uid, Id, Cid, IfMatch) ->
    Body = elib_param:post(Req0),
    case talk_record_logic:parse_abandon(Body) of
        {ok, _} ->
            case talk_record_logic:parse_if_match(IfMatch) of
                {ok, ExpectedRevision} ->
                    talk_record_http:respond_ok(
                        Req0, talk_record_ds:abandon_capture(Uid, Id, Cid, ExpectedRevision)
                    );
                {error, Reason} ->
                    talk_record_http:respond_error(Req0, Reason)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% ===================================================================
%% T07 marks Actions
%% ===================================================================

%% GET/POST /api/v1/talk-records/:id/marks（§4）
-spec marks_list(cowboy_req:req(), map()) -> cowboy_req:req().
marks_list(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case talk_record_http:path_id(Req0) of
        {ok, Id} ->
            talk_record_http:respond_ok(Req0, talk_record_marks_logic:list(Uid, Id, 100));
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

-spec mark_create(cowboy_req:req(), map()) -> cowboy_req:req().
mark_create(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case talk_record_http:path_id(Req0) of
        {ok, Id} ->
            Body = elib_param:post(Req0),
            case talk_record_marks_logic:create(Uid, Id, Body) of
                {ok, created, Payload} ->
                    talk_record_http:reply_created(Req0, Payload);
                {ok, replay, Payload} ->
                    Req1 = cowboy_req:set_resp_header(<<"idempotent-replayed">>, <<"true">>, Req0),
                    talk_record_http:reply_created(Req1, Payload);
                {error, Reason} ->
                    talk_record_http:respond_error(Req0, Reason)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

%% PATCH/DELETE /api/v1/talk-records/:id/marks/:mark_id（If-Match；§4）
-spec mark_update(cowboy_req:req(), map()) -> cowboy_req:req().
mark_update(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    IfMatch = cowboy_req:header(<<"if-match">>, Req0),
    case talk_record_http:path_ids(Req0, [id, mark_id]) of
        {ok, [Id, MarkId]} ->
            Body = elib_param:post(Req0),
            case talk_record_marks_logic:update(Uid, Id, MarkId, IfMatch, Body) of
                {ok, Payload} ->
                    Req1 = cowboy_req:set_resp_header(
                        <<"etag">>, maps:get(<<"etag">>, Payload, <<>>), Req0
                    ),
                    elib_response:success(Req1, Payload);
                {error, Reason} ->
                    talk_record_http:respond_error(Req0, Reason)
            end;
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

-spec mark_delete(cowboy_req:req(), map()) -> cowboy_req:req().
mark_delete(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    IfMatch = cowboy_req:header(<<"if-match">>, Req0),
    case talk_record_http:path_ids(Req0, [id, mark_id]) of
        {ok, [Id, MarkId]} ->
            talk_record_http:respond_ok(
                Req0, talk_record_marks_logic:delete(Uid, Id, MarkId, IfMatch)
            );
        {error, Reason} ->
            talk_record_http:respond_error(Req0, Reason)
    end.

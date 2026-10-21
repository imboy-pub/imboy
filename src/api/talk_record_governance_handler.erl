-module(talk_record_governance_handler).
-moduledoc "企业治理元数据入口；组织治理权限不授予录音内容读取权限。".
-export([init/2]).

init(Req, State) ->
    Reply =
        case cowboy_req:method(Req) of
            <<"GET">> -> dispatch(get, Req, State);
            <<"POST">> -> dispatch(post, Req, State);
            _ -> talk_record_http:method_not_allowed(Req)
        end,
    {ok, Reply, State}.

dispatch(Action, Req, State) ->
    case talk_record_http:path_ids(Req, [id, attachment_id]) of
        {ok, [Rid, Aid]} ->
            Uid = maps:get(current_uid, State, 0),
            Result =
                case Action of
                    get ->
                        talk_record_governance_logic:read(Uid, Rid, Aid);
                    post ->
                        talk_record_governance_logic:mutate(
                            Uid,
                            Rid,
                            Aid,
                            cowboy_req:header(<<"if-match">>, Req),
                            cowboy_req:header(<<"idempotency-key">>, Req),
                            elib_param:post(Req)
                        )
                end,
            respond(Req, Result);
        {error, Reason} ->
            talk_record_http:respond_error(Req, Reason)
    end.

respond(Req, {ok, Payload}) ->
    success(Req, Payload);
respond(Req, {ok, created, Payload}) ->
    success(Req, Payload);
respond(Req, {ok, replay, Payload}) ->
    success(cowboy_req:set_resp_header(<<"idempotent-replayed">>, <<"true">>, Req), Payload);
respond(Req, {error, Reason}) ->
    talk_record_http:respond_error(Req, Reason).

success(Req, #{<<"revision">> := Revision} = Payload) ->
    Reply = cowboy_req:set_resp_header(<<"etag">>, talk_record_ds:etag(Revision), Req),
    elib_response:success(Reply, Payload).

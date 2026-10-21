-module(talk_record_cleanup_retry_handler).
-export([init/2]).

init(Req, State) ->
    Reply =
        case cowboy_req:method(Req) of
            <<"POST">> -> dispatch(Req, State);
            _ -> talk_record_http:method_not_allowed(Req)
        end,
    {ok, Reply, State}.

dispatch(Req, State) ->
    case talk_record_http:path_ids(Req, [id]) of
        {ok, [Rid]} ->
            Result = talk_record_cleanup_retry_logic:retry(
                maps:get(current_uid, State, 0),
                Rid,
                cowboy_req:header(<<"if-match">>, Req),
                cowboy_req:header(<<"idempotency-key">>, Req),
                elib_param:post(Req)
            ),
            respond(Req, Result);
        {error, R} ->
            talk_record_http:respond_error(Req, R)
    end.

respond(Req, {ok, #{<<"revision">> := Rev} = P}) ->
    Q = cowboy_req:set_resp_header(<<"etag">>, talk_record_ds:etag(Rev), Req),
    R =
        case maps:get(<<"replayed">>, P, false) of
            true -> cowboy_req:set_resp_header(<<"idempotent-replayed">>, <<"true">>, Q);
            false -> Q
        end,
    %% 202 follows the same business success envelope as other accepted jobs.
    talk_record_http:reply_accepted(R, P);
respond(Req, {error, R}) ->
    talk_record_http:respond_error(Req, R).

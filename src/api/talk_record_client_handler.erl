-module(talk_record_client_handler).
-moduledoc "JWT current_uid/current_did only; run tokens are a separate bounded header.".
-export([init/2, processing_request/2]).
-include("error_code.hrl").

init(Req, State) ->
    Action = maps:get(action, State),
    Result =
        case method(Action) =:= cowboy_req:method(Req) of
            true -> handle(Action, Req, State);
            false -> talk_record_http:method_not_allowed(Req)
        end,
    {ok, Result, State}.

processing_request(Req, State) ->
    Body = elib_param:post(Req),
    case talk_record_http:path_id(Req) of
        {ok, Rid} ->
            Result =
                case execution_mode(Body) of
                    <<"client_byok">> ->
                        talk_record_client_logic:processing_request(
                            context(State), Rid, headers(Req), Body
                        );
                    undefined ->
                        talk_record_job_logic:processing_request(
                            maps:get(current_uid, State, 0),
                            Rid,
                            cowboy_req:header(<<"if-match">>, Req),
                            cowboy_req:header(<<"idempotency-key">>, Req),
                            Body
                        );
                    _ ->
                        {error, client_schema_invalid}
                end,
            respond(Req, Result);
        {error, E} ->
            talk_record_http:respond_error(Req, E)
    end.

handle(Action, Req, State) ->
    Keys =
        case Action of
            claim -> [id, job_id];
            _ -> [id, job_id, run_id]
        end,
    case talk_record_http:path_ids(Req, Keys) of
        {ok, Ids} ->
            [Rid, Jid | Rest] = Ids,
            RunId =
                case Rest of
                    [] -> 0;
                    [I] -> I
                end,
            Seq = seq(Action, Req),
            Body =
                case Action of
                    get -> #{};
                    _ -> elib_param:post(Req)
                end,
            respond(
                Req,
                talk_record_client_logic:execute(Action, context(State), Rid, Jid, RunId, Seq, {
                    headers(Req), Body
                })
            );
        {error, E} ->
            talk_record_http:respond_error(Req, E)
    end.

context(State) ->
    #{uid => maps:get(current_uid, State, 0), did => maps:get(current_did, State, undefined)}.
headers(Req) ->
    #{
        if_match => cowboy_req:header(<<"if-match">>, Req),
        idempotency_key => cowboy_req:header(<<"idempotency-key">>, Req),
        token => cowboy_req:header(<<"x-talk-run-token">>, Req)
    }.
seq(A, Req) when A =:= dispatch; A =:= result ->
    try
        binary_to_integer(cowboy_req:binding(seq, Req, <<"-1">>))
    catch
        _:_ -> -1
    end;
seq(_, _) ->
    0.
method(get) -> <<"GET">>;
method(plan) -> <<"PUT">>;
method(result) -> <<"PUT">>;
method(_) -> <<"POST">>.

respond(Req, {ok, Mark, P}) ->
    Req1 =
        case Mark of
            replay -> cowboy_req:set_resp_header(<<"idempotent-replayed">>, <<"true">>, Req);
            _ -> Req
        end,
    Req2 =
        case maps:get(<<"etag">>, P, undefined) of
            E when is_binary(E) ->
                cowboy_req:set_resp_header(<<"etag">>, talk_record_http:quote_etag(E), Req1);
            _ ->
                Req1
        end,
    case Mark of
        read -> elib_response:success(Req2, P);
        _ -> talk_record_http:reply_accepted(Req2, P)
    end;
respond(Req, {error, client_schema_invalid}) ->
    elib_response:error_with_status(
        Req, 422, <<"Invalid client execution data">>, ?ERR_PARAM_INVALID
    );
respond(Req, {error, E}) when
    E =:= client_lease_expired;
    E =:= client_execution_conflict;
    E =:= client_plan_conflict;
    E =:= client_plan_required;
    E =:= client_intent_conflict;
    E =:= client_result_conflict;
    E =:= client_queue_full
->
    elib_response:error_with_status(Req, 409, atom_to_binary(E), ?ERR_CONFLICT);
respond(Req, {error, E}) ->
    talk_record_http:respond_error(Req, E).

execution_mode(Body) when is_map(Body) -> maps:get(<<"execution_mode">>, Body, undefined);
execution_mode(_) -> invalid.

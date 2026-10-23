-module(talk_record_provider_handler).
-moduledoc "JWT-derived provider-task control endpoints; no credential or URL request fields.".
-export([init/2]).
-include("error_code.hrl").
init(Req, State) ->
    A = maps:get(action, State),
    Method =
        case A of
            latest -> <<"GET">>;
            get -> <<"GET">>;
            _ -> <<"POST">>
        end,
    Response =
        case cowboy_req:method(Req) =:= Method of
            true -> handle(A, Req, State);
            false -> talk_record_http:method_not_allowed(Req)
        end,
    {ok, Response, State}.
handle(A, Req, State) ->
    Keys =
        case A of
            latest -> [id, job_id];
            recover_unprepared -> [id, job_id];
            prepare -> [id, job_id, run_id];
            get -> [id, task_id];
            query_execution -> [id, task_id];
            resubmission -> [id, task_id];
            cancel -> [id, task_id];
            reconcile_receipt -> [id, task_id];
            _ -> [id, job_id, run_id, task_id]
        end,
    case talk_record_http:path_ids(Req, Keys) of
        {ok, Values} ->
            Ids = maps:from_list(lists:zip([key(K) || K <- Keys], Values)),
            Context = #{
                uid => maps:get(current_uid, State, 0),
                did => maps:get(current_did, State, undefined)
            },
            H = #{
                if_match => cowboy_req:header(<<"if-match">>, Req),
                idempotency_key => cowboy_req:header(<<"idempotency-key">>, Req),
                token => cowboy_req:header(<<"x-talk-run-token">>, Req)
            },
            B =
                case A of
                    latest -> #{};
                    get -> #{};
                    _ -> elib_param:post(Req)
                end,
            respond(Req, execute(A, Context, Ids, H, B));
        {error, E} ->
            talk_record_http:respond_error(Req, E)
    end.
key(id) -> rid;
key(job_id) -> jid;
key(K) -> K.
respond(Req, {ok, Mark, P}) ->
    Req1 = cowboy_req:set_resp_header(<<"cache-control">>, <<"private, no-store">>, Req),
    Req2 =
        case Mark of
            replay -> cowboy_req:set_resp_header(<<"idempotent-replayed">>, <<"true">>, Req1);
            _ -> Req1
        end,
    case Mark of
        read -> elib_response:success(Req2, P);
        _ -> talk_record_http:reply_accepted(Req2, P)
    end;
respond(Req, {error, client_schema_invalid}) ->
    elib_response:error_with_status(Req, 422, <<"Invalid provider task data">>, ?ERR_PARAM_INVALID);
respond(Req, {error, E}) when
    E =:= async_protocol_required;
    E =:= client_intent_conflict;
    E =:= client_execution_conflict;
    E =:= client_plan_conflict;
    E =:= client_result_conflict;
    E =:= audio_source_not_ready
->
    elib_response:error_with_status(Req, 409, atom_to_binary(E), ?ERR_CONFLICT);
respond(Req, {error, E}) ->
    talk_record_http:respond_error(Req, E).

execute(A, Ctx, #{rid := Rid, jid := Jid}, H, B) when A =:= latest; A =:= recover_unprepared ->
    talk_record_client_recovery_logic:execute(A, Ctx, Rid, Jid, {H, B});
execute(A, Ctx, Ids, H, B) ->
    talk_record_provider_logic:execute(A, Ctx, Ids, H, B).

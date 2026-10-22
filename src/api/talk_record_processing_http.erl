-module(talk_record_processing_http).
-moduledoc "面聊记录职责拆分；保留原端点、权限与事务语义。".
-include("error_code.hrl").
-export([processing_action/4]).

%% Persistent processing commands share the same authenticated request state.
processing_action(Action, Method, Req, State) ->
    case cowboy_req:method(Req) of
        Method -> processing_dispatch(Action, Req, State);
        _ -> talk_record_http:method_not_allowed(Req)
    end.

processing_dispatch(job_retry, Req, State) ->
    case talk_record_http:path_ids(Req, [id, job_id]) of
        {ok, [Id, JobId]} ->
            accepted_result(
                Req,
                talk_record_job_logic:retry_job(
                    maps:get(current_uid, State, 0),
                    Id,
                    JobId,
                    cowboy_req:header(<<"if-match">>, Req),
                    cowboy_req:header(<<"idempotency-key">>, Req)
                )
            );
        {error, Reason} ->
            talk_record_http:respond_error(Req, Reason)
    end;
processing_dispatch(processing_requests, Req, State) ->
    talk_record_client_handler:processing_request(Req, State);
processing_dispatch(Action, Req, State) ->
    case talk_record_http:path_id(Req) of
        {ok, Id} -> processing_record(Action, Req, maps:get(current_uid, State, 0), Id);
        {error, Reason} -> talk_record_http:respond_error(Req, Reason)
    end.

processing_record(jobs, Req, Uid, Id) ->
    talk_record_http:respond_ok(Req, talk_record_job_logic:list_jobs(Uid, Id));
processing_record(Action, Req, Uid, Id) ->
    IfMatch = cowboy_req:header(<<"if-match">>, Req),
    Key = cowboy_req:header(<<"idempotency-key">>, Req),
    Body = elib_param:post(Req),
    Result =
        case Action of
            complete ->
                talk_record_job_logic:complete(Uid, Id, IfMatch, Key, Body);
            processing_requests ->
                talk_record_job_logic:processing_request(Uid, Id, IfMatch, Key, Body)
        end,
    accepted_result(Req, Result).

accepted_result(Req, {ok, replay, Payload}) ->
    Req1 = cowboy_req:set_resp_header(<<"idempotent-replayed">>, <<"true">>, Req),
    talk_record_http:reply_accepted(Req1, Payload);
accepted_result(Req, {ok, created, Payload}) ->
    talk_record_http:reply_accepted(Req, Payload);
accepted_result(Req, {error, Reason}) ->
    talk_record_http:respond_error(Req, Reason).

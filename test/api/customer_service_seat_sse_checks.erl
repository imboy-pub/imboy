%%% Actual production Seat SSE, JWT/device signature, persisted events and revocation.
-module(customer_service_seat_sse_checks).
-export([send_with_stream/2]).
-include_lib("eunit/include/eunit.hrl").
-define(HTTP, customer_service_seat_http_checks).

send_with_stream(H, S) ->
    {ok, _} = application:ensure_all_started(gun),
    {P, Ref} = open(H, S, #{}),
    try
        {Initial, Raw0, false} = wait(P, Ref, fun(E) ->
            maps:get(<<"type">>, E) =:= <<"resync.required">>
        end),
        save("seat-sse-initial.txt", Raw0),
        Cursor = maps:get(<<"event_id">>, Initial),
        R = ?HTTP:send(H, S, <<"synthetic-http-first">>),
        ?assertEqual(200, maps:get(status, R)),
        {Message, Raw1, false} = wait(P, Ref, fun message/1),
        save("seat-sse-message.txt", Raw1),
        #{<<"payload">> := #{<<"message_id">> := MsgId}} = jsone:decode(maps:get(body, R)),
        ?assertEqual(MsgId, maps:get(<<"resource_id">>, Message)),
        ?assertEqual(
            integer_to_binary(maps:get(org_id, S)), maps:get(<<"organization_id">>, Message)
        ),
        ?assertEqual(
            lists:sort([
                <<"event_id">>,
                <<"type">>,
                <<"organization_id">>,
                <<"workspace_id">>,
                <<"resource_type">>,
                <<"resource_id">>,
                <<"resource_version">>,
                <<"occurred_at">>,
                <<"reason">>
            ]),
            lists:sort(maps:keys(Message))
        ),
        gun:close(P),
        reconnect(H, S, Cursor, Message),
        R
    after
        gun:close(P)
    end.

reconnect(H, S, Cursor, Message) ->
    {P, Ref} = open(H, S, #{<<"last-event-id">> => Cursor}),
    Params = #{
        workspace_id => maps:get(workspace_id, S),
        business_identity_id => maps:get(service_identity_id, S),
        at => os:system_time(second)
    },
    Org = maps:get(org_id, S),
    try
        {Replay, Raw, false} = wait(P, Ref, fun message/1),
        ?assertEqual(Message, Replay),
        save("seat-sse-reconnect.txt", Raw),
        {ok, _} = cs_seat_app:suspend_seat(Org, Params),
        {Revoked, Raw1, Finished} = wait(P, Ref, fun(E) ->
            maps:get(<<"event_id">>, E) =:= <<"0">> andalso
                maps:get(<<"reason">>, E) =:= <<"revoked">>
        end),
        ?assertEqual(
            integer_to_binary(maps:get(service_identity_id, S)),
            maps:get(<<"resource_id">>, Revoked)
        ),
        save("seat-sse-revoked.txt", Raw1),
        case Finished of
            true -> ok;
            false -> ?assertMatch({data, fin, _}, gun:await(P, Ref, 5000))
        end,
        denied(H, S)
    after
        gun:close(P),
        {ok, _} = cs_seat_app:resume_seat(Org, Params)
    end.

message(E) -> maps:get(<<"type">>, E) =:= <<"message.appended">>.
open(H, S, Extra) ->
    {ok, P} = gun:open({127, 0, 0, 1}, maps:get(port, H), #{protocols => [http]}),
    try
        {ok, http} = gun:await_up(P, 5000),
        Ref = gun:get(P, path(S), maps:to_list(maps:merge(?HTTP:headers(S), Extra))),
        ?assertMatch({response, nofin, 200, _}, gun:await(P, Ref, 5000)),
        {P, Ref}
    catch
        Class:Reason:Stack ->
            gun:close(P),
            erlang:raise(Class, Reason, Stack)
    end.

denied(H, S) ->
    {ok, P} = gun:open({127, 0, 0, 1}, maps:get(port, H), #{protocols => [http]}),
    try
        {ok, http} = gun:await_up(P, 5000),
        Ref = gun:get(P, path(S), maps:to_list(?HTTP:headers(S))),
        ?assertMatch({response, _, 403, _}, gun:await(P, Ref, 5000))
    after
        gun:close(P)
    end.
path(S) ->
    <<"/api/v1/cs/organizations/", (integer_to_binary(maps:get(org_id, S)))/binary,
        "/seats/me/events?workspace_id=", (integer_to_binary(maps:get(workspace_id, S)))/binary>>.
wait(P, Ref, Pred) -> wait(P, Ref, Pred, <<>>, erlang:monotonic_time(millisecond) + 20000).
wait(P, Ref, Pred, Raw, Deadline) ->
    case match(Pred, Raw) of
        {ok, E} ->
            {E, Raw, false};
        none ->
            Left = Deadline - erlang:monotonic_time(millisecond),
            ?assert(Left > 0),
            case gun:await(P, Ref, Left) of
                {data, nofin, Chunk} ->
                    wait(P, Ref, Pred, <<Raw/binary, Chunk/binary>>, Deadline);
                {data, fin, Chunk} ->
                    Full = <<Raw/binary, Chunk/binary>>,
                    case match(Pred, Full) of
                        {ok, E} -> {E, Full, true};
                        none -> error({sse_frame_missing, fin, Full})
                    end;
                Other ->
                    error({sse_frame_missing, Other, Raw})
            end
    end.
match(Pred, Raw) ->
    Frames = binary:split(Raw, <<"\n\n">>, [global]),
    Complete = lists:sublist(Frames, length(Frames) - 1),
    Events = [
        jsone:decode(Data)
     || Frame <- Complete,
        Line <- binary:split(Frame, <<"\n">>, [global]),
        <<"data: ", Data/binary>> <- [Line]
    ],
    case [E || E <- Events, Pred(E)] of
        [E | _] -> {ok, E};
        [] -> none
    end.

save(Name, Raw) ->
    file:write_file(filename:join(os:getenv("IMBOY_GATE_RUN_DIR", "/tmp"), Name), Raw).

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
        facts_revoked(H, S, maps:get(<<"event_id">>, Message), assignment),
        facts_revoked(H, S, maps:get(<<"event_id">>, Message), member),
        identity_changed(H, S, maps:get(<<"event_id">>, Message)),
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

%% Controlled authorization facts, actual stream recheck and actual request denial.
facts_revoked(H, S, Cursor, Kind) ->
    {P, Ref} = open(H, S, #{<<"last-event-id">> => Cursor}),
    try
        change_fact(S, Kind, false),
        {Revoked, Raw, Finished} = wait(P, Ref, fun(E) ->
            maps:get(<<"event_id">>, E) =:= <<"0">> andalso
                maps:get(<<"reason">>, E) =:= <<"revoked">>
        end),
        ?assertEqual(<<"assignment.changed">>, maps:get(<<"type">>, Revoked)),
        save("seat-sse-" ++ atom_to_list(Kind) ++ ".txt", Raw),
        case Finished of
            true -> ok;
            false -> ?assertMatch({data, fin, _}, gun:await(P, Ref, 5000))
        end,
        denied(H, S),
        ?assertEqual(
            403,
            maps:get(
                status, ?HTTP:send(H, S, <<"synthetic-revoked-", (atom_to_binary(Kind))/binary>>)
            )
        )
    after
        gun:close(P),
        change_fact(S, Kind, true)
    end.
change_fact(S, assignment, Active) ->
    Sql =
        case Active of
            true ->
                <<"UPDATE organization_business_identity_assignment SET status='active',ended_at=NULL WHERE organization_id=$1 AND user_id=$2">>;
            false ->
                <<"UPDATE organization_business_identity_assignment SET status='ended',ended_at=now() WHERE organization_id=$1 AND user_id=$2">>
        end,
    cs_pg_test_fixture:exec(Sql, [maps:get(org_id, S), maps:get(actor_user_id, S)]);
change_fact(S, member, Active) ->
    Status =
        case Active of
            true -> <<"active">>;
            false -> <<"suspended">>
        end,
    cs_pg_test_fixture:exec(
        <<"UPDATE organization_member SET status=$3 WHERE organization_id=$1 AND user_id=$2">>, [
            maps:get(org_id, S), maps:get(actor_user_id, S), Status
        ]
    ).

identity_changed(H, S, Cursor) ->
    {P, Ref} = open(H, S, #{<<"last-event-id">> => Cursor}),
    try
        change_identity(S, false),
        {E, Raw, Finished} = wait(P, Ref, fun(X) ->
            maps:get(<<"event_id">>, X) =:= <<"0">> andalso
                maps:get(<<"reason">>, X) =:= <<"revoked">>
        end),
        ?assertEqual(<<"assignment.changed">>, maps:get(<<"type">>, E)),
        ?assertEqual(
            integer_to_binary(maps:get(service_identity_id, S)), maps:get(<<"resource_id">>, E)
        ),
        save("seat-sse-identity-change.txt", Raw),
        case Finished of
            true -> ok;
            false -> ?assertMatch({data, fin, _}, gun:await(P, Ref, 5000))
        end,
        {Fresh, FRef} = open(H, S, #{}),
        try
            {_Initial, FRaw, false} = wait(Fresh, FRef, fun(X) ->
                maps:get(<<"type">>, X) =:= <<"resync.required">>
            end),
            save("seat-sse-new-identity.txt", FRaw)
        after
            gun:close(Fresh)
        end,
        ?assertEqual(403, maps:get(status, ?HTTP:send(H, S, <<"synthetic-old-identity-session">>)))
    after
        gun:close(P),
        change_identity(S, true)
    end.
change_identity(S, Restore) ->
    Org = maps:get(org_id, S),
    Actor = maps:get(actor_user_id, S),
    Next = maps:get(next_seat, S),
    Peer = maps:get(actor_user_id, Next),
    Target = maps:get(service_identity_id, Next),
    case Restore of
        false ->
            ok = cs_pg_test_fixture:exec(
                <<"UPDATE organization_business_identity_assignment SET status='ended',ended_at=now() WHERE organization_id=$1 AND user_id=$2">>,
                [Org, Peer]
            ),
            cs_pg_test_fixture:exec(
                <<"UPDATE organization_business_identity_assignment SET business_identity_id=$3,version=version+1 WHERE organization_id=$1 AND user_id=$2">>,
                [Org, Actor, Target]
            );
        true ->
            ok = cs_pg_test_fixture:exec(
                <<"UPDATE organization_business_identity_assignment SET business_identity_id=$3,version=version+1 WHERE organization_id=$1 AND user_id=$2">>,
                [Org, Actor, maps:get(service_identity_id, S)]
            ),
            cs_pg_test_fixture:exec(
                <<"UPDATE organization_business_identity_assignment SET status='active',ended_at=NULL WHERE organization_id=$1 AND user_id=$2">>,
                [Org, Peer]
            )
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

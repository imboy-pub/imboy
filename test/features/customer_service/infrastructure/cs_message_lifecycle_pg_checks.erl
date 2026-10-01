%%% Real canonical transactions; synthetic row holders control interleavings only.
-module(cs_message_lifecycle_pg_checks).
-include_lib("eunit/include/eunit.hrl").
-export([closed_visitor/0, close_during_send/0, claim_during_send/0]).
-define(FIX, cs_pg_test_fixture).

closed_visitor() ->
    with_session(fun(S) ->
        {ok, _} = cs_session_app:close(
            maps:get(org_id, S),
            #{
                workspace_id => maps:get(workspace_id, S),
                session_id => maps:get(session_id, S),
                expected_version => 1,
                at => 1700000010
            }
        ),
        ?assertEqual({error, session_already_closed}, send(S)),
        assert_no_message(S)
    end).

close_during_send() -> race(<<"closed">>, {error, session_already_closed}).
claim_during_send() -> race(<<"active">>, {error, conflict}).

with_session(Fun) ->
    S = ?FIX:new_scope(),
    try
        {ok, Session} = cs_session_app:open_session(
            maps:get(org_id, S),
            #{
                workspace_id => maps:get(workspace_id, S),
                contact_id => maps:get(contact_id, S),
                conversation_id => maps:get(conversation_id, S),
                at => 1700000000
            }
        ),
        Fun(S#{session_id => maps:get(id, Session)})
    after
        ?FIX:cleanup(S)
    end.

send(S) ->
    cs_session_app:append_session_message(
        maps:get(org_id, S),
        #{
            workspace_id => maps:get(workspace_id, S),
            session_id => maps:get(session_id, S),
            contact_id => maps:get(contact_id, S),
            client_msg_id => <<"synthetic-lifecycle">>,
            body => <<"synthetic visitor message">>,
            key_ref => ?FIX:key_ref(),
            accepted_at => 1700000011,
            notify => fun(_) -> ok end
        }
    ).

race(Status, Expected) ->
    with_session(fun(S) ->
        Parent = self(),
        Ref = make_ref(),
        Holder = spawn(fun() -> hold(S, Status, Parent, Ref) end),
        try
            Backend =
                receive
                    {Ref, locked, Pid} -> Pid
                after 10000 -> error(holder_timeout)
                end,
            spawn(fun() -> Parent ! {Ref, sent, send(S)} end),
            ?assert(wait_for_blocked(Backend, 100)),
            Holder ! {Ref, commit},
            receive
                {Ref, released, ok} -> ok
            after 10000 -> error(commit_timeout)
            end,
            Result =
                receive
                    {Ref, sent, R} -> R
                after 10000 -> error(send_timeout)
                end,
            ?assertEqual(Expected, Result),
            assert_no_message(S),
            case Status of
                <<"active">> ->
                    {ok, #{accepted := true, replayed := false}} = send(S),
                    ?assertEqual(1, ?FIX:count(maps:get(org_id, S), messages));
                _ ->
                    ok
            end
        after
            Holder ! {Ref, commit}
        end
    end).

hold(S, Status, Parent, Ref) ->
    Result = elib_pg:with_tx(fun(C) ->
        Org = maps:get(org_id, S),
        Session = maps:get(session_id, S),
        {ok, [_]} = elib_pg:query(
            C,
            <<"SELECT id FROM customer_service_session WHERE organization_id=$1 AND id=$2 FOR UPDATE">>,
            [Org, Session]
        ),
        Identity =
            case Status of
                <<"active">> -> maps:get(service_identity_id, S);
                _ -> null
            end,
        {ok, 1} = elib_pg:execute(
            C,
            <<
                "UPDATE customer_service_session SET status=$3,business_identity_id=$4,version=version+1,"
                "claimed_at=CASE WHEN $3='active' THEN now() ELSE NULL END,"
                "closed_at=CASE WHEN $3='closed' THEN now() ELSE NULL END WHERE organization_id=$1 AND id=$2"
            >>,
            [Org, Session, Status, Identity]
        ),
        {ok, [#{<<"pid">> := Pid}]} = elib_pg:query(C, <<"SELECT pg_backend_pid() AS pid">>, []),
        Parent ! {Ref, locked, Pid},
        receive
            {Ref, commit} -> ok
        after 10000 -> error(holder_commit_timeout)
        end
    end),
    Parent ! {Ref, released, Result}.

wait_for_blocked(_Backend, 0) ->
    false;
wait_for_blocked(Backend, Left) ->
    case
        ?FIX:scalar(
            <<"SELECT count(*) AS n FROM pg_stat_activity WHERE $1=ANY(pg_blocking_pids(pid))">>,
            [Backend],
            -1
        )
    of
        N when N > 0 -> true;
        _ ->
            timer:sleep(10),
            wait_for_blocked(Backend, Left - 1)
    end.

assert_no_message(S) ->
    Org = maps:get(org_id, S),
    ?assertEqual(0, ?FIX:count(Org, messages)),
    ?assertEqual(
        0,
        ?FIX:scalar(
            <<"SELECT count(*) AS n FROM enterprise_audit_event WHERE organization_id=$1">>,
            [Org],
            -1
        )
    ),
    ?assertEqual(
        0,
        ?FIX:scalar(
            <<"SELECT count(*) AS n FROM customer_service_event WHERE organization_id=$1 AND action='message.appended'">>,
            [Org],
            -1
        )
    ).

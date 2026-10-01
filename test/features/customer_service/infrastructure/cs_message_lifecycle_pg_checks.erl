%%% Real canonical transactions; synthetic row holders control interleavings only.
-module(cs_message_lifecycle_pg_checks).
-include_lib("eunit/include/eunit.hrl").
-export([
    closed_visitor/0,
    close_during_send/0,
    claim_during_send/0,
    suspended_seat/0,
    suspension_during_send/0,
    installation_revocation_during_send/0,
    widget_token_revocation_during_send/0,
    widget_token_expiry_during_send/0
]).
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

suspended_seat() ->
    with_session(fun(S) ->
        Org = maps:get(org_id, S),
        Params = #{
            workspace_id => maps:get(workspace_id, S),
            business_identity_id => maps:get(service_identity_id, S),
            actor_user_id => maps:get(actor_user_id, S),
            at => 1700000010
        },
        {ok, _} = cs_session_app:claim(Org, Params#{
            session_id => maps:get(session_id, S), expected_version => 1
        }),
        {ok, _} = cs_seat_app:suspend_seat(Org, Params),
        ?assertEqual({error, seat_disabled}, send_seat(S)),
        assert_no_message(S),
        {ok, #{accepted := true}} = send(S),
        {ok, _} = cs_seat_app:resume_seat(Org, Params),
        {ok, #{accepted := true, replayed := false}} = send_seat(S),
        ?assertEqual(2, ?FIX:count(Org, messages))
    end).

send_seat(S) ->
    cs_session_app:append_session_message(maps:get(org_id, S), #{
        workspace_id => maps:get(workspace_id, S),
        session_id => maps:get(session_id, S),
        business_identity_id => maps:get(service_identity_id, S),
        actor_user_id => maps:get(actor_user_id, S),
        client_msg_id => <<"synthetic-seat-lifecycle">>,
        body => <<"synthetic seat message">>,
        key_ref => ?FIX:key_ref(),
        accepted_at => 1700000011,
        notify => fun(_) -> ok end
    }).

suspension_during_send() ->
    with_session(fun(S) ->
        {ok, _} = cs_session_app:claim(maps:get(org_id, S), #{
            workspace_id => maps:get(workspace_id, S),
            session_id => maps:get(session_id, S),
            business_identity_id => maps:get(service_identity_id, S),
            expected_version => 1,
            at => 1700000010
        }),
        Parent = self(),
        Ref = make_ref(),
        Holder = spawn(fun() -> hold_seat(S, Parent, Ref) end),
        try
            Backend =
                receive
                    {Ref, locked, Pid} -> Pid
                after 10000 -> error(holder_timeout)
                end,
            spawn(fun() -> Parent ! {Ref, sent, send_seat(S)} end),
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
            ?assertEqual({error, seat_disabled}, Result),
            assert_no_message(S)
        after
            Holder ! {Ref, commit}
        end
    end).

hold_seat(S, Parent, Ref) ->
    Result = elib_pg:with_tx(fun(C) ->
        {ok, 1} = elib_pg:execute(
            C,
            <<"UPDATE customer_service_seat SET enabled=false WHERE organization_id=$1 AND business_identity_id=$2">>,
            [maps:get(org_id, S), maps:get(service_identity_id, S)]
        ),
        {ok, [#{<<"pid">> := Pid}]} = elib_pg:query(C, <<"SELECT pg_backend_pid() AS pid">>, []),
        Parent ! {Ref, locked, Pid},
        receive
            {Ref, commit} -> ok
        after 10000 -> error(holder_commit_timeout)
        end
    end),
    Parent ! {Ref, released, Result}.

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

installation_revocation_during_send() -> widget_race(installation, installation_revoked).
widget_token_revocation_during_send() -> widget_race(token, token_revoked).
widget_token_expiry_during_send() -> widget_race(expiry, token_expired).

widget_race(Kind, Expected) ->
    with_session(fun(S0) ->
        S = seed_widget(S0, Kind),
        Parent = self(),
        Ref = make_ref(),
        Holder = spawn(fun() -> hold_widget(S, Kind, Parent, Ref) end),
        try
            Result = widget_write_after_hold(S, Kind, Holder, Ref),
            ?assertEqual({error, Expected}, Result),
            assert_no_message(S),
            restore_widget(S),
            {ok, #{accepted := true, replayed := false}} = widget_write(S),
            ?assertEqual(1, ?FIX:count(maps:get(org_id, S), messages))
        after
            Holder ! {Ref, commit},
            Org = maps:get(org_id, S),
            ?FIX:exec(
                <<"DELETE FROM customer_service_visit_token WHERE organization_id=$1 AND widget_installation_id IS NOT NULL">>,
                [Org]
            ),
            ?FIX:exec(
                <<"DELETE FROM customer_service_widget_installation WHERE organization_id=$1">>, [
                    Org
                ]
            )
        end
    end).

widget_write_after_hold(S, Kind, Holder, Ref) ->
    Parent = self(),
    Backend =
        receive
            {Ref, locked, Pid} -> Pid
        after 10000 -> error(holder_timeout)
        end,
    spawn(fun() -> Parent ! {Ref, sent, widget_write(S)} end),
    ?assert(wait_for_blocked(Backend, 100)),
    case Kind of
        expiry ->
            timer:sleep(
                max(0, (maps:get(expires_at, S) - os:system_time(second) + 1) * 1000)
            );
        _ ->
            ok
    end,
    Holder ! {Ref, commit},
    receive
        {Ref, released, ok} -> ok
    after 10000 -> error(commit_timeout)
    end,
    receive
        {Ref, sent, R} -> R
    after 10000 -> error(send_timeout)
    end.

seed_widget(S, Kind) ->
    Org = maps:get(org_id, S),
    I = ?FIX:id(),
    T = ?FIX:id(),
    Secret = binary:encode_hex(crypto:strong_rand_bytes(32)),
    {ok, _} = cs_pg_widget:insert_widget_installation(Org, #{
        id => I,
        public_widget_id => integer_to_binary(I),
        display_name => <<"synthetic-race">>,
        allowed_origins => [],
        branding => #{},
        consent_version => <<"synthetic-v1">>
    }),
    Ttl =
        case Kind of
            expiry -> 5;
            _ -> 3600
        end,
    Expires = os:system_time(second) + Ttl,
    {ok, _} = cs_pg_widget:insert_widget_bootstrap_token(Org, #{
        id => T,
        contact_id => maps:get(contact_id, S),
        widget_installation_id => I,
        token_digest => cs_widget_support:token_digest(#{}, Secret),
        expires_at => Expires
    }),
    S#{installation_id => I, token_id => T, secret => Secret, expires_at => Expires}.

widget_write(S) ->
    cs_widget_session_app:visitor_message(maps:get(org_id, S), #{
        installation_id => maps:get(installation_id, S),
        secret => maps:get(secret, S),
        default_workspace => fun(_) -> {ok, maps:get(workspace_id, S)} end,
        session_id => maps:get(session_id, S),
        at => os:system_time(second),
        client_msg_id => <<"synthetic-widget-race">>,
        body => <<"synthetic body">>,
        key_ref => ?FIX:key_ref(),
        accepted_at => os:system_time(second),
        notify => fun(_) -> ok end
    }).

hold_widget(S, Kind, Parent, Ref) ->
    Result = elib_pg:with_tx(fun(C) ->
        Org = maps:get(org_id, S),
        {ok, [_]} = elib_pg:query(
            C,
            <<"SELECT id FROM customer_service_session WHERE organization_id=$1 AND id=$2 FOR UPDATE">>,
            [Org, maps:get(session_id, S)]
        ),
        case Kind of
            installation ->
                {ok, 1} = elib_pg:execute(
                    C,
                    <<"UPDATE customer_service_widget_installation SET status='revoked',revoked_at=now(),version=version+1 WHERE organization_id=$1 AND id=$2">>,
                    [Org, maps:get(installation_id, S)]
                );
            token ->
                {ok, 1} = elib_pg:execute(
                    C,
                    <<"UPDATE customer_service_visit_token SET revoked_at=now(),version=version+1 WHERE organization_id=$1 AND id=$2">>,
                    [Org, maps:get(token_id, S)]
                );
            expiry ->
                ok
        end,
        {ok, [#{<<"pid">> := Pid}]} = elib_pg:query(C, <<"SELECT pg_backend_pid() AS pid">>, []),
        Parent ! {Ref, locked, Pid},
        receive
            {Ref, commit} -> ok
        after 10000 -> error(holder_commit_timeout)
        end
    end),
    Parent ! {Ref, released, Result}.

restore_widget(S) ->
    Org = maps:get(org_id, S),
    ok = ?FIX:exec(
        <<"UPDATE customer_service_widget_installation SET status='active',revoked_at=NULL,version=version+1 WHERE organization_id=$1 AND id=$2">>,
        [Org, maps:get(installation_id, S)]
    ),
    ?FIX:exec(
        <<"UPDATE customer_service_visit_token SET revoked_at=NULL,expires_at=now()+interval '1 hour',version=version+1 WHERE organization_id=$1 AND id=$2">>,
        [Org, maps:get(token_id, S)]
    ).

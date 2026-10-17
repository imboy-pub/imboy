%% Real canonical writes; expected elapsed seconds are independently specified.
-module(cs_first_reply_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-define(FIX, cs_pg_test_fixture).

cs_first_reply_pg_test_() ->
    {setup, fun eunit_runner:eunit_setup_with_db/0,
        fun({ok, Conn}) -> eunit_runner:eunit_cleanup_db(Conn) end, fun({ok, _}) ->
            [
                {timeout, 60, fun first_facts_and_replay/0},
                {timeout, 60, fun unknown_is_sticky/0},
                {timeout, 60, fun negative_elapsed_rolls_back/0},
                {timeout, 60, fun human_before_visitor/0},
                {timeout, 60, fun invalid_context_has_no_side_effects/0},
                {timeout, 60, fun direct_canonical_marks_unknown/0},
                {timeout, 60, fun generic_message_marks_unknown/0},
                {timeout, 60, fun malformed_receipt_rolls_back/0},
                {timeout, 60, fun wrong_session_receipt_rolls_back/0},
                {timeout, 60, fun legacy_is_unknown/0},
                {timeout, 60, fun known_unreplied_has_no_samples/0},
                {timeout, 60, fun transaction_order_beats_timestamp_order/0},
                {timeout, 60, fun scope_and_shifted_window_sql/0}
            ]
        end}.

first_facts_and_replay() ->
    with_session(fun(S) ->
        {ok, _} = send(S, visitor, <<"v1">>, 1700000010),
        {ok, _} = send(S, visitor, <<"v2">>, 1700000020),
        {ok, _} = send(S, ai, <<"ai1">>, 1700000030),
        {ok, H} = send(S, human(S), <<"h1">>, 1700000060),
        {ok, Replay} = send(S, human(S), <<"h1">>, 1700000090),
        ?assertEqual(true, maps:get(replayed, Replay)),
        ?assertEqual(maps:get(message_id, H), maps:get(message_id, Replay)),
        {ok, _} = send(S, human(S), <<"h2">>, 1700000100),
        F = facts(S),
        ?assertEqual(1, maps:get(<<"response_measurement_version">>, F)),
        ?assertEqual(50, maps:get(<<"human_seconds">>, F)),
        ?assertEqual(20, maps:get(<<"ai_seconds">>, F)),
        {ok, Stats} = stats(S),
        ?assertEqual(#{count => 1, avg_seconds => 50.0}, maps:get(first_human_response, Stats)),
        ?assertEqual(#{count => 1, avg_seconds => 20.0}, maps:get(first_ai_response, Stats)),
        ?assertEqual(0, maps:get(response_unknown_count, Stats))
    end).

unknown_is_sticky() ->
    with_session(fun(S) ->
        {ok, _} = send_with(
            S,
            unknown,
            <<"unclassified">>,
            1700000005,
            #{
                response_origin => human,
                origin => human,
                first_reply => true,
                response_fact => {human, 1, 1700000005}
            }
        ),
        {ok, _} = send(S, visitor, <<"visitor">>, 1700000010),
        {ok, _} = send(S, human(S), <<"human">>, 1700000020),
        ?assertEqual(0, maps:get(<<"response_measurement_version">>, facts(S))),
        {ok, Stats} = stats(S),
        ?assertEqual(1, maps:get(response_unknown_count, Stats)),
        ?assertEqual(#{count => 0, avg_seconds => undefined}, maps:get(first_human_response, Stats))
    end).

negative_elapsed_rolls_back() ->
    with_session(fun(S) ->
        {ok, _} = send(S, visitor, <<"visitor">>, 1700000050),
        Before = counts(S),
        ?assertMatch({error, _}, send(S, human(S), <<"bad-time">>, 1700000040)),
        ?assertEqual(Before, counts(S)),
        ?assertEqual(null, maps:get(<<"human_seconds">>, facts(S))),
        {ok, _} = send(S, human(S), <<"bad-time">>, 1700000060),
        ?assertEqual(10, maps:get(<<"human_seconds">>, facts(S)))
    end).

human_before_visitor() ->
    with_session(fun(S) ->
        {ok, _} = send(S, human(S), <<"before">>, 1700000005),
        {ok, _} = send(S, ai, <<"ai-before">>, 1700000006),
        ?assertEqual(null, maps:get(<<"human_seconds">>, facts(S))),
        ?assertEqual(null, maps:get(<<"ai_seconds">>, facts(S))),
        {ok, _} = send(S, visitor, <<"visitor">>, 1700000010),
        {ok, _} = send(S, human(S), <<"after">>, 1700000040),
        ?assertEqual(30, maps:get(<<"human_seconds">>, facts(S)))
    end).

invalid_context_has_no_side_effects() ->
    with_session(fun(S) ->
        Before = counts(S),
        Bad =
            {authenticated_human, maps:get(actor_user_id, S) + 1, maps:get(service_identity_id, S)},
        ?assertEqual({error, invalid_response_origin}, send(S, Bad, <<"wrong-human">>, 1700000010)),
        ?assertEqual(Before, counts(S))
    end).

direct_canonical_marks_unknown() ->
    with_session(1640995200, fun(S) ->
        Org = maps:get(org_id, S),
        Ws = maps:get(workspace_id, S),
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) AS n FROM customer_service_session WHERE organization_id=$1"
                    " AND workspace_id=$2 AND id=$3 AND queued_at=to_timestamp($4)"
                    " AND claimed_at=to_timestamp($4)"
                >>,
                [Org, Ws, maps:get(session_id, S), 1640995200],
                -1
            )
        ),
        %% Existing 1095-day policy makes these accepted-in-2022 messages truly expired.
        {ok, V} = send(S, visitor, <<"v">>, 1640995210),
        {ok, A} = send(S, ai, <<"known-ai">>, 1640995212),
        {ok, H} = send(S, human(S), <<"known-human">>, 1640995215),
        ?assertEqual(5, maps:get(<<"human_seconds">>, facts(S))),
        {ok, Direct} = eb_pg_tx:accept_message(
            Org,
            Ws,
            direct_params(S, <<"direct">>, 1640995220)
        ),
        {ok, Later} = send(S, human(S), <<"later">>, 1640995230),
        Ids = lists:sort([
            maps:get(message_id, V),
            maps:get(message_id, A),
            maps:get(message_id, H),
            maps:get(id, maps:get(message, Direct)),
            maps:get(message_id, Later)
        ]),
        Before = retention_evidence(S),
        BeforeCounts = counts(S),
        BeforeMessages = retention_message_rows(S),
        assert_expired_messages(S, Ids),
        ?assertMatch(
            {error, #error{code = <<"23514">>}},
            ?FIX:exec(
                <<
                    "DELETE FROM enterprise_message WHERE organization_id=$1"
                    " AND workspace_id=$2 AND id=ANY($3::bigint[])"
                >>,
                [Org, Ws, Ids]
            )
        ),
        ?assertEqual(BeforeCounts, counts(S)),
        ?assertEqual(BeforeMessages, retention_message_rows(S)),
        ?assertEqual(Before, retention_evidence(S)),
        ?assertEqual(Ids, retention_message_ids(S)),
        Params = #{
            workspace_id => Ws,
            synthetic => true,
            actor_user_id => maps:get(actor_user_id, S)
        },
        {ok, Hold} = eb_retention_app:create_hold(Org, Params#{
            scope => <<"conversation">>,
            scope_conversation_id => maps:get(conversation_id, S),
            reason_code => <<"cs-first-reply-synthetic-retention">>
        }),
        Purge = #{workspace_id => Ws, now => erlang:system_time(second), batch_limit => 5},
        HeldCounts = counts(S),
        {ok, Held} = eb_retention_app:purge_batch(Org, Purge),
        ?assertEqual(0, maps:get(deleted, Held)),
        ?assertEqual(HeldCounts, counts(S)),
        ?assertEqual(BeforeMessages, retention_message_rows(S)),
        ?assertEqual(Ids, retention_message_ids(S)),
        ?assertEqual(Before, retention_evidence(S)),
        {ok, _} = eb_retention_app:release_hold(
            Org,
            Params#{hold_id => maps:get(hold_id, Hold)}
        ),
        {ok, Purged} = eb_retention_app:purge_batch(Org, Purge),
        ?assert(maps:get(deleted, Purged) > 0),
        ?assertEqual(length(Ids), maps:get(deleted, Purged)),
        ?assertEqual([], retention_message_ids(S)),
        ?assertEqual(Before, retention_evidence(S))
    end).

assert_expired_messages(S, Ids) ->
    ?assertEqual(
        length(Ids),
        ?FIX:scalar(
            <<
                "SELECT count(*) AS n FROM enterprise_message WHERE organization_id=$1"
                " AND workspace_id=$2 AND id=ANY($3::bigint[]) AND retain_until<=now()"
                " AND extract(epoch from retain_until)-retention_days::bigint*86400>=1640995200"
                " AND extract(epoch from retain_until)-retention_days::bigint*86400<1672531200"
            >>,
            [maps:get(org_id, S), maps:get(workspace_id, S), Ids],
            -1
        )
    ).

retention_message_ids(S) ->
    {ok, Rows} = elib_pg:query(
        <<
            "SELECT id FROM enterprise_message WHERE organization_id=$1"
            " AND workspace_id=$2 ORDER BY id"
        >>,
        [maps:get(org_id, S), maps:get(workspace_id, S)]
    ),
    [maps:get(<<"id">>, R) || R <- Rows].

retention_message_rows(S) ->
    {ok, Rows} = elib_pg:query(
        <<
            "SELECT to_jsonb(m)::text AS row FROM enterprise_message m"
            " WHERE organization_id=$1 AND workspace_id=$2 ORDER BY id"
        >>,
        [maps:get(org_id, S), maps:get(workspace_id, S)]
    ),
    Rows.

retention_evidence(S) ->
    {ok, [First]} = elib_pg:query(
        <<
            "SELECT response_measurement_version,"
            " first_visitor_message_id,first_visitor_message_at,first_human_reply_id,"
            " first_human_reply_at,first_ai_reply_id,first_ai_reply_at"
            " FROM customer_service_session WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
        >>,
        [maps:get(org_id, S), maps:get(workspace_id, S), maps:get(session_id, S)]
    ),
    ?assertEqual(0, maps:get(<<"response_measurement_version">>, First)),
    ?assertEqual(5, maps:get(<<"human_seconds">>, facts(S))),
    ?assertEqual(2, maps:get(<<"ai_seconds">>, facts(S))),
    {ok, Stats} = cs_session_app:session_stats(
        maps:get(org_id, S),
        #{workspace_id => maps:get(workspace_id, S), date => <<"2022-01-01">>}
    ),
    Unknown = maps:with([response_unknown_count, first_human_response, first_ai_response], Stats),
    ?assertEqual(
        #{
            response_unknown_count => 1,
            first_human_response => #{count => 0, avg_seconds => undefined},
            first_ai_response => #{count => 0, avg_seconds => undefined}
        },
        Unknown
    ),
    {First, Unknown}.

generic_message_marks_unknown() ->
    with_session(fun(S) ->
        P = (direct_params(S, <<"ordinary-hook">>, 1700000020))#{
            persist_hook => fun(_, _) -> ok end, notify => fun(_) -> ok end
        },
        {ok, _} = enterprise_business_facade:append_message(
            maps:get(org_id, S),
            P#{workspace_id => maps:get(workspace_id, S)}
        ),
        ?assertEqual(0, maps:get(<<"response_measurement_version">>, facts(S)))
    end).

malformed_receipt_rolls_back() ->
    with_session(fun(S) ->
        Before = counts(S),
        P = (direct_params(S, <<"malformed">>, 1700000020))#{
            persist_hook => fun(_, _) -> {cs_response_observed, forged} end
        },
        ?assertEqual(
            {error, invalid_response_observation},
            eb_pg_tx:accept_message(maps:get(org_id, S), maps:get(workspace_id, S), P)
        ),
        ?assertEqual(Before, counts(S)),
        ?assertEqual(1, maps:get(<<"response_measurement_version">>, facts(S)))
    end).

wrong_session_receipt_rolls_back() ->
    with_session(fun(S) ->
        Before = counts(S),
        P = (direct_params(S, <<"wrong-session">>, 1700000020))#{
            persist_hook => fun(_, Stored) ->
                {cs_response_observed, maps:get(org_id, S), maps:get(workspace_id, S),
                    maps:get(conversation_id, S), maps:get(session_id, S) + 1, maps:get(id, Stored)}
            end
        },
        ?assertEqual(
            {error, invalid_response_observation},
            eb_pg_tx:accept_message(maps:get(org_id, S), maps:get(workspace_id, S), P)
        ),
        ?assertEqual(Before, counts(S))
    end).

legacy_is_unknown() ->
    with_session(fun(S) ->
        ok = ?FIX:exec(
            <<
                "UPDATE customer_service_session SET response_measurement_version=0"
                " WHERE organization_id=$1 AND id=$2"
            >>,
            [maps:get(org_id, S), maps:get(session_id, S)]
        ),
        {ok, _} = send(S, visitor, <<"v">>, 1700000010),
        {ok, _} = send(S, human(S), <<"h">>, 1700000020),
        ?assertEqual(null, maps:get(<<"human_seconds">>, facts(S))),
        {ok, Stats} = stats(S),
        ?assertEqual(1, maps:get(response_unknown_count, Stats))
    end).

known_unreplied_has_no_samples() ->
    with_session(fun(S) ->
        {ok, _} = send(S, visitor, <<"v">>, 1700000010),
        {ok, Stats} = stats(S),
        ?assertEqual(0, maps:get(response_unknown_count, Stats)),
        ?assertEqual(
            #{count => 0, avg_seconds => undefined}, maps:get(first_human_response, Stats)
        ),
        ?assertEqual(#{count => 0, avg_seconds => undefined}, maps:get(first_ai_response, Stats))
    end).

transaction_order_beats_timestamp_order() ->
    with_session(fun(S) ->
        {ok, _} = send(S, visitor, <<"v">>, 1700000010),
        Parent = self(),
        Ref = make_ref(),
        Hook = fun(Conn, _Stored) ->
            {ok, [#{<<"pid">> := Pid}]} = elib_pg:query(
                Conn,
                <<"SELECT pg_backend_pid() AS pid">>,
                []
            ),
            Parent ! {Ref, locked, self(), Pid},
            receive
                {Ref, release} -> ok
            after 5000 -> error(release_timeout)
            end
        end,
        spawn(fun() ->
            Parent !
                {Ref, first,
                    send_with(
                        S,
                        human(S),
                        <<"first-tx">>,
                        1700000030,
                        #{ai_delivery_hook => Hook}
                    )}
        end),
        {First, Backend} =
            receive
                {Ref, locked, P, B} -> {P, B}
            after 5000 -> error(first_tx_timeout)
            end,
        try
            spawn(fun() ->
                Parent ! {Ref, second, send(S, human(S), <<"second-tx-earlier-clock">>, 1700000020)}
            end),
            ?assert(wait_for_blocked(Backend, 100)),
            First ! {Ref, release},
            receive
                {Ref, first, R1} -> ?assertMatch({ok, _}, R1)
            after 10000 -> error(first_result_timeout)
            end,
            receive
                {Ref, second, R2} -> ?assertMatch({ok, _}, R2)
            after 10000 -> error(second_result_timeout)
            end,
            ?assertEqual(20, maps:get(<<"human_seconds">>, facts(S)))
        after
            First ! {Ref, release}
        end
    end).

wait_for_blocked(_Backend, 0) ->
    false;
wait_for_blocked(Backend, Left) ->
    case
        ?FIX:scalar(
            <<
                "SELECT count(*) AS n FROM pg_stat_activity"
                " WHERE $1=ANY(pg_blocking_pids(pid))"
            >>,
            [Backend],
            -1
        )
    of
        N when N > 0 -> true;
        _ ->
            timer:sleep(10),
            wait_for_blocked(Backend, Left - 1)
    end.

scope_and_shifted_window_sql() ->
    with_session(fun(S) ->
        {ok, _} = send(S, visitor, <<"v">>, 1700000010),
        {ok, _} = send(S, ai, <<"ai">>, 1700000030),
        {ok, _} = send(S, human(S), <<"h">>, 1700000060),
        lists:foreach(
            fun(Tz) ->
                {ok, Stats} = cs_session_app:session_stats(
                    maps:get(org_id, S),
                    #{
                        workspace_id => maps:get(workspace_id, S),
                        date => <<"2023-11-14">>,
                        tz_offset => Tz
                    }
                ),
                Start = maps:get(window_start, Stats),
                End = maps:get(window_end, Stats),
                {ok, [Sql]} = elib_pg:query(
                    <<
                        "SELECT count(first_human_reply_at) AS n,"
                        " avg(extract(epoch from (first_human_reply_at-first_visitor_message_at)))::float8 AS avg"
                        " FROM customer_service_session WHERE organization_id=$1 AND workspace_id=$2"
                        " AND response_measurement_version=1 AND queued_at>=to_timestamp($3)"
                        " AND queued_at<to_timestamp($4)"
                    >>,
                    [maps:get(org_id, S), maps:get(workspace_id, S), Start, End]
                ),
                SqlAvg =
                    case maps:get(<<"avg">>, Sql) of
                        null -> undefined;
                        V -> V
                    end,
                Expected =
                    case Tz of
                        480 -> #{count => 0, avg_seconds => undefined};
                        -480 -> #{count => 1, avg_seconds => 50.0}
                    end,
                ?assertEqual(Expected, #{count => maps:get(<<"n">>, Sql), avg_seconds => SqlAvg}),
                ?assertEqual(Expected, maps:get(first_human_response, Stats))
            end,
            [-480, 480]
        ),
        {ok, Empty} = cs_session_app:session_stats(
            maps:get(other_org_id, S),
            #{workspace_id => maps:get(other_workspace_id, S), date => <<"2023-11-14">>}
        ),
        ?assertEqual(0, maps:get(response_unknown_count, Empty)),
        ?assertEqual(#{count => 0, avg_seconds => undefined}, maps:get(first_human_response, Empty))
    end).

direct_params(S, Key, At) ->
    #{
        conversation_id => maps:get(conversation_id, S),
        client_msg_id => Key,
        sender_type => <<"business_identity">>,
        identity_id => maps:get(service_identity_id, S),
        actor_user_id => maps:get(actor_user_id, S),
        body => <<"synthetic generic">>,
        key_ref => ?FIX:key_ref(),
        accepted_at => At
    }.

human(S) -> {authenticated_human, maps:get(actor_user_id, S), maps:get(service_identity_id, S)}.

send(S, Origin, Key, At) -> send_with(S, Origin, Key, At, #{}).

send_with(S, Origin, Key, At, Extra) ->
    Sender =
        case Origin of
            visitor ->
                #{contact_id => maps:get(contact_id, S)};
            _ ->
                #{
                    business_identity_id => maps:get(service_identity_id, S),
                    actor_user_id => maps:get(actor_user_id, S)
                }
        end,
    Params = maps:merge(
        Extra,
        maps:merge(Sender, #{
            workspace_id => maps:get(workspace_id, S),
            session_id => maps:get(session_id, S),
            client_msg_id => Key,
            body => <<"synthetic reply">>,
            key_ref => ?FIX:key_ref(),
            accepted_at => At,
            notify => fun(_) -> ok end
        })
    ),
    case Origin of
        unknown -> cs_session_app:append_session_message(maps:get(org_id, S), Params);
        _ -> cs_session_app:append_session_message(maps:get(org_id, S), Params, Origin)
    end.

facts(S) ->
    {ok, [Row]} = elib_pg:query(
        <<
            "SELECT response_measurement_version,"
            " extract(epoch from (first_human_reply_at-first_visitor_message_at))::bigint AS human_seconds,"
            " extract(epoch from (first_ai_reply_at-first_visitor_message_at))::bigint AS ai_seconds"
            " FROM customer_service_session WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
        >>,
        [maps:get(org_id, S), maps:get(workspace_id, S), maps:get(session_id, S)]
    ),
    Row.

counts(S) ->
    [
        ?FIX:count(maps:get(org_id, S), messages),
        ?FIX:scalar(
            <<"SELECT count(*) AS n FROM customer_service_event WHERE organization_id=$1">>,
            [maps:get(org_id, S)],
            -1
        ),
        ?FIX:scalar(
            <<"SELECT count(*) AS n FROM enterprise_audit_event WHERE organization_id=$1">>,
            [maps:get(org_id, S)],
            -1
        )
    ].

stats(S) ->
    cs_session_app:session_stats(
        maps:get(org_id, S),
        #{workspace_id => maps:get(workspace_id, S), date => <<"2023-11-14">>}
    ).

with_session(Fun) -> with_session(1700000000, Fun).

with_session(At, Fun) ->
    S = ?FIX:new_scope(),
    try
        {ok, Session} = cs_session_app:open_session(
            maps:get(org_id, S),
            #{
                workspace_id => maps:get(workspace_id, S),
                contact_id => maps:get(contact_id, S),
                conversation_id => maps:get(conversation_id, S),
                at => At
            }
        ),
        Id = maps:get(id, Session),
        {ok, _} = cs_session_app:claim(
            maps:get(org_id, S),
            #{
                workspace_id => maps:get(workspace_id, S),
                session_id => Id,
                business_identity_id => maps:get(service_identity_id, S),
                expected_version => 1,
                at => At
            }
        ),
        Fun(S#{session_id => Id})
    after
        ?FIX:cleanup(S)
    end.

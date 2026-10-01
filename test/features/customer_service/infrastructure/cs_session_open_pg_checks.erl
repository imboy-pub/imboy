%%% Actual session application and production store on an owned marker database.
-module(cs_session_open_pg_checks).
-include_lib("eunit/include/eunit.hrl").
-export([run/1]).

run(C) ->
    audit_rollback(C),
    concurrent_open(C).

audit_rollback(C) ->
    Scope = cs_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, Scope),
    Params = #{
        workspace_id => maps:get(workspace_id, Scope),
        contact_id => maps:get(contact_id, Scope),
        conversation_id => maps:get(conversation_id, Scope),
        at => 1760000000
    },
    ok = intbe02_http_support:sql_exec(
        C,
        <<"ALTER TABLE customer_service_event ADD CONSTRAINT synthetic_open_audit_reject CHECK(action <> 'session.opened') NOT VALID">>
    ),
    try
        ?assertMatch({error, _}, cs_session_app:open_session(Org, Params)),
        assert_count(C, <<"customer_service_session">>, Org, 0),
        assert_count(C, <<"customer_service_event">>, Org, 0)
    after
        ok = intbe02_http_support:sql_exec(
            C,
            <<"ALTER TABLE customer_service_event DROP CONSTRAINT synthetic_open_audit_reject">>
        )
    end,
    {ok, Session} = cs_session_app:open_session(Org, Params),
    ?assertEqual(queued, maps:get(status, Session)),
    assert_count(C, <<"customer_service_session">>, Org, 1),
    assert_count(C, <<"customer_service_event">>, Org, 1),
    ?assertEqual({error, conflict}, cs_session_app:open_session(Org, Params)),
    assert_count(C, <<"customer_service_session">>, Org, 1),
    assert_count(C, <<"customer_service_event">>, Org, 1).

assert_count(C, Table, Org, Expected) ->
    Sql = <<"SELECT count(*)::integer AS n FROM ", Table/binary, " WHERE organization_id=$1">>,
    #{<<"n">> := N} = intbe02_http_support:one(C, Sql, [Org]),
    ?assertEqual(Expected, N, {table, Table, organization_id, Org}).

concurrent_open(C) ->
    Scope = cs_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, Scope),
    Params = #{
        workspace_id => maps:get(workspace_id, Scope),
        contact_id => maps:get(contact_id, Scope),
        conversation_id => maps:get(conversation_id, Scope),
        at => 1760000000
    },
    Parent = self(),
    Workers = [
        spawn(fun() ->
            receive
                go -> Parent ! {self(), cs_session_app:open_session(Org, Params)}
            end
        end)
     || _ <- [1, 2]
    ],
    [Pid ! go || Pid <- Workers],
    Results = [
        receive
            {Pid, Result} -> Result
        after 10000 -> error(open_race_timeout)
        end
     || Pid <- Workers
    ],
    ?assertEqual(1, length([S || {ok, S} <- Results]), {results, Results}),
    ?assertEqual(1, length([conflict || {error, conflict} <- Results])),
    assert_count(C, <<"customer_service_session">>, Org, 1),
    assert_count(C, <<"customer_service_event">>, Org, 1).

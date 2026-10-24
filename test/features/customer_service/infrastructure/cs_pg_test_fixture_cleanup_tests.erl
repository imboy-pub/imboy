-module(cs_pg_test_fixture_cleanup_tests).
-include_lib("eunit/include/eunit.hrl").

scope() ->
    #{
        owner_user_id => 1,
        actor_user_id => 2,
        peer_user_id => 3,
        org_id => 4,
        workspace_id => 5,
        other_org_id => 6,
        other_workspace_id => 7
    }.

scope_guard_test() ->
    S = scope(),
    ?assertEqual(ok, cs_pg_test_fixture:cleanup_scope(S)),
    ?assertException(error, _, cs_pg_test_fixture:cleanup_scope(#{})),
    ?assertException(error, _, cs_pg_test_fixture:cleanup_scope(maps:remove(other_org_id, S))),
    ?assertException(error, _, cs_pg_test_fixture:cleanup_scope(S#{org_id => 1})),
    ?assertException(error, _, cs_pg_test_fixture:cleanup_scope(S#{workspace_id => 0})),
    ?assertException(error, _, cs_pg_test_fixture:cleanup_scope(undefined)).

sql_errors_propagate_test() ->
    ?assertEqual(ok, cs_pg_test_fixture:checked_cleanup_result(<<"sql">>, {ok, 0})),
    ?assertEqual(ok, cs_pg_test_fixture:checked_cleanup_result(<<"sql">>, {ok, 1, []})),
    lists:foreach(
        fun(Error) ->
            ?assertError(
                {cs_fixture_cleanup_sql_failed, <<"sql">>, Error},
                cs_pg_test_fixture:checked_cleanup_result(<<"sql">>, Error)
            )
        end,
        [{error, fk_csais_session}, {rollback, failed}, unexpected]
    ).

fk_order_and_exact_scope_test() ->
    Plan = cs_pg_test_fixture:cleanup_plan(scope()),
    Deletes = [
        {Sql, Params}
     || {Sql, Params} <- Plan,
        binary:match(Sql, <<"DELETE FROM ">>) =:= {0, 12}
    ],
    lists:foreach(
        fun({Sql, Params}) ->
            ?assertEqual(nomatch, binary:match(Sql, <<"TRUNCATE">>)),
            ?assert(binary:match(Sql, <<" WHERE ">>) =/= nomatch),
            ?assert(length(Params) > 0)
        end,
        Deletes
    ),
    Sqls = [Sql || {Sql, _} <- Deletes],
    assert_widget_retention_order(Deletes, Sqls),
    ?assert(
        index(<<"customer_service_ai_session">>, Sqls) < index(<<"customer_service_session">>, Sqls)
    ),
    ?assert(
        index(<<"customer_service_read_cursor">>, Sqls) <
            index(<<"customer_service_session">>, Sqls)
    ),
    ?assert(index(<<"customer_service_session">>, Sqls) < index(<<"customer_service_seat">>, Sqls)),
    ?assertEqual(2, length([ok || {<<"DELETE FROM workspace", _/binary>>, _} <- Deletes])),
    ?assertEqual(2, length([ok || {<<"DELETE FROM organization WHERE", _/binary>>, _} <- Deletes])),
    ?assertEqual(3, length([ok || {<<"DELETE FROM \"user\"", _/binary>>, _} <- Deletes])),
    Disabled = [Sql || {Sql, _} <- Plan, binary:match(Sql, <<" DISABLE TRIGGER ">>) =/= nomatch],
    Enabled = [Sql || {Sql, _} <- Plan, binary:match(Sql, <<" ENABLE TRIGGER ">>) =/= nomatch],
    ?assertEqual(5, length(Disabled)),
    ?assert(
        lists:member(
            <<"ALTER TABLE enterprise_retention_hold DISABLE TRIGGER trg_enterprise_retention_hold_append_only">>,
            Disabled
        )
    ),
    ?assertEqual(
        [binary:replace(Sql, <<" DISABLE ">>, <<" ENABLE ">>) || Sql <- Disabled], Enabled
    ).

assert_widget_retention_order(Deletes, Sqls) ->
    Installation = index(<<"customer_service_widget_installation">>, Sqls),
    ScopedTables = [
        <<"customer_service_widget_nonce">>,
        <<"customer_service_widget_identity_key">>,
        <<"customer_service_visit_token">>,
        <<"customer_service_widget_installation">>,
        <<"enterprise_retention_hold">>
    ],
    lists:foreach(
        fun(Table) ->
            Exact = delete_sql(Table),
            [Params] = [P || {Sql, P} <- Deletes, Sql =:= Exact],
            ?assertEqual([4, 6], Params)
        end,
        ScopedTables
    ),
    lists:foreach(
        fun(Table) ->
            ?assert(index(Table, Sqls) < Installation)
        end,
        [
            <<"customer_service_widget_nonce">>,
            <<"customer_service_widget_identity_key">>,
            <<"customer_service_visit_token">>
        ]
    ),
    Hold = index(<<"enterprise_retention_hold">>, Sqls),
    lists:foreach(
        fun(Table) ->
            ?assert(Hold < index(Table, Sqls))
        end,
        [<<"enterprise_message">>, <<"enterprise_conversation">>, <<"enterprise_retention_policy">>]
    ),
    ParentPositions = [
        N
     || {N, <<"DELETE FROM organization WHERE", _/binary>>} <-
            lists:zip(lists:seq(1, length(Sqls)), Sqls)
    ],
    ?assertEqual(2, length(ParentPositions)),
    lists:foreach(
        fun(N) ->
            ?assert(Installation < N),
            ?assert(Hold < N)
        end,
        ParentPositions
    ).

index(Table, Sqls) ->
    Exact = delete_sql(Table),
    [N] = [I || {I, Sql} <- lists:zip(lists:seq(1, length(Sqls)), Sqls), Sql =:= Exact],
    N.

delete_sql(Table) ->
    <<"DELETE FROM ", Table/binary, " WHERE organization_id IN ($1,$2)">>.

index_requires_exactly_one_delete_test() ->
    Table = <<"customer_service_widget_installation">>,
    ?assertException(error, _, index(Table, [])),
    ?assertException(error, _, index(Table, [delete_sql(Table), delete_sql(Table)])),
    ?assertEqual(1, index(Table, [delete_sql(Table)])).

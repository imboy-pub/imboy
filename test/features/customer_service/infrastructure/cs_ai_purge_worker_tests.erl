-module(cs_ai_purge_worker_tests).
-include_lib("eunit/include/eunit.hrl").

error_propagation_test_() ->
    [
        {timeout, 35, ?_test(pure_mock_fixture:run(fun() -> failure_case(Kind) end))}
     || Kind <- [connect, query]
    ].

failure_case(Kind) ->
    pure_mock_fixture:with_mocks(
        [
            {config_ds, [{env, 1, fun(cs_ai_purge) -> #{host => synthetic} end}]},
            {elib_tsid, [{generate, 1, fun(enterprise_audit_event) -> 123 end}]},
            {epgsql, [
                {connect, 1, fun(_) ->
                    case Kind of
                        connect -> {error, synthetic_connect_failure};
                        query -> {ok, conn}
                    end
                end},
                {squery, 2, fun(conn, Sql) ->
                    case Sql of
                        <<"ROLLBACK">> -> put(rolled_back, true);
                        _ -> ok
                    end,
                    {ok, [], []}
                end},
                {equery, 3, fun(conn, _, [10, _, <<"retention">>, <<"request">>, 123]) ->
                    {error, synthetic_query_failure}
                end},
                {close, 1, fun(conn) ->
                    put(closed, true),
                    ok
                end}
            ]}
        ],
        fun() ->
            Expected =
                case Kind of
                    connect -> {error, {purge_connect_failed, synthetic_connect_failure}};
                    query -> {error, synthetic_query_failure}
                end,
            ?assertEqual(Expected, cs_ai_purge_worker:purge(10, 0, <<"retention">>, <<"request">>)),
            ?assertEqual(Kind =:= query, get(rolled_back) =:= true),
            ?assertEqual(Kind =:= query, get(closed) =:= true)
        end
    ).

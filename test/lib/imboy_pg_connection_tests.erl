-module(imboy_pg_connection_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

connect_sets_statement_timeout_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 2, fun(statement_timeout_ms, _) -> 2500 end}
            ]},
            {epgsql, [
                {'connect', 1, fun(_) -> {ok, self()} end},
                {'squery', 2, fun(_, Sql) ->
                    case iolist_to_binary(Sql) of
                        <<"SET statement_timeout TO 2500">> -> {ok, [], []};
                        <<"SELECT pg_catalog.to_tsvector(", _/binary>> -> {ok, [], []}
                    end
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, self()}, imboy_pg_connection:connect(#{})),
            [{_, {epgsql, squery, [_, Setup]}, _}, {_, {epgsql, squery, [_, Warmup]}, _}] =
                [Call || {_, {epgsql, squery, _}, _} = Call <- meck:history(epgsql)],
            ?assertEqual(<<"SET statement_timeout TO 2500">>, iolist_to_binary(Setup)),
            ?assertNotEqual(
                nomatch, binary:match(Warmup, <<"n.nspname = 'public' AND c.cfgname = 'jiebacfg'">>)
            )
        end
    ).

connect_rejects_unconfigured_session_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 2, fun(statement_timeout_ms, _) -> 15000 end}
            ]},
            {epgsql, [
                {'connect', 1, fun(_) -> {ok, self()} end},
                {'squery', 2, fun(_, _) -> {error, denied} end},
                {'close', 1, fun(_) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, {statement_timeout_setup_failed, denied}},
                imboy_pg_connection:connect(#{})
            ),
            ?assertEqual(1, meck:num_calls(epgsql, close, 1))
        end
    ).

warmup_timeout_preserves_connection_test_() ->
    with_warmup_error(#error{severity = error, code = <<"57014">>}, fun() ->
        ?assertEqual({ok, self()}, imboy_pg_connection:connect(#{})),
        ?assertEqual(0, meck:num_calls(epgsql, close, 1))
    end).

warmup_transport_error_closes_connection_test_() ->
    with_warmup_error(closed, fun() ->
        ?assertEqual(
            {error, {search_warmup_connection_failed, closed}},
            imboy_pg_connection:connect(#{})
        ),
        ?assertEqual(1, meck:num_calls(epgsql, close, 1))
    end).

with_warmup_error(Reason, Check) ->
    ?WITH_MECKS(
        [
            {config_ds, [{'env', 2, fun(statement_timeout_ms, _) -> 100 end}]},
            {epgsql, [
                {'connect', 1, fun(_) -> {ok, self()} end},
                {'squery', 2, fun
                    (_, <<"SELECT pg_catalog.to_tsvector(", _/binary>>) -> {error, Reason};
                    (_, [<<"SET statement_timeout TO ">>, <<"100">>]) -> {ok, [], []}
                end},
                {'close', 1, fun(_) -> ok end}
            ]}
        ],
        Check
    ).

-module(imboy_pg_connection_tests).

-include_lib("eunit/include/eunit.hrl").
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
                    ?assertEqual(<<"SET statement_timeout TO 2500">>, iolist_to_binary(Sql)),
                    {ok, [], []}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, self()}, imboy_pg_connection:connect(#{}))
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

-module(elib_pg_error_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

sqlstate_shapes_test() ->
    Error = #error{
        severity = error,
        code = <<"23505">>,
        codename = unique_violation,
        message = <<"synthetic constraint">>,
        extra = []
    },
    ?assertEqual(<<"23505">>, elib_pg_error:sqlstate(Error)),
    ?assertEqual(<<"23505">>, elib_pg_error:sqlstate({error, Error})),
    ?assertEqual(undefined, elib_pg_error:sqlstate(Error#error{code = <<"bad">>})),
    ?assertEqual(undefined, elib_pg_error:sqlstate({error, malformed})),
    ?assertEqual(undefined, elib_pg_error:sqlstate(unexpected)).

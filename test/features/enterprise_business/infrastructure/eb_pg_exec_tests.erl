-module(eb_pg_exec_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

returning_contract_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        {Name, fun() -> check_contract(DriverResult, ExecuteResult, InsertResult) end}
     || {Name, DriverResult, ExecuteResult, InsertResult} <- cases()
    ]}.

setup() ->
    meck:new(elib_pg, [no_link]).

cleanup(_) ->
    meck:unload(elib_pg).

cases() ->
    SqlError = #error{
        severity = error,
        code = <<"23505">>,
        codename = unique_violation,
        message = <<"fixture conflict">>,
        extra = [{constraint_name, <<"fixture_unique">>}]
    },
    NormalizedError = {error, {sql, <<"23505">>, <<"fixture_unique">>}},
    [
        {"tuple row", {ok, 1, [{7}]}, {ok, [{7}]}, {ok, [{7}]}},
        {"multiple tuple rows", {ok, 2, [{7}, {8}]}, {ok, [{7}, {8}]}, {ok, [{7}, {8}]}},
        {"empty rows", {ok, 0, []}, {ok, []}, {error, no_row}},
        {"SQL error", {error, SqlError}, NormalizedError, NormalizedError},
        {"connection error", {error, closed}, {error, {db, closed}}, {error, {db, closed}}}
    ].

check_contract(DriverResult, ExecuteResult, InsertResult) ->
    Sql = <<"INSERT INTO fixture(id) VALUES ($1) RETURNING id">>,
    Params = [7],
    meck:expect(elib_pg, execute, fun(SqlArg, ParamsArg) ->
        ?assertEqual(Sql, SqlArg),
        ?assertEqual(Params, ParamsArg),
        DriverResult
    end),
    ?assertEqual(ExecuteResult, eb_pg_exec:execute_returning(Sql, Params)),
    ?assertEqual(InsertResult, eb_pg_exec:insert_returning(Sql, Params)),
    ?assertEqual(2, meck:num_calls(elib_pg, execute, [Sql, Params])),
    ?assert(meck:validate(elib_pg)).

-module(attachment_derived_artifact_state_tests).
-moduledoc "派生工件两条写入入口保留 complete/unknown 与隔离裁决。".

-include_lib("eunit/include/eunit.hrl").

write_outcome_state_test_() ->
    [
        {Name, fun() ->
            with_mocks(Result, State, SaveResult, fun() ->
                Actual =
                    case Mode of
                        bytes -> attachment_derived_artifact:write(21, <<"content">>);
                        worker -> attachment_derived_artifact:finish_write(21, Result)
                    end,
                ?assertEqual(Expected, Actual)
            end)
        end}
     || {Name, Mode, Result, State, SaveResult, Expected} <- [
            {"legacy success", bytes, ok, <<"complete">>, {ok, 1}, ok},
            {"legacy uncertain", bytes, {error, timeout}, <<"unknown">>,
                {ok, 1}, {error, timeout}},
            {"worker success", worker, {ok, done}, <<"complete">>, {ok, 1}, {ok, done}},
            {"worker uncertain", worker, {error, timeout}, <<"unknown">>,
                {ok, 1}, {error, timeout}},
            {"legacy quarantine", bytes, ok, <<"complete">>, {ok, 0},
                {error, artifact_write_quarantined}},
            {"worker quarantine", worker, {ok, done}, <<"complete">>, {error, db_error},
                {error, artifact_write_quarantined}}
        ]
    ].

with_mocks(Result, State, SaveResult, F) ->
    Mods = [elib_pg, attachment_storage, attachment_storage_garage],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Mods),
    try
        meck:expect(elib_pg, with_tx, fun(Fn, _) -> Fn(conn) end),
        meck:expect(elib_pg, query, fun(conn, _, [21]) ->
            {ok, [#{<<"path">> => <<"artifact/test">>}]}
        end),
        meck:expect(elib_pg, execute, fun(conn, _, [21]) -> {ok, 1} end),
        meck:expect(elib_pg, execute, fun(Sql, [21, ActualState]) ->
            ?assertEqual(State, ActualState),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"AND state='writing'">>)),
            SaveResult
        end),
        meck:expect(attachment_storage, frozen_location, fun(<<"artifact/test">>, _) ->
            {ok, #{driver => garage, mime => <<"text/plain">>}}
        end),
        meck:expect(attachment_storage_garage, put_snapshot, fun(_, <<"content">>, _, _) ->
            Result
        end),
        F()
    after
        lists:foreach(fun meck:unload/1, Mods)
    end.

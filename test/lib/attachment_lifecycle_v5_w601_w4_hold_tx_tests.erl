-module(attachment_lifecycle_v5_w601_w4_hold_tx_tests).

-include_lib("eunit/include/eunit.hrl").

marker_tx_result_test_() ->
    {setup,
        fun() ->
            ok = meck:new(epgsql, [passthrough, no_link]),
            ok = meck:expect(epgsql, with_transaction, fun(C, F, [{reraise, true}]) -> F(C) end),
            ok
        end,
        fun(_) -> meck:unload(epgsql) end, fun(_) ->
            [
                fun committed/0,
                fun rolled_back_error/0,
                fun unknown_commit/0,
                fun abort_result/0
            ]
        end}.

committed() ->
    ok = meck:expect(epgsql, get_cmd_status, fun(_) -> {ok, commit} end),
    ?assertEqual({ok, result}, tx(fun(_) -> {ok, result} end)).

rolled_back_error() ->
    ok = meck:expect(epgsql, get_cmd_status, fun(_) -> {ok, rollback} end),
    ?assertEqual({error, rejected}, tx(fun(_) -> {error, rejected} end)).

unknown_commit() ->
    ok = meck:expect(epgsql, get_cmd_status, fun(_) -> {error, connection_lost} end),
    ?assertEqual(
        {error, {commit_not_confirmed, ok, {error, connection_lost}}},
        tx(fun(_) -> ok end)
    ).

abort_result() ->
    ?assertEqual({error, rejected}, tx(fun(_) -> throw({abort_tx, rejected}) end)).

tx(F) -> attachment_lifecycle_v5_w601_w4_hold_tests:marker_with_tx(conn, F).

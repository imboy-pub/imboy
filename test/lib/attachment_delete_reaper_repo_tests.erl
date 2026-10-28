-module(attachment_delete_reaper_repo_tests).
-include_lib("eunit/include/eunit.hrl").
reap_contract_test_() ->
    {foreach,
        fun() ->
            meck:new(elib_pg, [non_strict, no_link]),
            meck:expect(elib_pg, with_tx, fun(F) -> F(conn) end)
        end,
        fun(_) -> meck:unload(elib_pg) end, [
            fun bounded_reapers/0, fun malformed_result_rejected/0
        ]}.
bounded_reapers() ->
    meck:expect(elib_pg, query, fun(conn, Sql, Params) ->
        case Sql of
            <<"SELECT set_config('statement_timeout','5000',true)">> ->
                ?assertEqual([], Params),
                put(timeout_set, true),
                {ok, []};
            <<"SELECT fn_v5_reaper_ordinary($1)">> ->
                ?assertEqual(true, erase(timeout_set)),
                ?assertEqual([100], Params),
                {ok, [#{<<"fn_v5_reaper_ordinary">> => 4}]};
            <<"SELECT fn_v5_reaper_eb($1)">> ->
                ?assertEqual(true, erase(timeout_set)),
                ?assertEqual([100], Params),
                {ok, [#{<<"fn_v5_reaper_eb">> => 2}]}
        end
    end),
    ?assertEqual({ok, 4}, attachment_gc_repo:v5_reap(100)),
    ?assertEqual({ok, 2}, eb_pg_asset_delete_queue:reap(100)),
    ?assertError(function_clause, attachment_gc_repo:v5_reap(101)),
    ?assertError(function_clause, eb_pg_asset_delete_queue:reap(101)).
malformed_result_rejected() ->
    meck:expect(elib_pg, query, fun(_, _, _) -> {ok, []} end),
    ?assertEqual({error, {unexpected_reaper_result, []}}, attachment_gc_repo:v5_reap(100)),
    ?assertEqual({error, {unexpected_reaper_result, []}}, eb_pg_asset_delete_queue:reap(100)).

-module(msg_delivery_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

mark_acked_batch_builds_upsert_test_() ->
    repo_case(fun() ->
        ?assertEqual(
            {ok, 2},
            msg_delivery_repo:mark_acked_batch(<<"c2c">>, [<<"m1">>, <<"m2">>], 100, <<"did-a">>)
        ),
        [{_, {elib_pg, execute_count, [_, Sql, Params]}, _}] = [
            H
         || H = {_, {elib_pg, execute_count, _}, _} <- meck:history(elib_pg)
        ],
        ?assertEqual([<<"c2c">>, 100, <<"did-a">>, [<<"m1">>, <<"m2">>]], Params),
        ?assertMatch({_, _}, binary:match(Sql, <<"unnest($4::varchar[])">>)),
        ?assertMatch(
            {_, _}, binary:match(Sql, <<"s.processed_at IS NOT NULL AND s.error_msg IS NULL">>)
        ),
        ?assertMatch({_, _}, binary:match(Sql, <<"ON CONFLICT DO NOTHING">>))
    end).
mark_acked_batch_empty_msgids_noop_test() ->
    ?assertEqual({ok, 0}, msg_delivery_repo:mark_acked_batch(<<"c2c">>, [], 100, <<"a">>)).
mark_acked_delegates_to_batch_test_() ->
    repo_case(fun() ->
        ?assertEqual({ok, 1}, msg_delivery_repo:mark_acked(<<"s2c">>, <<"m1">>, 100, <<"a">>))
    end).

delete_delivered_batch_cleans_marks_after_main_delete_test_() ->
    repo_case(fun() ->
        put(deleted, [#{<<"msg_id">> => <<"m1">>}]),
        ?assertEqual(ok, msg_delivery_repo:delete_delivered_batch(<<"c2c">>, [<<"m1">>], 100, 30)),
        ?assertEqual(1, meck:num_calls(elib_pg, execute, 3))
    end).
delete_delivered_batch_checks_terminal_evidence_without_main_test_() ->
    repo_case(fun() ->
        ?assertEqual(ok, msg_delivery_repo:delete_delivered_batch(<<"s2c">>, [<<"m1">>], 100, 30)),
        ?assertEqual(1, meck:num_calls(elib_pg, execute, 3))
    end).
delete_delivered_batch_propagates_cleanup_failure_test_() ->
    repo_case(fun() ->
        put(cleanup_result, {error, timeout}),
        ?assertEqual(
            {error, timeout},
            msg_delivery_repo:delete_delivered_batch(<<"c2c">>, [<<"m1">>], 100, 30)
        )
    end).
delete_delivered_batch_empty_msgids_noop_test() ->
    ?assertEqual(ok, msg_delivery_repo:delete_delivered_batch(<<"c2c">>, [], 100, 30)).

repo_case(Fun) ->
    {timeout, 45, fun() ->
        pure_mock_fixture:run(fun() ->
            pure_mock_fixture:with_mocks(
                [
                    {elib_pg, [
                        {with_tx, 1, fun(F) ->
                            try
                                F(self())
                            catch
                                throw:{rollback, R} -> {rollback, R}
                            end
                        end},
                        {query, 3, fun(_, Sql, _) ->
                            case binary:match(Sql, <<"pg_advisory_xact_lock">>) of
                                nomatch ->
                                    {ok,
                                        case get(deleted) of
                                            undefined -> [];
                                            D -> D
                                        end};
                                _ ->
                                    {ok, []}
                            end
                        end},
                        {execute_count, 3, fun(_, _, [_, _, _, Ids]) -> {ok, length(Ids)} end},
                        {execute, 3, fun(_, Sql, [_, _, _, _]) ->
                            ?assertMatch(
                                {_, _},
                                binary:match(
                                    Sql, <<"s.processed_at IS NULL OR s.error_msg IS NOT NULL">>
                                )
                            ),
                            ?assertMatch(
                                {_, _},
                                binary:match(
                                    Sql, <<"s.processed_at IS NOT NULL AND s.error_msg IS NULL">>
                                )
                            ),
                            case get(cleanup_result) of
                                undefined -> {ok, 1};
                                R -> R
                            end
                        end}
                    ]}
                ],
                Fun
            )
        end)
    end}.

pending_filter_c2c_test_() ->
    ?TEST_SIMPLE(fun() ->
        F = msg_delivery_repo:pending_filter(<<"c2c">>, 1, 2),
        ?assertMatch({_, _}, binary:match(F, <<"NOT EXISTS">>)),
        ?assertMatch({_, _}, binary:match(F, <<"a.msg_kind = 'c2c'">>)),
        ?assertMatch({_, _}, binary:match(F, <<"a.msg_id = msg_c2c.msg_id">>)),
        ?assertMatch({_, _}, binary:match(F, <<"a.to_uid = $1">>)),
        ?assertMatch({_, _}, binary:match(F, <<"a.to_did = $2">>))
    end).

pending_filter_s2c_param_index_test_() ->
    ?TEST_SIMPLE(fun() ->
        F = msg_delivery_repo:pending_filter(<<"s2c">>, 1, 3),
        ?assertMatch({_, _}, binary:match(F, <<"a.msg_id = msg_s2c.msg_id">>)),
        ?assertMatch({_, _}, binary:match(F, <<"a.to_did = $3">>))
    end).

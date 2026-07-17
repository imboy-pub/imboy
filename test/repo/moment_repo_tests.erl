-module(moment_repo_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

tablename_returns_expected_table_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(<<"public.moment_post">>, moment_post_repo:tablename()),
        ?assertEqual(<<"public.moment_like">>, moment_like_repo:tablename())
    end).

page_feed_candidates_with_cursor_uses_cursor_filter_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, [88, 20]) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assert(re:run(SqlBin, <<"id < \\$1">>) =/= nomatch),
                    {ok, [#{<<"id">> => 77}]}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch({ok, [#{<<"id">> := 77}]}, moment_post_repo:page_feed_candidates(88, 20))
        end
    ).

like_add_returns_changed_state_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [
                {'generate', 1, fun(_Table) -> 999999 end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, [999999, 100, 200]) ->
                    {ok, [#{<<"inserted">> => 1}]}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, true}, moment_like_repo:add(100, 200))
        end
    ).

recent_comments_by_posts_empty_ids_short_circuits_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({ok, []}, moment_comment_repo:recent_comments_by_posts([], 3))
    end).

recent_comments_by_posts_uses_window_function_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, [[100, 200], 3]) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assert(re:run(SqlBin, <<"ROW_NUMBER\\(\\)">>) =/= nomatch),
                    ?assert(re:run(SqlBin, <<"post_id = ANY\\(\\$1\\)">>) =/= nomatch),
                    ?assert(re:run(SqlBin, <<"status = 1">>) =/= nomatch),
                    ?assert(re:run(SqlBin, <<"rn <= \\$2">>) =/= nomatch),
                    {ok, [#{<<"id">> => 9, <<"post_id">> => 100}]}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, [#{<<"post_id">> := 100}]},
                moment_comment_repo:recent_comments_by_posts([100, 200], 3)
            )
        end
    ).

like_remove_returns_noop_when_not_found_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'execute', 2, fun(_Sql, [100, 200]) ->
                    {ok, 0}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, false}, moment_like_repo:remove(100, 200))
        end
    ).

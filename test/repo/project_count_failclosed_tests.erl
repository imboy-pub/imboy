-module(project_count_failclosed_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

count_failures_stop_before_data_query_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'one', 2, fun(_, _) -> {error, count_failed} end},
            {'query', 2, fun(_, _) -> erlang:error(data_query_must_not_run) end}
        ],
        fun() ->
            Expected = {error, count_failed},
            ?assertEqual(Expected, project_repo:page_by_workspace(1, 1, 10, <<"id">>)),
            ?assertEqual(
                Expected, project_member_repo:page_by_project(1, 1, 10, <<"pm.user_id">>)
            ),
            ?assertEqual(
                Expected, project_channel_rel_repo:page_channels_by_project(1, 1, 10)
            ),
            ?assertEqual(Expected, project_channel_agg_repo:pinned_page(1, 1, 10)),
            ?assertEqual(Expected, project_channel_agg_repo:activity_page(1, 1, 10))
        end
    ).

lookup_failures_are_not_reported_as_missing_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'one', 2, fun(_, _) -> {error, lookup_failed} end},
            {'query', 3, fun(_, _, _) -> {error, lookup_failed} end}
        ],
        fun() ->
            Expected = {error, lookup_failed},
            ?assertEqual(Expected, project_member_repo:find(1, 2, <<"status">>)),
            ?assertEqual(Expected, project_member_repo:find_tx(fake_conn, 1, 2, <<"status">>)),
            ?assertEqual(Expected, project_milestone_repo:find_tx(fake_conn, 1, <<"status">>)),
            ?assertEqual(
                Expected, project_channel_rel_repo:find_channel_tx(fake_conn, 1, <<"status">>)
            )
        end
    ).

member_lookup_failure_maps_to_internal_error_test_() ->
    Project = #{<<"id">> => 1, <<"workspace_id">> => 2, <<"owner_id">> => 3},
    ?WITH_MECK(
        project_ds,
        [{'find_by_id', 1, fun(1) -> Project end}],
        fun() ->
            ?WITH_MECK(
                workspace_logic,
                [{'my_role', 2, fun(2, 4) -> {ok, <<"member">>} end}],
                fun() ->
                    ?WITH_MECK(
                        project_member_ds,
                        [{'find', 2, fun(1, 4) -> {error, db_down} end}],
                        fun() ->
                            ?assertMatch(
                                {error, {500, _}}, project_member_logic:ensure_can_read(4, 1)
                            )
                        end
                    )
                end
            )
        end
    ).

disabled_channels_are_filtered_from_project_aggregations_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'one', 2, fun(Sql, _) ->
                ?assertNotEqual(nomatch, binary:match(Sql, <<"c.status = 1">>)),
                {ok, #{<<"count">> => 0}}
            end},
            {'query', 2, fun(Sql, _) ->
                ?assertNotEqual(nomatch, binary:match(Sql, <<"c.status = 1">>)),
                {ok, []}
            end}
        ],
        fun() ->
            meck:reset(elib_pg),
            {ok, _} = project_channel_agg_repo:pinned_page(1, 1, 10),
            {ok, []} = project_channel_agg_repo:related_posts(1, 5, 50)
        end
    ).

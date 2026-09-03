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

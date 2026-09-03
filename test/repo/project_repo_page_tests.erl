-module(project_repo_page_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

member_page_filters_count_and_data_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'one', 2, fun(Sql, Params) ->
                assert_member_filter(iolist_to_binary(Sql)),
                ?assertEqual([10, 20], Params),
                {ok, #{<<"count">> => 1}}
            end},
            {'query', 2, fun(Sql, Params) ->
                assert_member_filter(iolist_to_binary(Sql)),
                ?assertEqual([10, 20, 5, 5], Params),
                {ok, [#{<<"id">> => 1}]}
            end}
        ],
        fun() ->
            ?assertMatch(
                {ok, #{total := 1, list := [_]}},
                project_repo:page_by_workspace_member(10, 20, 2, 5, <<"p.id">>)
            )
        end
    ).

assert_member_filter(Sql) ->
    ?assertNotEqual(nomatch, binary:match(Sql, <<"JOIN project_member pm">>)),
    ?assertNotEqual(nomatch, binary:match(Sql, <<"pm.user_id = $2">>)),
    ?assertNotEqual(nomatch, binary:match(Sql, <<"pm.status = 'active'">>)),
    ?assertNotEqual(nomatch, binary:match(Sql, <<"p.workspace_id = $1">>)).

member_page_stops_when_count_fails_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'one', 2, fun(_, _) -> {error, count_failed} end},
            {'query', 2, fun(_, _) -> erlang:error(data_query_must_not_run) end}
        ],
        fun() ->
            ?assertEqual(
                {error, count_failed},
                project_repo:page_by_workspace_member(10, 20, 1, 5, <<"p.id">>)
            )
        end
    ).

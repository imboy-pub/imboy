-module(project_admin_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% 双体验 v2.5.2 WP7/T11b — project_logic admin 函数单元测试（只读面）
%%% 覆盖：admin_page（分页 + 批量任务计数挂载）、admin_detail
%%% （404 / workspace 概要 + owner + task 状态分布 + assignee 概览）。

-define(WS_ID, 800001).
-define(PROJECT_ID, 700001).
-define(OWNER, 900001).

%%% ===================================================================
%%% admin_page
%%% ===================================================================

admin_page_attaches_task_counts_test_() ->
    Row = #{
        <<"id">> => ?PROJECT_ID,
        <<"workspace_id">> => ?WS_ID,
        <<"owner_id">> => ?OWNER,
        <<"name">> => <<"p1">>,
        <<"status">> => <<"active">>,
        <<"workspace_name">> => <<"ops-ws">>
    },
    ?WITH_MECKS(
        [
            {project_ds, [
                {'admin_page', 4, fun(1, 10, all, <<>>) ->
                    {ok, #{list => [Row], page => 1, size => 10, total => 1, total_page => 1}}
                end},
                {'admin_batch_task_counts', 1, fun([?PROJECT_ID]) ->
                    #{?PROJECT_ID => #{total => 4, done => 1}}
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = project_logic:admin_page(1, 10, <<"all">>, <<>>),
            [Row2 | _] = maps:get(list, Result),
            ?assertEqual(4, maps:get(<<"task_total">>, Row2)),
            ?assertEqual(1, maps:get(<<"task_done">>, Row2))
        end
    ).

admin_page_invalid_status_normalized_to_all_test_() ->
    [
        {"admin page invalid status normalized to all", fun() ->
            Self = self(),
            ?WITH_MECKS(
                [
                    {project_ds, [
                        {'admin_page', 4, fun(_P, _S, Status, _K) ->
                            Self ! {status_arg, Status},
                            {ok, #{list => [], page => 1, size => 10, total => 0, total_page => 0}}
                        end}
                    ]}
                ],
                fun() ->
                    {ok, _} = project_logic:admin_page(1, 10, <<"bogus">>, <<>>),
                    receive
                        {status_arg, all} -> ok
                    after 500 -> ?assert(false, "status not normalized to all")
                    end
                end
            )
        end}
    ].

%%% ===================================================================
%%% admin_detail
%%% ===================================================================

admin_detail_not_found_test_() ->
    ?WITH_MECKS(
        [
            {project_ds, [
                {'find_by_id', 1, fun(?PROJECT_ID) -> #{} end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {error, {404, _}}, project_logic:admin_detail(?PROJECT_ID)
            )
        end
    ).

admin_detail_assembly_test_() ->
    ?WITH_MECKS(
        [
            {project_ds, [
                {'find_by_id', 1, fun(?PROJECT_ID) ->
                    #{
                        <<"id">> => ?PROJECT_ID,
                        <<"workspace_id">> => ?WS_ID,
                        <<"owner_id">> => ?OWNER,
                        <<"name">> => <<"p1">>,
                        <<"status">> => <<"active">>
                    }
                end},
                {'admin_task_status_stats', 1, fun(?PROJECT_ID) ->
                    #{<<"todo">> => 2, <<"doing">> => 1, <<"done">> => 1}
                end},
                {'admin_assignee_overview', 2, fun(?PROJECT_ID, 20) ->
                    {ok, [#{<<"assignee_id">> => ?OWNER, <<"total">> => 4, <<"done">> => 1}]}
                end}
            ]},
            {workspace_ds, [
                {'find_by_id', 2, fun(?WS_ID, <<"id,name,status,owner_id">>) ->
                    #{<<"id">> => ?WS_ID, <<"name">> => <<"ops-ws">>, <<"status">> => <<"active">>}
                end}
            ]},
            {user_ds, [
                {'find_by_id', 2, fun(?OWNER, _Col) ->
                    #{<<"id">> => ?OWNER, <<"nickname">> => <<"alice">>}
                end}
            ]}
        ],
        fun() ->
            {ok, Detail} = project_logic:admin_detail(?PROJECT_ID),
            ?assertEqual(
                #{<<"id">> => ?WS_ID, <<"name">> => <<"ops-ws">>, <<"status">> => <<"active">>},
                maps:get(workspace, Detail)
            ),
            ?assertEqual(
                #{<<"todo">> => 2, <<"doing">> => 1, <<"done">> => 1}, maps:get(task_stats, Detail)
            ),
            ?assertMatch([#{<<"assignee_id">> := ?OWNER} | _], maps:get(assignees, Detail))
        end
    ).

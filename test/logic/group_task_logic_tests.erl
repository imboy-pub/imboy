-module(group_task_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_task_logic 模块的 EUnit 测试
%%%
%%% 目标：验证群作业业务逻辑功能
%%% 覆盖：创建作业、分配作业、提交作业、批改作业、查询作业
%%%===================================================================

%% ===================================================================
%% create/4 测试 - 创建作业
%% ===================================================================

create_success_test_() ->
    ?WITH_MECKS(
        [
            {group_task_repo, [
                {'insert', 1, fun(_Data) ->
                    {ok, 1001, [{<<"id">>, 1001}]}
                end}
            ]},
            {group_ds, [{'is_member', 2, fun(_Uid, _Gid) -> true end}]}
        ],
        fun() ->
            Result = group_task_logic:create(123, 456, <<"完成第一章练习"/utf8>>, #{
                description => <<"完成课本第一章的所有习题"/utf8>>,
                deadline => <<"2026-12-31 23:59:59">>
            }),
            ?assertMatch({ok, _}, Result)
        end
    ).

create_with_missing_title_test_() ->
    ?_test(begin
        Result = group_task_logic:create(123, 456, <<>>, #{}),
        ?assertMatch({error, _, _}, Result)
    end).

%% 回归：task_id 必须是 binary（表列 varchar(40) + repo is_binary 校验）。
%% 此前 logic 用整数相加生成 task_id → repo guard 不匹配 invalid_param，
%% 创建作业 100% 失败（2026-07-14 生产坐实）。mock 掉 insert 的旧测试抓
%% 不到该类型契约，这里显式断言传给 repo 的 task_id 类型。
create_task_id_is_binary_contract_test_() ->
    ?WITH_MECKS(
        [
            {group_task_repo, [
                {'insert', 1, fun(Data) ->
                    self() ! {captured_task_id, maps:get(task_id, Data)},
                    {ok, 1001, [{<<"id">>, 1001}]}
                end}
            ]},
            {group_ds, [{'is_member', 2, fun(_Uid, _Gid) -> true end}]}
        ],
        fun() ->
            ?assertMatch(
                {ok, _},
                group_task_logic:create(123, 456, <<"契约"/utf8>>, #{})
            ),
            receive
                {captured_task_id, TaskId} ->
                    ?assert(is_binary(TaskId)),
                    ?assert(byte_size(TaskId) =< 40)
            after 1000 -> ?assert(false)
            end
        end
    ).

%% 回归（IDOR）：创建者不是该群成员 → 拒绝，防止对任意 group_id 创建作业
create_non_member_rejected_test_() ->
    ?WITH_MECK(
        group_ds,
        [{'is_member', 2, fun(_Uid, _Gid) -> false end}],
        fun() ->
            Result = group_task_logic:create(123, 456, <<"标题"/utf8>>, #{}),
            ?assertMatch({error, _, _}, Result)
        end
    ).

%% ===================================================================
%% update/3 测试 - 更新作业
%% ===================================================================

update_success_test_() ->
    ?WITH_MECK(
        group_task_repo,
        [
            {'find_by_id', 1, fun(_Id) ->
                {ok, #{<<"id">> => 1001, <<"creator_id">> => 456, <<"status">> => 1}}
            end},
            {'update', 2, fun(_Id, _Data) ->
                {ok, 1}
            end}
        ],
        fun() ->
            Result = group_task_logic:update(1001, 456, #{
                <<"title">> => <<"更新后的标题"/utf8>>
            }),
            ?assertEqual(ok, Result)
        end
    ).

update_success_with_atom_keys_test_() ->
    ?WITH_MECK(
        group_task_repo,
        [
            {'find_by_id', 1, fun(_Id) ->
                {ok, #{<<"id">> => 1001, <<"creator_id">> => 456, <<"status">> => 1}}
            end},
            {'update', 2, fun(_Id, Data) ->
                ?assertEqual(<<"Atom Key 标题"/utf8>>, maps:get(title, Data)),
                {ok, 1}
            end}
        ],
        fun() ->
            Result = group_task_logic:update(1001, 456, #{
                title => <<"Atom Key 标题"/utf8>>
            }),
            ?assertEqual(ok, Result)
        end
    ).

update_not_creator_test_() ->
    ?WITH_MECK(
        group_task_repo,
        [
            {'find_by_id', 1, fun(_Id) ->
                {ok, #{<<"id">> => 1001, <<"creator_id">> => 999, <<"status">> => 1}}
            end}
        ],
        fun() ->
            Result = group_task_logic:update(1001, 456, #{
                <<"title">> => <<"更新后的标题"/utf8>>
            }),
            ?assertMatch({error, _, _}, Result)
        end
    ).

%% ===================================================================
%% assign/2 测试 - 分配作业给成员
%% ===================================================================

assign_success_test_() ->
    ?WITH_MECKS(
        [
            {group_task_repo, [
                {'find_by_id', 1, fun(_Id) ->
                    {ok, #{
                        <<"id">> => 1001, <<"task_id">> => <<"task123">>, <<"creator_id">> => 456
                    }}
                end}
            ]},
            {group_task_assignment_repo, [
                {'find_by_task_and_user', 2, fun(_TaskId, _UserId) ->
                    {error, not_found}
                end},
                {'insert', 1, fun(_Data) ->
                    {ok, 2001, [{<<"id">>, 2001}]}
                end}
            ]}
        ],
        fun() ->
            Result = group_task_logic:assign(1001, [789, 790], 456),
            ?assertEqual(ok, Result)
        end
    ).

%% 安全回归：非创建者不能给作业分配成员（曾经零权限校验）
assign_permission_denied_test_() ->
    ?WITH_MECK(
        group_task_repo,
        [
            {'find_by_id', 1, fun(_Id) ->
                {ok, #{<<"id">> => 1001, <<"task_id">> => <<"task123">>, <<"creator_id">> => 456}}
            end}
        ],
        fun() ->
            Result = group_task_logic:assign(1001, [789], 999),
            ?assertMatch({error, _, _}, Result)
        end
    ).

assign_empty_list_test_() ->
    ?_test(begin
        Result = group_task_logic:assign(1001, [], 456),
        ?assertMatch({error, _, _}, Result)
    end).

%% ===================================================================
%% submit/3 测试 - 提交作业
%% ===================================================================

submit_success_test_() ->
    ?WITH_MECKS(
        [
            {group_task_repo, [
                {'find_by_task_id', 1, fun(_TaskId) ->
                    {ok, #{
                        <<"id">> => 1001,
                        <<"task_id">> => <<"task123">>,
                        <<"deadline">> => undefined
                    }}
                end}
            ]},
            {group_task_assignment_repo, [
                {'find_by_task_and_user', 2, fun(_TaskId, _UserId) ->
                    {ok, #{<<"id">> => 2001, <<"status">> => 0}}
                end},
                {'update', 2, fun(_Id, _Data) ->
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            Result = group_task_logic:submit(<<"task123">>, 789, #{
                content => <<"作业内容"/utf8>>
            }),
            ?assertEqual(ok, Result)
        end
    ).

submit_assignment_not_found_test_() ->
    ?WITH_MECKS(
        [
            {group_task_repo, [
                {'find_by_task_id', 1, fun(_TaskId) ->
                    {ok, #{<<"id">> => 1001, <<"task_id">> => <<"task123">>}}
                end}
            ]},
            {group_task_assignment_repo, [
                {'find_by_task_and_user', 2, fun(_TaskId, _UserId) ->
                    {error, not_found}
                end}
            ]}
        ],
        fun() ->
            Result = group_task_logic:submit(<<"task123">>, 789, #{
                content => <<"作业内容"/utf8>>
            }),
            ?assertMatch({error, _, _}, Result)
        end
    ).

%% ===================================================================
%% review/3 测试 - 批改作业
%% ===================================================================

review_success_test_() ->
    ?WITH_MECKS(
        [
            {group_task_assignment_repo, [
                {'find_by_id', 1, fun(_Id) ->
                    {ok, #{<<"id">> => 2001, <<"status">> => 2, <<"task_id">> => <<"task123">>}}
                end},
                {'update', 2, fun(_Id, _Data) ->
                    {ok, 1}
                end}
            ]},
            {group_task_repo, [
                {'find_by_task_id', 1, fun(<<"task123">>) ->
                    {ok, #{<<"id">> => 1001, <<"creator_id">> => 456}}
                end}
            ]}
        ],
        fun() ->
            Result = group_task_logic:review(2001, 456, #{
                score => 95,
                comment => <<"完成得很好"/utf8>>
            }),
            ?assertEqual(ok, Result)
        end
    ).

%% 安全回归：非创建者不能批改作业（曾经零权限校验）
review_permission_denied_test_() ->
    ?WITH_MECKS(
        [
            {group_task_assignment_repo, [
                {'find_by_id', 1, fun(_Id) ->
                    {ok, #{<<"id">> => 2001, <<"status">> => 2, <<"task_id">> => <<"task123">>}}
                end}
            ]},
            {group_task_repo, [
                {'find_by_task_id', 1, fun(<<"task123">>) ->
                    {ok, #{<<"id">> => 1001, <<"creator_id">> => 456}}
                end}
            ]}
        ],
        fun() ->
            Result = group_task_logic:review(2001, 999, #{score => 95}),
            ?assertMatch({error, _, _}, Result)
        end
    ).

review_not_submitted_test_() ->
    ?WITH_MECKS(
        [
            {group_task_assignment_repo, [
                {'find_by_id', 1, fun(_Id) ->
                    {ok, #{<<"id">> => 2001, <<"status">> => 1, <<"task_id">> => <<"task123">>}}
                end}
            ]},
            {group_task_repo, [
                {'find_by_task_id', 1, fun(<<"task123">>) ->
                    {ok, #{<<"id">> => 1001, <<"creator_id">> => 456}}
                end}
            ]}
        ],
        fun() ->
            Result = group_task_logic:review(2001, 456, #{
                score => 95
            }),
            ?assertMatch({error, _, _}, Result)
        end
    ).

%% ===================================================================
%% list/3 测试 - 查询作业列表
%% ===================================================================

list_success_test_() ->
    ?WITH_MECK(
        group_task_repo,
        [
            {'list_by_group_id', 3, fun(_GroupId, _Page, _Size) ->
                {ok, [#{<<"id">> => 1001, <<"title">> => <<"作业1"/utf8>>}]}
            end}
        ],
        fun() ->
            Result = group_task_logic:list(123, 1, 20),
            ?assertMatch({ok, [_ | _]}, Result)
        end
    ).

list_with_assignee_status_filter_success_test_() ->
    ?WITH_MECK(
        group_task_repo,
        [
            {'list_by_group_and_user', 5, fun(123, 789, 0, 1, 20) ->
                {ok, [#{<<"id">> => 1002, <<"title">> => <<"作业2"/utf8>>, <<"status">> => 0}]}
            end}
        ],
        fun() ->
            Result = group_task_logic:list(123, 0, 789, 1, 20),
            ?assertMatch({ok, [_ | _]}, Result)
        end
    ).

list_group_view_with_status_success_test_() ->
    ?WITH_MECK(
        group_task_repo,
        [
            {'list_by_group_id', 4, fun(123, 2, 1, 20) ->
                {ok, [#{<<"id">> => 1003, <<"status">> => 2}]}
            end}
        ],
        fun() ->
            Result = group_task_logic:list(123, 2, undefined, 1, 20),
            ?assertMatch({ok, [_ | _]}, Result)
        end
    ).

%% ===================================================================
%% detail/1 测试 - 查询作业详情
%% ===================================================================

detail_success_test_() ->
    ?WITH_MECK(
        group_task_repo,
        [
            {'find_by_id', 1, fun(_Id) ->
                {ok, #{<<"id">> => 1001, <<"title">> => <<"作业1"/utf8>>}}
            end}
        ],
        fun() ->
            Result = group_task_logic:detail(1001),
            ?assertMatch({ok, _}, Result)
        end
    ).

detail_not_found_test_() ->
    ?WITH_MECK(
        group_task_repo,
        [
            {'find_by_id', 1, fun(_Id) ->
                {error, not_found}
            end}
        ],
        fun() ->
            Result = group_task_logic:detail(9999),
            ?assertMatch({error, _, _}, Result)
        end
    ).

%% ===================================================================
%% my_tasks/3 测试 - 查询我的作业
%% ===================================================================

my_tasks_success_test_() ->
    ?WITH_MECK(
        group_task_assignment_repo,
        [
            {'list_by_user_id', 4, fun(_UserId, undefined, _Page, _Size) ->
                {ok, [#{<<"id">> => 2001, <<"status">> => 2}]}
            end}
        ],
        fun() ->
            Result = group_task_logic:my_tasks(789, 1, 20),
            ?assertMatch({ok, [_ | _]}, Result)
        end
    ).

%% ===================================================================
%% pending_review/3 测试 - 查询待批改作业
%% ===================================================================

pending_review_success_test_() ->
    ?WITH_MECK(
        group_task_assignment_repo,
        [
            {'list_by_task_id', 3, fun(_TaskId, _Page, _Size) ->
                {ok, [#{<<"id">> => 2001, <<"status">> => 2}]}
            end}
        ],
        fun() ->
            Result = group_task_logic:pending_review(<<"task123">>, 1, 20),
            ?assertMatch({ok, [_ | _]}, Result)
        end
    ).

%% ===================================================================
%% 注意
%% ===================================================================
%% 测试使用 meck 模拟数据库操作
%% 实际使用时请确保数据库连接正常
%%===================================================================

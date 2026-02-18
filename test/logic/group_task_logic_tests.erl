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
    ?WITH_MECK(group_task_repo, [
        {'insert', 1, fun(_Data) ->
            {ok, 1001, [{<<"id">>, 1001}]}
        end}
    ], fun() ->
        Result = group_task_logic:create(123, 456, <<"完成第一章练习"/utf8>>, #{
            description => <<"完成课本第一章的所有习题"/utf8>>,
            deadline => <<"2026-12-31 23:59:59">>
        }),
        ?assertMatch({ok, _}, Result)
    end).

create_with_missing_title_test_() ->
    Result = group_task_logic:create(123, 456, <<>>, #{}),
    ?assertMatch({error, _}, Result).

%% ===================================================================
%% update/3 测试 - 更新作业
%% ===================================================================

update_success_test_() ->
    ?WITH_MECK(group_task_repo, [
        {'find_by_id', 1, fun(_Id) ->
            {ok, #{<<"id">> => 1001, <<"creator_id">> => 456, <<"status">> => 1}}
        end},
        {'update', 2, fun(_Id, _Data) ->
            {ok, 1}
        end}
    ], fun() ->
        Result = group_task_logic:update(1001, 456, #{
            <<"title">> => <<"更新后的标题"/utf8>>
        }),
        ?assertEqual(ok, Result)
    end).

update_not_creator_test_() ->
    ?WITH_MECK(group_task_repo, [
        {'find_by_id', 1, fun(_Id) ->
            {ok, #{<<"id">> => 1001, <<"creator_id">> => 999, <<"status">> => 1}}
        end}
    ], fun() ->
        Result = group_task_logic:update(1001, 456, #{
            <<"title">> => <<"更新后的标题"/utf8>>
        }),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% assign/2 测试 - 分配作业给成员
%% ===================================================================

assign_success_test_() ->
    ?WITH_MECKS([
        {group_task_repo, [
            {'find_by_id', 1, fun(_Id) ->
                {ok, #{<<"id">> => 1001, <<"task_id">> => <<"task123">>}}
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
    ], fun() ->
        Result = group_task_logic:assign(1001, [789, 790]),
        ?assertEqual(ok, Result)
    end).

assign_empty_list_test_() ->
    Result = group_task_logic:assign(1001, []),
    ?assertMatch({error, _}, Result).

%% ===================================================================
%% submit/3 测试 - 提交作业
%% ===================================================================

submit_success_test_() ->
    ?WITH_MECKS([
        {group_task_repo, [
            {'find_by_id', 1, fun(_Id) ->
                {ok, #{<<"id">> => 1001, <<"task_id">> => <<"task123">>, <<"deadline">> => <<"2099-12-31 23:59:59">>}}
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
    ], fun() ->
        Result = group_task_logic:submit(<<"task123">>, 789, #{
            content => <<"作业内容"/utf8>>
        }),
        ?assertEqual(ok, Result)
    end).

submit_assignment_not_found_test_() ->
    ?WITH_MECKS([
        {group_task_repo, [
            {'find_by_id', 1, fun(_Id) ->
                {ok, #{<<"id">> => 1001, <<"task_id">> => <<"task123">>}}
            end}
        ]},
        {group_task_assignment_repo, [
            {'find_by_task_and_user', 2, fun(_TaskId, _UserId) ->
                {error, not_found}
            end}
        ]}
    ], fun() ->
        Result = group_task_logic:submit(<<"task123">>, 789, #{
            content => <<"作业内容"/utf8>>
        }),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% review/3 测试 - 批改作业
%% ===================================================================

review_success_test_() ->
    ?WITH_MECK(group_task_assignment_repo, [
        {'find_by_id', 1, fun(_Id) ->
            {ok, #{<<"id">> => 2001, <<"status">> => 2}}
        end},
        {'update', 2, fun(_Id, _Data) ->
            {ok, 1}
        end}
    ], fun() ->
        Result = group_task_logic:review(2001, 456, #{
            score => 95,
            comment => <<"完成得很好"/utf8>>
        }),
        ?assertEqual(ok, Result)
    end).

review_not_submitted_test_() ->
    ?WITH_MECK(group_task_assignment_repo, [
        {'find_by_id', 1, fun(_Id) ->
            {ok, #{<<"id">> => 2001, <<"status">> => 1}}
        end}
    ], fun() ->
        Result = group_task_logic:review(2001, 456, #{
            score => 95
        }),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% list/3 测试 - 查询作业列表
%% ===================================================================

list_success_test_() ->
    ?WITH_MECK(group_task_repo, [
        {'list_by_group_id', 3, fun(_GroupId, _Page, _Size) ->
            {ok, [#{<<"id">> => 1001, <<"title">> => <<"作业1"/utf8>>}]}
        end}
    ], fun() ->
        Result = group_task_logic:list(123, 1, 20),
        ?assertMatch({ok, [_|_]}, Result)
    end).

%% ===================================================================
%% detail/1 测试 - 查询作业详情
%% ===================================================================

detail_success_test_() ->
    ?WITH_MECK(group_task_repo, [
        {'find_by_id', 1, fun(_Id) ->
            {ok, #{<<"id">> => 1001, <<"title">> => <<"作业1"/utf8>>}}
        end}
    ], fun() ->
        Result = group_task_logic:detail(1001),
        ?assertMatch({ok, _}, Result)
    end).

detail_not_found_test_() ->
    ?WITH_MECK(group_task_repo, [
        {'find_by_id', 1, fun(_Id) ->
            {error, not_found}
        end}
    ], fun() ->
        Result = group_task_logic:detail(9999),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% my_tasks/3 测试 - 查询我的作业
%% ===================================================================

my_tasks_success_test_() ->
    ?WITH_MECK(group_task_assignment_repo, [
        {'list_by_user_id', 3, fun(_UserId, _Page, _Size) ->
            {ok, [#{<<"id">> => 2001, <<"status">> => 2}]}
        end}
    ], fun() ->
        Result = group_task_logic:my_tasks(789, 1, 20),
        ?assertMatch({ok, [_|_]}, Result)
    end).

%% ===================================================================
%% pending_review/3 测试 - 查询待批改作业
%% ===================================================================

pending_review_success_test_() ->
    ?WITH_MECK(group_task_assignment_repo, [
        {'list_by_task_id', 3, fun(_TaskId, _Page, _Size) ->
            {ok, [#{<<"id">> => 2001, <<"status">> => 2}]}
        end}
    ], fun() ->
        Result = group_task_logic:pending_review(<<"task123">>, 1, 20),
        ?assertMatch({ok, [_|_]}, Result)
    end).

%% ===================================================================
%% 注意
%% ===================================================================
%% 测试使用 meck 模拟数据库操作
%% 实际使用时请确保数据库连接正常
%%===================================================================

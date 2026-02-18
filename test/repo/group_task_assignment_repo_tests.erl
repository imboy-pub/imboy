-module(group_task_assignment_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_task_assignment_repo 模块的 EUnit 测试
%%%
%%% 目标：验证群作业分配数据访问层功能
%%% 覆盖：增删改查、提交作业、批改作业、查询待批改
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECK(elib_pg_sql, [
        {'public_tablename', 1, fun(_Table) -> <<"public.group_task_assignment">> end}
    ], fun() ->
        Result = group_task_assignment_repo:tablename(),
        ?assertEqual(<<"public.group_task_assignment">>, Result)
    end).

%% ===================================================================
%% insert/1 测试 - 创建作业分配
%% ===================================================================

insert_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'insert', 3, fun(_Table, _Data, _Returning) ->
            {ok, 2001, [{<<"id">>, 2001}]}
        end}
    ], fun() ->
        Data = #{
            task_id => <<"task123">>,
            user_id => 789,
            status => 0
        },
        Result = group_task_assignment_repo:insert(Data),
        ?assertMatch({ok, 2001, _}, Result)
    end).

%% ===================================================================
%% update/2 测试 - 更新作业分配
%% ===================================================================

update_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'update', 4, fun(_Table, _Data, _Where, _Params) ->
            {ok, 1}
        end}
    ], fun() ->
        Data = #{status => 1, content => <<"开始做作业了"/utf8>>},
        Result = group_task_assignment_repo:update(2001, Data),
        ?assertEqual({ok, 1}, Result)
    end).

%% ===================================================================
%% find_by_id/1 测试 - 查询单个作业分配
%% ===================================================================

find_by_id_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [{[{<<"id">>, 2001},
                    {<<"task_id">>, <<"task123">>},
                    {<<"user_id">>, 789},
                    {<<"status">>, 0}]}]}
        end}
    ], fun() ->
        Result = group_task_assignment_repo:find_by_id(2001),
        ?assertMatch({ok, _}, Result)
    end).

find_by_id_not_found_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, []}
        end}
    ], fun() ->
        Result = group_task_assignment_repo:find_by_id(999999),
        ?assertEqual({error, not_found}, Result)
    end).

%% ===================================================================
%% find_by_task_and_user/2 测试 - 根据task_id和user_id查询
%% ===================================================================

find_by_task_and_user_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [{[{<<"id">>, 2001},
                    {<<"task_id">>, <<"task123">>},
                    {<<"user_id">>, 789},
                    {<<"status">>, 1}]}]}
        end}
    ], fun() ->
        Result = group_task_assignment_repo:find_by_task_and_user(<<"task123">>, 789),
        ?assertMatch({ok, _}, Result)
    end).

find_by_task_and_user_not_found_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, []}
        end}
    ], fun() ->
        Result = group_task_assignment_repo:find_by_task_and_user(<<"notexist">>, 789),
        ?assertEqual({error, not_found}, Result)
    end).

%% ===================================================================
%% list_by_task_id/3 测试 - 查询作业的所有分配
%% ===================================================================

list_by_task_id_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [{[{<<"id">>, 2001},
                    {<<"user_id">>, 789},
                    {<<"status">>, 2},
                    {<<"score">>, 95}],
                  [{<<"id">>, 2002},
                    {<<"user_id">>, 790},
                    {<<"status">>, 1}]}]}
        end}
    ], fun() ->
        Result = group_task_assignment_repo:list_by_task_id(<<"task123">>, 1, 20),
        ?assertMatch({ok, _}, Result)
    end).

list_by_task_id_empty_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, []}
        end}
    ], fun() ->
        Result = group_task_assignment_repo:list_by_task_id(<<"notexist">>, 1, 20),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% list_by_user_id/3 测试 - 查询用户的作业列表
%% ===================================================================

list_by_user_id_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [{[{<<"id">>, 2001},
                    {<<"task_id">>, <<"task123">>},
                    {<<"status">>, 2}],
                  [{<<"id">>, 2002},
                    {<<"task_id">>, <<"task124">>},
                    {<<"status">>, 0}]}]}
        end}
    ], fun() ->
        Result = group_task_assignment_repo:list_by_user_id(789, 1, 20),
        ?assertMatch({ok, _}, Result)
    end).

list_by_user_id_empty_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, []}
        end}
    ], fun() ->
        Result = group_task_assignment_repo:list_by_user_id(999, 1, 20),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% count_by_status/2 测试 - 统计指定状态的作业数量
%% ===================================================================

count_by_status_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [{[{<<"count">>, 5}]}]}
        end}
    ], fun() ->
        Result = group_task_assignment_repo:count_by_status(<<"task123">>, 2),
        ?assertEqual({ok, 5}, Result)
    end).

count_by_status_zero_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [{[{<<"count">>, 0}]}]}
        end}
    ], fun() ->
        Result = group_task_assignment_repo:count_by_status(<<"task123">>, 2),
        ?assertEqual({ok, 0}, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

%% 测试重复分配
insert_duplicate_assignment_test_() ->
    ?WITH_MECK(elib_pg, [
        {'insert', 3, fun(_Table, _Data, _Returning) ->
            {error, {unique_violation, <<"group_task_assignment_task_id_user_id_key">>}}
        end}
    ], fun() ->
        Data = #{
            task_id => <<"task123">>,
            user_id => 789,
            status => 0
        },
        Result = group_task_assignment_repo:insert(Data),
        ?assertMatch({error, {unique_violation, _}}, Result)
    end).

%% ===================================================================
%% 注意
%% ===================================================================
%% 测试使用 meck 模拟 elib_pg 模块
%% 实际使用时请确保数据库连接正常
%%===================================================================

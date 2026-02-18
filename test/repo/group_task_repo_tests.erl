-module(group_task_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_task_repo 模块的 EUnit 测试
%%%
%%% 目标：验证群作业数据访问层功能
%%% 覆盖：增删改查、分页查询、状态更新、软删除
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECK(elib_pg_sql, [
        {'public_tablename', 1, fun(_Table) -> <<"public.group_task">> end}
    ], fun() ->
        Result = group_task_repo:tablename(),
        ?assertEqual(<<"public.group_task">>, Result)
    end).

%% ===================================================================
%% insert/1 测试 - 创建作业
%% ===================================================================

insert_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'insert', 3, fun(_Table, _Data, _Returning) ->
            {ok, 1001, [{<<"id">>, 1001}]}
        end}
    ], fun() ->
        Data = #{
            group_id => 123,
            task_id => <<"task123">>,
            title => <<"完成第一章练习"/utf8>>,
            description => <<"完成课本第一章的所有习题"/utf8>>,
            creator_id => 456,
            deadline => <<"2026-12-31 23:59:59">>
        },
        Result = group_task_repo:insert(Data),
        ?assertMatch({ok, 1001, _}, Result)
    end).

insert_with_missing_required_field_test_() ->
    ?WITH_MECK(elib_pg, [
        {'insert', 3, fun(_Table, _Data, _Returning) ->
            {error, {missing_field, group_id}}
        end}
    ], fun() ->
        Data = #{title => <<"作业标题"/utf8>>},
        Result = group_task_repo:insert(Data),
        ?assertMatch({error, {missing_field, _}}, Result)
    end).

%% ===================================================================
%% update/2 测试 - 更新作业
%% ===================================================================

update_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'update', 4, fun(_Table, _Data, _Where, _Params) ->
            {ok, 1}
        end}
    ], fun() ->
        Data = #{title => <<"更新后的标题"/utf8>>, status => 2},
        Result = group_task_repo:update(1001, Data),
        ?assertEqual({ok, 1}, Result)
    end).

update_not_found_test_() ->
    ?WITH_MECK(elib_pg, [
        {'update', 4, fun(_Table, _Data, _Where, _Params) ->
            {ok, 0}
        end}
    ], fun() ->
        Data = #{title => <<"新标题"/utf8>>},
        Result = group_task_repo:update(999999, Data),
        ?assertEqual({ok, 0}, Result)
    end).

%% ===================================================================
%% find_by_id/1 测试 - 查询单个作业
%% ===================================================================

find_by_id_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [{[{<<"id">>, 1001},
                    {<<"group_id">>, 123},
                    {<<"title">>, <<"完成第一章练习"/utf8>>},
                    {<<"status">>, 1}]}]}
        end}
    ], fun() ->
        Result = group_task_repo:find_by_id(1001),
        ?assertMatch({ok, _}, Result)
    end).

find_by_id_not_found_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, []}
        end}
    ], fun() ->
        Result = group_task_repo:find_by_id(999999),
        ?assertEqual({error, not_found}, Result)
    end).

%% ===================================================================
%% find_by_task_id/1 测试 - 根据task_id查询
%% ===================================================================

find_by_task_id_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [{[{<<"id">>, 1001},
                    {<<"task_id">>, <<"task123">>},
                    {<<"title">>, <<"完成第一章练习"/utf8>>}]}]}
        end}
    ], fun() ->
        Result = group_task_repo:find_by_task_id(<<"task123">>),
        ?assertMatch({ok, _}, Result)
    end).

find_by_task_id_not_found_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, []}
        end}
    ], fun() ->
        Result = group_task_repo:find_by_task_id(<<"notexist">>),
        ?assertEqual({error, not_found}, Result)
    end).

%% ===================================================================
%% list_by_group_id/3 测试 - 分页查询群作业列表
%% ===================================================================

list_by_group_id_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [{[{<<"id">>, 1001},
                    {<<"group_id">>, 123},
                    {<<"title">>, <<"作业1"/utf8>>},
                    {<<"status">>, 1}],
                  [{<<"id">>, 1002},
                    {<<"group_id">>, 123},
                    {<<"title">>, <<"作业2"/utf8>>},
                    {<<"status">>, 2}]}]}
        end}
    ], fun() ->
        Result = group_task_repo:list_by_group_id(123, 1, 20),
        ?assertMatch({ok, _}, Result)
    end).

list_by_group_id_empty_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, []}
        end}
    ], fun() ->
        Result = group_task_repo:list_by_group_id(999, 1, 20),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% count_by_group_id/1 测试 - 统计群作业数量
%% ===================================================================

count_by_group_id_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [{[{<<"count">>, 15}]}]}
        end}
    ], fun() ->
        Result = group_task_repo:count_by_group_id(123),
        ?assertEqual({ok, 15}, Result)
    end).

count_by_group_id_zero_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [{[{<<"count">>, 0}]}]}
        end}
    ], fun() ->
        Result = group_task_repo:count_by_group_id(999),
        ?assertEqual({ok, 0}, Result)
    end).

%% ===================================================================
%% soft_delete/1 测试 - 软删除
%% ===================================================================

soft_delete_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'update', 4, fun(_Table, _Data, _Where, _Params) ->
            {ok, 1}
        end}
    ], fun() ->
        Result = group_task_repo:soft_delete(1001),
        ?assertEqual({ok, 1}, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

insert_with_empty_title_test_() ->
    ?WITH_MECK(elib_pg, [
        {'insert', 3, fun(_Table, _Data, _Returning) ->
            {error, {missing_field, title}}
        end}
    ], fun() ->
        Data = #{
            group_id => 123,
            task_id => <<"task123">>,
            title => <<>>
        },
        Result = group_task_repo:insert(Data),
        ?assertMatch({error, {missing_field, _}}, Result)
    end).

%% ===================================================================
%% 注意
%% ===================================================================
%% 测试使用 meck 模拟 elib_pg 模块
%% 实际使用时请确保数据库连接正常
%%===================================================================

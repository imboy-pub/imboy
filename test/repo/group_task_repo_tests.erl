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
        {'query', 2, fun(Sql, _Params) ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"deleted_at IS NULL">>)),
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
        {'query', 2, fun(Sql, _Params) ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"deleted_at IS NULL">>)),
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
        {'query', 2, fun(Sql, _Params) ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"deleted_at IS NULL">>)),
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

list_deleted_by_group_id_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(Sql, _Params) ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"deleted_at IS NOT NULL">>)),
            {ok, [{[{<<"id">>, 1009},
                    {<<"group_id">>, 123},
                    {<<"title">>, <<"已删除作业"/utf8>>},
                    {<<"status">>, 1},
                    {<<"deleted_at">>, <<"2026-02-24T09:00:00+08:00">>}]}]}
        end}
    ], fun() ->
        Result = group_task_repo:list_deleted_by_group_id(123, 1, 20),
        ?assertMatch({ok, _}, Result)
    end).

list_by_group_and_user_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun
                (<<"group_task">>) -> <<"public.group_task">>;
                (<<"group_task_assignment">>) -> <<"public.group_task_assignment">>
            end}
        ]},
        {elib_pg, [
            {'query', 2, fun(Sql, [123, 789]) ->
                ?assertMatch(nomatch, binary:match(Sql, <<"a.status = 2">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"LEFT JOIN public.group_task_assignment">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"t.deleted_at IS NULL">>)),
                {ok, []}
            end}
        ]}
    ], fun() ->
        Result = group_task_repo:list_by_group_and_user(123, 789, 1, 20),
        ?assertEqual({ok, []}, Result)
    end).

list_by_group_and_user_status_filter_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun
                (<<"group_task">>) -> <<"public.group_task">>;
                (<<"group_task_assignment">>) -> <<"public.group_task_assignment">>
            end}
        ]},
        {elib_pg, [
            {'query', 2, fun(Sql, [123, 789]) ->
                ?assertNotEqual(nomatch, binary:match(Sql, <<"CASE WHEN a.status IS NULL THEN 0 WHEN a.status >= 2 THEN 1 ELSE 0 END) = 1">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"t.deleted_at IS NULL">>)),
                {ok, []}
            end}
        ]}
    ], fun() ->
        Result = group_task_repo:list_by_group_and_user(123, 789, 1, 1, 20),
        ?assertEqual({ok, []}, Result)
    end).

list_by_group_and_user_invalid_status_test() ->
    Result = group_task_repo:list_by_group_and_user(123, 789, 9, 1, 20),
    ?assertEqual({error, invalid_status}, Result).

%% ===================================================================
%% count_by_group_id/1 测试 - 统计群作业数量
%% ===================================================================

count_by_group_id_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(Sql, _Params) ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"deleted_at IS NULL">>)),
            {ok, [{[{<<"count">>, 15}]}]}
        end}
    ], fun() ->
        Result = group_task_repo:count_by_group_id(123),
        ?assertEqual({ok, 15}, Result)
    end).

count_by_group_id_zero_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(Sql, _Params) ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"deleted_at IS NULL">>)),
            {ok, [{[{<<"count">>, 0}]}]}
        end}
    ], fun() ->
        Result = group_task_repo:count_by_group_id(999),
        ?assertEqual({ok, 0}, Result)
    end).

count_by_group_id_with_status_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(Sql, Params) ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"status = $2">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"deleted_at IS NULL">>)),
            ?assertEqual([123, 2], Params),
            {ok, [{[{<<"count">>, 7}]}]}
        end}
    ], fun() ->
        Result = group_task_repo:count_by_group_id(123, 2),
        ?assertEqual({ok, 7}, Result)
    end).

count_by_group_id_with_invalid_status_test() ->
    Result = group_task_repo:count_by_group_id(123, 9),
    ?assertEqual({error, invalid_status}, Result).

count_deleted_by_group_id_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(Sql, _Params) ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"deleted_at IS NOT NULL">>)),
            {ok, [{[{<<"count">>, 4}]}]}
        end}
    ], fun() ->
        Result = group_task_repo:count_deleted_by_group_id(123),
        ?assertEqual({ok, 4}, Result)
    end).

%% ===================================================================
%% soft_delete/1 测试 - 软删除
%% ===================================================================

soft_delete_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'update', 4, fun(_Table, Data, Where, _Params) ->
            ?assert(maps:is_key(deleted_at, Data)),
            ?assert(maps:is_key(updated_at, Data)),
            ?assertNotEqual(nomatch, binary:match(Where, <<"deleted_at IS NULL">>)),
            {ok, 1}
        end}
    ], fun() ->
        Result = group_task_repo:soft_delete(1001),
        ?assertEqual({ok, 1}, Result)
    end).

find_any_by_task_id_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(Sql, _Params) ->
            ?assertEqual(nomatch, binary:match(Sql, <<"deleted_at IS NULL">>)),
            {ok, [{[{<<"id">>, 1001},
                    {<<"task_id">>, <<"task123">>},
                    {<<"deleted_at">>, <<"2026-02-24T08:00:00+08:00">>}]}]}
        end}
    ], fun() ->
        Result = group_task_repo:find_any_by_task_id(<<"task123">>),
        ?assertMatch({ok, _}, Result)
    end).

restore_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'update', 4, fun(_Table, Data, Where, _Params) ->
            ?assertEqual({raw, <<"NULL">>}, maps:get(deleted_at, Data)),
            ?assert(maps:is_key(updated_at, Data)),
            ?assertNotEqual(nomatch, binary:match(Where, <<"deleted_at IS NOT NULL">>)),
            {ok, 1}
        end}
    ], fun() ->
        Result = group_task_repo:restore(1001),
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

-module(friend_category_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% friend_category_repo 模块的 EUnit 测试
%%%
%%% 目标：验证好友分类数据访问层功能
%%% 覆盖：分类查询、创建、删除
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECK(elib_pg_sql, [
        {'public_tablename', 1, fun(_Table) -> <<"public.user_friend_category">> end}
    ], fun() ->
        Result = friend_category_repo:tablename(),
        ?assertEqual(<<"public.user_friend_category">>, Result)
    end).

%% ===================================================================
%% list_by_uid/2 测试
%% ===================================================================

list_by_uid_default_limit_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [{<<"id">>, <<"name">>}]}
        end}
    ], fun() ->
        Uid = 12345,
        Column = <<"id,name">>,

        Result = friend_category_repo:list_by_uid(Uid, Column),
        ?assertMatch({ok, _}, Result)
    end).

list_by_uid_with_custom_limit_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [{<<"id">>, <<"name">>}]}
        end}
    ], fun() ->
        Uid = 12345,
        Column = <<"id,name">>,
        Limit = 100,

        Result = friend_category_repo:list_by_uid(Uid, Column, Limit),
        ?assertMatch({ok, _}, Result)
    end).

list_by_uid_empty_result_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, []}
        end}
    ], fun() ->
        Uid = 99999,
        Column = <<"id,name">>,

        Result = friend_category_repo:list_by_uid(Uid, Column),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% find_by_name/2 测试
%% ===================================================================

find_by_name_existing_test_() ->
    ?WITH_MECK(elib_pg, [
        {'one', 3, fun(_Sql, _Params, _Opts) ->
            {ok, #{<<"id">> => 1, <<"name">> => <<"工作"/utf8>>, <<"owner_user_id">> => 12345}}
        end}
    ], fun() ->
        Uid = 12345,
        Name = <<"工作"/utf8>>,

        Result = friend_category_repo:find_by_name(Uid, Name),
        ?assertMatch({ok, _}, Result)
    end).

find_by_name_not_found_test_() ->
    ?WITH_MECK(elib_pg, [
        {'one', 3, fun(_Sql, _Params, _Opts) ->
            {error, not_found}
        end}
    ], fun() ->
        Uid = 12345,
        Name = <<"不存在的分类"/utf8>>,

        Result = friend_category_repo:find_by_name(Uid, Name),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% add/2 测试
%% ===================================================================

add_category_success_test_() ->
    ?WITH_MECKS([
        {elib_tsid, [
            {'generate', 1, fun(_Table) -> 8000001 end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) ->
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Uid = 12345,
        Name = <<"新分类"/utf8>>,

        Result = friend_category_repo:add(Uid, Name),
        ?assertMatch({ok, _}, Result)
    end).

add_category_with_string_name_test_() ->
    ?WITH_MECKS([
        {elib_tsid, [
            {'generate', 1, fun(_Table) -> 8000002 end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) ->
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Uid = 12345,
        Name = "New Category",

        Result = friend_category_repo:add(Uid, Name),
        ?assertMatch({ok, _}, Result)
    end).

add_category_error_test_() ->
    ?WITH_MECKS([
        {elib_tsid, [
            {'generate', 1, fun(_Table) -> 8000003 end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) ->
                {error, duplicate_key}
            end}
        ]}
    ], fun() ->
        Uid = 12345,
        Name = <<"重复分类"/utf8>>,

        Result = friend_category_repo:add(Uid, Name),
        ?assertEqual({error, duplicate_key}, Result)
    end).

%% ===================================================================
%% delete/2 测试
%% ===================================================================

delete_category_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
    ], fun() ->
        Uid = 12345,
        Id = 1,

        Result = friend_category_repo:delete(Uid, Id),
        ?assertEqual({ok, 1}, Result)
    end).

delete_category_not_found_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
    ], fun() ->
        Uid = 12345,
        Id = 99999,

        Result = friend_category_repo:delete(Uid, Id),
        ?assertEqual({ok, 0}, Result)
    end).

delete_category_error_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {error, database_error} end}
    ], fun() ->
        Uid = 12345,
        Id = 1,

        Result = friend_category_repo:delete(Uid, Id),
        ?assertEqual({error, database_error}, Result)
    end).

%% ===================================================================
%% 集成测试
%% ===================================================================

add_and_delete_category_flow_test_() ->
    ?WITH_MECKS([
        {elib_tsid, [
            {'generate', 1, fun(_Table) -> 8000004 end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) ->
                {ok, 1}
            end},
            {'one', 3, fun(_Sql, _Params, _Opts) ->
                {ok, #{<<"id">> => 1, <<"name">> => <<"测试"/utf8>>, <<"owner_user_id">> => 12345}}
            end},
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [{<<"id">>, <<"name">>}]}
            end}
        ]}
    ], fun() ->
        Uid = 12345,
        Name = <<"测试"/utf8>>,

        % 1. 添加分类
        ?assertMatch({ok, _}, friend_category_repo:add(Uid, Name)),

        % 2. 查询分类
        ?assertMatch({ok, _}, friend_category_repo:find_by_name(Uid, Name)),

        % 3. 列出分类
        ?assertMatch({ok, _}, friend_category_repo:list_by_uid(Uid, <<"id,name">>))
    end).

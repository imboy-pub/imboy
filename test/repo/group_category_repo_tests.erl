-module(group_category_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% ===================================================================
%%   EUnit Tests for group_category_repo
%% ===================================================================

%% @doc 测试基本的表名获取
tablename_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]}
    ], fun() ->
        Result = group_category_repo:tablename(),
        ?assertEqual(<<"public.user_group_category">>, Result)
    end).

%% @doc 测试添加群组分类
add_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]},
        {elib_tsid, [
            {'generate', 1, fun(_Table) -> 10001 end}
        ]}
    ], fun() ->
        Uid = 999999,
        CategoryName = <<"测试分类"/utf8>>,
        Result = group_category_repo:add(Uid, CategoryName),
        ?assertEqual({ok, 10001}, Result)
    end).

%% @doc 测试查找用户的所有分类
list_by_uid_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [
                    #{<<"id">> => 1, <<"category_name">> => <<"工作群"/utf8>>, <<"sort_order">> => 0},
                    #{<<"id">> => 2, <<"category_name">> => <<"生活群"/utf8>>, <<"sort_order">> => 1}
                ]}
            end}
        ]}
    ], fun() ->
        Uid = 999998,
        Result = group_category_repo:list_by_uid(Uid, <<"id, category_name, sort_order">>),
        case Result of
            {ok, Categories} when is_list(Categories), length(Categories) >= 2 ->
                ?assert(true);
            _ ->
                ?assert(false)
        end
    end).

%% @doc 测试根据名称查找分类
find_by_name_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'one', 3, fun(_Sql, _Params, _Default) ->
                {ok, #{<<"id">> => 1, <<"user_id">> => 999997, <<"category_name">> => <<"测试分类名称"/utf8>>, <<"sort_order">> => 0}}
            end}
        ]}
    ], fun() ->
        Uid = 999997,
        CategoryName = <<"测试分类名称"/utf8>>,
        Result = group_category_repo:find_by_name(Uid, CategoryName),
        case Result of
            {ok, Category} when is_map(Category), map_size(Category) > 0 ->
                ?assertEqual(CategoryName, maps:get(<<"category_name">>, Category));
            _ ->
                ?assert(false)
        end
    end).

%% @doc 测试更新分类名称
update_name_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]}
    ], fun() ->
        Uid = 999996,
        CategoryId = 100,
        NewName = <<"新名称"/utf8>>,
        Result = group_category_repo:update_name(Uid, CategoryId, NewName),
        ?assertEqual({ok, 1}, Result)
    end).

%% @doc 测试删除分类
delete_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]}
    ], fun() ->
        Uid = 999995,
        CategoryId = 200,
        Result = group_category_repo:delete(Uid, CategoryId),
        ?assertEqual({ok, 1}, Result)
    end).

%% @doc 测试更新排序
update_sort_order_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]}
    ], fun() ->
        Uid = 999994,
        CategoryId = 300,
        NewSortOrder = 100,
        Result = group_category_repo:update_sort_order(Uid, CategoryId, NewSortOrder),
        ?assertEqual({ok, 1}, Result)
    end).

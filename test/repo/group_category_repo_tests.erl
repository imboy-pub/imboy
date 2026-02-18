-module(group_category_repo_tests).
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%%   EUnit Tests for group_category_repo
%% ===================================================================

%% @doc 测试基本的表名获取
tablename_test() ->
    ?assertEqual(<<"user_group_category">>, group_category_repo:tablename()).

%% @doc 测试添加群组分类
add_test() ->
    %% 设置测试环境
    application:set_env(imboy, env, test),

    Uid = 999999,
    CategoryName = <<"测试分类"/utf8>>,

    %% 清理可能存在的测试数据
    group_category_repo:delete(Uid, 0),

    %% 测试添加分类
    case group_category_repo:add(Uid, CategoryName) of
        {ok, CategoryId} when is_integer(CategoryId), CategoryId > 0 ->
            %% 测试通过
            ?assert(true);
        {error, Reason} ->
            ?debugFmt("添加分类失败: ~p~n", [Reason]),
            ?assert(false)
    end,

    %% 清理测试数据
    group_category_repo:delete(Uid, 0).

%% @doc 测试查找用户的所有分类
list_by_uid_test() ->
    application:set_env(imboy, env, test),

    Uid = 999998,
    CategoryName1 = <<"工作群"/utf8>>,
    CategoryName2 = <<"生活群"/utf8>>,

    %% 清理测试数据
    cleanup_test_categories(Uid),

    %% 添加测试分类
    {ok, _Id1} = group_category_repo:add(Uid, CategoryName1),
    {ok, _Id2} = group_category_repo:add(Uid, CategoryName2),

    %% 测试查询分类列表
    case group_category_repo:list_by_uid(Uid, <<"id, category_name, sort_order">>) of
        {ok, Categories} when is_list(Categories), length(Categories) >= 2 ->
            ?assert(true);
        {ok, []} ->
            ?debugFmt("未找到分类~n", []),
            ?assert(false);
        {error, Reason} ->
            ?debugFmt("查询分类失败: ~p~n", [Reason]),
            ?assert(false)
    end,

    %% 清理测试数据
    cleanup_test_categories(Uid).

%% @doc 测试根据名称查找分类
find_by_name_test() ->
    application:set_env(imboy, env, test),

    Uid = 999997,
    CategoryName = <<"测试分类名称"/utf8>>,

    %% 清理测试数据
    cleanup_test_categories(Uid),

    %% 添加测试分类
    {ok, _CategoryId} = group_category_repo:add(Uid, CategoryName),

    %% 测试根据名称查找
    case group_category_repo:find_by_name(Uid, CategoryName) of
        {ok, Category} when is_map(Category) ->
            ?assertEqual(CategoryName, maps:get(<<"category_name">>, Category));
        {ok, #{}} ->
            ?assert(false);
        {error, Reason} ->
            ?debugFmt("根据名称查找分类失败: ~p~n", [Reason]),
            ?assert(false)
    end,

    %% 清理测试数据
    cleanup_test_categories(Uid).

%% @doc 测试更新分类名称
update_name_test() ->
    application:set_env(imboy, env, test),

    Uid = 999996,
    OldName = <<"旧名称"/utf8>>,
    NewName = <<"新名称"/utf8>>,

    %% 清理测试数据
    cleanup_test_categories(Uid),

    %% 添加测试分类
    {ok, CategoryId} = group_category_repo:add(Uid, OldName),

    %% 测试更新名称
    case group_category_repo:update_name(Uid, CategoryId, NewName) of
        {ok, 1} ->
            %% 验证更新成功
            case group_category_repo:find_by_name(Uid, NewName) of
                {ok, Category} ->
                    ?assertEqual(NewName, maps:get(<<"category_name">>, Category));
                _ ->
                    ?assert(false)
            end;
        {ok, 0} ->
            ?debugFmt("未找到要更新的分类~n", []),
            ?assert(false);
        {error, Reason} ->
            ?debugFmt("更新分类名称失败: ~p~n", [Reason]),
            ?assert(false)
    end,

    %% 清理测试数据
    cleanup_test_categories(Uid).

%% @doc 测试删除分类
delete_test() ->
    application:set_env(imboy, env, test),

    Uid = 999995,
    CategoryName = <<"待删除分类"/utf8>>,

    %% 清理测试数据
    cleanup_test_categories(Uid),

    %% 添加测试分类
    {ok, CategoryId} = group_category_repo:add(Uid, CategoryName),

    %% 测试删除分类
    case group_category_repo:delete(Uid, CategoryId) of
        {ok, 1} ->
            %% 验证删除成功
            case group_category_repo:find_by_name(Uid, CategoryName) of
                {ok, #{}} ->
                    ?assert(true);
                _ ->
                    ?assert(false)
            end;
        {ok, 0} ->
            ?debugFmt("未找到要删除的分类~n", []),
            ?assert(false);
        {error, Reason} ->
            ?debugFmt("删除分类失败: ~p~n", [Reason]),
            ?assert(false)
    end,

    %% 清理测试数据
    cleanup_test_categories(Uid).

%% @doc 测试更新排序
update_sort_order_test() ->
    application:set_env(imboy, env, test),

    Uid = 999994,
    CategoryName = <<"排序测试分类"/utf8>>,

    %% 清理测试数据
    cleanup_test_categories(Uid),

    %% 添加测试分类
    {ok, CategoryId} = group_category_repo:add(Uid, CategoryName),

    %% 测试更新排序
    NewSortOrder = 100,
    case group_category_repo:update_sort_order(Uid, CategoryId, NewSortOrder) of
        {ok, 1} ->
            ?assert(true);
        {ok, 0} ->
            ?debugFmt("未找到要更新的分类~n", []),
            ?assert(false);
        {error, Reason} ->
            ?debugFmt("更新排序失败: ~p~n", [Reason]),
            ?assert(false)
    end,

    %% 清理测试数据
    cleanup_test_categories(Uid).

%% ===================================================================
%% Internal Helper Functions
%% ===================================================================

%% @doc 清理测试分类数据
cleanup_test_categories(Uid) ->
    case group_category_repo:list_by_uid(Uid, <<"id">>) of
        {ok, Categories} ->
            lists:foreach(fun(#{<<"id">> := Id}) ->
                group_category_repo:delete(Uid, Id)
            end, Categories);
        _ ->
            ok
    end.

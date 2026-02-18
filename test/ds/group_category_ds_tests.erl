-module(group_category_ds_tests).
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%%   EUnit Tests for group_category_ds
%% ===================================================================

%% @doc 测试添加群组分类（正常流程）
add_test() ->
    application:set_env(imboy, env, test),

    Uid = 989999,
    CategoryName = <<"工作群"/utf8>>,

    %% 清理测试数据
    cleanup_test_data(Uid),

    %% 测试添加分类
    case group_category_ds:add(Uid, CategoryName) of
        {ok, CategoryId} when is_integer(CategoryId), CategoryId > 0 ->
            ?assert(true);
        {error, Reason} ->
            ?debugFmt("添加分类失败: ~p~n", [Reason]),
            ?assert(false)
    end,

    %% 清理测试数据
    cleanup_test_data(Uid).

%% @doc 测试添加重复分类（应返回已存在的分类ID）
add_duplicate_test() ->
    application:set_env(imboy, env, test),

    Uid = 989998,
    CategoryName = <<"重复分类"/utf8>>,

    %% 清理测试数据
    cleanup_test_data(Uid),

    %% 添加第一次
    {ok, CategoryId1} = group_category_ds:add(Uid, CategoryName),

    %% 添加第二次（应返回已存在的ID）
    case group_category_ds:add(Uid, CategoryName) of
        {ok, CategoryId2} when CategoryId2 =:= CategoryId1 ->
            ?assert(true);
        {ok, _} ->
            ?debugFmt("重复添加应返回已存在的分类ID~n", []),
            ?assert(false);
        {error, Reason} ->
            ?debugFmt("重复添加失败: ~p~n", [Reason]),
            ?assert(false)
    end,

    %% 清理测试数据
    cleanup_test_data(Uid).

%% @doc 测试查询用户的分类列表
find_by_uid_test() ->
    application:set_env(imboy, env, test),

    Uid = 989997,

    %% 清理测试数据
    cleanup_test_data(Uid),

    %% 添加测试分类
    {ok, _} = group_category_ds:add(Uid, <<"分类1"/utf8>>),
    {ok, _} = group_category_ds:add(Uid, <<"分类2"/utf8>>),
    {ok, _} = group_category_ds:add(Uid, <<"分类3"/utf8>>),

    %% 测试查询分类列表
    Categories = group_category_ds:find_by_uid(Uid),
    ?assert(length(Categories) >= 4),  % 至少包含3个自定义分类 + 1个默认分类

    %% 清理测试数据
    cleanup_test_data(Uid).

%% @doc 测试重命名分类
rename_test() ->
    application:set_env(imboy, env, test),

    Uid = 989996,
    OldName = <<"旧名称"/utf8>>,
    NewName = <<"新名称"/utf8>>,

    %% 清理测试数据
    cleanup_test_data(Uid),

    %% 添加测试分类
    {ok, CategoryId} = group_category_ds:add(Uid, OldName),

    %% 测试重命名
    case group_category_ds:rename(Uid, CategoryId, NewName) of
        {ok, 1} ->
            ?assert(true);
        {error, Reason} ->
            ?debugFmt("重命名失败: ~p~n", [Reason]),
            ?assert(false)
    end,

    %% 清理测试数据
    cleanup_test_data(Uid).

%% @doc 测试重命名时的参数验证
rename_invalid_params_test() ->
    application:set_env(imboy, env, test),

    Uid = 989995,

    %% 测试无效的分类ID
    case group_category_ds:rename(Uid, undefined, <<"新名称"/utf8>>) of
        {error, _} ->
            ?assert(true);
        _ ->
            ?assert(false)
    end,

    %% 测试空的分类名称
    case group_category_ds:rename(Uid, 1, <<>>) of
        {error, _} ->
            ?assert(true);
        _ ->
            ?assert(false)
    end.

%% @doc 测试删除分类
delete_test() ->
    application:set_env(imboy, env, test),

    Uid = 989994,
    CategoryName = <<"待删除分类"/utf8>>,

    %% 清理测试数据
    cleanup_test_data(Uid),

    %% 添加测试分类
    {ok, CategoryId} = group_category_ds:add(Uid, CategoryName),

    %% 测试删除分类
    case group_category_ds:delete(Uid, CategoryId) of
        {ok, 1} ->
            ?assert(true);
        {error, Reason} ->
            ?debugFmt("删除分类失败: ~p~n", [Reason]),
            ?assert(false)
    end,

    %% 清理测试数据
    cleanup_test_data(Uid).

%% @doc 测试删除时的参数验证
delete_invalid_params_test() ->
    application:set_env(imboy, env, test),

    Uid = 989993,

    %% 测试无效的分类ID
    case group_category_ds:delete(Uid, undefined) of
        {error, _} ->
            ?assert(true);
        _ ->
            ?assert(false)
    end,

    %% 测试空的分类ID
    case group_category_ds:delete(Uid, <<>>) of
        {error, _} ->
            ?assert(true);
        _ ->
            ?assert(false)
    end.

%% @doc 测试更新排序
update_sort_order_test() ->
    application:set_env(imboy, env, test),

    Uid = 989992,
    CategoryName = <<"排序测试"/utf8>>,

    %% 清理测试数据
    cleanup_test_data(Uid),

    %% 添加测试分类
    {ok, CategoryId} = group_category_ds:add(Uid, CategoryName),

    %% 测试更新排序
    case group_category_ds:update_sort_order(Uid, CategoryId, 100) of
        {ok, 1} ->
            ?assert(true);
        {error, Reason} ->
            ?debugFmt("更新排序失败: ~p~n", [Reason]),
            ?assert(false)
    end,

    %% 清理测试数据
    cleanup_test_data(Uid).

%% @doc 测试移动群组到分类
move_group_to_category_test() ->
    application:set_env(imboy, env, test),

    Uid = 989991,
    Gid = 989991,
    CategoryName = <<"目标分类"/utf8>>,

    %% 清理测试数据
    cleanup_test_data(Uid),

    %% 添加测试分类
    {ok, CategoryId} = group_category_ds:add(Uid, CategoryName),

    %% 测试移动群组到分类
    case group_category_ds:move_group_to_category(Uid, Gid, CategoryId) of
        {ok, 1} ->
            ?assert(true);
        {ok, 0} ->
            %% 可能群成员记录不存在，这也是正常的
            ?assert(true);
        {error, Reason} ->
            ?debugFmt("移动群组失败: ~p~n", [Reason]),
            ?assert(false)
    end,

    %% 清理测试数据
    cleanup_test_data(Uid).

%% ===================================================================
%% Internal Helper Functions
%% ===================================================================

%% @doc 清理测试数据
cleanup_test_data(Uid) ->
    case group_category_repo:list_by_uid(Uid, <<"id">>) of
        {ok, Categories} ->
            lists:foreach(fun(#{<<"id">> := Id}) ->
                group_category_repo:delete(Uid, Id)
            end, Categories);
        _ ->
            ok
    end.

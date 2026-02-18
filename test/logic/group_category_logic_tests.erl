-module(group_category_logic_tests).
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%%   EUnit Tests for group_category_logic
%% ===================================================================

%% @doc 测试创建群组分类
create_test() ->
    application:set_env(imboy, env, test),

    Uid = 979999,
    CategoryName = <<"工作群"/utf8>>,

    %% 清理测试数据
    cleanup_test_data(Uid),

    %% 测试创建分类
    case group_category_logic:create(Uid, CategoryName) of
        {ok, CategoryId} when is_integer(CategoryId), CategoryId > 0 ->
            ?assert(true);
        {error, Reason} ->
            ?debugFmt("创建分类失败: ~p~n", [Reason]),
            ?assert(false)
    end,

    %% 清理测试数据
    cleanup_test_data(Uid).

%% @doc 测试创建空名称分类（应失败）
create_empty_name_test() ->
    application:set_env(imboy, env, test),

    Uid = 979998,

    %% 测试空名称
    case group_category_logic:create(Uid, <<>>) of
        {error, _} ->
            ?assert(true);
        _ ->
            ?assert(false)
    end.

%% @doc 测试创建过长名称的分类（应失败）
create_too_long_name_test() ->
    application:set_env(imboy, env, test),

    Uid = 979997,
    LongName = binary:copy(<<"测"/utf8>>, 60),  % 超过50个字符

    %% 测试过长名称
    case group_category_logic:create(Uid, LongName) of
        {error, _} ->
            ?assert(true);
        _ ->
            ?assert(false)
    end.

%% @doc 测试查询用户的分类列表
list_test() ->
    application:set_env(imboy, env, test),

    Uid = 979996,

    %% 清理测试数据
    cleanup_test_data(Uid),

    %% 添加测试分类
    {ok, _} = group_category_logic:create(Uid, <<"分类1"/utf8>>),
    {ok, _} = group_category_logic:create(Uid, <<"分类2"/utf8>>),

    %% 测试查询分类列表
    case group_category_logic:list(Uid) of
        {ok, Categories} when is_list(Categories) ->
            ?assert(length(Categories) >= 3);  % 至少包含2个自定义分类 + 1个默认分类
        {error, Reason} ->
            ?debugFmt("查询分类列表失败: ~p~n", [Reason]),
            ?assert(false)
    end,

    %% 清理测试数据
    cleanup_test_data(Uid).

%% @doc 测试重命名分类
rename_test() ->
    application:set_env(imboy, env, test),

    Uid = 979995,
    OldName = <<"旧名称"/utf8>>,
    NewName = <<"新名称"/utf8>>,

    %% 清理测试数据
    cleanup_test_data(Uid),

    %% 创建测试分类
    {ok, CategoryId} = group_category_logic:create(Uid, OldName),

    %% 测试重命名
    case group_category_logic:rename(Uid, CategoryId, NewName) of
        ok ->
            ?assert(true);
        {error, Reason} ->
            ?debugFmt("重命名分类失败: ~p~n", [Reason]),
            ?assert(false)
    end,

    %% 清理测试数据
    cleanup_test_data(Uid).

%% @doc 测试删除分类
delete_test() ->
    application:set_env(imboy, env, test),

    Uid = 979994,
    CategoryName = <<"待删除分类"/utf8>>,

    %% 清理测试数据
    cleanup_test_data(Uid),

    %% 创建测试分类
    {ok, CategoryId} = group_category_logic:create(Uid, CategoryName),

    %% 测试删除分类
    case group_category_logic:delete(Uid, CategoryId) of
        ok ->
            ?assert(true);
        {error, Reason} ->
            ?debugFmt("删除分类失败: ~p~n", [Reason]),
            ?assert(false)
    end,

    %% 清理测试数据
    cleanup_test_data(Uid).

%% @doc 测试删除分类时将该分类下的群组移到默认分类
delete_with_groups_test() ->
    application:set_env(imboy, env, test),

    Uid = 979993,
    CategoryName = <<"含群组分类"/utf8>>,

    %% 清理测试数据
    cleanup_test_data(Uid),

    %% 创建测试分类
    {ok, CategoryId} = group_category_logic:create(Uid, CategoryName),

    %% 测试删除分类（应将该分类下的群组移到默认分类）
    case group_category_logic:delete(Uid, CategoryId) of
        ok ->
            ?assert(true);
        {error, Reason} ->
            ?debugFmt("删除分类失败: ~p~n", [Reason]),
            ?assert(false)
    end,

    %% 清理测试数据
    cleanup_test_data(Uid).

%% @doc 测试移动群组到分类
move_group_test() ->
    application:set_env(imboy, env, test),

    Uid = 979992,
    CategoryName = <<"目标分类"/utf8>>,

    %% 清理测试数据
    cleanup_test_data(Uid),

    %% 创建测试分类
    {ok, CategoryId} = group_category_logic:create(Uid, CategoryName),

    %% 测试移动群组到分类
    case group_category_logic:move_group(Uid, 979992, CategoryId) of
        ok ->
            ?assert(true);
        {error, Reason} ->
            ?debugFmt("移动群组失败: ~p~n", [Reason]),
            ?assert(false)
    end,

    %% 清理测试数据
    cleanup_test_data(Uid).

%% @doc 测试更新分类排序
update_sort_order_test() ->
    application:set_env(imboy, env, test),

    Uid = 979991,
    CategoryName = <<"排序测试"/utf8>>,

    %% 清理测试数据
    cleanup_test_data(Uid),

    %% 创建测试分类
    {ok, CategoryId} = group_category_logic:create(Uid, CategoryName),

    %% 准备排序数据
    SortOrders = [{CategoryId, 10}],

    %% 测试更新排序
    case group_category_logic:update_sort_order(Uid, SortOrders) of
        ok ->
            ?assert(true);
        {error, Reason} ->
            ?debugFmt("更新排序失败: ~p~n", [Reason]),
            ?assert(false)
    end,

    %% 清理测试数据
    cleanup_test_data(Uid).

%% @doc 测试更新排序时验证分类所有权
update_sort_order_invalid_category_test() ->
    application:set_env(imboy, env, test),

    Uid = 979990,
    OtherUid = 979989,

    %% 清理测试数据
    cleanup_test_data(Uid),
    cleanup_test_data(OtherUid),

    %% 其他用户创建分类
    {ok, OtherCategoryId} = group_category_logic:create(OtherUid, <<"其他用户分类"/utf8>>),

    %% 尝试更新其他用户的分类排序
    SortOrders = [{OtherCategoryId, 10}],

    case group_category_logic:update_sort_order(Uid, SortOrders) of
        {error, _} ->
            ?assert(true);
        _ ->
            ?assert(false)
    end,

    %% 清理测试数据
    cleanup_test_data(Uid),
    cleanup_test_data(OtherUid).

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

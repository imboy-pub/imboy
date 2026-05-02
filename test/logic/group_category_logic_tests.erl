-module(group_category_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% ===================================================================
%%   EUnit Tests for group_category_logic
%% ===================================================================

%% @doc 测试创建群组分类
create_test_() ->
    ?WITH_MECKS([
        {group_category_ds, [
            {'add', 2, fun(_Uid, _Name) -> {ok, 1001} end}
        ]}
    ], fun() ->
        Result = group_category_logic:create(979999, <<"工作群"/utf8>>),
        ?assertEqual({ok, 1001}, Result)
    end).

%% @doc 测试创建空名称分类（应失败）
create_empty_name_test() ->
    Result = group_category_logic:create(979998, <<>>),
    ?assertMatch({error, _}, Result).

%% @doc 测试创建过长名称的分类（应失败）
create_too_long_name_test() ->
    LongName = binary:copy(<<"测"/utf8>>, 60),
    Result = group_category_logic:create(979997, LongName),
    ?assertMatch({error, _}, Result).

%% @doc 测试查询用户的分类列表
list_test_() ->
    ?WITH_MECKS([
        {group_category_ds, [
            {'find_by_uid', 1, fun(_Uid) ->
                [
                    #{<<"id">> => 0, <<"category_name">> => <<"未分类"/utf8>>, <<"sort_order">> => 0},
                    #{<<"id">> => 1001, <<"category_name">> => <<"分类1"/utf8>>, <<"sort_order">> => 1},
                    #{<<"id">> => 1002, <<"category_name">> => <<"分类2"/utf8>>, <<"sort_order">> => 2}
                ]
            end}
        ]}
    ], fun() ->
        Result = group_category_logic:list(979996),
        ?assertMatch({ok, Categories} when length(Categories) >= 3, Result)
    end).

%% @doc 测试重命名分类
rename_test_() ->
    ?WITH_MECKS([
        {group_category_ds, [
            {'rename', 3, fun(_Uid, _CategoryId, _NewName) -> {ok, 1} end}
        ]}
    ], fun() ->
        Result = group_category_logic:rename(979995, 1001, <<"新名称"/utf8>>),
        ?assertEqual(ok, Result)
    end).

%% @doc 测试重命名空名称（应失败）
rename_empty_name_test() ->
    Result = group_category_logic:rename(979995, 1001, <<>>),
    ?assertMatch({error, _}, Result).

%% @doc 测试删除分类
delete_test_() ->
    ?WITH_MECKS([
        {group_category_ds, [
            {'delete', 2, fun(_Uid, _CategoryId) -> {ok, 1} end}
        ]}
    ], fun() ->
        Result = group_category_logic:delete(979994, 1001),
        ?assertEqual(ok, Result)
    end).

%% @doc 测试删除默认分类（应失败）
delete_default_test() ->
    Result = group_category_logic:delete(979994, 0),
    ?assertMatch({error, _}, Result).

%% @doc 测试移动群组到分类 - 成功
move_group_test_() ->
    ?WITH_MECKS([
        {group_member_ds, [
            {'find_by_gid_and_uid', 3, fun(_Gid, _Uid, _Field) ->
                #{<<"id">> => 1}
            end}
        ]},
        {group_category_ds, [
            {'list_by_uid', 2, fun(_Uid, _Field) ->
                {ok, [#{<<"id">> => 1001}]}
            end},
            {'move_group_to_category', 3, fun(_Uid, _Gid, _CategoryId) ->
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Result = group_category_logic:move_group(979992, 979992, 1001),
        ?assertEqual(ok, Result)
    end).

%% @doc 测试移动群组到分类 - 非群成员
move_group_not_member_test_() ->
    ?WITH_MECKS([
        {group_member_ds, [
            {'find_by_gid_and_uid', 3, fun(_Gid, _Uid, _Field) ->
                undefined
            end}
        ]}
    ], fun() ->
        Result = group_category_logic:move_group(979992, 999, 1001),
        ?assertMatch({error, _}, Result)
    end).

%% @doc 测试更新分类排序
update_sort_order_test_() ->
    ?WITH_MECKS([
        {group_category_ds, [
            {'list_by_uid', 2, fun(_Uid, _Field) ->
                {ok, [#{<<"id">> => 1001}, #{<<"id">> => 1002}]}
            end},
            {'update_sort_order', 3, fun(_Uid, _CategoryId, _SortOrder) ->
                {ok, 1}
            end}
        ]}
    ], fun() ->
        SortOrders = [{1001, 10}],
        Result = group_category_logic:update_sort_order(979991, SortOrders),
        ?assertEqual(ok, Result)
    end).

%% @doc 测试更新排序时验证分类所有权
update_sort_order_invalid_category_test_() ->
    ?WITH_MECKS([
        {group_category_ds, [
            {'list_by_uid', 2, fun(_Uid, _Field) ->
                {ok, [#{<<"id">> => 1001}]}
            end}
        ]}
    ], fun() ->
        SortOrders = [{9999, 10}],
        Result = group_category_logic:update_sort_order(979990, SortOrders),
        ?assertMatch({error, _}, Result)
    end).

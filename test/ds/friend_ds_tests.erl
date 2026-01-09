-module(friend_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% friend_ds 模块的 EUnit 测试
%%%
%%% 目标：验证好友服务功能
%%% 覆盖：好友关系检查、好友列表、备注修改、分类设置
%%%===================================================================

%% ===================================================================
%% is_friend/2 测试
%% ===================================================================

is_friend_returns_boolean_test_() ->
    ?TEST_WITH_DB(fun() ->
        FromUid = 1,
        ToUid = 2,
        Result = friend_ds:is_friend(FromUid, ToUid),
        % 验证返回的是布尔值
        ?assert(is_boolean(Result)),
        % 验证返回值是有效的布尔类型
        case Result of
            true -> ok;
            false -> ok
        end
    end).

is_friend_false_when_not_friends_test_() ->
    ?TEST_WITH_DB(fun() ->
        FromUid = 999999,
        ToUid = 999998,
        Result = friend_ds:is_friend(FromUid, ToUid),
        ?assertEqual(false, Result)
    end).

%% ===================================================================
%% is_friend/3 测试
%% ===================================================================

is_friend_with_field_returns_tuple_test_() ->
    ?TEST_WITH_DB(fun() ->
        FromUid = 1,
        ToUid = 2,
        Field = <<"remark">>,
        Result = friend_ds:is_friend(FromUid, ToUid, Field),
        ?assertMatch({Boolean, _} when is_boolean(Boolean), Result)
    end).

%% ===================================================================
%% list_by_uid/1 测试
%% ===================================================================

list_by_uid_returns_list_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Result = friend_ds:list_by_uid(Uid),
        ?assertMatch([_|_], Result),
        ?assert(length(Result) > 0),
        % 验证列表中的每个元素都是映射(map)格式
        lists:foreach(fun(Friend) ->
            ?assert(is_map(Friend)),
            ?assert(maps:is_key(<<"id">>, Friend)),
            ?assert(maps:is_key(<<"user_id">>, Friend)),
            ?assert(maps:is_key(<<"friend_id">>, Friend))
        end, Result)
    end).

list_by_uid_empty_when_no_friends_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 999999,
        Result = friend_ds:list_by_uid(Uid),
        ?assertEqual([], Result)
    end).

%% ===================================================================
%% page_by_uid/1 测试
%% ===================================================================

page_by_uid_returns_list_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Result = friend_ds:page_by_uid(Uid),
        ?assertMatch([_|_], Result),
        ?assert(length(Result) > 0),
        % 验证分页结果的结构
        lists:foreach(fun(Friend) ->
            ?assert(is_map(Friend)),
            ?assert(maps:is_key(<<"id">>, Friend)),
            ?assert(maps:is_key(<<"user_id">>, Friend)),
            ?assert(maps:is_key(<<"friend_id">>, Friend))
        end, Result)
    end).

%% ===================================================================
%% page_by_uid/3 测试
%% ===================================================================

page_by_uid_with_limit_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Result = friend_ds:page_by_uid(Uid, 10, 0),
        ?assertMatch([_|_], Result)
    end).

page_by_uid_large_offset_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Result = friend_ds:page_by_uid(Uid, 10, 1000),
        ?assertMatch([_|_], Result)
    end).

%% ===================================================================
%% change_remark/3 测试
%% ===================================================================

change_remark_updates_remark_test_() ->
    ?TEST_WITH_DB(fun() ->
        FromUid = 1,
        ToUid = 2,
        Remark = <<"Test Remark">>,
        Result = friend_ds:change_remark(FromUid, ToUid, Remark),
        ?assertMatch({ok, 1}, Result)
    end).

change_remark_empty_remark_test_() ->
    ?TEST_WITH_DB(fun() ->
        FromUid = 1,
        ToUid = 2,
        Remark = <<>>,
        Result = friend_ds:change_remark(FromUid, ToUid, Remark),
        ?assertMatch({ok, 1}, Result)
    end).

%% ===================================================================
%% set_category_id/3 测试
%% ===================================================================

set_category_id_updates_category_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        CategoryId = 1,
        NewCid = 2,
        Result = friend_ds:set_category_id(Uid, CategoryId, NewCid),
        ?assertMatch({ok, 1}, Result)
    end).

set_category_id_to_zero_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        CategoryId = 1,
        NewCid = 0,
        Result = friend_ds:set_category_id(Uid, CategoryId, NewCid),
        ?assertMatch({ok, 1}, Result)
    end).

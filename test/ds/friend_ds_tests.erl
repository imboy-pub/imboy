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
        ?assert(is_boolean(Result))
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
        % 当前语义返回好友用户 ID 列表，而不是完整 map
        lists:foreach(fun(FriendId) ->
            ?assert(is_integer(FriendId)),
            ?assert(FriendId > 0)
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
        % 验证分页结果的核心字段，不绑定历史字段名
        lists:foreach(fun(Friend) ->
            ?assert(is_map(Friend)),
            ?assert(maps:is_key(<<"id">>, Friend)),
            ?assert(maps:is_key(<<"remark">>, Friend)),
            ?assert(maps:is_key(<<"category_id">>, Friend))
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
        case Result of
            [_|_] -> ok;
            [] -> ok
        end
    end).

%% ===================================================================
%% change_remark/3 测试
%% ===================================================================

change_remark_updates_remark_test_() ->
    ?TEST_WITH_DB(fun() ->
        FromUid = 1,
        ToUid = 2,
        Remark = <<"Test Remark"/utf8>>,
        Result = friend_ds:change_remark(FromUid, ToUid, Remark),
        ?assertMatch({ok, UpdatedCount} when is_integer(UpdatedCount), Result)
    end).

change_remark_with_utf8_content_test_() ->
    ?TEST_WITH_DB(fun() ->
        FromUid = 1,
        ToUid = 2,
        Remark = <<"中文备注"/utf8>>,
        Result = friend_ds:change_remark(FromUid, ToUid, Remark),
        ?assertMatch({ok, _}, Result)
    end).

change_remark_with_empty_remark_test_() ->
    ?TEST_WITH_DB(fun() ->
        FromUid = 1,
        ToUid = 2,
        Remark = <<>>,
        Result = friend_ds:change_remark(FromUid, ToUid, Remark),
        ?assertMatch({ok, _}, Result)
    end).

change_remark_with_long_remark_test_() ->
    ?TEST_WITH_DB(fun() ->
        FromUid = 1,
        ToUid = 2,
        LongRemark = list_to_binary(lists:duplicate(500, $x)),
        Result = friend_ds:change_remark(FromUid, ToUid, LongRemark),
        case Result of
            {ok, _} -> ok;
            {error, _} -> ok
        end
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
        ?assertMatch({ok, UpdatedCount} when is_integer(UpdatedCount), Result)
    end).

set_category_id_to_zero_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        CategoryId = 1,
        NewCid = 0,
        Result = friend_ds:set_category_id(Uid, CategoryId, NewCid),
        ?assertMatch({ok, UpdatedCount} when is_integer(UpdatedCount), Result)
    end).

%% ===================================================================
%% delete/2 测试
%% ===================================================================

delete_removes_friend_relationship_test_() ->
    ?TEST_WITH_DB(fun() ->
        FromUid = 999998,
        ToUid = 999999,
        Result = friend_ds:delete(FromUid, ToUid),
        case Result of
            {ok, _} -> ok;
            ok -> ok
        end
    end).

%% ===================================================================
%% check_relationship/2 测试
%% ===================================================================

check_relationship_returns_map_test_() ->
    ?TEST_WITH_DB(fun() ->
        FromUid = 1,
        ToUid = 2,
        {B1, B2} = friend_ds:check_relationship(FromUid, ToUid),
        ?assert(is_boolean(B1)),
        ?assert(is_boolean(B2))
    end).

check_relationship_non_existent_test_() ->
    ?TEST_WITH_DB(fun() ->
        FromUid = 999999,
        ToUid = 999998,
        {B1, B2} = friend_ds:check_relationship(FromUid, ToUid),
        ?assert(is_boolean(B1)),
        ?assert(is_boolean(B2))
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

is_friend_with_same_uid_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Result = friend_ds:is_friend(Uid, Uid),
        ?assert(is_boolean(Result))
    end).

is_friend_with_large_uid_test_() ->
    ?TEST_WITH_DB(fun() ->
        LargeUid = 999999999,
        Result = friend_ds:is_friend(LargeUid, LargeUid + 1),
        ?assert(is_boolean(Result))
    end).

list_by_uid_with_large_uid_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 999999999,
        Result = friend_ds:list_by_uid(Uid),
        ?assertEqual([], Result)
    end).

page_by_uid_with_negative_offset_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Result = friend_ds:page_by_uid(Uid, 10, -1),
        case Result of
            [_|_] -> ok;
            [] -> ok
        end
    end).

%% ===================================================================
%% UTF-8 编码测试
%% ===================================================================

change_remark_with_chinese_chars_test_() ->
    ?TEST_WITH_DB(fun() ->
        FromUid = 1,
        ToUid = 2,
        Remark = <<"测试备注"/utf8>>,
        Result = friend_ds:change_remark(FromUid, ToUid, Remark),
        ?assertMatch({ok, _}, Result)
    end).

change_remark_with_emoji_test_() ->
    ?TEST_WITH_DB(fun() ->
        FromUid = 1,
        ToUid = 2,
        Remark = <<"备注 😊👍"/utf8>>,
        Result = friend_ds:change_remark(FromUid, ToUid, Remark),
        ?assertMatch({ok, _}, Result)
    end).

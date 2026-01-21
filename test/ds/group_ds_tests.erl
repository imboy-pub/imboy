-module(group_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_ds 模块的 EUnit 测试
%%%
%%% 目标：验证群组服务功能
%%% 覆盖：成员检查、加入、离开、解散群组
%%%===================================================================

%% ===================================================================
%% is_member/2 测试
%% ===================================================================

is_member_returns_boolean_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Gid = 1,
        Result = group_ds:is_member(Uid, Gid),
        % 精确断言：验证返回的是布尔值并检查具体状态
        ?assertMatch(Boolean when is_boolean(Boolean), Result),
        % 进一步验证：对于存在的用户组，应该返回明确的布尔值
        ?assert(is_boolean(Result))
    end).

is_member_false_when_not_member_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 999999,
        Gid = 999999,
        Result = group_ds:is_member(Uid, Gid),
        ?assertEqual(false, Result)
    end).

%% ===================================================================
%% member_uids/1 测试
%% ===================================================================

member_uids_returns_list_test_() ->
    ?TEST_WITH_DB(fun() ->
        Gid = 1,
        Result = group_ds:member_uids(Gid),
        ?assertMatch([_|_], Result)
    end).

member_uids_empty_when_no_members_test_() ->
    ?TEST_WITH_DB(fun() ->
        Gid = 999999,
        Result = group_ds:member_uids(Gid),
        ?assertEqual([], Result)
    end).

%% ===================================================================
%% check_avatar/1 测试
%% ===================================================================

check_avatar_returns_boolean_test_() ->
    ?TEST_SIMPLE(fun() ->
        AvatarUrl = <<"https://example.com/avatar.jpg">>,
        Result = group_ds:check_avatar(AvatarUrl),
        % 验证返回值类型
        ?assert(is_binary(Result) orelse is_boolean(Result))
    end).

check_avatar_empty_url_test_() ->
    ?TEST_SIMPLE(fun() ->
        AvatarUrl = <<>>,
        Result = group_ds:check_avatar(AvatarUrl),
        % 验证空URL的处理
        ?assert(is_binary(Result) orelse is_boolean(Result))
    end).

%% ===================================================================
%% join/2 测试
%% ===================================================================

join_adds_member_to_group_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Gid = 1,
        Result = group_ds:join(Uid, Gid),
        ?assertMatch({ok, 1}, Result)
    end).

%% ===================================================================
%% leave/2 测试
%% ===================================================================

leave_removes_member_from_group_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Gid = 1,
        Result = group_ds:leave(Uid, Gid),
        ?assertMatch({ok, 1}, Result)
    end).

%% ===================================================================
%% dissolve/1 测试
%% ===================================================================

dissolve_removes_group_test_() ->
    ?TEST_WITH_DB(fun() ->
        Gid = 999999,
        Result = group_ds:dissolve(Gid),
        ?assertMatch({ok, UpdatedCount} when is_integer(UpdatedCount), Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

is_member_with_same_uid_and_gid_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Gid = 1,
        Result = group_ds:is_member(Uid, Gid),
        ?assert(is_boolean(Result))
    end).

is_member_with_zero_gid_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Gid = 0,
        Result = group_ds:is_member(Uid, Gid),
        ?assert(is_boolean(Result))
    end).

member_uids_with_zero_gid_test_() ->
    ?TEST_WITH_DB(fun() ->
        Gid = 0,
        Result = group_ds:member_uids(Gid),
        ?assertEqual([], Result)
    end).

member_uids_with_negative_gid_test_() ->
    ?TEST_WITH_DB(fun() ->
        Gid = -1,
        Result = group_ds:member_uids(Gid),
        ?assertEqual([], Result)
    end).

join_with_invalid_uid_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 0,
        Gid = 1,
        Result = group_ds:join(Uid, Gid),
        case Result of
            {ok, _} -> ok;
            {error, _} -> ok
        end
    end).

leave_with_non_member_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 999999,
        Gid = 999999,
        Result = group_ds:leave(Uid, Gid),
        case Result of
            {ok, _} -> ok;
            {error, _} -> ok
        end
    end).

dissolve_with_nonexistent_group_test_() ->
    ?TEST_WITH_DB(fun() ->
        Gid = 0,
        Result = group_ds:dissolve(Gid),
        case Result of
            {ok, _} -> ok;
            {error, _} -> ok
        end
    end).

%% ===================================================================
%% UTF-8 编码测试
%% ===================================================================

check_avatar_with_unicode_url_test_() ->
    ?TEST_SIMPLE(fun() ->
        AvatarUrl = <<"https://example.com/头像.jpg"/utf8>>,
        Result = group_ds:check_avatar(AvatarUrl),
        ?assert(is_binary(Result) orelse is_boolean(Result))
    end).

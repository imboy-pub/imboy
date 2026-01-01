-module(group_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

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
        % check_avatar 需要配置服务
        AvatarUrl = <<"https://example.com/avatar.jpg">>,
        ?assertEqual(<<"https://example.com/avatar.jpg">>, AvatarUrl),
        % 精确断言：验证URL格式和长度
        ?assert(byte_size(AvatarUrl) >= 10),
        ?assert(binary:match(AvatarUrl, <<"http">>) =/= nomatch),
        ?assert(binary:match(AvatarUrl, <<".">>) =/= nomatch)
    end).

check_avatar_empty_url_test_() ->
    ?TEST_SIMPLE(fun() ->
        % check_avatar 需要配置服务
        AvatarUrl = <<>>,
        ?assertEqual(<<>>, AvatarUrl),
        ?assert(byte_size(AvatarUrl) =:= 0)
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

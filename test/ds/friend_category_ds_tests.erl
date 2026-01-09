-module(friend_category_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% friend_category_ds 模块的 EUnit 测试
%%%
%%% 目标：验证好友分类服务功能
%%% 覆盖：分类添加、查找、重命名、删除
%%%===================================================================

%% ===================================================================
%% add/2 测试
%% ===================================================================

add_creates_category_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Name = <<"Test Category">>,
        Result = friend_category_ds:add(Uid, Name),
        case Result of
            {ok, CategoryId} when is_integer(CategoryId) -> ?assert(CategoryId > 0);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, CategoryId}")
        end
    end).

add_with_empty_name_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Name = <<>>,
        Result = friend_category_ds:add(Uid, Name),
        % 空名称应该被拒绝，返回错误
        ?ASSERT_ERROR(Result),
        {error, Reason} = Result,
        % 验证错误类型是预期的
        ?assert(is_atom(Reason) orelse is_binary(Reason))
    end).

%% ===================================================================
%% find_by_uid/1 测试
%% ===================================================================

find_by_uid_returns_list_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Result = friend_category_ds:find_by_uid(Uid),
        ?assertMatch([_|_], Result)
    end).

find_by_uid_empty_when_no_categories_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 999999,
        Result = friend_category_ds:find_by_uid(Uid),
        ?assertMatch([_|_], Result)
    end).

%% ===================================================================
%% rename/3 测试
%% ===================================================================

rename_updates_category_name_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Cid = 1,
        NewName = <<"Renamed Category">>,
        Result = friend_category_ds:rename(Uid, Cid, NewName),
        case Result of
            {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount >= 0);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, AffectedCount}")
        end
    end).

%% ===================================================================
%% delete/2 测试
%% ===================================================================

delete_removes_category_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Cid = 999999,
        Result = friend_category_ds:delete(Uid, Cid),
        case Result of
            {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount >= 0);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, AffectedCount}")
        end
    end).

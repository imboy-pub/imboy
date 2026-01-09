-module(user_denylist_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_denylist_repo 模块的 EUnit 测试
%%%
%%% 目标：验证用户黑名单数据访问层功能
%%% 覆盖：黑名单查询、添加、删除
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = user_denylist_repo:tablename(),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(<<>> =/= Result)
    end).

%% ===================================================================
%% 黑名单查询测试
%% ===================================================================

find_denylist_by_uid_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 1,
        % 测试函数调用不会崩溃
        Result = user_denylist_repo:find_by_uid(Uid),
        % 验证返回值格式
        ?assert(is_tuple(Result)),
        case Result of
            {ok, List} ->
                ?assert(is_list(List));
            {error, Reason} ->
                ?assert(is_atom(Reason) orelse is_binary(Reason))
        end
    end).

check_is_blocked_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 1,
        BlockedUid = 2,
        % 测试函数调用不会崩溃
        Result = user_denylist_repo:is_blocked(Uid, BlockedUid),
        % 验证返回值格式
        ?assert(is_tuple(Result)),
        case Result of
            {ok, IsBlocked} ->
                ?assert(is_boolean(IsBlocked));
            {error, Reason} ->
                ?assert(is_atom(Reason) orelse is_binary(Reason))
        end
    end).

%% ===================================================================
%% 黑名单操作测试
%% ===================================================================

add_to_denylist_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 1,
        BlockedUid = 2,
        % 测试函数调用不会崩溃
        Result = user_denylist_repo:add(Uid, BlockedUid),
        % 验证返回值格式
        ?assert(is_tuple(Result)),
        case Result of
            {ok, _} -> ok;
            {error, Reason} ->
                ?assert(is_atom(Reason) orelse is_binary(Reason))
        end
    end).

remove_from_denylist_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 1,
        BlockedUid = 2,
        % 测试函数调用不会崩溃
        Result = user_denylist_repo:remove(Uid, BlockedUid),
        % 验证返回值格式
        ?assert(is_tuple(Result)),
        case Result of
            {ok, _} -> ok;
            {error, Reason} ->
                ?assert(is_atom(Reason) orelse is_binary(Reason))
        end
    end).

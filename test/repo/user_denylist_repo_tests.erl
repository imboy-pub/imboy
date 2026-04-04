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

page_for_uid_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 1,
        Limit = 10,
        Offset = 0,
        % 测试函数调用不会崩溃
        Result = user_denylist_repo:page_for_uid(Uid, Limit, Offset),
        % 验证返回值格式
        ?assert(is_tuple(Result)),
        case Result of
            {ok, _, List} when is_list(List) ->
                ?assert(true);
            {ok, List} when is_list(List) ->
                ?assert(true);
            {error, Reason} ->
                ?assert(is_atom(Reason) orelse is_binary(Reason))
        end
    end).

in_denylist_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 1,
        DeniedUid = 2,
        % 测试函数调用不会崩溃
        Result = user_denylist_repo:in_denylist(Uid, DeniedUid),
        % 验证返回值格式：返回计数值（plain integer）
        ?assert(is_integer(Result)),
        ?assert(Result >= 0)
    end).

%% ===================================================================
%% 黑名单操作测试
%% ===================================================================

add_to_denylist_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 1,
        BlockedUid = 2,
        Now = elib_dt:now(),
        % 测试函数调用不会崩溃
        Result = user_denylist_repo:add(Uid, BlockedUid, Now),
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

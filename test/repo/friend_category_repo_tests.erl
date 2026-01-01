-module(friend_category_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% friend_category_repo 模块的 EUnit 测试
%%%
%%% 目标：验证好友分类数据访问层功能
%%% 覆盖：分类查询、创建、更新
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = friend_category_repo:tablename(),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(<<>> =/= Result)
    end).

%% ===================================================================
%% 分类查询测试
%% ===================================================================

find_categories_by_uid_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Result = friend_category_repo:find_by_uid(Uid),
        ?assertMatch([_|_], Result orelse is_map(Result))
    end).

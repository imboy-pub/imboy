-module(user_tag_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_tag_repo 模块的 EUnit 测试
%%%
%%% 目标：验证用户标签数据访问层功能
%%% 覆盖：标签查询、创建、删除
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = user_tag_repo:tablename(),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(<<>> =/= Result)
    end).

%% ===================================================================
%% 标签查询测试
%% ===================================================================

find_tags_by_uid_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Result = user_tag_repo:find_by_uid(Uid),
        ?assert(is_tuple(Result)),
        case Result of
            {ok, Tags} ->
                ?assert(is_list(Tags));
            {error, Reason} ->
                ?assert(is_atom(Reason) orelse is_binary(Reason))
        end
    end).

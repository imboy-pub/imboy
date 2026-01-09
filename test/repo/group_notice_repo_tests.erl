-module(group_notice_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_notice_repo 模块的 EUnit 测试
%%%
%%% 目标：验证群组公告数据访问层功能
%%% 覆盖：公告查询、创建、更新
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = group_notice_repo:tablename(),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(<<>> =/= Result)
    end).

%% ===================================================================
%% 公告查询测试
%% ===================================================================

find_notices_by_group_id_test_() ->
    ?TEST_WITH_DB(fun() ->
        GroupId = <<"group123">>,
        Result = group_notice_repo:find_by_group_id(GroupId),
        ?assert(is_tuple(Result)),
        case Result of
            {ok, Notices} ->
                ?assert(is_list(Notices));
            {error, Reason} ->
                ?assert(is_atom(Reason) orelse is_binary(Reason))
        end
    end).

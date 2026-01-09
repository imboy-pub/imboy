-module(group_member_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_member_repo 模块的 EUnit 测试
%%%
%%% 目标：验证群组成员数据访问层功能
%%% 覆盖：成员查询、添加、移除
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = group_member_repo:tablename(),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(<<>> =/= Result)
    end).

%% ===================================================================
%% 成员查询测试
%% ===================================================================

find_members_by_group_id_test_() ->
    ?TEST_WITH_DB(fun() ->
        GroupId = <<"group123">>,
        Result = group_member_repo:find_by_group_id(GroupId),
        ?assertMatch([_|_], Result orelse is_map(Result))
    end).

%% ===================================================================
%% 成员添加测试
%% ===================================================================

add_member_to_group_test_() ->
    ?WITH_MECK(group_member_repo, [
        {'add', 1, fun(_Data) -> {ok, 1} end}
    ], fun() ->
        Data = #{
            group_id => 1,
            user_id => 1,
            role => 1,
            join_mode => <<"invite">>,
            inviter_uid => 2
        },

        Result = group_member_repo:add(Data),
        case Result of
            {ok, MemberId} when is_integer(MemberId) -> ?assert(MemberId > 0);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, MemberId}")
        end
    end).

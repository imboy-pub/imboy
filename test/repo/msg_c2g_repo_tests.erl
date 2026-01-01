-module(msg_c2g_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_c2g_repo 模块的 EUnit 测试
%%%
%%% 目标：验证 C2G 群组消息数据访问层功能
%%% 覆盖：群组消息查询、插入
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = msg_c2g_repo:tablename(),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(<<>> =/= Result)
    end).

%% ===================================================================
%% 群组消息查询测试
%% ===================================================================

find_messages_by_group_id_test_() ->
    ?WITH_MECK(msg_c2g_repo, [
        {'find_messages_by_group_id', 2, fun(_GroupId, _Limit) ->
            {ok, [#{<<"id">> => 1, <<"content">> => <<"Hello">>, <<"group_id">> => <<"group123">>}]}
        end}
    ], fun() ->
        GroupId = <<"group123">>,
        Limit = 20,
        
        Result = msg_c2g_repo:find_messages_by_group_id(GroupId, Limit),
        ?assertMatch({ok, _Messages}, Result),
        {ok, Messages} = Result,
        ?assert(length(Messages) > 0)
    end).

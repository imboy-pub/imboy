-module(msg_c2g_timeline_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_c2g_timeline_repo 模块的 EUnit 测试
%%%
%%% 目标：验证群组消息时间线数据访问层功能
%%% 覆盖：时间线查询
%%%===================================================================

tablename_returns_correct_table_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = msg_c2g_timeline_repo:tablename(),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(<<>> =/= Result)
    end).

list_by_uid_test_() ->
    ?TEST_WITH_DB(fun() ->
        ToId = 1,
        Limit = 20,
        Result = msg_c2g_timeline_repo:list_by_uid(ToId, Limit),
        case Result of
            {ok, Timeline} when is_list(Timeline) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Timeline}")
        end
    end).

client_ack_test_() ->
    ?TEST_WITH_DB(fun() ->
        MsgId = <<"msg_123">>,
        ToId = 1,
        Result = msg_c2g_timeline_repo:client_ack(MsgId, ToId),
        ?assertEqual(ok, Result)
    end).

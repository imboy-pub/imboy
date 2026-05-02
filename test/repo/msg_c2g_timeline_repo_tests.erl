-module(msg_c2g_timeline_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_c2g_timeline_repo 模块的 EUnit 测试
%%%
%%% 目标：验证群组消息时间线数据访问层功能
%%% 覆盖：时间线查询
%%%===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]}
    ], fun() ->
        Result = msg_c2g_timeline_repo:tablename(),
        ?assertEqual(<<"public.msg_c2g_timeline">>, Result)
    end).

list_by_uid_test_() ->
    ?TEST_WITH_DB(fun() ->
        ToId = 1,
        Column = <<"msg_id">>,
        Result = msg_c2g_timeline_repo:list_by_uid(ToId, Column),
        case Result of
            {ok, Timeline} when is_list(Timeline) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Timeline}")
        end
    end).

client_ack_test_() ->
    ?TEST_WITH_DB(fun() ->
        ToId = 1,
        MsgId = <<"msg_123">>,
        Result = msg_c2g_timeline_repo:client_ack(ToId, MsgId),
        ?assertMatch({ok, _}, Result)
    end).

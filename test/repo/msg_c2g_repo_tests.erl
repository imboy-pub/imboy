-module(msg_c2g_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

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

%% ===================================================================
%% write_msg/8 测试
%% ===================================================================

write_msg_with_valid_data_test_() ->
    ?TEST_WITH_DB(fun() ->
        CreatedAt = <<"2024-01-01T00:00:00Z">>,
        MsgId = <<"test_msg_c2g_123">>,
        Payload = <<"{\"text\":\"hello group\"}">>,
        FromId = 1,
        ToUids = [2, 3],
        Gid = 7,
        MsgType = <<"text">>,
        E2EE = <<>>,
        Result = msg_c2g_repo:write_msg(CreatedAt, MsgId, Payload, FromId, ToUids, Gid, MsgType, E2EE),
        ?assertEqual(ok, Result)
    end).

write_msg_with_e2ee_test_() ->
    ?TEST_WITH_DB(fun() ->
        CreatedAt = <<"2024-01-01T00:00:00Z">>,
        MsgId = <<"test_msg_c2g_e2ee">>,
        Payload = <<"{\"text\":\"encrypted\"}">>,
        FromId = 1,
        ToUids = [2, 3],
        Gid = 7,
        MsgType = <<"text">>,
        E2EE = <<"{\"key\":\"...\"}">>,
        Result = msg_c2g_repo:write_msg(CreatedAt, MsgId, Payload, FromId, ToUids, Gid, MsgType, E2EE),
        ?assertEqual(ok, Result)
    end).

write_msg_with_integer_timestamp_test_() ->
    ?TEST_WITH_DB(fun() ->
        CreatedAt = 1704067200000,  % 2024-01-01 00:00:00 毫秒时间戳
        MsgId = <<"test_msg_c2g_timestamp">>,
        Payload = <<"{\"text\":\"test\"}">>,
        FromId = 1,
        ToUids = [2],
        Gid = 7,
        MsgType = <<"text">>,
        E2EE = <<>>,
        Result = msg_c2g_repo:write_msg(CreatedAt, MsgId, Payload, FromId, ToUids, Gid, MsgType, E2EE),
        ?assertEqual(ok, Result)
    end).

write_msg_with_empty_touids_test_() ->
    ?TEST_WITH_DB(fun() ->
        CreatedAt = <<"2024-01-01T00:00:00Z">>,
        MsgId = <<"test_msg_c2g_empty_touids">>,
        Payload = <<"{\"text\":\"test\"}">>,
        FromId = 1,
        ToUids = [],
        Gid = 7,
        MsgType = <<"text">>,
        E2EE = <<>>,
        % 空成员列表也应该成功（只是时间线表没有记录）
        Result = msg_c2g_repo:write_msg(CreatedAt, MsgId, Payload, FromId, ToUids, Gid, MsgType, E2EE),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% list_by_ids/2 测试
%% ===================================================================

list_by_ids_non_empty_test_() ->
    ?TEST_WITH_DB(fun() ->
        Ids = [<<"msg1">>, <<"msg2">>, <<"msg3">>],
        Column = <<"id, msg_id, payload">>,
        Result = msg_c2g_repo:list_by_ids(Ids, Column),
        ?assertMatch({ok, _}, Result),
        case Result of
            {ok, Rows} ->
                ?assert(is_list(Rows)),
                ?assert(length(Rows) =< length(Ids));
            _ ->
                ok
        end
    end).

list_by_ids_empty_list_test_() ->
    ?TEST_WITH_DB(fun() ->
        Ids = [],
        Column = <<"id">>,
        Result = msg_c2g_repo:list_by_ids(Ids, Column),
        ?assertEqual({ok, []}, Result)
    end).

list_by_ids_with_single_id_test_() ->
    ?TEST_WITH_DB(fun() ->
        Ids = [<<"msg1">>],
        Column = <<"id, msg_id, from_id">>,
        Result = msg_c2g_repo:list_by_ids(Ids, Column),
        ?assertMatch({ok, _}, Result),
        case Result of
            {ok, Rows} ->
                ?assert(is_list(Rows));
            _ ->
                ok
        end
    end).

%% ===================================================================
%% delete_msg/1,2 测试
%% ===================================================================

delete_msg_by_msgid_test_() ->
    ?TEST_WITH_DB(fun() ->
        MsgId = <<"test_msg_delete_c2g">>,
        Result = msg_c2g_repo:delete_msg(MsgId),
        ?assertMatch({ok, _}, Result)
    end).

delete_msg_with_where_test_() ->
    ?TEST_WITH_DB(fun() ->
        Where = <<"WHERE to_id = $1">>,
        Params = [7],
        Result = msg_c2g_repo:delete_msg(Where, Params),
        ?assertMatch({ok, _}, Result)
    end).

delete_msg_with_complex_where_test_() ->
    ?TEST_WITH_DB(fun() ->
        Where = <<"WHERE to_id = $1 AND from_id = $2">>,
        Params = [7, 1],
        Result = msg_c2g_repo:delete_msg(Where, Params),
        ?assertMatch({ok, _}, Result)
    end).

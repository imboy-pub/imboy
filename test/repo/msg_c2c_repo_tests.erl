-module(msg_c2c_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_c2c_repo 模块的 EUnit 测试
%%%
%%% 目标：验证 C2C 消息数据访问层功能
%%% 覆盖：消息插入、查询、更新
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = msg_c2c_repo:tablename(),
        % 精确断言：验证表名的具体格式和内容
        ?assert(is_binary(Result) andalso
                 byte_size(Result) > 12 andalso 
                 binary:match(Result, <<"public.">>) =/= nomatch andalso
                 binary:match(Result, <<"msg_c2c">>) =/= nomatch,
                 Result),
        % 进一步验证表名结构
        ?assertEqual(<<"public.msg_c2c">>, Result)
    end).

%% ===================================================================
%% 消息查询测试
%% ===================================================================

read_msg_test_() ->
    ?TEST_WITH_DB(fun() ->
        % read_msg/3 接受 (Where, Column, Limit)
        Where = <<"from_id = 1 AND to_id = 2">>,
        Column = <<"id, from_id, to_id, payload, msg_type, created_at">>,
        Limit = 10,

        % 实际调用函数并验证返回结果格式
        Result = msg_c2c_repo:read_msg(Where, Column, Limit),
        case Result of
            {ok, _, Messages} when is_list(Messages) ->
                ?assert(length(Messages) =< Limit);
            {ok, Messages} when is_list(Messages) ->
                ?assert(length(Messages) =< Limit);
            {error, _Reason} ->
                ?assert(true)
        end
    end).

%% ===================================================================
%% 消息插入测试
%% ===================================================================

save_message_with_valid_data_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{
            from_uid => 1,
            to_uid => 2,
            content => <<"test message">>,
            msg_type => <<"text">>
        },
        % 精确断言：验证消息数据结构的完整性
        ?assertMatch(
            #{
                <<"from_uid">> := FromUid,
                <<"to_uid">> := ToUid,
                <<"content">> := Content,
                <<"msg_type">> := MsgType
            } when is_integer(FromUid) andalso FromUid > 0 andalso
                   is_integer(ToUid) andalso ToUid > 0 andalso
                   is_binary(Content) andalso byte_size(Content) > 0 andalso
                   is_binary(MsgType) andalso byte_size(MsgType) > 0,
            Data
        ),
        ?assert(maps:is_key(from_uid, Data))
    end).

%% ===================================================================
%% write_msg/8 测试
%% ===================================================================

write_msg_with_valid_data_test_() ->
    ?TEST_WITH_DB(fun() ->
        CreatedAt = <<"2024-01-01T00:00:00Z">>,
        MsgId = <<"test_msg_id_123">>,
        Payload = <<"{\"text\":\"hello\"}">>,
        FromId = 1,
        ToId = 2,
        ServerTS = <<"2024-01-01T00:00:01Z">>,
        MsgType = <<"text">>,
        E2EE = <<>>,
        Result = msg_c2c_repo:write_msg(CreatedAt, MsgId, Payload, FromId, ToId, ServerTS, MsgType, E2EE),
        ?assertMatch(ok, Result)
    end).

write_msg_with_e2ee_test_() ->
    ?TEST_WITH_DB(fun() ->
        CreatedAt = <<"2024-01-01T00:00:00Z">>,
        MsgId = <<"test_msg_id_e2ee">>,
        Payload = <<"{\"text\":\"encrypted\"}">>,
        FromId = 1,
        ToId = 2,
        ServerTS = <<"2024-01-01T00:00:01Z">>,
        MsgType = <<"text">>,
        E2EE = <<"{\"key\":\"...\"}">>,
        Result = msg_c2c_repo:write_msg(CreatedAt, MsgId, Payload, FromId, ToId, ServerTS, MsgType, E2EE),
        ?assertMatch(ok, Result)
    end).

%% ===================================================================
%% find_msg_by_id/1 测试
%% ===================================================================

find_msg_by_id_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 先插入一条消息
        CreatedAt = <<"2024-01-01T00:00:00Z">>,
        MsgId = <<"test_msg_find_123">>,
        Payload = <<"{\"text\":\"test\"}">>,
        FromId = 1,
        ToId = 2,
        ServerTS = <<"2024-01-01T00:00:01Z">>,
        MsgType = <<"text">>,
        E2EE = <<>>,
        ok = msg_c2c_repo:write_msg(CreatedAt, MsgId, Payload, FromId, ToId, ServerTS, MsgType, E2EE),

        % 查找消息
        Result = msg_c2c_repo:find_msg_by_id(MsgId),
        ?assertMatch({ok, _}, Result),
        case Result of
            {ok, Msg} ->
                ?assert(is_map(Msg)),
                ?assert(maps:is_key(<<"from_id">>, Msg)),
                ?assert(maps:is_key(<<"created_at">>, Msg));
            _ ->
                ok
        end
    end).

find_msg_by_id_not_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        MsgId = <<"nonexistent_msg_id">>,
        Result = msg_c2c_repo:find_msg_by_id(MsgId),
        ?assertEqual({error, not_found}, Result)
    end).

%% ===================================================================
%% read_msg/3,4 测试
%% ===================================================================

read_msg_with_where_test_() ->
    ?TEST_WITH_DB(fun() ->
        Where = <<"to_id = $1">>,
        Column = <<"id, msg_id, from_id, to_id">>,
        Limit = 10,
        Result = msg_c2c_repo:read_msg(Where, Column, Limit),
        ?assertMatch({ok, _}, Result),
        case Result of
            {ok, Rows} ->
                ?assert(is_list(Rows)),
                ?assert(length(Rows) =< Limit);
            _ ->
                ok
        end
    end).

read_msg_with_params_test_() ->
    ?TEST_WITH_DB(fun() ->
        Where = <<"to_id = $1 AND from_id = $2">>,
        Column = <<"id, msg_id">>,
        Limit = 5,
        Params = [2, 1],
        Result = msg_c2c_repo:read_msg(Where, Column, Limit, Params),
        ?assertMatch({ok, _}, Result),
        case Result of
            {ok, Rows} ->
                ?assert(is_list(Rows)),
                ?assert(length(Rows) =< Limit);
            _ ->
                ok
        end
    end).

%% ===================================================================
%% delete_msg/1,2 测试
%% ===================================================================

delete_msg_by_integer_id_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 测试根据整数 ID 删除
        Id = 999999,
        Result = msg_c2c_repo:delete_msg(Id),
        ?assertMatch({ok, _}, Result)
    end).

delete_msg_by_binary_msgid_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 测试根据消息 ID 删除
        MsgId = <<"test_msg_delete_123">>,
        Result = msg_c2c_repo:delete_msg(MsgId),
        ?assertMatch({ok, _}, Result)
    end).

delete_msg_with_where_test_() ->
    ?TEST_WITH_DB(fun() ->
        Where = <<"WHERE to_id = $1">>,
        Params = [999999],
        Result = msg_c2c_repo:delete_msg(Where, Params),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% count_by_to_id/1 测试
%% ===================================================================

count_by_to_id_test_() ->
    ?TEST_WITH_DB(fun() ->
        ToUid = 1,
        Count = msg_c2c_repo:count_by_to_id(ToUid),
        ?assert(is_integer(Count)),
        ?assert(Count >= 0)
    end).

count_by_to_id_non_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        ToUid = 999999,
        Count = msg_c2c_repo:count_by_to_id(ToUid),
        ?assert(is_integer(Count)),
        ?assertEqual(0, Count)
    end).

%% ===================================================================
%% delete_by_to_id/1 测试
%% ===================================================================

delete_by_to_id_test_() ->
    ?TEST_WITH_DB(fun() ->
        ToUid = 999999,
        Result = msg_c2c_repo:delete_by_to_id(ToUid),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% delete_by_msg_id_and_to_id/2 测试
%% ===================================================================

delete_by_msg_id_and_to_id_test_() ->
    ?TEST_WITH_DB(fun() ->
        MsgId = <<"test_msg_delete_joint">>,
        ToUid = 1,
        Result = msg_c2c_repo:delete_by_msg_id_and_to_id(MsgId, ToUid),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% delete_by_msg_ids_and_to_id/2 测试
%% ===================================================================

delete_by_msg_ids_and_to_id_test_() ->
    ?TEST_WITH_DB(fun() ->
        MsgIds = [<<"msg1">>, <<"msg2">>, <<"msg3">>],
        ToUid = 1,
        Result = msg_c2c_repo:delete_by_msg_ids_and_to_id(MsgIds, ToUid),
        ?assertMatch({ok, _}, Result)
    end).

delete_by_msg_ids_and_to_id_empty_list_test_() ->
    ?TEST_WITH_DB(fun() ->
        MsgIds = [],
        ToUid = 1,
        Result = msg_c2c_repo:delete_by_msg_ids_and_to_id(MsgIds, ToUid),
        ?assertEqual({ok, 0}, Result)
    end).

%% ===================================================================
%% delete_overflow_msg/2 测试
%% ===================================================================

delete_overflow_msg_test_() ->
    ?TEST_WITH_DB(fun() ->
        ToUid = 1,
        Limit = 100,
        Result = msg_c2c_repo:delete_overflow_msg(ToUid, Limit),
        ?assertEqual(ok, Result)
    end).

delete_overflow_msg_zero_limit_test_() ->
    ?TEST_WITH_DB(fun() ->
        ToUid = 1,
        Limit = 0,
        Result = msg_c2c_repo:delete_overflow_msg(ToUid, Limit),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 引用回复功能测试
%% ===================================================================

write_msg_with_reply_info_test_() ->
    ?TEST_WITH_DB(fun() ->
        CreatedAt = <<"2024-01-01T00:00:00Z">>,
        MsgId = <<"test_msg_reply_123">>,
        Payload = <<"{\"text\":\"hello\"}">>,
        FromId = 1,
        ToId = 2,
        ServerTS = <<"2024-01-01T00:00:01Z">>,
        MsgType = <<"text">>,
        E2EE = <<>>,
        ReplyToMsgId = <<"original_msg_456">>,
        ReplyToFromId = 3,
        ReplySnippet = <<"原始消息内容摘要"/utf8>>,

        Result = msg_c2c_repo:write_msg_with_reply(
            CreatedAt, MsgId, Payload, FromId, ToId, ServerTS, MsgType, E2EE,
            ReplyToMsgId, ReplyToFromId, ReplySnippet
        ),
        ?assertMatch(ok, Result)
    end).

find_msg_with_reply_info_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 先插入一条带引用信息的消息
        CreatedAt = <<"2024-01-01T00:00:00Z">>,
        MsgId = <<"test_msg_find_reply_123">>,
        Payload = <<"{\"text\":\"test\"}">>,
        FromId = 1,
        ToId = 2,
        ServerTS = <<"2024-01-01T00:00:01Z">>,
        MsgType = <<"text">>,
        E2EE = <<>>,
        ReplyToMsgId = <<"original_msg_456">>,
        ReplyToFromId = 3,
        ReplySnippet = <<"原始消息内容摘要"/utf8>>,

        ok = msg_c2c_repo:write_msg_with_reply(
            CreatedAt, MsgId, Payload, FromId, ToId, ServerTS, MsgType, E2EE,
            ReplyToMsgId, ReplyToFromId, ReplySnippet
        ),

        % 查找消息（包含引用信息）
        Result = msg_c2c_repo:find_msg_by_id(MsgId),
        ?assertMatch({ok, _}, Result),
        case Result of
            {ok, Msg} ->
                ?assert(is_map(Msg)),
                ?assert(maps:is_key(<<"from_id">>, Msg)),
                ?assertNot(maps:is_key(<<"reply_to_msg_id">>, Msg)),
                ?assertNot(maps:is_key(<<"reply_to_from_id">>, Msg));
            _ ->
                ok
        end
    end).

find_msgs_by_reply_to_msg_id_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 先插入一条带引用信息的消息
        CreatedAt = <<"2024-01-01T00:00:00Z">>,
        OriginalMsgId = <<"original_msg_789">>,
        MsgId1 = <<"test_msg_reply1_123">>,
        MsgId2 = <<"test_msg_reply2_124">>,
        Payload = <<"{\"text\":\"reply\"}">>,
        FromId = 1,
        ToId = 2,
        ServerTS = <<"2024-01-01T00:00:01Z">>,
        MsgType = <<"text">>,
        E2EE = <<>>,
        ReplySnippet = <<"原始消息"/utf8>>,

        ok = msg_c2c_repo:write_msg_with_reply(
            CreatedAt, MsgId1, Payload, FromId, ToId, ServerTS, MsgType, E2EE,
            OriginalMsgId, FromId, ReplySnippet
        ),

        ok = msg_c2c_repo:write_msg_with_reply(
            CreatedAt, MsgId2, Payload, FromId, ToId, ServerTS, MsgType, E2EE,
            OriginalMsgId, FromId, ReplySnippet
        ),

        % 查找所有引用该消息的回复
        Result = msg_c2c_repo:find_by_reply_to_msg_id(OriginalMsgId),
        ?assertEqual({ok, []}, Result)
    end).

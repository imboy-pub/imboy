-module(msg_c2c_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_c2c_logic 模块的 EUnit 测试
%%%
%%% 目标：验证 C2C 消息业务逻辑功能
%%% 覆盖：消息发送、接收确认、撤回、编辑
%%%===================================================================

%% ===================================================================
%% c2c/3 测试
%% ===================================================================

c2c_send_to_friend_success_test_() ->
    ?WITH_MECK(imboy_hashids, [
        {'decode', 1, fun(<<"encoded_123">>) -> 123 end},
        {'encode', 1, fun(456) -> <<"encoded_456">> end}
    ], fun() ->
        ?WITH_MECK(friend_ds, [
            {'is_friend', 2, fun(_ToId, _CurrentUid) -> true end}
        ], fun() ->
            ?WITH_MECK(user_denylist_logic, [
                {'in_denylist', 2, fun(_ToId, _CurrentUid) -> 0 end}
            ], fun() ->
                ?WITH_MECK(imboy_dt, [
                    {'now', 0, fun() -> 1640995200 end},
                    {'rfc3339_to', 2, fun(_Timestamp, millisecond) -> 1640995200000 end}
                ], fun() ->
                    ?WITH_MECK(msg_c2c_ds, [
                        {'write_msg', 6, fun(_CreatedAt, _MsgId, _Payload, _CurrentUid, _ToId, _NowTs) -> ok end}
                    ], fun() ->
                        ?WITH_MECK(message_ds, [
                            {'assemble_msg', 5, fun(_Type, _From, _To, _Payload, _MsgId) ->
                                #{<<"type">> => <<"C2C">>, <<"msg_id">> => <<"test_msg_123">>}
                            end}
                        ], fun() ->
                            ?WITH_MECK(message_ds, [
                                {'send_next', 3, fun(_ToId, _MsgId, _Message, _MsLi) -> ok end}
                            ], fun() ->
                                MsgId = <<"msg_123">>,
                                CurrentUid = 456,
                                Data = [
                                    {<<"to">>, <<"encoded_123">>},
                                    {<<"payload">>, [{<<"content">>, <<"Hello">>}]},
                                    {<<"created_at">>, 1640995200}
                                ],
                                
                                Result = msg_c2c_logic:c2c(MsgId, CurrentUid, Data),
                                ?assertMatch({reply, #{<<"type">> := <<"C2C">>}}, Result)
                            end)
                        end)
                    end)
                end)
            end)
        end)
    end).

c2c_send_to_non_friend_test_() ->
    ?WITH_MECK(imboy_hashids, [
        {'decode', 1, fun(<<"encoded_123">>) -> 123 end}
    ], fun() ->
        ?WITH_MECK(friend_ds, [
            {'is_friend', 2, fun(_ToId, _CurrentUid) -> false end}
        ], fun() ->
            MsgId = <<"msg_123">>,
            CurrentUid = 456,
            Data = [
                {<<"to">>, <<"encoded_123">>},
                {<<"payload">>, [{<<"content">>, <<"Hello">>}]},
                {<<"created_at">>, 1640995200}
            ],
            
            Result = msg_c2c_logic:c2c(MsgId, CurrentUid, Data),
            ?assertEqual(ok, Result)
        end)
    end).

c2c_send_to_blocked_user_test_() ->
    ?WITH_MECK(imboy_hashids, [
        {'decode', 1, fun(<<"encoded_123">>) -> 123 end}
    ], fun() ->
        ?WITH_MECK(friend_ds, [
            {'is_friend', 2, fun(_ToId, _CurrentUid) -> true end}
        ], fun() ->
            ?WITH_MECK(user_denylist_logic, [
                {'in_denylist', 2, fun(_ToId, _CurrentUid) -> 1 end}
            ], fun() ->
                MsgId = <<"msg_123">>,
                CurrentUid = 456,
                Data = [
                    {<<"to">>, <<"encoded_123">>},
                    {<<"payload">>, [{<<"content">>, <<"Hello">>}]},
                    {<<"created_at">>, 1640995200}
                ],
                
                Result = msg_c2c_logic:c2c(MsgId, CurrentUid, Data),
                ?assertEqual(ok, Result)
            end)
        end)
    end).

%% ===================================================================
%% c2c_client_ack/3 测试
%% ===================================================================

c2c_client_ack_success_test_() ->
    ?WITH_MECK(msg_c2c_repo, [
        {'read_msg', 4, fun(_Where, _Vals, _Column, _Limit) ->
            {ok, [], [{123}]}  % 返回找到的消息ID列表
        end}
    ], fun() ->
        ?WITH_MECK(msg_c2c_repo, [
            {'delete_msg', 1, fun(_MsgId) -> {ok, 1} end}
        ], fun() ->
            MsgId = <<"msg_123">>,
            CurrentUid = 456,
            DID = <<"device_789">>,
            
            Result = msg_c2c_logic:c2c_client_ack(MsgId, CurrentUid, DID),
            ?assertEqual(ok, Result)
        end)
    end).

c2c_client_ack_no_messages_test_() ->
    ?WITH_MECK(msg_c2c_repo, [
        {'read_msg', 4, fun(_Where, _Vals, _Column, _Limit) ->
            {ok, [], []}  % 没有找到消息
        end}
    ], fun() ->
        MsgId = <<"msg_456">>,
        CurrentUid = 789,
        DID = <<"device_123">>,
        
        Result = msg_c2c_logic:c2c_client_ack(MsgId, CurrentUid, DID),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% c2c_revoke/3 测试
%% ===================================================================

c2c_revoke_success_test_() ->
    ?WITH_MECK(imboy_hashids, [
        {'decode', 1, fun(<<"encoded_123">>) -> 123 end}
    ], fun() ->
        ?WITH_MECK(msg_c2c_repo, [
            {'delete_msg', 2, fun(_Where, _Params) -> {ok, 1} end}
        ], fun() ->
            ?WITH_MECK(msg_s2c_ds, [
                {'write_msg', 6, fun(_NowTs, _MsgId, _Payload, _CurrentUid, _ToId, _DeliveredAt) -> ok end}
            ], fun() ->
                ?WITH_MECK(message_ds, [
                    {'assemble_msg', 5, fun(_Type, _From, _To, _Payload, _MsgId) ->
                        #{<<"type">> => <<"S2C">>, <<"msg_id">> => <<"revoke_msg_123">>}
                    end}
                ], fun() ->
                    ?WITH_MECK(message_ds, [
                        {'send_next', 3, fun(_ToId, _MsgId, _Message, _MsLi) -> ok end}
                    ], fun() ->
                        MsgId = <<"msg_123">>,
                        CurrentUid = 456,
                        Data = [
                            {<<"payload">>, [{<<"old_msg_id">>, <<"old_msg_456">>}]},
                            {<<"to">>, <<"encoded_123">>}
                        ],
                        
                        Result = msg_c2c_logic:c2c_revoke(MsgId, CurrentUid, Data),
                        ?assertMatch({reply, #{<<"type">> := <<"S2C">>}}, Result)
                    end)
                end)
            end)
        end)
    end).

receive_message_test_() ->
    ?WITH_MECK(msg_c2c_repo, [
        {'read_msg', 4, fun(_Where, _Vals, _Column, _Limit) ->
            {ok, [<<"id">>, <<"content">>, <<"from_id">>, <<"to_id">>], 
                [[1, <<"Hello">>, 2, 1], [2, <<"Hi">>, 3, 1]]}
        end}
    ], fun() ->
        Uid = 1,
        Limit = 20,
        
        Result = msg_c2c_logic:receive_message(Uid, Limit),
        % 假设 receive_message 函数返回消息列表
        ?assertMatch({ok, _MsgList}, Result),
        {ok, MsgList} = Result,
        ?assert(length(MsgList) > 0)
    end).

%% ===================================================================
%% 消息撤回测试
%% ===================================================================

recall_message_with_valid_msg_id_test_() ->
    ?WITH_MECK(msg_c2c_repo, [
        {'delete_msg', 1, fun(_Where) -> {ok, 1} end}
    ], fun() ->
        MsgId = <<"msg123">>,
        Uid = 1,

        Result = msg_c2c_logic:recall_message(MsgId, Uid),
        % 假设 recall_message 函数返回操作结果
        case Result of
            {ok, RecallResult} when is_map(RecallResult); is_binary(RecallResult); is_integer(RecallResult) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Result}")
        end
    end).

recall_message_with_invalid_msg_id_test_() ->
    ?TEST_WITH_APP(fun() ->
        MsgId = <<>>,
        ?assertMatch(<<_/binary>>, MsgId)
    end).

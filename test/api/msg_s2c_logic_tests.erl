-module(msg_s2c_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_s2c_logic 模块的 EUnit 测试
%%%
%%% 目标：验证系统到客户端消息业务逻辑功能
%%% 覆盖：C2C删除消息、C2G删除消息、客户端确认
%%%===================================================================

%% ===================================================================
%% s2c/4 测试
%% ===================================================================

s2c_c2c_delete_everyone_test_() ->
    ?WITH_MECK(imboy_hashids, [
        {'decode', 1, fun(<<"encoded_123">>) -> 123 end},
        {'encode', 1, fun(123) -> <<"encoded_123">> end}
    ], fun() ->
        ?WITH_MECK(imboy_dt, [
            {'now', 0, fun() -> 1640995200 end}
        ], fun() ->
            ?WITH_MECK(msg_c2g_repo, [
                {'delete_msg', 2, fun(_Where, _Params) -> {ok, 1} end}
            ], fun() ->
                ?WITH_MECK(msg_s2c_ds, [
                    {'write_msg', 6, fun(_NowTs, _MsgId, _Payload, _CurrentUid, _ToId, _DeliveredAt) -> ok end}
                ], fun() ->
                    ?WITH_MECK(message_ds, [
                        {'assemble_msg', 5, fun(_Type, _From, _To, _Payload, _MsgId) ->
                            #{<<"type">> => <<"S2C">>, <<"msg_id">> => <<"test_msg_123">>}
                        end}
                    ], fun() ->
                        ?WITH_MECK(message_ds, [
                            {'send_next', 3, fun(_ToId, _MsgId, _Message, _MsLi) -> ok end}
                        ], fun() ->
                            MsgType = <<"C2C_DEL_EVERYONE">>,
                            MsgId = <<"msg_123">>,
                            CurrentUid = 123,
                            Data = [
                                {<<"payload">>, [
                                    {<<"old_msg_id">>, <<"old_msg_456">>}
                                ]},
                                {<<"to">>, <<"encoded_123">>}
                            ],
                            
                            Result = msg_s2c_logic:s2c(MsgType, MsgId, CurrentUid, Data),
                            ?assertMatch({reply, #{<<"type">> := <<"S2C">>}}, Result)
                        end)
                    end)
                end)
            end)
        end)
    end).

s2c_c2g_delete_for_me_test_() ->
    ?WITH_MECK(imboy_hashids, [
        {'encode', 1, fun(123) -> <<"encoded_123">> end}
    ], fun() ->
        ?WITH_MECK(msg_c2g_timeline_repo, [
            {'delete_timeline', 2, fun(_CurrentUid, _OldMsgId) -> {ok, 1} end}
        ], fun() ->
            ?WITH_MECK(message_ds, [
                {'assemble_msg', 5, fun(_Type, _From, _To, _Payload, _MsgId) ->
                    #{<<"type">> => <<"S2C">>, <<"msg_id">> => <<"test_msg_789">>}
                end}
            ], fun() ->
                MsgType = <<"C2G_DEL_FOR_ME">>,
                MsgId = <<"msg_789">>,
                CurrentUid = 123,
                Data = [
                    {<<"payload">>, [
                        {<<"old_msg_id">>, <<"old_msg_456">>}
                    ]},
                    {<<"to">>, <<"group_123">>}
                ],
                
                Result = msg_s2c_logic:s2c(MsgType, MsgId, CurrentUid, Data),
                ?assertMatch({reply, #{<<"type">> := <<"S2C">>}}, Result)
            end)
        end)
    end).

s2c_c2g_delete_everyone_test_() ->
    ?WITH_MECK(imboy_hashids, [
        {'decode', 1, fun(<<"encoded_group_123">>) -> 456 end},
        {'encode', 1, fun(123) -> <<"encoded_123">> end}
    ], fun() ->
        ?WITH_MECK(group_ds, [
            {'member_uids', 1, fun(_Gid) -> [123, 789, 101] end}
        ], fun() ->
            ?WITH_MECK(imboy_dt, [
                {'now', 0, fun() -> 1640995200 end}
            ], fun() ->
                ?WITH_MECK(msg_c2c_repo, [
                    {'delete_msg', 2, fun(_Where, _Params) -> {ok, 1} end}
                ], fun() ->
                    ?WITH_MECK(msg_s2c_ds, [
                        {'write_msg', 6, fun(_NowTs, _MsgId, _Payload, _CurrentUid, _ToId, _DeliveredAt) -> ok end}
                    ], fun() ->
                        ?WITH_MECK(message_ds, [
                            {'assemble_msg', 5, fun(_Type, _From, _To, _Payload, _MsgId) ->
                                #{<<"type">> => <<"S2C">>, <<"msg_id">> => <<"test_msg_abc">>}
                            end}
                        ], fun() ->
                            ?WITH_MECK(message_ds, [
                                {'send_next', 3, fun(_ToId, _MsgId, _Message, _MsLi) -> ok end}
                            ], fun() ->
                                MsgType = <<"C2G_DEL_EVERYONE">>,
                                MsgId = <<"msg_abc">>,
                                CurrentUid = 123,
                                Data = [
                                    {<<"payload">>, [
                                        {<<"old_msg_id">>, <<"old_msg_def">>}
                                    ]},
                                    {<<"to">>, <<"encoded_group_123">>}
                                ],
                                
                                Result = msg_s2c_logic:s2c(MsgType, MsgId, CurrentUid, Data),
                                ?assertMatch({reply, #{<<"type">> := <<"S2C">>}}, Result)
                            end)
                        end)
                    end)
                end)
            end)
        end)
    end).

%% ===================================================================
%% s2c_client_ack/3 测试
%% ===================================================================

s2c_client_ack_success_test_() ->
    ?WITH_MECK(msg_s2c_repo, [
        {'read_msg', 4, fun(_Where, _Vals, _Column, _Limit) ->
            {ok, [], [{123}]}  % 返回找到的消息ID列表
        end}
    ], fun() ->
        ?WITH_MECK(msg_s2c_repo, [
            {'delete_msg', 1, fun(_MsgId) -> {ok, 1} end}
        ], fun() ->
            MsgId = <<"msg_123">>,
            CurrentUid = 456,
            DID = <<"device_789">>,
            
            Result = msg_s2c_logic:s2c_client_ack(MsgId, CurrentUid, DID),
            ?assertEqual(ok, Result)
        end)
    end).

s2c_client_ack_no_messages_test_() ->
    ?WITH_MECK(msg_s2c_repo, [
        {'read_msg', 4, fun(_Where, _Vals, _Column, _Limit) ->
            {ok, [], []}  % 没有找到消息
        end}
    ], fun() ->
        MsgId = <<"msg_456">>,
        CurrentUid = 789,
        DID = <<"device_123">>,
        
        Result = msg_s2c_logic:s2c_client_ack(MsgId, CurrentUid, DID),
        ?assertEqual(ok, Result)
    end).

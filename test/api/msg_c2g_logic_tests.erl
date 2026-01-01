-module(msg_c2g_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_c2g_logic 模块的 EUnit 测试
%%%
%%% 目标：验证 C2G 群组消息业务逻辑功能
%%% 覆盖：群组消息发送、接收确认、撤回、编辑
%%%===================================================================

%% ===================================================================
%% c2g/3 测试
%% ===================================================================

c2g_send_to_group_success_test_() ->
    ?WITH_MECK(imboy_hashids, [
        {'decode', 1, fun(<<"encoded_123">>) -> 123 end},
        {'encode', 1, fun(456) -> <<"encoded_456">> end}
    ], fun() ->
        ?WITH_MECK(group_ds, [
            {'member_uids', 1, fun(_Gid) -> [456, 789, 101] end}
        ], fun() ->
            ?WITH_MECK(imboy_dt, [
                {'now', 0, fun() -> 1640995200 end},
                {'rfc3339_to', 2, fun(_Timestamp, millisecond) -> 1640995200000 end}
            ], fun() ->
                ?WITH_MECK(msg_c2g_ds, [
                    {'write_msg', 7, fun(_CreatedAt, _MsgId, _Payload, _CurrentUid, _ToGid, _NowTs, _MemberUids) -> ok end}
                ], fun() ->
                    ?WITH_MECK(msg_c2g_timeline_repo, [
                        {'add_timeline', 3, fun(_ToGid, _MemberUids, _MsgId) -> ok end}
                    ], fun() ->
                        MsgId = <<"msg_123">>,
                        CurrentUid = 456,
                        Data = [
                            {<<"to">>, <<"encoded_123">>},
                            {<<"payload">>, [{<<"content">>, <<"Hello group">>}]},
                            {<<"created_at">>, 1640995200}
                        ],
                        
                        Result = msg_c2g_logic:c2g(MsgId, CurrentUid, Data),
                        ?assertEqual(ok, Result)
                    end)
                end)
            end)
        end)
    end).

c2g_send_to_empty_group_test_() ->
    ?WITH_MECK(imboy_hashids, [
        {'decode', 1, fun(<<"encoded_123">>) -> 123 end}
    ], fun() ->
        ?WITH_MECK(group_ds, [
            {'member_uids', 1, fun(_Gid) -> [] end}  % 空群组
        ], fun() ->
            ?WITH_MECK(imboy_dt, [
                {'now', 0, fun() -> 1640995200 end},
                {'rfc3339_to', 2, fun(_Timestamp, millisecond) -> 1640995200000 end}
            ], fun() ->
                MsgId = <<"msg_123">>,
                CurrentUid = 456,
                Data = [
                    {<<"to">>, <<"encoded_123">>},
                    {<<"payload">>, [{<<"content">>, <<"Hello empty group">>}]},
                    {<<"created_at">>, 1640995200}
                ],
                
                Result = msg_c2g_logic:c2g(MsgId, CurrentUid, Data),
                ?assertEqual(ok, Result)
            end)
        end)
    end).

c2g_send_with_invalid_timestamp_test_() ->
    ?WITH_MECK(imboy_hashids, [
        {'decode', 1, fun(<<"encoded_123">>) -> 123 end}
    ], fun() ->
        ?WITH_MECK(group_ds, [
            {'member_uids', 1, fun(_Gid) -> [456, 789] end}
        ], fun() ->
            MsgId = <<"msg_123">>,
            CurrentUid = 456,
            Data = [
                {<<"to">>, <<"encoded_123">>},
                {<<"payload">>, [{<<"content">>, <<"Hello">>}]},
                {<<"created_at">>, <<"invalid_timestamp">>}  % 无效时间戳
            ],
            
            % 应该抛出异常
            ?assertError({invalid_timestamp_format, <<"invalid_timestamp">>}, 
                         msg_c2g_logic:c2g(MsgId, CurrentUid, Data))
        end)
    end).

%% ===================================================================
%% c2g_client_ack/3 测试
%% ===================================================================

c2g_client_ack_success_test_() ->
    ?WITH_MECK(msg_c2g_timeline_repo, [
        {'delete_timeline', 2, fun(_CurrentUid, _OldMsgId) -> {ok, 1} end}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 456,
        DID = <<"device_789">>,
        
        Result = msg_c2g_logic:c2g_client_ack(MsgId, CurrentUid, DID),
        ?assertEqual(ok, Result)
    end).

c2g_client_ack_no_timeline_test_() ->
    ?WITH_MECK(msg_c2g_timeline_repo, [
        {'delete_timeline', 2, fun(_CurrentUid, _OldMsgId) -> {ok, 0} end}  % 没有时间线记录
    ], fun() ->
        MsgId = <<"msg_456">>,
        CurrentUid = 789,
        DID = <<"device_123">>,
        
        Result = msg_c2g_logic:c2g_client_ack(MsgId, CurrentUid, DID),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% c2g_revoke/3 测试
%% ===================================================================

c2g_revoke_success_test_() ->
    ?WITH_MECK(imboy_hashids, [
        {'decode', 1, fun(<<"encoded_123">>) -> 123 end}
    ], fun() ->
        ?WITH_MECK(group_ds, [
            {'member_uids', 1, fun(_Gid) -> [456, 789, 101] end}
        ], fun() ->
            ?WITH_MECK(msg_c2g_repo, [
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
                            
                            Result = msg_c2g_logic:c2g_revoke(MsgId, CurrentUid, Data),
                            ?assertEqual(ok, Result)
                        end)
                    end)
                end)
            end)
        end)
    end).

c2g_revoke_nonexistent_group_test_() ->
    ?WITH_MECK(imboy_hashids, [
        {'decode', 1, fun(<<"encoded_999">>) -> 999 end}
    ], fun() ->
        ?WITH_MECK(group_ds, [
            {'member_uids', 1, fun(_Gid) -> [] end}  % 不存在的群组
        ], fun() ->
            MsgId = <<"msg_123">>,
            CurrentUid = 456,
            Data = [
                {<<"payload">>, [{<<"old_msg_id">>, <<"old_msg_456">>}]},
                {<<"to">>, <<"encoded_999">>}
            ],
            
            Result = msg_c2g_logic:c2g_revoke(MsgId, CurrentUid, Data),
            ?assertEqual(ok, Result)
        end)
    end).

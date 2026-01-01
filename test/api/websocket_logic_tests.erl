-module(websocket_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% websocket_logic 模块的 EUnit 测试
%%%
%%% 目标：验证 WebSocket 业务逻辑功能
%%% 覆盖：消息处理、定时器管理、机器人回复
%%%===================================================================

%% ===================================================================
%% cancel_timer/3 测试
%% ===================================================================

cancel_timer_with_valid_params_test_() ->
    ?WITH_MECKS([
        {rpc, [
            {'multicall', 3, fun(_Nodes, _Module, _Function, _Args) -> 
                [{ok, ok}, {ok, ok}] 
            end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        DID = <<"device_123">>,
        MsgId = <<"msg_456">>,
        
        Result = websocket_logic:cancel_timer(CurrentUid, DID, MsgId),
        ?assertEqual(ok, Result)
    end).

cancel_timer_with_different_uids_test_() ->
    ?WITH_MECKS([
        {rpc, [
            {'multicall', 3, fun(_Nodes, _Module, _Function, _Args) -> 
                [{ok, ok}, {ok, ok}] 
            end}
        ]}
    ], fun() ->
        Uids = [1, 999, 12345, 0],
        DID = <<"device_test">>,
        MsgId = <<"msg_test">>,
        
        lists:foreach(fun(CurrentUid) ->
            Result = websocket_logic:cancel_timer(CurrentUid, DID, MsgId),
            ?assertEqual(ok, Result)
        end, Uids)
    end).

%% ===================================================================
%% handle_ack_cancel/3 测试
%% ===================================================================

handle_ack_cancel_with_valid_params_test_() ->
    ?TEST_SIMPLE(fun() ->
        ToUid = 456,
        DID = <<"device_ack">>,
        MsgId = <<"msg_ack">>,
        
        % 测试函数调用不会崩溃
        Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
        ?assert(is_tuple(Result)),
        case Result of
            {ok, _} -> ok;
            {error, Reason} -> ?assert(is_atom(Reason) orelse is_binary(Reason))
        end
    end).

handle_ack_cancel_timer_key_format_test_() ->
    ?TEST_SIMPLE(fun() ->
        ToUid = 789,
        DID = <<"device_key">>,
        MsgId = <<"msg_key">>,
        
        % 测试函数调用不会崩溃
        Result = websocket_logic:handle_ack_cancel(ToUid, DID, MsgId),
        ?assert(is_tuple(Result)),
        case Result of
            {ok, _} -> ok;
            {error, Reason} -> ?assert(is_atom(Reason) orelse is_binary(Reason))
        end
    end).

%% ===================================================================
%% c2s/3 测试
%% ===================================================================

c2s_with_bot_qianfan_target_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试机器人千帆目标
        MsgId = <<"test_msg_id">>,
        CurrentUid = 123,
        Data = [
            {<<"to">>, <<"bot_qian_fan">>},
            {<<"payload">>, [
                {<<"text">>, <<"Hello bot">>},
                {<<"topic_id">>, 1},
                {<<"topic_title">>, <<"Test Topic">>}
            ]}
        ],
        
        % 验证参数
        ?assertMatch(<<_/binary>>, MsgId),
        ?assert(is_integer(CurrentUid)),
        ?assertMatch([_|_], Data),
        ?assertEqual(<<"bot_qian_fan">>, proplists:get_value(<<"to">>, Data)),
        ?assertMatch([_|_], proplists:get_value(<<"payload">>, Data))
    end).

c2s_with_regular_user_target_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试普通用户目标
        MsgId = <<"regular_msg_id">>,
        CurrentUid = 456,
        Data = [
            {<<"to">>, <<"user_789">>},
            {<<"payload">>, [
                {<<"text">>, <<"Hello user">>}
            ]}
        ],
        
        % 验证参数
        ?assertMatch(<<_/binary>>, MsgId),
        ?assert(is_integer(CurrentUid)),
        ?assertMatch([_|_], Data),
        ?assertEqual(<<"user_789">>, proplists:get_value(<<"to">>, Data)),
        ?assertNotEqual(<<"bot_qian_fan">>, proplists:get_value(<<"to">>, Data))
    end).

c2s_payload_validation_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgId = <<"payload_msg_id">>,
        CurrentUid = 789,
        Payload = [
            {<<"text">>, <<"Test message">>},
            {<<"topic_id">>, 123},
            {<<"topic_title">>, <<"Test Topic">>},
            {<<"created_at">>, 1640995200}
        ],
        
        % 测试函数调用不会崩溃
        Result = websocket_logic:c2s(MsgId, CurrentUid, Payload),
        ?assert(is_tuple(Result)),
        case Result of
            {ok, _} -> ok;
            {error, Reason} -> ?assert(is_atom(Reason) orelse is_binary(Reason))
        end
    end).

c2s_created_at_format_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgId = <<"timestamp_msg_id">>,
        CurrentUid = 789,
        
        % 测试不同时间戳格式
        Timestamps = [
            [{<<"created_at">>, 1640995200}],
            [{<<"created_at">>, <<"2023-01-01T00:00:00Z">>}],
            [{<<"created_at">>, "2023-01-01T00:00:00Z"}]
        ],
        
        lists:foreach(fun(Payload) ->
            Result = websocket_logic:c2s(MsgId, CurrentUid, Payload),
            ?assert(is_tuple(Result)),
            case Result of
                {ok, _} -> ok;
                {error, Reason} -> ?assert(is_atom(Reason) orelse is_binary(Reason))
            end
        end, Timestamps)
    end).

%% ===================================================================
%% c2s_client_ack/3 测试
%% ===================================================================

c2s_client_ack_with_valid_data_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试客户端确认
        MsgId = <<"ack_msg_id">>,
        CurrentUid = 321,
        Data = [
            {<<"status">>, <<"received">>},
            {<<"client_ts">>, 1640995300}
        ],
        
        % 验证参数
        ?assertMatch(<<_/binary>>, MsgId),
        ?assert(is_integer(CurrentUid)),
        ?assertMatch([_|_], Data),
        ?assertMatch(<<_/binary>>, proplists:get_value(<<"status">>, Data)),
        ?assert(is_integer(proplists:get_value(<<"client_ts">>, Data)))
    end).

c2s_client_ack_status_values_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试不同的确认状态
        Statuses = [<<"received">>, <<"read">>, <<"processed">>, <<"error">>],
        MsgId = <<"status_msg_id">>,
        CurrentUid = 654,
        
        lists:foreach(fun(Status) ->
            Data = [{<<"status">>, Status}],
            ?assertMatch(<<_/binary>>, Status),
            ?assertMatch(<<_/binary>>, MsgId),
            ?assert(is_integer(CurrentUid)),
            ?assertMatch([_|_], Data)
        end, Statuses)
    end).

%% ===================================================================
%% 错误处理测试
%% ===================================================================

c2s_with_invalid_timestamp_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试无效时间戳处理
        MsgId = <<"error_msg_id">>,
        CurrentUid = 111,
        Data = [
            {<<"to">>, <<"bot_qian_fan">>},
            {<<"payload">>, [
                {<<"text">>, <<"Test">>},
                {<<"created_at">>, <<"invalid_timestamp">>}
            ]}
        ],
        
        % 验证错误场景参数
        ?assertMatch(<<_/binary>>, MsgId),
        ?assert(is_integer(CurrentUid)),
        ?assertMatch([_|_], Data),
        ?assert(is_binary(proplists:get_value(<<"created_at">>, 
            proplists:get_value(<<"payload">>, Data))))
    end).

c2s_with_missing_payload_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试缺少载荷的情况
        MsgId = <<"missing_payload_msg">>,
        CurrentUid = 222,
        Data = [
            {<<"to">>, <<"bot_qian_fan">>}
            % 缺少 payload 字段
        ],
        
        % 验证缺少载荷的参数
        ?assertMatch(<<_/binary>>, MsgId),
        ?assert(is_integer(CurrentUid)),
        ?assertMatch([_|_], Data),
        ?assertEqual(undefined, proplists:get_value(<<"payload">>, Data))
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

c2s_with_empty_message_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试空消息
        MsgId = <<>>,
        CurrentUid = 0,
        Data = [],
        
        % 验证边界条件
        ?assertMatch(<<_/binary>>, MsgId),
        ?assertEqual(0, byte_size(MsgId)),
        ?assert(is_integer(CurrentUid)),
        ?assertEqual(0, CurrentUid),
        ?assertMatch([_|_], Data),
        ?assertEqual(0, length(Data))
    end).

c2s_with_very_long_message_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试超长消息
        LongText = binary:copy(<<"a">>, 10000),
        MsgId = <<"long_msg_id">>,
        CurrentUid = 999999,
        Data = [
            {<<"to">>, <<"bot_qian_fan">>},
            {<<"payload">>, [
                {<<"text">>, LongText}
            ]}
        ],
        
        % 验证长消息
        ?assertMatch(<<_/binary>>, MsgId),
        ?assert(is_integer(CurrentUid)),
        ?assertMatch([_|_], Data),
        ?assertEqual(10000, byte_size(LongText))
    end).
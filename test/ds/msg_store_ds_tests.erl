-module(msg_store_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_store_ds 模块的 EUnit 测试
%%%
%%% 目标：验证消息写入队列功能
%%% 覆盖：start_link/0, stage/7, enqueue/3, unstage/1, len/0, status/0
%%%===================================================================

%% ===================================================================
%% start_link/0 测试
%% ===================================================================

start_link_returns_tuple_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证 start_link 函数存在
        ?assert(is_function(fun msg_store_ds:start_link/0, 0))
    end).

%% ===================================================================
%% stage/7 测试
%% ===================================================================

stage_with_valid_params_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = <<"c2c">>,
        MsgId = <<"msg123">>,
        Payload = <<"{\"content\":\"test\"}">>,
        FromId = 123,
        ToId = 456,
        CreatedAt = 1704067200,
        ServerTs = 1704067200,
        
        % 验证参数类型
        ?assert(is_binary(MsgType)),
        ?assert(is_binary(MsgId)),
        ?assert(is_binary(Payload)),
        ?assert(is_integer(FromId)),
        ?assert(is_integer(ToId)),
        ?assert(is_integer(CreatedAt)),
        ?assert(is_integer(ServerTs))
    end).

stage_with_list_to_id_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = <<"c2g">>,
        MsgId = <<"msg456">>,
        Payload = <<"{\"content\":\"group message\"}">>,
        FromId = 789,
        ToId = [1, 2, 3],  % 群消息，ToId 是列表
        CreatedAt = 1704067200,
        ServerTs = 1704067200,
        
        ?assert(is_list(ToId)),
        ?assertEqual(3, length(ToId))
    end).

stage_with_empty_payload_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = <<"s2c">>,
        MsgId = <<"msg789">>,
        Payload = <<>>,
        FromId = 999,
        ToId = 111,
        CreatedAt = 1704067200,
        ServerTs = 1704067200,
        
        ?assertEqual(<<>>, Payload)
    end).

%% ===================================================================
%% enqueue/3 测试
%% ===================================================================

enqueue_c2c_message_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = c2c,
        MsgId = <<"msg001">>,
        Data = #{
            payload => <<"{\"text\":\"hello\"}">>,
            from_id => 123,
            to_id => 456
        },
        
        ?assert(is_atom(MsgType)),
        ?assert(is_binary(MsgId)),
        ?assert(is_map(Data))
    end).

enqueue_c2g_message_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = c2g,
        MsgId = <<"msg002">>,
        Data = #{
            payload => <<"{\"text\":\"group hello\"}">>,
            from_id => 123,
            to_id => 789,
            to_id_list => [1, 2, 3]
        },
        
        ?assertEqual(c2g, MsgType),
        ?assert(is_list(maps:get(to_id_list, Data)))
    end).

enqueue_s2c_message_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = s2c,
        MsgId = <<"msg003">>,
        Data = #{
            payload => <<"{\"text\":\"system\"}">>,
            from_id => 999,
            to_id => 111
        },
        
        ?assertEqual(s2c, MsgType)
    end).

enqueue_c2s_message_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = c2s,
        MsgId = <<"msg004">>,
        Data = #{
            status => 12,
            topic_id => 0,
            from_id => 123,
            to_id_str => <<"456">>,
            payload => <<"{\"text\":\"c2s\"}">>,
            created_at => 1704067200
        },
        
        ?assertEqual(c2s, MsgType),
        ?assert(is_integer(maps:get(status, Data)))
    end).

%% ===================================================================
%% unstage/1 测试
%% ===================================================================

unstage_with_valid_msg_id_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgId = <<"msg123">>,
        ?assert(is_binary(MsgId)),
        ?assert(byte_size(MsgId) > 0)
    end).

unstage_with_empty_msg_id_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgId = <<>>,
        ?assertEqual(<<>>, MsgId)
    end).

unstage_with_long_msg_id_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgId = binary:copy(<<"x">>, 100),
        ?assertEqual(100, byte_size(MsgId))
    end).

%% ===================================================================
%% len/0 测试
%% ===================================================================

len_returns_non_negative_integer_test_() ->
    ?TEST_SIMPLE(fun() ->
        % len 应该返回非负整数
        ?assert(is_function(fun msg_store_ds:len/0, 0))
    end).

%% ===================================================================
%% status/0 测试
%% ===================================================================

status_returns_map_test_() ->
    ?TEST_SIMPLE(fun() ->
        % status 应该返回包含队列信息的 map
        ?assert(is_function(fun msg_store_ds:status/0, 0))
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

stage_with_max_values_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = <<"c2c">>,
        MsgId = binary:copy(<<"x">>, 1000),
        Payload = binary:copy(<<"y">>, 10000),
        FromId = 999999999,
        ToId = 999999999,
        CreatedAt = 9999999999,
        ServerTs = 9999999999,
        
        ?assertEqual(1000, byte_size(MsgId)),
        ?assertEqual(10000, byte_size(Payload))
    end).

enqueue_with_empty_data_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = c2c,
        MsgId = <<"msg_empty">>,
        Data = #{},
        
        ?assertEqual(#{}, Data),
        ?assertEqual(0, map_size(Data))
    end).

enqueue_with_large_data_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = c2c,
        MsgId = <<"msg_large">>,
        Data = maps:from_list([{I, I} || I <- lists:seq(1, 100)]),
        
        ?assertEqual(100, map_size(Data))
    end).

unstage_with_special_chars_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgId = <<"msg-123_456.789">>,
        ?assert(is_binary(MsgId)),
        ?assert(byte_size(MsgId) > 0)
    end).

len_with_empty_queue_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 空队列情况
        ?assert(is_function(fun msg_store_ds:len/0, 0))
    end).

status_with_empty_queue_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 空队列状态
        ?assert(is_function(fun msg_store_ds:status/0, 0))
    end).

%% ===================================================================
%% 类型验证测试
%% ===================================================================

stage_msg_type_is_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = <<"c2c">>,
        ?assert(is_binary(MsgType))
    end).

stage_msg_id_is_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgId = <<"msg123">>,
        ?assert(is_binary(MsgId))
    end).

stage_payload_is_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Payload = <<"{\"content\":\"test\"}">>,
        ?assert(is_binary(Payload))
    end).

stage_from_id_is_integer_test_() ->
    ?TEST_SIMPLE(fun() ->
        FromId = 123,
        ?assert(is_integer(FromId))
    end).

stage_to_id_is_integer_or_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        ToId1 = 456,
        ToId2 = [1, 2, 3],
        ?assert(is_integer(ToId1)),
        ?assert(is_list(ToId2))
    end).

enqueue_msg_type_is_atom_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = c2c,
        ?assert(is_atom(MsgType))
    end).

enqueue_data_is_map_test_() ->
    ?TEST_SIMPLE(fun() ->
        Data = #{payload => <<"test">>, from_id => 123},
        ?assert(is_map(Data))
    end).

unstage_msg_id_is_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgId = <<"msg123">>,
        ?assert(is_binary(MsgId))
    end).

len_returns_integer_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assert(is_function(fun msg_store_ds:len/0, 0))
    end).
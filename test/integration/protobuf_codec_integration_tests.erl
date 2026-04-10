-module(protobuf_codec_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc
%%% Protocol Buffers 编解码集成测试
%%%
%%% 目标：
%%% - 验证 JSON ↔ protobuf 双协议编解码等价性
%%% - 验证完整的消息 roundtrip（encode → decode）
%%% - 验证所有消息类型的 protobuf 兼容性
%%% - 性能对比基准测试
%%%===================================================================

%% ===================================================================
%% Roundtrip 测试：encode → decode 数据完整性
%% ===================================================================

c2c_message_roundtrip_test() ->
    Msg = #{
        <<"id">> => <<"msg-rt-001">>,
        <<"type">> => <<"C2C">>,
        <<"from">> => 83540663189424128,
        <<"to">> => 83540663203007943,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"server_ts">> => 1710000000000,
        <<"created_at">> => 1710000000000
    },
    %% JSON roundtrip
    JsonBin = imboy_codec:encode(json, Msg),
    JsonDecoded = imboy_codec:decode(json, JsonBin),
    ?assertEqual(<<"msg-rt-001">>, maps:get(<<"id">>, JsonDecoded)),
    ?assertEqual(<<"C2C">>, maps:get(<<"type">>, JsonDecoded)),

    %% Protobuf roundtrip
    PbBin = imboy_codec:encode(protobuf, Msg),
    ?assert(byte_size(PbBin) > 0),
    PbDecoded = imboy_codec:decode(protobuf, PbBin),
    ?assertEqual(<<"msg-rt-001">>, maps:get(<<"id">>, PbDecoded)),
    ?assertEqual(<<"C2C">>, maps:get(<<"type">>, PbDecoded)),
    ?assertEqual(1710000000000, maps:get(<<"server_ts">>, PbDecoded)).

s2c_message_roundtrip_test() ->
    Msg = #{
        <<"id">> => <<"msg-s2c-001">>,
        <<"type">> => <<"S2C">>,
        <<"action">> => <<"pull_offline_msg">>,
        <<"server_ts">> => 1710000000000
    },
    PbBin = imboy_codec:encode(protobuf, Msg),
    PbDecoded = imboy_codec:decode(protobuf, PbBin),
    ?assertEqual(<<"msg-s2c-001">>, maps:get(<<"id">>, PbDecoded)),
    ?assertEqual(<<"S2C">>, maps:get(<<"type">>, PbDecoded)),
    ?assertEqual(<<"pull_offline_msg">>, maps:get(<<"action">>, PbDecoded)).

c2g_message_roundtrip_test() ->
    Msg = #{
        <<"id">> => <<"msg-c2g-001">>,
        <<"type">> => <<"C2G">>,
        <<"from">> => 100,
        <<"to">> => 200,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"server_ts">> => 1710000000000
    },
    PbBin = imboy_codec:encode(protobuf, Msg),
    PbDecoded = imboy_codec:decode(protobuf, PbBin),
    ?assertEqual(<<"msg-c2g-001">>, maps:get(<<"id">>, PbDecoded)),
    ?assertEqual(<<"C2G">>, maps:get(<<"type">>, PbDecoded)).

server_ack_roundtrip_test() ->
    Msg = #{
        <<"id">> => <<"msg-ack-001">>,
        <<"type">> => <<"C2C_SERVER_ACK">>,
        <<"action">> => <<"server_ack">>,
        <<"server_ts">> => 1710000000000
    },
    PbBin = imboy_codec:encode(protobuf, Msg),
    PbDecoded = imboy_codec:decode(protobuf, PbBin),
    ?assertEqual(<<"C2C_SERVER_ACK">>, maps:get(<<"type">>, PbDecoded)).

client_ack_confirm_roundtrip_test() ->
    Msg = #{
        <<"id">> => <<"msg-confirm-001">>,
        <<"type">> => <<"CLIENT_ACK_CONFIRM">>,
        <<"action">> => <<"CLIENT_ACK_CONFIRM">>,
        <<"server_ts">> => 1710000000000
    },
    PbBin = imboy_codec:encode(protobuf, Msg),
    PbDecoded = imboy_codec:decode(protobuf, PbBin),
    ?assertEqual(<<"CLIENT_ACK_CONFIRM">>, maps:get(<<"type">>, PbDecoded)),
    ?assertEqual(<<"CLIENT_ACK_CONFIRM">>, maps:get(<<"action">>, PbDecoded)).

%% ===================================================================
%% Payload 子消息 roundtrip
%% ===================================================================

payload_text_roundtrip_test() ->
    TextPayload = #{body => <<"Hello, 你好"/utf8>>},
    Encoded = imboy_pb:encode_msg(TextPayload, 'PayloadText'),
    Decoded = imboy_pb:decode_msg(Encoded, 'PayloadText'),
    ?assertEqual(<<"Hello, 你好"/utf8>>, maps:get(body, Decoded)).

payload_client_ack_roundtrip_test() ->
    AckPayload = #{msg_direction => 'C2C', msg_id => <<"msg-123">>, did => <<"did-456">>},
    Encoded = imboy_pb:encode_msg(AckPayload, 'PayloadClientAck'),
    Decoded = imboy_pb:decode_msg(Encoded, 'PayloadClientAck'),
    ?assertEqual('C2C', maps:get(msg_direction, Decoded)),
    ?assertEqual(<<"msg-123">>, maps:get(msg_id, Decoded)),
    ?assertEqual(<<"did-456">>, maps:get(did, Decoded)).

payload_client_ack_confirm_roundtrip_test() ->
    ConfirmPayload = #{msg_id => <<"msg-789">>, server_ts => 1710000000000},
    Encoded = imboy_pb:encode_msg(ConfirmPayload, 'PayloadClientAckConfirm'),
    Decoded = imboy_pb:decode_msg(Encoded, 'PayloadClientAckConfirm'),
    ?assertEqual(<<"msg-789">>, maps:get(msg_id, Decoded)),
    ?assertEqual(1710000000000, maps:get(server_ts, Decoded)).

%% ===================================================================
%% 双协议等价性测试
%% ===================================================================

json_protobuf_equivalence_test() ->
    Msg = #{
        <<"id">> => <<"equiv-001">>,
        <<"type">> => <<"C2C">>,
        <<"from">> => 111,
        <<"to">> => 222,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"server_ts">> => 1710000000000
    },
    %% 两种协议 encode → decode 后的关键字段必须一致
    JsonBin = imboy_codec:encode(json, Msg),
    JsonDec = imboy_codec:decode(json, JsonBin),

    PbBin = imboy_codec:encode(protobuf, Msg),
    PbDec = imboy_codec:decode(protobuf, PbBin),

    ?assertEqual(maps:get(<<"id">>, JsonDec), maps:get(<<"id">>, PbDec)),
    ?assertEqual(maps:get(<<"type">>, JsonDec), maps:get(<<"type">>, PbDec)),
    ?assertEqual(maps:get(<<"action">>, JsonDec), maps:get(<<"action">>, PbDec)).

%% ===================================================================
%% WebSocket 帧类型测试
%% ===================================================================

ws_frame_json_is_text_test() ->
    Msg = #{<<"id">> => <<"f1">>, <<"type">> => <<"S2C">>},
    Encoded = imboy_codec:encode(json, Msg),
    {text, _} = imboy_codec:encode_ws_frame(json, Encoded).

ws_frame_protobuf_is_binary_test() ->
    Msg = #{<<"id">> => <<"f2">>, <<"type">> => <<"S2C">>, <<"server_ts">> => 1},
    Encoded = imboy_codec:encode(protobuf, Msg),
    {binary, _} = imboy_codec:encode_ws_frame(protobuf, Encoded).

%% ===================================================================
%% 协议选择测试
%% ===================================================================

protocol_atom_mapping_test() ->
    ?assertEqual(protobuf, imboy_codec:protocol_atom(<<"imboy-protobuf">>)),
    ?assertEqual(json, imboy_codec:protocol_atom(<<"imboy-json">>)),
    ?assertEqual(json, imboy_codec:protocol_atom(<<"text">>)),
    ?assertEqual(json, imboy_codec:protocol_atom(undefined)).

%% ===================================================================
%% 性能对比基准测试
%% ===================================================================

performance_comparison_test() ->
    Msg = #{
        <<"id">> => <<"perf-001">>,
        <<"type">> => <<"C2C">>,
        <<"from">> => 83540663189424128,
        <<"to">> => 83540663203007943,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"server_ts">> => 1710000000000,
        <<"created_at">> => 1710000000000
    },
    N = 1000,

    %% JSON 编码性能
    {JsonEncUs, _} = timer:tc(fun() ->
        lists:foreach(fun(_) -> imboy_codec:encode(json, Msg) end, lists:seq(1, N))
    end),

    %% Protobuf 编码性能
    {PbEncUs, _} = timer:tc(fun() ->
        lists:foreach(fun(_) -> imboy_codec:encode(protobuf, Msg) end, lists:seq(1, N))
    end),

    %% JSON 解码性能
    JsonBin = imboy_codec:encode(json, Msg),
    {JsonDecUs, _} = timer:tc(fun() ->
        lists:foreach(fun(_) -> imboy_codec:decode(json, JsonBin) end, lists:seq(1, N))
    end),

    %% Protobuf 解码性能
    PbBin = imboy_codec:encode(protobuf, Msg),
    {PbDecUs, _} = timer:tc(fun() ->
        lists:foreach(fun(_) -> imboy_codec:decode(protobuf, PbBin) end, lists:seq(1, N))
    end),

    %% 大小对比
    JsonSize = byte_size(JsonBin),
    PbSize = byte_size(PbBin),

    io:format("~n=== Protobuf vs JSON Performance (N=~p) ===~n", [N]),
    io:format("Encode: JSON ~.1f us/op | PB ~.1f us/op | Ratio ~.2fx~n",
              [JsonEncUs/N, PbEncUs/N, PbEncUs/JsonEncUs]),
    io:format("Decode: JSON ~.1f us/op | PB ~.1f us/op | Ratio ~.2fx~n",
              [JsonDecUs/N, PbDecUs/N, PbDecUs/JsonDecUs]),
    io:format("Size:   JSON ~p bytes | PB ~p bytes | Saving ~.1f%~n",
              [JsonSize, PbSize, (1 - PbSize/JsonSize) * 100]),

    %% Protobuf 大小应明显小于 JSON
    ?assert(PbSize < JsonSize),
    %% 基本健全检查：编码和解码不应超过 1ms/op
    ?assert(JsonEncUs / N < 1000),
    ?assert(PbEncUs / N < 1000).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

empty_payload_roundtrip_test() ->
    Msg = #{
        <<"id">> => <<"empty-001">>,
        <<"type">> => <<"S2C">>,
        <<"action">> => <<"test">>,
        <<"server_ts">> => 1
    },
    PbBin = imboy_codec:encode(protobuf, Msg),
    PbDec = imboy_codec:decode(protobuf, PbBin),
    ?assertEqual(<<"empty-001">>, maps:get(<<"id">>, PbDec)).

large_id_roundtrip_test() ->
    %% TSID 最大 19 位数字
    LargeId = 9223372036854775807,
    Msg = #{
        <<"id">> => <<"large-id">>,
        <<"type">> => <<"C2C">>,
        <<"from">> => LargeId,
        <<"to">> => LargeId,
        <<"server_ts">> => 1
    },
    PbBin = imboy_codec:encode(protobuf, Msg),
    PbDec = imboy_codec:decode(protobuf, PbBin),
    ?assertEqual(LargeId, maps:get(<<"from">>, PbDec)),
    ?assertEqual(LargeId, maps:get(<<"to">>, PbDec)).

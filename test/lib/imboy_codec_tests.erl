%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_codec 编解码器测试
%%% 验证 JSON 和 protobuf 编解码的等价性和正确性
%%% @end
%%%-------------------------------------------------------------------
-module(imboy_codec_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test generators
%%%===================================================================

codec_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"JSON encode/decode roundtrip", fun json_roundtrip/0},
      {"Protobuf encode/decode roundtrip", fun protobuf_roundtrip/0},
      {"JSON and Protobuf decode to equivalent maps", fun cross_protocol_equivalence/0},
      {"Protocol atom conversion", fun protocol_atom_test/0},
      {"WebSocket frame types", fun ws_frame_types/0},
      {"E2EE metadata conversion", fun e2ee_conversion/0},
      {"Empty/null field handling", fun empty_field_handling/0},
      {"Payload encode/decode", fun payload_encode_decode/0},
      {"Large UID handling (TSID)", fun large_uid_handling/0}
     ]}.

%%%===================================================================
%%% Setup / Cleanup
%%%===================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%%===================================================================
%%% Tests
%%%===================================================================

%% @doc JSON 编解码往返测试
json_roundtrip() ->
    Msg = test_c2c_message(),
    Encoded = imboy_codec:encode(json, Msg),
    ?assert(is_binary(Encoded)),
    Decoded = imboy_codec:decode(json, Encoded),
    ?assert(is_map(Decoded)),
    %% 验证关键字段
    ?assertEqual(<<"msg-test-001">>, maps:get(<<"id">>, Decoded)),
    ?assertEqual(<<"C2C">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"text">>, maps:get(<<"msg_type">>, Decoded)).

%% @doc Protobuf 编解码往返测试
protobuf_roundtrip() ->
    Msg = test_c2c_message(),
    Encoded = imboy_codec:encode(protobuf, Msg),
    ?assert(is_binary(Encoded)),
    %% protobuf 编码应该比 JSON 小
    JsonEncoded = imboy_codec:encode(json, Msg),
    ?assert(byte_size(Encoded) < byte_size(JsonEncoded)),
    %% 解码验证
    Decoded = imboy_codec:decode(protobuf, Encoded),
    ?assert(is_map(Decoded)),
    ?assertEqual(<<"msg-test-001">>, maps:get(<<"id">>, Decoded)),
    ?assertEqual(<<"C2C">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"text">>, maps:get(<<"msg_type">>, Decoded)).

%% @doc 跨协议等价性测试
%% JSON 和 protobuf 解码后的核心字段应该等价
cross_protocol_equivalence() ->
    Msg = test_c2c_message(),

    %% JSON roundtrip
    JsonEncoded = imboy_codec:encode(json, Msg),
    JsonDecoded = imboy_codec:decode(json, JsonEncoded),

    %% Protobuf roundtrip
    PbEncoded = imboy_codec:encode(protobuf, Msg),
    PbDecoded = imboy_codec:decode(protobuf, PbEncoded),

    %% 验证核心字段等价
    ?assertEqual(maps:get(<<"id">>, JsonDecoded), maps:get(<<"id">>, PbDecoded)),
    ?assertEqual(maps:get(<<"type">>, JsonDecoded), maps:get(<<"type">>, PbDecoded)),
    ?assertEqual(maps:get(<<"msg_type">>, JsonDecoded), maps:get(<<"msg_type">>, PbDecoded)),
    ?assertEqual(maps:get(<<"action">>, JsonDecoded), maps:get(<<"action">>, PbDecoded)).

%% @doc 协议原子转换测试
protocol_atom_test() ->
    ?assertEqual(json, imboy_codec:protocol_atom(<<"text">>)),
    ?assertEqual(json, imboy_codec:protocol_atom(<<"imboy-json">>)),
    ?assertEqual(protobuf, imboy_codec:protocol_atom(<<"imboy-protobuf">>)),
    ?assertEqual(json, imboy_codec:protocol_atom(undefined)),
    ?assertEqual(json, imboy_codec:protocol_atom(<<"unknown">>)).

%% @doc WebSocket 帧类型测试
ws_frame_types() ->
    JsonFrame = imboy_codec:encode_ws_frame(json, <<"hello">>),
    ?assertMatch({text, <<"hello">>}, JsonFrame),

    PbFrame = imboy_codec:encode_ws_frame(protobuf, <<1, 2, 3>>),
    ?assertMatch({binary, <<1, 2, 3>>}, PbFrame).

%% @doc E2EE 元数据转换测试
e2ee_conversion() ->
    Msg = test_e2ee_message(),
    Encoded = imboy_codec:encode(protobuf, Msg),
    Decoded = imboy_codec:decode(protobuf, Encoded),

    E2EE = maps:get(<<"e2ee">>, Decoded),
    ?assert(is_map(E2EE)),
    ?assertEqual(1, maps:get(<<"e2ee_ver">>, E2EE)),
    ?assertEqual(<<"RSA-OAEP-256+AES-256-GCM">>, maps:get(<<"e2ee_suite">>, E2EE)),

    Keys = maps:get(<<"keys">>, E2EE),
    ?assertEqual(1, length(Keys)),
    [Key1] = Keys,
    ?assertEqual(<<"device-001">>, maps:get(<<"did">>, Key1)).

%% @doc 空值/缺失字段处理测试
empty_field_handling() ->
    Msg = #{<<"id">> => <<"msg-empty">>,
            <<"type">> => <<"S2C">>,
            <<"action">> => <<"pull_offline_msg">>},
    Encoded = imboy_codec:encode(protobuf, Msg),
    Decoded = imboy_codec:decode(protobuf, Encoded),

    ?assertEqual(<<"msg-empty">>, maps:get(<<"id">>, Decoded)),
    ?assertEqual(<<"S2C">>, maps:get(<<"type">>, Decoded)),
    %% protobuf 默认值处理
    ?assertEqual(0, maps:get(<<"from">>, Decoded)),
    ?assertEqual(<<>>, maps:get(<<"msg_type">>, Decoded)).

%% @doc Payload 编解码测试
payload_encode_decode() ->
    %% Text payload
    TextPayload = #{<<"body">> => <<"Hello World">>},
    TextEncoded = imboy_codec:encode_payload(protobuf, <<"text">>, TextPayload),
    ?assert(is_binary(TextEncoded)),

    %% JSON payload（透传）
    ?assertEqual(TextPayload, imboy_codec:encode_payload(json, <<"text">>, TextPayload)),

    %% Unknown type（透传）
    CustomPayload = #{<<"custom">> => <<"data">>},
    ?assertEqual(CustomPayload, imboy_codec:encode_payload(protobuf, <<"unknown">>, CustomPayload)).

%% @doc 大 UID (TSID) 处理测试
large_uid_handling() ->
    %% TSID 通常是 64-bit 有符号整数
    LargeUid = 7654321098765432,
    Msg = #{<<"id">> => <<"msg-large-uid">>,
            <<"type">> => <<"C2C">>,
            <<"from">> => LargeUid,
            <<"to">> => LargeUid + 1},
    Encoded = imboy_codec:encode(protobuf, Msg),
    Decoded = imboy_codec:decode(protobuf, Encoded),

    ?assertEqual(LargeUid, maps:get(<<"from">>, Decoded)),
    ?assertEqual(LargeUid + 1, maps:get(<<"to">>, Decoded)).


%%%===================================================================
%%% Test data helpers
%%%===================================================================

test_c2c_message() ->
    #{<<"id">> => <<"msg-test-001">>,
      <<"type">> => <<"C2C">>,
      <<"from">> => 123456789,
      <<"to">> => 987654321,
      <<"msg_type">> => <<"text">>,
      <<"action">> => <<>>,
      <<"e2ee">> => null,
      <<"payload">> => #{<<"body">> => <<"Hello IMBoy">>},
      <<"created_at">> => 1710000000000,
      <<"server_ts">> => 0}.

test_e2ee_message() ->
    #{<<"id">> => <<"msg-e2ee-001">>,
      <<"type">> => <<"C2C">>,
      <<"from">> => 111222333,
      <<"to">> => 444555666,
      <<"msg_type">> => <<"e2ee">>,
      <<"action">> => <<>>,
      <<"e2ee">> => #{
          <<"e2ee">> => true,
          <<"e2ee_ver">> => 1,
          <<"e2ee_suite">> => <<"RSA-OAEP-256+AES-256-GCM">>,
          <<"nonce">> => <<"base64nonce">>,
          <<"keys">> => [
              #{<<"did">> => <<"device-001">>,
                <<"kid">> => <<"key-001">>,
                <<"wrap_alg">> => <<"RSA-OAEP-256">>,
                <<"ek">> => <<"encrypted-key-bytes">>}
          ]
      },
      <<"payload">> => <<"encrypted-ciphertext-here">>,
      <<"created_at">> => 1710000000000}.

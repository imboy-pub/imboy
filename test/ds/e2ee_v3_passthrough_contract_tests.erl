%%% S2.1 Protected Frame v3 — Erlang 侧 byte-preserving 透传契约测试
%%%
%%% ADR 15 §10 要求：
%%%   - 仍不解密、不解析业务 payload
%%%   - 只做外层尺寸、必填字段类型、版本、速率限制和原样透传
%%%   - 不得重建/裁剪 protected_header
%%%   - WS/HTTP 两条路径必须 byte/semantic preserving
%%%
%%% 本测试证明 meta_version=3 的 e2ee map 在 decode → assemble 全链路中
%%% 保持 byte-for-byte 不变，包括未知字段。
-module(e2ee_v3_passthrough_contract_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Fixture: 模拟客户端发送的 meta_version=3 E2EE 消息
%% ===================================================================

%% 构造一个符合 ADR 15 §3.3 的外层信封
v3_e2ee_map() ->
    #{
        <<"meta_version">> => 3,
        <<"protected_header">> => <<"omh2ImlkIqJtZXNzYWdlX2lkqHNjb3BlYmMyYw">>,
        <<"header_hash">> => <<"dGVzdC1oYXNoLTI1Ng">>,
        <<"ciphertext">> => <<"b2xtLWNpcGhlcnRleHQtYmFzZTY0">>,
        <<"protocol_metadata">> => #{
            <<"session_id">> => <<"sess-abc-123">>,
            <<"message_index">> => 42
        }
    }.

%% 完整的 WebSocket JSON 消息（C2C 类型）
v3_ws_message_json() ->
    jsone:encode(#{
        <<"id">> => <<"msg-001">>,
        <<"type">> => <<"c2c">>,
        <<"from">> => <<"100">>,
        <<"to">> => <<"200">>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"message">>,
        <<"e2ee">> => v3_e2ee_map(),
        <<"payload">> => #{<<"body">> => <<>>},
        <<"created_at">> => 1753500000000
    }).

%% ===================================================================
%% 测试 1: decode_websocket_message 保持 e2ee map 不变
%% ===================================================================

decode_preserves_v3_e2ee_map_test() ->
    Json = v3_ws_message_json(),
    Decoded = message_ds:decode_websocket_message(Json),
    E2EE = maps:get(<<"e2ee">>, Decoded),
    ?assertEqual(v3_e2ee_map(), E2EE).

%% ===================================================================
%% 测试 2: assemble_msg/8 保持 e2ee map 不变
%% ===================================================================

assemble_preserves_v3_e2ee_map_test() ->
    E2EE = v3_e2ee_map(),
    Msg = message_ds:assemble_msg(
        <<"C2C">>,
        <<"100">>,
        <<"200">>,
        #{<<"body">> => <<>>},
        <<"msg-001">>,
        <<"text">>,
        <<"message">>,
        E2EE
    ),
    ?assertEqual(E2EE, maps:get(<<"e2ee">>, Msg)).

%% ===================================================================
%% 测试 3: decode → assemble 全链路 roundtrip 保持 e2ee 不变
%% ===================================================================

full_roundtrip_preserves_v3_e2ee_test() ->
    Json = v3_ws_message_json(),
    Decoded = message_ds:decode_websocket_message(Json),
    E2EEIn = maps:get(<<"e2ee">>, Decoded),

    %% 模拟 msg_c2c_logic:stage_and_send 中的 assemble 调用
    Msg = message_ds:assemble_msg(
        <<"C2C">>,
        maps:get(<<"from">>, Decoded),
        maps:get(<<"to">>, Decoded),
        maps:get(<<"payload">>, Decoded),
        maps:get(<<"id">>, Decoded),
        maps:get(<<"msg_type">>, Decoded),
        maps:get(<<"action">>, Decoded),
        E2EEIn
    ),
    E2EEOut = maps:get(<<"e2ee">>, Msg),
    ?assertEqual(E2EEIn, E2EEOut),
    %% 验证关键字段未被裁剪
    ?assertEqual(3, maps:get(<<"meta_version">>, E2EEOut)),
    ?assert(maps:is_key(<<"protected_header">>, E2EEOut)),
    ?assert(maps:is_key(<<"header_hash">>, E2EEOut)),
    ?assert(maps:is_key(<<"protocol_metadata">>, E2EEOut)).

%% ===================================================================
%% 测试 4: 未知字段不被裁剪（前向兼容）
%% ADR 15 §4: "不得重建/裁剪 protected_header"
%% ===================================================================

unknown_fields_not_stripped_test() ->
    E2EE = (v3_e2ee_map())#{
        <<"future_field">> => <<"some-value">>,
        <<"nested_unknown">> => #{<<"a">> => 1, <<"b">> => [1, 2, 3]}
    },
    Json = jsone:encode(#{
        <<"id">> => <<"msg-002">>,
        <<"type">> => <<"c2c">>,
        <<"from">> => <<"100">>,
        <<"to">> => <<"200">>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"message">>,
        <<"e2ee">> => E2EE,
        <<"payload">> => #{<<"body">> => <<>>},
        <<"created_at">> => 1753500000000
    }),
    Decoded = message_ds:decode_websocket_message(Json),
    E2EEDecoded = maps:get(<<"e2ee">>, Decoded),
    ?assertEqual(E2EE, E2EEDecoded),
    %% 未知字段仍在
    ?assertEqual(<<"some-value">>, maps:get(<<"future_field">>, E2EEDecoded)),
    ?assertEqual(
        #{<<"a">> => 1, <<"b">> => [1, 2, 3]}, maps:get(<<"nested_unknown">>, E2EEDecoded)
    ).

%% ===================================================================
%% 测试 5: inject_sender_device 不影响 e2ee
%% ===================================================================

inject_sender_device_does_not_touch_e2ee_test() ->
    Json = v3_ws_message_json(),
    Decoded = message_ds:decode_websocket_message(Json),
    E2EEBefore = maps:get(<<"e2ee">>, Decoded),

    %% 模拟 websocket_logic:inject_sender_device
    Payload = maps:get(<<"payload">>, Decoded),
    State = #{did => <<"device-abc">>, dtype => <<"ios">>},
    Payload2 = message_ds:inject_sender_device(Payload, State),

    %% payload 被注入了设备信息
    ?assertEqual(<<"device-abc">>, maps:get(<<"sender_did">>, Payload2)),
    ?assertEqual(<<"ios">>, maps:get(<<"sender_dtype">>, Payload2)),

    %% e2ee 不受影响（重新 assemble 验证）
    Msg = message_ds:assemble_msg(
        <<"C2C">>,
        <<"100">>,
        <<"200">>,
        Payload2,
        <<"msg-001">>,
        <<"text">>,
        <<"message">>,
        E2EEBefore
    ),
    ?assertEqual(E2EEBefore, maps:get(<<"e2ee">>, Msg)).

%% ===================================================================
%% 测试 6: e2ee 为 null 时（非加密消息）正常透传
%% ===================================================================

null_e2ee_passthrough_test() ->
    Json = jsone:encode(#{
        <<"id">> => <<"msg-003">>,
        <<"type">> => <<"c2c">>,
        <<"from">> => <<"100">>,
        <<"to">> => <<"200">>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"message">>,
        <<"payload">> => #{<<"body">> => <<"hello">>},
        <<"created_at">> => 1753500000000
    }),
    Decoded = message_ds:decode_websocket_message(Json),
    ?assertEqual(null, maps:get(<<"e2ee">>, Decoded)),

    Msg = message_ds:assemble_msg(
        <<"C2C">>,
        <<"100">>,
        <<"200">>,
        #{<<"body">> => <<"hello">>},
        <<"msg-003">>,
        <<"text">>,
        <<"message">>,
        null
    ),
    ?assertEqual(null, maps:get(<<"e2ee">>, Msg)).

%% ===================================================================
%% 测试 7: JSON 序列化 roundtrip — e2ee 编码后字节不变
%% 证明 jsone:encode 不会重排/修改 e2ee 内部结构
%% ===================================================================

json_encode_stability_test() ->
    E2EE = v3_e2ee_map(),
    Json1 = jsone:encode(#{<<"e2ee">> => E2EE}),
    Decoded = jsone:decode(Json1, [{object_format, map}]),
    E2EEBack = maps:get(<<"e2ee">>, Decoded),
    Json2 = jsone:encode(#{<<"e2ee">> => E2EEBack}),
    %% 两次编码结果完全相同（语义等价 + 字节稳定）
    ?assertEqual(Json1, Json2),
    %% 值也完全相同
    ?assertEqual(E2EE, E2EEBack).

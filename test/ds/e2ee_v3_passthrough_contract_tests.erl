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
-include("imboy_frame.hrl").

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

%%%===================================================================
%%% E2EE-060 — 后端 PFv3 不透明透传契约（21/E2EE-022）
%%%
%%% 上面的测试 1–7 只覆盖 **JSON 进程内** 路径（message_ds decode/assemble），
%%% 从未穿过真正的出站编码器。生产的 WS 连接协商 `imboy.v2` 子协议后
%%% protocol=protobuf，同步响应经 websocket_handler:ws_reply/3 走
%%% imboy_codec:encode(protobuf, _)，而 protobuf 的 E2EEMeta schema
%%% （proto/imboy.proto §128）根本没有 protected_header / header_hash /
%%% ciphertext / protocol_metadata / fan_out / devices 字段。
%%%
%%% 服务端把它们**静默裁掉**再投递，等价于消息永久不可解密——服务端不持
%%% 有密钥，无法事后修复。ADR 15 §10 要求"不得重建/裁剪 protected_header"，
%%% 故此处契约是 fail-closed：protobuf 无法无损表达时必须**拒绝编码**，
%%% 由调用方退回 byte-preserving 的 JSON，而不是投递被裁剪的信封。
%%%===================================================================

%% ADR 15 §3.3 单信封形态
v3_single_envelope_e2ee_map() ->
    (v3_e2ee_map())#{
        <<"protocol">> => <<"olm">>,
        <<"version">> => 1
    }.

%% E2EE-029 per-device fan-out 形态（chat_network_service.dart 实际产出）
v3_fanout_e2ee_map() ->
    #{
        <<"meta_version">> => 3,
        <<"protocol">> => <<"olm">>,
        <<"version">> => 1,
        <<"fan_out">> => <<"per_device">>,
        <<"devices">> => #{
            <<"dev-a">> => #{
                <<"protected_header">> => <<"omh2ImlkIqJtZXNzYWdlX2lk">>,
                <<"header_hash">> => <<"dGVzdC1oYXNoLTI1Ng">>,
                <<"ciphertext">> => <<"b2xtLWNpcGhlcnRleHQtYQ">>,
                <<"protocol_metadata">> => #{<<"session_id">> => <<"sess-a">>}
            },
            <<"dev-b">> => #{
                <<"protected_header">> => <<"omh2ImlkIqJtZXNzYWdlX2ll">>,
                <<"header_hash">> => <<"dGVzdC1oYXNoLTI1Nw">>,
                <<"ciphertext">> => <<"b2xtLWNpcGhlcnRleHQtYg">>,
                <<"protocol_metadata">> => #{<<"session_id">> => <<"sess-b">>}
            }
        }
    }.

%% 遗留 v2（RSA-OAEP / Megolm）信封：protobuf 可无损表达，不得被本次改动误伤
v2_legacy_e2ee_map() ->
    #{
        <<"e2ee">> => true,
        <<"e2ee_ver">> => 1,
        <<"e2ee_suite">> => <<"OLM.V1">>,
        <<"nonce">> => <<"bm9uY2U">>,
        <<"keys">> => [
            #{
                <<"did">> => <<"dev-a">>,
                <<"kid">> => <<"kid-1">>,
                <<"wrap_alg">> => <<"olm">>,
                <<"ek">> => <<"ZWs">>
            }
        ],
        <<"protocol">> => <<"olm">>,
        <<"session_id">> => <<"sess-a">>
    }.

c2c_msg_with(E2EE) ->
    #{
        <<"id">> => <<"msg-060">>,
        <<"type">> => <<"C2C">>,
        <<"from">> => <<"100">>,
        <<"to">> => <<"200">>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"message">>,
        <<"e2ee">> => E2EE,
        <<"payload">> => <<>>,
        <<"created_at">> => 1753500000000
    }.

%% ---------- 出站编码：protobuf 不得静默裁剪 PFv3 信封 ----------

%% E2EE-060-01 fan-out 形态
pb_encode_refuses_lossy_v3_fanout_envelope_test() ->
    Msg = c2c_msg_with(v3_fanout_e2ee_map()),
    ?assertEqual(<<>>, imboy_codec:encode(protobuf, Msg)).

%% E2EE-060-02 ADR 15 §3.3 单信封形态
pb_encode_refuses_lossy_v3_single_envelope_test() ->
    Msg = c2c_msg_with(v3_single_envelope_e2ee_map()),
    ?assertEqual(<<>>, imboy_codec:encode(protobuf, Msg)).

%% E2EE-060-03 未知扩展字段同样不可裁剪（前向兼容）
pb_encode_refuses_unknown_e2ee_extension_test() ->
    E2EE = (v2_legacy_e2ee_map())#{<<"future_field">> => <<"x">>},
    ?assertEqual(<<>>, imboy_codec:encode(protobuf, c2c_msg_with(E2EE))).

%% E2EE-060-04 回归护栏：遗留 v2 信封仍走 protobuf 且无损往返
pb_encode_keeps_legacy_v2_envelope_lossless_test() ->
    Msg = c2c_msg_with(v2_legacy_e2ee_map()),
    Bin = imboy_codec:encode(protobuf, Msg),
    ?assertNotEqual(<<>>, Bin),
    Back = imboy_codec:decode(protobuf, Bin),
    ?assertEqual(v2_legacy_e2ee_map(), maps:get(<<"e2ee">>, Back)).

%% E2EE-060-05 回归护栏：无 e2ee 的普通消息不受影响
pb_encode_plain_message_unaffected_test() ->
    Msg = maps:put(<<"e2ee">>, null, c2c_msg_with(null)),
    Bin = imboy_codec:encode(protobuf, Msg),
    ?assertNotEqual(<<>>, Bin),
    Back = imboy_codec:decode(protobuf, Bin),
    ?assertEqual(<<"msg-060">>, maps:get(<<"id">>, Back)),
    ?assertEqual(null, maps:get(<<"e2ee">>, Back)).

%% ---------- 入站校验：外层信封必填字段与尺寸（服务端不解析内容） ----------

%% E2EE-060-06 meta_version=3 但缺 protected_header → 拒绝
validate_rejects_v3_envelope_missing_protected_header_test() ->
    E2EE = maps:remove(<<"protected_header">>, v3_single_envelope_e2ee_map()),
    ?assertMatch(
        {error, <<"e2ee_envelope_invalid">>},
        message_ds:validate_message(c2c_msg_with(E2EE))
    ).

%% E2EE-060-07 meta_version=3 但缺 ciphertext → 拒绝
validate_rejects_v3_envelope_missing_ciphertext_test() ->
    E2EE = maps:remove(<<"ciphertext">>, v3_single_envelope_e2ee_map()),
    ?assertMatch(
        {error, <<"e2ee_envelope_invalid">>},
        message_ds:validate_message(c2c_msg_with(E2EE))
    ).

%% E2EE-060-08 fan-out 形态下某个设备条目缺 header_hash → 拒绝
validate_rejects_v3_fanout_device_missing_header_hash_test() ->
    Full = v3_fanout_e2ee_map(),
    Devices = maps:get(<<"devices">>, Full),
    Broken = maps:update_with(
        <<"dev-b">>, fun(D) -> maps:remove(<<"header_hash">>, D) end, Devices
    ),
    E2EE = Full#{<<"devices">> => Broken},
    ?assertMatch(
        {error, <<"e2ee_envelope_invalid">>},
        message_ds:validate_message(c2c_msg_with(E2EE))
    ).

%% E2EE-060-09 字段类型错误（protected_header 不是字符串）→ 拒绝
validate_rejects_v3_envelope_wrong_field_type_test() ->
    E2EE = (v3_single_envelope_e2ee_map())#{<<"protected_header">> => 12345},
    ?assertMatch(
        {error, <<"e2ee_envelope_invalid">>},
        message_ds:validate_message(c2c_msg_with(E2EE))
    ).

%% E2EE-060-10 超限信封在 DB 写入与广播前返回稳定错误（不是断连）
validate_rejects_oversized_e2ee_envelope_test() ->
    Huge = binary:copy(<<"A">>, 1048577),
    E2EE = (v3_single_envelope_e2ee_map())#{<<"ciphertext">> => Huge},
    ?assertMatch(
        {error, <<"e2ee_envelope_too_large">>},
        message_ds:validate_message(c2c_msg_with(E2EE))
    ).

%% E2EE-060-11 回归护栏：合法 v3 单信封必须通过（不得过度拒绝）
validate_accepts_wellformed_v3_single_envelope_test() ->
    ?assertMatch(
        {ok, _},
        message_ds:validate_message(c2c_msg_with(v3_single_envelope_e2ee_map()))
    ).

%% E2EE-060-12 回归护栏：合法 v3 fan-out 必须通过
validate_accepts_wellformed_v3_fanout_test() ->
    ?assertMatch(
        {ok, _},
        message_ds:validate_message(c2c_msg_with(v3_fanout_e2ee_map()))
    ).

%% E2EE-060-13 回归护栏：遗留 v2 信封与无 e2ee 消息不受新校验影响
validate_accepts_legacy_and_plain_messages_test() ->
    ?assertMatch({ok, _}, message_ds:validate_message(c2c_msg_with(v2_legacy_e2ee_map()))),
    ?assertMatch({ok, _}, message_ds:validate_message(c2c_msg_with(null))).

%% E2EE-060-14 未知非关键扩展字段不因校验被拒（前向兼容）
validate_accepts_v3_envelope_with_unknown_extension_test() ->
    E2EE = (v3_single_envelope_e2ee_map())#{<<"future_field">> => <<"x">>},
    ?assertMatch({ok, _}, message_ds:validate_message(c2c_msg_with(E2EE))).

%% ---------- 出站线上帧：客户端实际收到的字节必须保住信封 ----------

%% 模拟 Dart v2 客户端：帧载荷先按 JSON 解，再回退 protobuf
wire_e2ee(Payload) ->
    try jsone:decode(Payload, [{object_format, map}]) of
        M when is_map(M) -> maps:get(<<"e2ee">>, M, null)
    catch
        _:_ -> maps:get(<<"e2ee">>, imboy_codec:decode(protobuf, Payload), null)
    end.

%% E2EE-060-15 imboy.v2 连接（protocol=protobuf, framing=v2）：fan-out 信封完整到达
v2_wire_preserves_v3_fanout_envelope_test() ->
    Msg = c2c_msg_with(v3_fanout_e2ee_map()),
    {binary, Frame} = imboy_codec:encode_ws_msg(protobuf, v2, ?FRAME_TYPE_MSG_C2C, Msg),
    {ok, #imboy_frame{type = Type, payload = Payload}} = imboy_codec:unwrap_v2_frame(Frame),
    ?assertEqual(?FRAME_TYPE_MSG_C2C, Type),
    ?assertEqual(v3_fanout_e2ee_map(), wire_e2ee(Payload)).

%% E2EE-060-16 imboy.v2 连接：ADR 15 §3.3 单信封同样完整到达
v2_wire_preserves_v3_single_envelope_test() ->
    Msg = c2c_msg_with(v3_single_envelope_e2ee_map()),
    {binary, Frame} = imboy_codec:encode_ws_msg(protobuf, v2, ?FRAME_TYPE_MSG_C2C, Msg),
    {ok, #imboy_frame{payload = Payload}} = imboy_codec:unwrap_v2_frame(Frame),
    ?assertEqual(v3_single_envelope_e2ee_map(), wire_e2ee(Payload)).

%% E2EE-060-17 imboy-protobuf 连接（framing=none）：退回 JSON text 帧，信封完整
pb_none_framing_wire_preserves_v3_envelope_test() ->
    Msg = c2c_msg_with(v3_fanout_e2ee_map()),
    {text, Payload} = imboy_codec:encode_ws_msg(protobuf, none, ?FRAME_TYPE_MSG_C2C, Msg),
    ?assertEqual(v3_fanout_e2ee_map(), wire_e2ee(Payload)).

%% E2EE-060-18 回归护栏：遗留 v2 信封在 imboy.v2 上仍以 protobuf 载荷下发
v2_wire_keeps_protobuf_for_legacy_envelope_test() ->
    Msg = c2c_msg_with(v2_legacy_e2ee_map()),
    {binary, Frame} = imboy_codec:encode_ws_msg(protobuf, v2, ?FRAME_TYPE_MSG_C2C, Msg),
    {ok, #imboy_frame{payload = Payload}} = imboy_codec:unwrap_v2_frame(Frame),
    %% protobuf 载荷不是合法 JSON；直接按 protobuf 解必须还原出同一信封
    Back = imboy_codec:decode(protobuf, Payload),
    ?assertEqual(v2_legacy_e2ee_map(), maps:get(<<"e2ee">>, Back)).

%% E2EE-060-19 回归护栏：JSON 连接不受影响，仍是 text 帧且信封完整
json_wire_unaffected_test() ->
    Msg = c2c_msg_with(v3_fanout_e2ee_map()),
    {text, Payload} = imboy_codec:encode_ws_msg(json, none, ?FRAME_TYPE_MSG_C2C, Msg),
    ?assertEqual(v3_fanout_e2ee_map(), wire_e2ee(Payload)).

%% E2EE-060-20 base64url 密文逐字节不变（含 - _ 与无填充）
v2_wire_preserves_base64url_bytes_test() ->
    Cipher = <<"a-b_c9ZpQ2mR7sT9uWaBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789-_">>,
    E2EE = (v3_single_envelope_e2ee_map())#{<<"ciphertext">> => Cipher},
    Msg = c2c_msg_with(E2EE),
    {binary, Frame} = imboy_codec:encode_ws_msg(protobuf, v2, ?FRAME_TYPE_MSG_C2C, Msg),
    {ok, #imboy_frame{payload = Payload}} = imboy_codec:unwrap_v2_frame(Frame),
    ?assertEqual(Cipher, maps:get(<<"ciphertext">>, wire_e2ee(Payload))).

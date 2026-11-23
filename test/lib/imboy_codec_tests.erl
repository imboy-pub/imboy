%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_codec 编解码器测试
%%% 验证 JSON 和 protobuf 编解码的等价性和正确性
%%% @end
%%%-------------------------------------------------------------------
-module(imboy_codec_tests).

-include_lib("eunit/include/eunit.hrl").
-include("imboy_frame.hrl").

%%%===================================================================
%%% Test generators
%%%===================================================================

codec_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"JSON encode/decode roundtrip", fun json_roundtrip/0},
        {"Protobuf encode/decode roundtrip", fun protobuf_roundtrip/0},
        {"JSON and Protobuf decode to equivalent maps", fun cross_protocol_equivalence/0},
        {"Protocol atom conversion", fun protocol_atom_test/0},
        {"WebSocket frame types", fun ws_frame_types/0},
        {"E2EE metadata conversion", fun e2ee_conversion/0},
        {"E2EE Olm metadata protobuf roundtrip", fun e2ee_olm_roundtrip/0},
        {"E2EE Megolm metadata protobuf roundtrip", fun e2ee_megolm_roundtrip/0},
        {"E2EE explicit zero survives WebSocket encoding", fun e2ee_explicit_zero_ws/0},
        {"Empty/null field handling", fun empty_field_handling/0},
        {"Payload encode/decode", fun payload_encode_decode/0},
        {"Large UID handling (TSID)", fun large_uid_handling/0},
        {"Framing atom conversion", fun framing_atom_test/0},
        {"v2 frame wrap/unwrap roundtrip", fun v2_frame_roundtrip/0},
        {"v2 protocol atom maps to protobuf", fun v2_protocol_atom_test/0},
        {"v2 frame unwrap error on bad input", fun v2_frame_unwrap_error/0},
        {"a2a_task_update accepted on JSON channel", fun a2a_task_update_json_channel/0},
        {"CLIENT_ACK_ERROR 在 v2 帧内保留 id/in_reply_to/reason", fun client_ack_error_v2_lossless/0},
        {"C2S_SERVER_ACK 在 v2 帧内保留 id/in_reply_to", fun c2s_server_ack_v2_lossless/0},
        {"C2G_ERROR 在 v2 帧内保留 id/error/code", fun c2g_error_v2_lossless/0},
        {"普通 C2C 消息仍走 protobuf 编码", fun c2c_still_protobuf/0},
        {"语音 voice 映射 AUDIO 且 protobuf 往返不丢类型", fun voice_content_type_roundtrip/0},
        {"chat_history_bundle JSON 通道识别与原样往返", fun chat_history_bundle_json_channel/0},
        {"chat_history_bundle protobuf 连接回退 JSON 四象限无损", fun chat_history_bundle_pb_fallback/0},
        {"顶层扩展字段 sender_did/sender_dtype/topic_id 回退 JSON 不丢字段",
            fun toplevel_extra_fields_pb_fallback/0},
        {"控制帧 in_reply_to/reason 白名单化后仍回退 JSON（回归）", fun ctrl_fields_pb_fallback_regression/0},
        {"垃圾 protobuf 字节解码得空 map 哨兵（不崩溃）", fun garbage_pb_decode_sentinel/0},
        {"未知 MsgDirection 枚举解码为空串哨兵（不崩溃）", fun unknown_direction_enum_sentinel/0},
        {"FL-05 四通道操作等价（C2C/C2G/S2C/C2S × json none/pb none/pb v2）",
            fun four_channel_operation_equivalence/0},
        {"FL-05 双向版本兼容（协商矩阵 + 新旧端组合等价 + 回退组合明确）", fun bidirectional_version_compat/0}
    ]}.

timestamp_fields_protobuf_roundtrip_test_() ->
    Cases = [
        {integer_ms, 1700000000123, 1700000000123},
        {numeric_binary_ms, <<"1700000000123">>, 1700000000123},
        {utc_rfc3339, <<"2023-11-14T22:13:20.123Z">>, 1700000000123},
        {offset_rfc3339, <<"2023-11-15T06:13:20.123+08:00">>, 1700000000123},
        {epoch_zero, 0, 0},
        {negative_epoch_ms, -1, -1},
        {missing_zone, <<"2023-11-14T22:13:20.123">>, 0},
        {invalid_timestamp, <<"not-a-time">>, 0},
        {absent_timestamp, undefined, 0}
    ],
    [
        {atom_to_list(Name), fun() ->
            Base = test_c2c_message(),
            Fields =
                case Value of
                    undefined -> maps:without([<<"created_at">>, <<"server_ts">>], Base);
                    _ -> Base#{<<"created_at">> => Value, <<"server_ts">> => Value}
                end,
            Msg = Fields#{<<"from">> => 9007199254740993, <<"conv_seq">> => 37},
            Decoded = imboy_codec:decode(protobuf, imboy_codec:encode(protobuf, Msg)),
            ?assertEqual(Expected, maps:get(<<"created_at">>, Decoded)),
            ?assertEqual(Expected, maps:get(<<"server_ts">>, Decoded)),
            ?assertEqual(9007199254740993, maps:get(<<"from">>, Decoded)),
            ?assertEqual(37, maps:get(<<"conv_seq">>, Decoded))
        end}
     || {Name, Value, Expected} <- Cases
    ].

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
    Msg = #{
        <<"id">> => <<"msg-empty">>,
        <<"type">> => <<"S2C">>,
        <<"action">> => <<"pull_offline_msg">>
    },
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
    Msg = #{
        <<"id">> => <<"msg-large-uid">>,
        <<"type">> => <<"C2C">>,
        <<"from">> => LargeUid,
        <<"to">> => LargeUid + 1
    },
    Encoded = imboy_codec:encode(protobuf, Msg),
    Decoded = imboy_codec:decode(protobuf, Encoded),

    ?assertEqual(LargeUid, maps:get(<<"from">>, Decoded)),
    ?assertEqual(LargeUid + 1, maps:get(<<"to">>, Decoded)).

%% @doc framing_atom/1 测试
framing_atom_test() ->
    ?assertEqual(v2, imboy_codec:framing_atom(<<"imboy.v2">>)),
    ?assertEqual(none, imboy_codec:framing_atom(<<"imboy-protobuf">>)),
    ?assertEqual(none, imboy_codec:framing_atom(<<"imboy-json">>)),
    ?assertEqual(none, imboy_codec:framing_atom(<<"text">>)),
    ?assertEqual(none, imboy_codec:framing_atom(undefined)),
    ?assertEqual(none, imboy_codec:framing_atom(<<"unknown">>)).

%% @doc v2 framing 子协议映射为 protobuf 编码
v2_protocol_atom_test() ->
    ?assertEqual(protobuf, imboy_codec:protocol_atom(<<"imboy.v2">>)).

%% @doc v2 frame wrap/unwrap 往返测试
v2_frame_roundtrip() ->
    Msg = test_c2c_message(),
    Payload = imboy_codec:encode(protobuf, Msg),
    ?assert(is_binary(Payload)),
    Frame = imboy_codec:wrap_v2_frame(?FRAME_TYPE_MSG_C2C, 0, Payload),
    ?assert(is_binary(Frame)),
    ?assert(byte_size(Frame) > byte_size(Payload)),
    {ok, Decoded} = imboy_codec:unwrap_v2_frame(Frame),
    ?assertEqual(?FRAME_TYPE_MSG_C2C, imboy_frame:type(Decoded)),
    ?assertEqual(0, imboy_frame:flags(Decoded)),
    ?assertEqual(Payload, imboy_frame:payload(Decoded)),
    %% 二次 decode payload
    DecodedMsg = imboy_codec:decode(protobuf, imboy_frame:payload(Decoded)),
    ?assertEqual(<<"msg-test-001">>, maps:get(<<"id">>, DecodedMsg)).

%% @doc CLIENT_ACK_ERROR 走 protobuf/v2 时不得丢字段
%%
%% IMBoyMessage 的 MsgDirection 没有 CLIENT_ACK_ERROR 枚举，to_pb_map 也不搬
%% in_reply_to / reason：一旦按 protobuf 编码，客户端只能收到 type=
%% MSG_DIRECTION_UNSPECIFIED、id 为空的空壳帧，无法关联回原消息 → 确认超时
%% 重发死循环。故这类消息必须退回 JSON 载荷。
client_ack_error_v2_lossless() ->
    Msg = #{
        <<"id">> => <<"msg-ack-001">>,
        <<"type">> => <<"CLIENT_ACK_ERROR">>,
        <<"in_reply_to">> => <<"msg-ack-001">>,
        <<"action">> => <<"CLIENT_ACK_ERROR">>,
        <<"reason">> => <<"invalid_did">>,
        <<"server_ts">> => 1785312537582
    },
    {binary, Frame} = imboy_codec:encode_ws_msg(
        protobuf, v2, ?FRAME_TYPE_MSG_S2C, Msg
    ),
    {ok, Decoded} = imboy_codec:unwrap_v2_frame(Frame),
    Payload = imboy_frame:payload(Decoded),
    DecodedMsg = imboy_codec:decode(json, Payload),
    ?assertEqual(<<"msg-ack-001">>, maps:get(<<"id">>, DecodedMsg)),
    ?assertEqual(<<"CLIENT_ACK_ERROR">>, maps:get(<<"type">>, DecodedMsg)),
    ?assertEqual(<<"msg-ack-001">>, maps:get(<<"in_reply_to">>, DecodedMsg)),
    ?assertEqual(<<"invalid_did">>, maps:get(<<"reason">>, DecodedMsg)),
    %% 非 v2 的 protobuf 连接同样退回 JSON text 帧
    {text, Json} = imboy_codec:encode_ws_msg(protobuf, none, ?FRAME_TYPE_MSG_S2C, Msg),
    ?assertEqual(
        <<"invalid_did">>, maps:get(<<"reason">>, imboy_codec:decode(json, Json))
    ).

%% @doc C2S_SERVER_ACK（msg_c2s_logic 经 self()!{reply,Map} 下发）走
%% protobuf/v2 时退回 JSON 载荷：in_reply_to 无 schema 字段，protobuf 编码
%% 会被静默丢弃，客户端无法把 ACK 关联回原消息。
c2s_server_ack_v2_lossless() ->
    Msg = #{
        <<"id">> => <<"msg-c2s-ack-001">>,
        <<"type">> => <<"C2S_SERVER_ACK">>,
        <<"in_reply_to">> => <<"msg-c2s-ack-001">>,
        <<"server_ts">> => 1785312537582
    },
    {binary, Frame} = imboy_codec:encode_ws_msg(
        protobuf, v2, ?FRAME_TYPE_MSG_S2C, Msg
    ),
    {ok, Decoded} = imboy_codec:unwrap_v2_frame(Frame),
    DecodedMsg = imboy_codec:decode(json, imboy_frame:payload(Decoded)),
    ?assertEqual(<<"C2S_SERVER_ACK">>, maps:get(<<"type">>, DecodedMsg)),
    ?assertEqual(<<"msg-c2s-ack-001">>, maps:get(<<"id">>, DecodedMsg)),
    ?assertEqual(<<"msg-c2s-ack-001">>, maps:get(<<"in_reply_to">>, DecodedMsg)),
    ?assertEqual(1785312537582, maps:get(<<"server_ts">>, DecodedMsg)),
    %% 非 v2 的 protobuf 连接同样退回 JSON text 帧
    {text, Json} = imboy_codec:encode_ws_msg(protobuf, none, ?FRAME_TYPE_MSG_S2C, Msg),
    ?assertEqual(
        <<"msg-c2s-ack-001">>,
        maps:get(<<"in_reply_to">>, imboy_codec:decode(json, Json))
    ).

%% @doc C2G_ERROR（msg_c2g_logic 经 route 返回值 / self()! 下发）走
%% protobuf/v2 时退回 JSON 载荷：C2G_ERROR 不在 MsgDirection 枚举内，
%% protobuf 编码会压成 MSG_DIRECTION_UNSPECIFIED 空壳帧。
c2g_error_v2_lossless() ->
    Msg = #{
        <<"id">> => <<"msg-c2g-err-001">>,
        <<"type">> => <<"C2G_ERROR">>,
        <<"error">> => <<"Not a group member">>,
        <<"code">> => 403
    },
    {binary, Frame} = imboy_codec:encode_ws_msg(
        protobuf, v2, ?FRAME_TYPE_MSG_S2C, Msg
    ),
    {ok, Decoded} = imboy_codec:unwrap_v2_frame(Frame),
    DecodedMsg = imboy_codec:decode(json, imboy_frame:payload(Decoded)),
    ?assertEqual(<<"C2G_ERROR">>, maps:get(<<"type">>, DecodedMsg)),
    ?assertEqual(<<"msg-c2g-err-001">>, maps:get(<<"id">>, DecodedMsg)),
    ?assertEqual(403, maps:get(<<"code">>, DecodedMsg)),
    ?assertEqual(<<"Not a group member">>, maps:get(<<"error">>, DecodedMsg)),
    %% 非 v2 的 protobuf 连接同样退回 JSON text 帧
    {text, Json} = imboy_codec:encode_ws_msg(protobuf, none, ?FRAME_TYPE_MSG_S2C, Msg),
    ?assertEqual(403, maps:get(<<"code">>, imboy_codec:decode(json, Json))).

%% @doc schema 装得下的普通消息不受回退影响，仍走 protobuf
c2c_still_protobuf() ->
    Msg = test_c2c_message(),
    {binary, Frame} = imboy_codec:encode_ws_msg(
        protobuf, v2, ?FRAME_TYPE_MSG_C2C, Msg
    ),
    {ok, Decoded} = imboy_codec:unwrap_v2_frame(Frame),
    DecodedMsg = imboy_codec:decode(protobuf, imboy_frame:payload(Decoded)),
    ?assertEqual(<<"msg-test-001">>, maps:get(<<"id">>, DecodedMsg)),
    ?assertEqual(<<"C2C">>, maps:get(<<"type">>, DecodedMsg)).

%% @doc 语音 voice 在 protobuf 通道映射 AUDIO 且往返不丢类型。
%%
%% 客户端全站发送口径是 <<"voice">>（JSON 通道原样透传），protobuf
%% 通道若落 CONTENT_TYPE_UNSPECIFIED，客户端解码 default 成 <<"text">>
%% ——语音消息被渲染成文本（比「不支持的消息类型」更隐蔽）。
voice_content_type_roundtrip() ->
    Msg = test_c2c_message(<<"voice">>),
    {binary, Frame} = imboy_codec:encode_ws_msg(
        protobuf, v2, ?FRAME_TYPE_MSG_C2C, Msg
    ),
    {ok, Decoded} = imboy_codec:unwrap_v2_frame(Frame),
    DecodedMsg = imboy_codec:decode(protobuf, imboy_frame:payload(Decoded)),
    %% enum 解码侧口径是 <<"audio">>（客户端 normalize 再归一为 voice 渲染）；
    %% 修复前落 CONTENT_TYPE_UNSPECIFIED → 解码空串 → 客户端 default 'text'
    ?assertEqual(<<"audio">>, maps:get(<<"msg_type">>, DecodedMsg)),
    ?assertEqual(<<"msg-test-001">>, maps:get(<<"id">>, DecodedMsg)),
    ?assertEqual(<<"C2C">>, maps:get(<<"type">>, DecodedMsg)).

%% @doc chat_history_bundle（多选消息合并转发快照，W1-D）在 JSON 通道被识别
%% 并原样透传（仿 a2a_task_update 先例；合同见
%% docs/reference/chat-history-bundle-contract.md）。
chat_history_bundle_json_channel() ->
    %% JSON-only 内容类型识别（精确匹配，无前缀放行）
    ?assert(imboy_codec:is_json_channel_type(<<"chat_history_bundle">>)),
    ?assertNot(imboy_codec:is_json_channel_type(<<"chat_history_bundle_x">>)),

    %% JSON 通道 msg_type / bundle payload 原样往返，不落 protobuf enum
    Msg = test_chat_history_bundle_msg(),
    Encoded = imboy_codec:encode(json, Msg),
    ?assert(is_binary(Encoded)),
    Decoded = imboy_codec:decode(json, Encoded),
    ?assertEqual(<<"chat_history_bundle">>, maps:get(<<"msg_type">>, Decoded)),
    Payload = maps:get(<<"payload">>, Decoded),
    ?assertEqual(1, maps:get(<<"bundle_version">>, Payload)),
    ?assertEqual(1, length(maps:get(<<"entries">>, Payload))).

%% @doc protobuf 连接上 chat_history_bundle 回退 JSON：C2C/C2G × none/v2
%% 四象限往返无损（仿 talk_record_card 四象限先例——JSON-only 类型不进
%% protobuf enum，pb_lossless 自动回退 JSON 帧）。
chat_history_bundle_pb_fallback() ->
    [
        begin
            Msg = test_chat_history_bundle_msg(Direction),
            FrameType =
                case Direction of
                    <<"C2C">> -> ?FRAME_TYPE_MSG_C2C;
                    _ -> ?FRAME_TYPE_MSG_C2G
                end,
            Wire = imboy_codec:encode_ws_msg(protobuf, Framing, FrameType, Msg),
            Json =
                case {Framing, Wire} of
                    {none, {text, Binary}} ->
                        Binary;
                    {v2, {binary, Binary}} ->
                        {ok, Frame} = imboy_codec:unwrap_v2_frame(Binary),
                        imboy_frame:payload(Frame)
                end,
            ?assertEqual(Msg, jsone:decode(Json, [{object_format, map}]))
        end
     || Direction <- [<<"C2C">>, <<"C2G">>], Framing <- [none, v2]
    ].

e2ee_explicit_zero_ws() ->
    lists:foreach(
        fun({Msg, FrameType, Key}) ->
            E2EE = maps:get(<<"e2ee">>, Msg),
            WithZero = Msg#{<<"e2ee">> := E2EE#{Key := 0}},
            Missing = Msg#{<<"e2ee">> := maps:remove(Key, E2EE)},
            ?assertEqual(<<>>, imboy_codec:encode(protobuf, WithZero)),
            {text, Json} = imboy_codec:encode_ws_msg(protobuf, none, FrameType, WithZero),
            ?assertEqual(WithZero, imboy_codec:decode(json, Json)),
            {binary, Packet} = imboy_codec:encode_ws_msg(protobuf, v2, FrameType, WithZero),
            {ok, Frame} = imboy_codec:unwrap_v2_frame(Packet),
            ?assertEqual(WithZero, imboy_codec:decode(json, imboy_frame:payload(Frame))),
            {binary, NonzeroPb} = imboy_codec:encode_ws_msg(protobuf, none, FrameType, Msg),
            Nonzero = imboy_codec:decode(protobuf, NonzeroPb),
            ?assertEqual(maps:get(Key, E2EE), maps:get(Key, maps:get(<<"e2ee">>, Nonzero))),
            {binary, MissingPb} = imboy_codec:encode_ws_msg(protobuf, none, FrameType, Missing),
            WithoutKey = imboy_codec:decode(protobuf, MissingPb),
            ?assertNot(maps:is_key(Key, maps:get(<<"e2ee">>, WithoutKey)))
        end,
        e2ee_zero_cases()
    ).

e2ee_zero_cases() ->
    [
        {test_e2ee_olm_message(), ?FRAME_TYPE_MSG_C2C, <<"message_type">>},
        {test_e2ee_megolm_message(), ?FRAME_TYPE_MSG_C2G, <<"message_index">>}
    ].

%%%===================================================================
%%% FL-05：四通道操作等价与双向版本兼容
%%%===================================================================

%% @doc FL-05:LOCAL:01 四通道（C2C/C2G/S2C/C2S）经各支持编码完成同一操作：
%% 身份（from/to）、关联（id/type/action/conv_seq/v2 帧类型）、业务结果
%% （msg_type/payload/e2ee）在三条支持传输组合上解码等价：
%%   json+none（text 帧）＝老 JSON 通道；
%%   protobuf+none（裸 binary 帧）＝老 protobuf 子协议连接；
%%   protobuf+v2（v2 帧）＝新分层二进制连接（pb 不可无损时载荷回退 JSON）。
four_channel_operation_equivalence() ->
    lists:foreach(
        fun({Channel, FrameType, Msg}) ->
            Views = channel_views(FrameType, Msg),
            ?assertEqual(3, length(Views)),
            lists:foreach(
                fun({Combo, _Effective, Decoded, FrameTypeObserved}) ->
                    Core = view_core(Decoded),
                    lists:foreach(
                        fun(Field) ->
                            ?assertEqual(
                                {Channel, Combo, Field, maps:get(Field, Msg)},
                                {Channel, Combo, Field, maps:get(Field, Core)}
                            )
                        end,
                        view_core_fields()
                    ),
                    case Combo of
                        pb_v2 ->
                            %% 帧级关联：v2 帧类型按通道保持
                            ?assertEqual(FrameType, FrameTypeObserved);
                        _ ->
                            ok
                    end
                end,
                Views
            ),
            %% 各通道彼此等价：三视图核心字段两两相等
            [V1, V2, V3] = [view_core(D) || {_, _, D, _} <- Views],
            ?assertEqual(V1, V2),
            ?assertEqual(V1, V3)
        end,
        four_channel_cases()
    ).

%% @doc FL-05:LOCAL:02 旧端/新端双向版本兼容：
%%   1) 子协议协商矩阵——老端组合（text/imboy-json/imboy-protobuf/缺省）永远
%%      落在既有 json|protobuf + none；新端 imboy.v2 → protobuf+v2；未知子协议
%%      可理解回退 json+none，不产生无法解释的拒绝组合；
%%   2) 服务端→客户端：同一 S2C 操作发给老端（json+none）与新端（protobuf+v2），
%%      两端解码结果等价；
%%   3) 客户端→服务端：老端 text JSON 请求与新端 v2 protobuf 请求解码等价；
%%   4) 回退组合明确：pb 不可无损的控制帧在全部组合都回退 JSON 载荷，
%%      新老两端收到语义一致的字段。
bidirectional_version_compat() ->
    %% 1) 协商矩阵
    Neg = fun(Sub) ->
        {imboy_codec:protocol_atom(Sub), imboy_codec:framing_atom(Sub)}
    end,
    ?assertEqual({json, none}, Neg(undefined)),
    ?assertEqual({json, none}, Neg(<<"text">>)),
    ?assertEqual({json, none}, Neg(<<"imboy-json">>)),
    ?assertEqual({protobuf, none}, Neg(<<"imboy-protobuf">>)),
    ?assertEqual({protobuf, v2}, Neg(<<"imboy.v2">>)),
    ?assertEqual({json, none}, Neg(<<"future-subprotocol-9">>)),

    %% 2) 服务端→客户端方向
    S2CMsg = four_channel_message(<<"S2C">>),
    S2CViews = channel_views(?FRAME_TYPE_MSG_S2C, S2CMsg),
    ?assertEqual(
        view_core(view_of(json_none, S2CViews)),
        view_core(view_of(pb_v2, S2CViews))
    ),

    %% 3) 客户端→服务端方向（同一请求身份/游标语义）
    C2SMsg = four_channel_message(<<"C2S">>),
    C2SViews = channel_views(?FRAME_TYPE_MSG_C2S, C2SMsg),
    ?assertEqual(
        view_core(view_of(json_none, C2SViews)),
        view_core(view_of(pb_v2, C2SViews))
    ),

    %% 4) 回退组合明确：控制帧（in_reply_to/reason 不可 pb 无损）
    Ctrl = #{
        <<"id">> => <<"msg-vc-ctrl-001">>,
        <<"type">> => <<"CLIENT_ACK_ERROR">>,
        <<"in_reply_to">> => <<"msg-vc-ctrl-001">>,
        <<"reason">> => <<"stale_did">>,
        <<"server_ts">> => 1785312537582
    },
    CtrlViews = channel_views(?FRAME_TYPE_MSG_S2C, Ctrl),
    lists:foreach(
        fun({Combo, Effective, Decoded, _FT}) ->
            ?assertEqual({Combo, json}, {Combo, Effective}),
            ?assertEqual(<<"CLIENT_ACK_ERROR">>, maps:get(<<"type">>, Decoded)),
            ?assertEqual(<<"msg-vc-ctrl-001">>, maps:get(<<"in_reply_to">>, Decoded)),
            ?assertEqual(<<"stale_did">>, maps:get(<<"reason">>, Decoded))
        end,
        CtrlViews
    ),
    ?assertEqual(
        view_core(view_of(json_none, CtrlViews)),
        view_core(view_of(pb_v2, CtrlViews))
    ).

%% @private 三条支持传输组合的解码视图。
%% 返回 [{Combo, 生效编码, 解码 map, v2 帧类型|undefined}]；v2 载荷按首字节
%% '{' 判 JSON 回退（与客户端分发规则一致），否则按 protobuf 解码。
channel_views(FrameType, Msg) ->
    Combos = [
        {json_none, json, none},
        {pb_none, protobuf, none},
        {pb_v2, protobuf, v2}
    ],
    lists:map(
        fun({Combo, Protocol, Framing}) ->
            Wire = imboy_codec:encode_ws_msg(Protocol, Framing, FrameType, Msg),
            {Effective, Payload, FrameTypeObserved} =
                case {Framing, Wire} of
                    {none, {text, T}} ->
                        {json, T, undefined};
                    {none, {binary, B}} ->
                        {protobuf, B, undefined};
                    {v2, {binary, B}} ->
                        {ok, Frame} = imboy_codec:unwrap_v2_frame(B),
                        P = imboy_frame:payload(Frame),
                        E =
                            case P of
                                <<16#7B, _/binary>> -> json;
                                _ -> protobuf
                            end,
                        {E, P, imboy_frame:type(Frame)}
                end,
            {Combo, Effective, imboy_codec:decode(Effective, Payload), FrameTypeObserved}
        end,
        Combos
    ).

view_core_fields() ->
    [
        <<"id">>,
        <<"type">>,
        <<"from">>,
        <<"to">>,
        <<"msg_type">>,
        <<"action">>,
        <<"e2ee">>,
        <<"payload">>,
        <<"created_at">>,
        <<"server_ts">>,
        <<"expire_secs">>,
        <<"conv_seq">>
    ].

view_core(Decoded0) ->
    Decoded = normalize_view_payload(Decoded0),
    lists:foldl(
        fun(F, M) -> M#{F => maps:get(F, Decoded, undefined)} end,
        #{},
        view_core_fields()
    ).

%% protobuf 通道的 map payload 会以 JSON binary 传输；归一化回 map 再比较
normalize_view_payload(#{<<"payload">> := P} = M) when is_binary(P), byte_size(P) > 0 ->
    try
        M#{<<"payload">> => jsone:decode(P, [{object_format, map}])}
    catch
        _:_ -> M
    end;
normalize_view_payload(M) ->
    M.

view_of(Combo, Views) ->
    [Decoded] = [D || {C, _, D, _} <- Views, C =:= Combo],
    Decoded.

four_channel_cases() ->
    [
        {<<"C2C">>, ?FRAME_TYPE_MSG_C2C, four_channel_message(<<"C2C">>)},
        {<<"C2G">>, ?FRAME_TYPE_MSG_C2G, four_channel_message(<<"C2G">>)},
        {<<"S2C">>, ?FRAME_TYPE_MSG_S2C, four_channel_message(<<"S2C">>)},
        {<<"C2S">>, ?FRAME_TYPE_MSG_C2S, four_channel_message(<<"C2S">>)}
    ].

%% @private 四通道同一操作的代表性消息：字段全集与 protobuf schema 白名单一致，
%% 保证三条传输组合都走真实编码路径（不触发整帧回退）。
four_channel_message(<<"C2C">>) ->
    base_four_channel_message(<<"C2C">>, <<"text">>, #{<<"body">> => <<"Hello IMBoy">>});
four_channel_message(<<"C2G">>) ->
    M = base_four_channel_message(
        <<"C2G">>, <<"text">>, #{<<"body">> => <<"Hello group">>}
    ),
    M#{<<"conv_seq">> := 42};
four_channel_message(<<"S2C">>) ->
    M = base_four_channel_message(
        <<"S2C">>, <<>>, #{<<"msg_ids">> => [<<"m-1">>, <<"m-2">>]}
    ),
    M#{<<"action">> := <<"message_read_sync">>};
four_channel_message(<<"C2S">>) ->
    M = base_four_channel_message(
        <<"C2S">>, <<>>, #{<<"cursors">> => #{<<"c2g:123">> => 7}}
    ),
    M#{<<"action">> := <<"sync">>}.

base_four_channel_message(Direction, MsgType, Payload) ->
    #{
        <<"id">> => <<"msg-four-channel-001">>,
        <<"type">> => Direction,
        <<"from">> => 123456789,
        <<"to">> => 987654321,
        <<"msg_type">> => MsgType,
        <<"action">> => <<>>,
        <<"e2ee">> => null,
        <<"payload">> => Payload,
        <<"created_at">> => 1710000000000,
        <<"server_ts">> => 0,
        <<"expire_secs">> => 0,
        <<"conv_seq">> => 0
    }.

%% @doc v2 frame unwrap 错误用例
v2_frame_unwrap_error() ->
    %% 魔数不对
    ?assertMatch({error, bad_magic}, imboy_codec:unwrap_v2_frame(<<"not-a-frame">>)),
    %% 不完整的 header
    ?assertMatch({error, incomplete_frame}, imboy_codec:unwrap_v2_frame(<<>>)).

%% @doc a2a_task_update 在 JSON 通道被识别并原样透传（无 proto 变更）
a2a_task_update_json_channel() ->
    %% JSON-only 内容类型识别
    ?assert(imboy_codec:is_json_channel_type(<<"a2a_task_update">>)),
    ?assert(imboy_codec:is_json_channel_type(<<"agent_task">>)),
    ?assert(imboy_codec:is_json_channel_type(<<"stream_delta">>)),
    ?assertNot(imboy_codec:is_json_channel_type(<<"text">>)),
    ?assertNot(imboy_codec:is_json_channel_type(<<"custom">>)),

    %% JSON 通道 msg_type 原样往返，不落 protobuf enum
    Msg = #{
        <<"id">> => <<"msg-a2a-001">>,
        <<"type">> => <<"C2G">>,
        <<"from">> => 123456789,
        <<"to">> => 987654321,
        <<"msg_type">> => <<"a2a_task_update">>,
        <<"action">> => <<>>,
        <<"e2ee">> => null,
        <<"payload">> => #{<<"task_id">> => <<"t-001">>, <<"status">> => <<"running">>},
        <<"created_at">> => 1710000000000
    },
    Encoded = imboy_codec:encode(json, Msg),
    ?assert(is_binary(Encoded)),
    Decoded = imboy_codec:decode(json, Encoded),
    ?assertEqual(<<"a2a_task_update">>, maps:get(<<"msg_type">>, Decoded)),
    ?assertEqual(
        <<"t-001">>,
        maps:get(<<"task_id">>, maps:get(<<"payload">>, Decoded))
    ).

%% @doc E2EE Olm 元数据 protobuf roundtrip（S14 fields 5-13）
e2ee_olm_roundtrip() ->
    Msg = test_e2ee_olm_message(),
    Encoded = imboy_codec:encode(protobuf, Msg),
    ?assert(is_binary(Encoded)),
    Decoded = imboy_codec:decode(protobuf, Encoded),

    E2EE = maps:get(<<"e2ee">>, Decoded),
    ?assert(is_map(E2EE)),
    %% 基础字段
    ?assertEqual(1, maps:get(<<"e2ee_ver">>, E2EE)),
    ?assertEqual(<<"OLM.V1">>, maps:get(<<"e2ee_suite">>, E2EE)),
    %% v2 Olm 扩展字段
    ?assertEqual(<<"olm">>, maps:get(<<"protocol">>, E2EE)),
    ?assertEqual(1, maps:get(<<"version">>, E2EE)),
    ?assertEqual(<<"111222333">>, maps:get(<<"peer_uid">>, E2EE)),
    ?assertEqual(<<"device-abc">>, maps:get(<<"peer_device_id">>, E2EE)),
    ?assertEqual(1, maps:get(<<"message_type">>, E2EE)),
    ?assertEqual(<<"sess-xyz-001">>, maps:get(<<"session_id">>, E2EE)),
    %% 不应有 Megolm 字段
    ?assertNot(maps:is_key(<<"message_index">>, E2EE)),
    ?assertNot(maps:is_key(<<"group_id">>, E2EE)).

%% @doc E2EE Megolm 元数据 protobuf roundtrip（S14 fields 5-13）
e2ee_megolm_roundtrip() ->
    Msg = test_e2ee_megolm_message(),
    Encoded = imboy_codec:encode(protobuf, Msg),
    ?assert(is_binary(Encoded)),
    Decoded = imboy_codec:decode(protobuf, Encoded),

    E2EE = maps:get(<<"e2ee">>, Decoded),
    ?assert(is_map(E2EE)),
    %% 基础字段
    ?assertEqual(1, maps:get(<<"e2ee_ver">>, E2EE)),
    ?assertEqual(<<"MEGOLM.V1">>, maps:get(<<"e2ee_suite">>, E2EE)),
    %% v2 Megolm 扩展字段
    ?assertEqual(<<"megolm">>, maps:get(<<"protocol">>, E2EE)),
    ?assertEqual(1, maps:get(<<"version">>, E2EE)),
    ?assertEqual(42, maps:get(<<"message_index">>, E2EE)),
    ?assertEqual(<<"group-999">>, maps:get(<<"group_id">>, E2EE)),
    ?assertEqual(<<"megolm-sess-001">>, maps:get(<<"session_id">>, E2EE)),
    %% 不应有 Olm 字段
    ?assertNot(maps:is_key(<<"peer_uid">>, E2EE)),
    ?assertNot(maps:is_key(<<"peer_device_id">>, E2EE)).

%%%===================================================================
%%% Test data helpers
%%%===================================================================

test_c2c_message() ->
    test_c2c_message(<<"text">>).

test_c2c_message(MsgType) ->
    #{
        <<"id">> => <<"msg-test-001">>,
        <<"type">> => <<"C2C">>,
        <<"from">> => 123456789,
        <<"to">> => 987654321,
        <<"msg_type">> => MsgType,
        <<"action">> => <<>>,
        <<"e2ee">> => null,
        <<"payload">> => #{<<"body">> => <<"Hello IMBoy">>},
        <<"created_at">> => 1710000000000,
        <<"server_ts">> => 0
    }.

test_e2ee_message() ->
    #{
        <<"id">> => <<"msg-e2ee-001">>,
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
                #{
                    <<"did">> => <<"device-001">>,
                    <<"kid">> => <<"key-001">>,
                    <<"wrap_alg">> => <<"RSA-OAEP-256">>,
                    <<"ek">> => <<"encrypted-key-bytes">>
                }
            ]
        },
        <<"payload">> => <<"encrypted-ciphertext-here">>,
        <<"created_at">> => 1710000000000
    }.

test_e2ee_olm_message() ->
    #{
        <<"id">> => <<"msg-olm-001">>,
        <<"type">> => <<"C2C">>,
        <<"from">> => 111222333,
        <<"to">> => 444555666,
        <<"msg_type">> => <<"e2ee">>,
        <<"action">> => <<"message">>,
        <<"e2ee">> => #{
            <<"e2ee_ver">> => 1,
            <<"e2ee_suite">> => <<"OLM.V1">>,
            <<"nonce">> => <<>>,
            <<"keys">> => [],
            <<"protocol">> => <<"olm">>,
            <<"version">> => 1,
            <<"peer_uid">> => <<"111222333">>,
            <<"peer_device_id">> => <<"device-abc">>,
            <<"message_type">> => 1,
            <<"session_id">> => <<"sess-xyz-001">>
        },
        <<"payload">> => <<"olm-ciphertext-binary">>,
        <<"created_at">> => 1710000000000
    }.

test_e2ee_megolm_message() ->
    #{
        <<"id">> => <<"msg-megolm-001">>,
        <<"type">> => <<"C2G">>,
        <<"from">> => 111222333,
        <<"to">> => 999888777,
        <<"msg_type">> => <<"e2ee">>,
        <<"action">> => <<"message">>,
        <<"e2ee">> => #{
            <<"e2ee_ver">> => 1,
            <<"e2ee_suite">> => <<"MEGOLM.V1">>,
            <<"nonce">> => <<>>,
            <<"keys">> => [],
            <<"protocol">> => <<"megolm">>,
            <<"version">> => 1,
            <<"message_index">> => 42,
            <<"group_id">> => <<"group-999">>,
            <<"session_id">> => <<"megolm-sess-001">>
        },
        <<"payload">> => <<"megolm-ciphertext-binary">>,
        <<"created_at">> => 1710000000000
    }.

test_chat_history_bundle_msg() ->
    test_chat_history_bundle_msg(<<"C2C">>).

test_chat_history_bundle_msg(Direction) ->
    #{
        <<"id">> => <<"msg-bundle-codec-001">>,
        <<"type">> => Direction,
        <<"from">> => 123456789,
        <<"to">> => 987654321,
        <<"msg_type">> => <<"chat_history_bundle">>,
        <<"action">> => <<>>,
        <<"e2ee">> => null,
        <<"payload">> => #{
            <<"bundle_version">> => 1,
            <<"entries">> => [
                #{
                    <<"msg_type">> => <<"text">>,
                    <<"payload">> => #{<<"text">> => <<"hello bundle">>}
                }
            ]
        },
        <<"created_at">> => 1710000000000
    }.

%% ===================================================================
%% BE-01：C-01 deviation_register 关闭断言
%% 顶层扩展字段不再被 to_pb_map 静默丢弃：白名单漏斗判不可无损 →
%% JSON 回退 / encode(protobuf) fail-closed 拒绝（ADR 15 同款）。
%% ===================================================================

toplevel_extra_fields_pb_fallback() ->
    Msg0 = test_c2c_message(),
    Msg = Msg0#{
        <<"sender_did">> => <<"did-123">>,
        <<"sender_dtype">> => 1,
        <<"topic_id">> => <<"topic-42">>
    },
    %% 漏斗判定（行为面）：直接 encode(protobuf) fail-closed 拒绝，
    %% 不产出裁剪帧（pb_lossless/1 为内部函数，经 encode/2 出口验证）
    ?assertEqual(<<>>, imboy_codec:encode(protobuf, Msg)),
    %% auto 通道：JSON 回退后全字段保留（none 与 v2 两种 framing）
    [
        begin
            Wire = imboy_codec:encode_ws_msg(protobuf, Framing, ?FRAME_TYPE_MSG_C2C, Msg),
            Json =
                case {Framing, Wire} of
                    {none, {text, Binary}} ->
                        Binary;
                    {v2, {binary, Binary}} ->
                        {ok, Frame} = imboy_codec:unwrap_v2_frame(Binary),
                        imboy_frame:payload(Frame)
                end,
            Decoded = jsone:decode(Json, [{object_format, map}]),
            ?assertEqual(<<"did-123">>, maps:get(<<"sender_did">>, Decoded)),
            ?assertEqual(1, maps:get(<<"sender_dtype">>, Decoded)),
            ?assertEqual(<<"topic-42">>, maps:get(<<"topic_id">>, Decoded))
        end
     || Framing <- [none, v2]
    ].

%% @doc 原 ctrl_fields 检查被顶层白名单覆盖的回归锚：
%% in_reply_to / reason 仍判不可无损并回退 JSON（client_ack_error 语义不变）。
ctrl_fields_pb_fallback_regression() ->
    Msg0 = test_c2c_message(),
    Msg = Msg0#{
        <<"type">> => <<"CLIENT_ACK_ERROR">>,
        <<"in_reply_to">> => <<"msg-ack-target">>,
        <<"reason">> => <<"rate_limited">>
    },
    ?assertEqual(<<>>, imboy_codec:encode(protobuf, Msg)),
    {binary, Frame} = imboy_codec:encode_ws_msg(protobuf, v2, ?FRAME_TYPE_MSG_C2C, Msg),
    {ok, Unwrapped} = imboy_codec:unwrap_v2_frame(Frame),
    Decoded = jsone:decode(imboy_frame:payload(Unwrapped), [{object_format, map}]),
    ?assertEqual(<<"msg-ack-target">>, maps:get(<<"in_reply_to">>, Decoded)),
    ?assertEqual(<<"rate_limited">>, maps:get(<<"reason">>, Decoded)),
    ?assertEqual(<<"CLIENT_ACK_ERROR">>, maps:get(<<"type">>, Decoded)).

%% @doc 垃圾 protobuf 字节：decode 落空 map 哨兵并告警（不崩溃、不投毒）。
garbage_pb_decode_sentinel() ->
    ?assertEqual(#{}, imboy_codec:decode(protobuf, <<"garbage-bytes-not-proto", 0, 1, 2>>)),
    %% 空二进制是合法 proto 帧全默认值（非错误路径）：解码不崩溃、字段为哨兵默认
    Defaults = imboy_codec:decode(protobuf, <<>>),
    ?assertEqual(<<>>, maps:get(<<"id">>, Defaults)),
    ?assertEqual(0, maps:get(<<"conv_seq">>, Defaults)).

%% @doc 外部服务器发来未知 MsgDirection 枚举：from_pb_map 落空串哨兵，
%% 上层按未知类型处置（router 记日志），不崩溃、不伪装成已知方向。
unknown_direction_enum_sentinel() ->
    PbBin = imboy_pb:encode_msg(
        #{
            id => <<"msg-unknown-enum">>,
            type => 'MSG_DIRECTION_UNSPECIFIED',
            from => 1,
            to => 2,
            payload => <<"hello">>
        },
        'IMBoyMessage'
    ),
    Decoded = imboy_codec:decode(protobuf, PbBin),
    ?assertEqual(<<"msg-unknown-enum">>, maps:get(<<"id">>, Decoded)),
    ?assertEqual(<<>>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"hello">>, maps:get(<<"payload">>, Decoded)).

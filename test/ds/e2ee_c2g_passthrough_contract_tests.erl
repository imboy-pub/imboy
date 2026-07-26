%%% S20 C2G E2EE 透传契约守护测试
%%%
%%% 不变量：群消息（C2G）的 e2ee 字段在服务端全链路中保持
%%% byte/semantic preserving —— 不解析、不修改、不裁剪。
%%%
%%% 覆盖路径：
%%%   1. decode_websocket_message（C2G 类型）
%%%   2. assemble_msg（C2G 类型）
%%%   3. encode_websocket_message 往返
%%%   4. 离线拉取两条路径格式一致性（修复 GAP 1 回归守护）
%%%   5. jsone:encode 字节稳定性
-module(e2ee_c2g_passthrough_contract_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Fixture: 模拟 Megolm 群加密消息的 e2ee map
%% ===================================================================

c2g_e2ee_map() ->
    #{
        <<"e2ee">> => true,
        <<"e2ee_ver">> => 1,
        <<"e2ee_suite">> => <<"MEGOLM.V1">>,
        <<"protocol">> => <<"megolm">>,
        <<"version">> => 1,
        <<"group_id">> => <<"group-session-xyz-789">>,
        <<"message_index">> => 17,
        <<"session_id">> => <<"megolm-sess-abc">>,
        <<"ciphertext">> => <<"bWVnb2xtLWNpcGhlcnRleHQtZm9yLWdyb3Vw">>,
        <<"unknown_future_field">> => <<"must-survive">>
    }.

c2g_ws_message_json() ->
    jsone:encode(#{
        <<"id">> => <<"msg-c2g-001">>,
        <<"type">> => <<"c2g">>,
        <<"from">> => <<"100">>,
        <<"to">> => <<"500">>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"message">>,
        <<"e2ee">> => c2g_e2ee_map(),
        <<"payload">> => #{<<"body">> => <<>>},
        <<"created_at">> => 1753500000000
    }).

%% ===================================================================
%% 测试 1: decode_websocket_message 保持 C2G e2ee map 不变
%% ===================================================================

c2g_decode_preserves_e2ee_test() ->
    Json = c2g_ws_message_json(),
    Decoded = message_ds:decode_websocket_message(Json),
    E2EE = maps:get(<<"e2ee">>, Decoded),
    ?assertEqual(c2g_e2ee_map(), E2EE),
    %% 未知字段也必须存活
    ?assertEqual(<<"must-survive">>, maps:get(<<"unknown_future_field">>, E2EE)).

%% ===================================================================
%% 测试 2: assemble_msg 保持 C2G e2ee map 不变
%% ===================================================================

c2g_assemble_preserves_e2ee_test() ->
    E2EE = c2g_e2ee_map(),
    Msg = message_ds:assemble_msg(
        <<"C2G">>,
        <<"100">>,
        <<"500">>,
        #{<<"body">> => <<>>},
        <<"msg-c2g-001">>,
        <<"text">>,
        <<"message">>,
        E2EE
    ),
    ?assertEqual(E2EE, maps:get(<<"e2ee">>, Msg)).

%% ===================================================================
%% 测试 3: decode → assemble → JSON 序列化往返 — e2ee 完整存活
%% ===================================================================

c2g_encode_roundtrip_test() ->
    %% 入站：客户端 WebSocket JSON
    Json = c2g_ws_message_json(),
    Decoded = message_ds:decode_websocket_message(Json),
    %% 服务端组装内部消息
    Assembled = message_ds:assemble_msg(
        <<"C2G">>,
        <<"100">>,
        <<"500">>,
        maps:get(<<"payload">>, Decoded),
        maps:get(<<"id">>, Decoded),
        maps:get(<<"msg_type">>, Decoded),
        maps:get(<<"action">>, Decoded, <<>>),
        maps:get(<<"e2ee">>, Decoded, null)
    ),
    %% 出站：序列化为 JSON 投递给接收方
    OutJson = jsone:encode(Assembled),
    OutDecoded = jsone:decode(OutJson, [{object_format, map}]),
    %% e2ee 字段完整保留（含未知字段）
    ?assertEqual(c2g_e2ee_map(), maps:get(<<"e2ee">>, OutDecoded)).

%% ===================================================================
%% 测试 4: 离线拉取格式一致性（GAP 1 回归守护）
%%
%% 模拟 DB 行中 e2ee 为 JSON binary（PostgreSQL JSONB 返回格式），
%% 验证 json_decode_field 双路径输出一致。
%% ===================================================================

c2g_offline_pull_format_consistency_test() ->
    %% 模拟 PostgreSQL 返回的 JSONB 行（e2ee 是 JSON binary）
    E2EEJson = jsone:encode(c2g_e2ee_map()),
    Row = #{
        <<"id">> => 1,
        <<"payload">> => jsone:encode(#{<<"body">> => <<"hi">>}),
        <<"from_id">> => 100,
        <<"to_id">> => 500,
        <<"created_at">> => <<"2025-07-25T10:00:00Z">>,
        <<"server_ts">> => 1753500000000,
        <<"msg_id">> => <<"msg-001">>,
        <<"msg_type">> => <<"text">>,
        <<"e2ee">> => E2EEJson
    },
    %% 两条路径都应用相同的 json_decode_field 链
    Decoded1 = elib_response:json_decode_field(
        elib_response:json_decode_field(Row, <<"payload">>), <<"e2ee">>
    ),
    %% 验证 e2ee 被正确反序列化为 map
    E2EE1 = maps:get(<<"e2ee">>, Decoded1),
    ?assert(is_map(E2EE1)),
    ?assertEqual(c2g_e2ee_map(), E2EE1),
    %% 验证 payload 也被正确反序列化
    ?assert(is_map(maps:get(<<"payload">>, Decoded1))).

%% ===================================================================
%% 测试 5: jsone:encode 字节稳定性 — 同一 e2ee map 多次编码结果一致
%% ===================================================================

c2g_jsone_byte_stability_test() ->
    E2EE = c2g_e2ee_map(),
    Msg1 = #{<<"e2ee">> => E2EE, <<"payload">> => #{<<"body">> => <<>>}},
    Msg2 = #{<<"e2ee">> => E2EE, <<"payload">> => #{<<"body">> => <<>>}},
    Encoded1 = jsone:encode(Msg1),
    Encoded2 = jsone:encode(Msg2),
    ?assertEqual(Encoded1, Encoded2),
    %% 解码后再编码仍然一致（roundtrip 稳定）
    Decoded = jsone:decode(Encoded1, [{object_format, map}]),
    ReEncoded = jsone:encode(Decoded),
    ?assertEqual(Encoded1, ReEncoded).

%% ===================================================================
%% 测试 6: null e2ee 在 C2G 路径中保持 null（非加密群消息）
%% ===================================================================

c2g_null_e2ee_passthrough_test() ->
    Json = jsone:encode(#{
        <<"id">> => <<"msg-c2g-plain">>,
        <<"type">> => <<"c2g">>,
        <<"from">> => <<"100">>,
        <<"to">> => <<"500">>,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"message">>,
        <<"payload">> => #{<<"body">> => <<"hello group">>},
        <<"created_at">> => 1753500000000
    }),
    Decoded = message_ds:decode_websocket_message(Json),
    %% 无 e2ee 字段时应为 null
    E2EE = maps:get(<<"e2ee">>, Decoded, null),
    ?assertEqual(null, E2EE).

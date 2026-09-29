-module(message_envelope_id_contract_tests).

%%%
%%% S0 协议契约测试：JSON 信封中的 ID 字段（from/to/group_id）一律为
%%% binary（JSON string）。
%%%
%%% 背景：DB bigint 出来的 integer 直传信封会被 jsone 编码成 JSON
%%% number，客户端按 String 比较身份（data['from'] == currentUid），
%%% int 与 String 恒不等——曾导致自己发的群消息被误判为他人消息
%%% （未读/@ 提醒误计、给自己弹通知）。
%%%
%%% 本测试锁定三个纯函数出口的归一化行为：
%%%   1. message_ds:envelope_id_to_binary/1     —— 集中归一化工具
%%%   2. message_ds:encode_websocket_message/1  —— WS 统一编码出口
%%%      （offline_envelope 离线重放信封经此出口）
%%%   3. messaging_logic:encode_history_msg/2 与 process_message/1
%%%      —— 历史消息 HTTP API 出口
%%%
%%% 实时推送出口（msg_c2c_logic / msg_c2g_logic 的 ec_cnv:to_binary）
%%% 依赖 DB 流程无法纯函数直测，由 e2ee_safety_contract_tests 等
%%% 集成用例覆盖。
%%%

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% envelope_id_to_binary/1
%% ===================================================================

envelope_id_integer_to_binary_test() ->
    ?assertEqual(<<"50">>, message_ds:envelope_id_to_binary(50)),
    %% TSID 量级大整数同样转换
    ?assertEqual(<<"7234017284046690048">>,
                 message_ds:envelope_id_to_binary(7234017284046690048)).

envelope_id_binary_identity_test() ->
    ?assertEqual(<<"50">>, message_ds:envelope_id_to_binary(<<"50">>)).

envelope_id_passthrough_test() ->
    %% null/undefined 等非 ID 值原样返回，调用方无需 guard
    ?assertEqual(null, message_ds:envelope_id_to_binary(null)),
    ?assertEqual(undefined, message_ds:envelope_id_to_binary(undefined)).

%% ===================================================================
%% encode_websocket_message/1（WS 统一出口）
%% ===================================================================

encode_websocket_db_row_ids_test() ->
    %% DB 格式（from_id/to_id 为 bigint integer）→ 信封 from/to 必须是 binary
    Msg = message_ds:encode_websocket_message(#{
        <<"id">> => <<"m1">>,
        <<"type">> => <<"C2C">>,
        <<"from_id">> => 50,
        <<"to_id">> => 4,
        <<"msg_type">> => <<"text">>,
        <<"payload">> => #{<<"text">> => <<"hi">>}
    }),
    ?assertEqual(<<"50">>, maps:get(<<"from">>, Msg)),
    ?assertEqual(<<"4">>, maps:get(<<"to">>, Msg)),
    %% 编码后必须是 JSON string 而非 number：decode 回来若曾是 number，
    %% 会得到 integer 50，与 <<"50">> 不等
    Decoded = jsone:decode(jsone:encode(Msg)),
    ?assertEqual(<<"50">>, maps:get(<<"from">>, Decoded)),
    ?assertEqual(<<"4">>, maps:get(<<"to">>, Decoded)).

encode_websocket_internal_ids_test() ->
    %% 内部格式已带 from（binary）→ 保持不变
    Msg = message_ds:encode_websocket_message(#{
        <<"id">> => <<"m2">>,
        <<"type">> => <<"C2C">>,
        <<"from">> => <<"50">>,
        <<"to">> => <<"4">>,
        <<"msg_type">> => <<"text">>,
        <<"payload">> => #{<<"text">> => <<"hi">>}
    }),
    ?assertEqual(<<"50">>, maps:get(<<"from">>, Msg)),
    ?assertEqual(<<"4">>, maps:get(<<"to">>, Msg)).

%% ===================================================================
%% messaging_logic 历史消息出口
%% ===================================================================

history_msg_from_id_normalized_test() ->
    Row = #{
        <<"id">> => <<"m3">>,
        <<"from_id">> => 50,
        <<"to_id">> => 4,
        <<"payload">> => <<"{\"text\":\"hi\"}">>,
        <<"e2ee">> => null
    },
    Encoded = messaging_logic:encode_history_msg(4, Row),
    ?assertEqual(<<"50">>, maps:get(<<"from">>, Encoded)),
    ?assertEqual(<<"4">>, maps:get(<<"to">>, Encoded)),
    ?assertEqual(error, maps:find(<<"from_id">>, Encoded)).

history_msg_group_id_normalized_test() ->
    Row = #{
        <<"id">> => <<"m4">>,
        <<"from_id">> => 50,
        <<"group_id">> => 7,
        <<"to_id">> => null,
        <<"payload">> => null,
        <<"e2ee">> => null
    },
    Encoded = messaging_logic:encode_history_msg(7, Row),
    ?assertEqual(<<"7">>, maps:get(<<"group_id">>, Encoded)).

process_message_ids_normalized_test() ->
    Msg = messaging_logic:process_message(#{
        <<"id">> => <<"m5">>,
        <<"from_id">> => 50,
        <<"to_id">> => 4,
        <<"payload">> => #{}
    }),
    ?assertEqual(<<"50">>, maps:get(<<"from">>, Msg)),
    ?assertEqual(<<"4">>, maps:get(<<"to">>, Msg)),
    ?assertEqual(error, maps:find(<<"from_id">>, Msg)),
    ?assertEqual(error, maps:find(<<"to_id">>, Msg)).

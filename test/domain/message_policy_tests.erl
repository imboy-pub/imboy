%%% @doc message_policy 纯函数 eunit 测试（零 mock）。
%%% 验证「可发/暂存/构建」决策与 msg_c2c_logic 现有语义逐字对齐。
-module(message_policy_tests).

-include_lib("eunit/include/eunit.hrl").

%% ---- send_decision（可发）----

%% 好友且未拉黑 → 放行。
send_allow_test() ->
    ?assertEqual(allow, message_policy:send_decision(true, false)).

%% 整型黑名单计数 > 0 → 拒绝（in_denylist）。
send_denylist_integer_test() ->
    ?assertEqual({reject, in_denylist}, message_policy:send_decision(false, 1)),
    ?assertEqual({reject, in_denylist}, message_policy:send_decision(true, 2)).

%% 布尔 true 黑名单 → 拒绝（true > 0 成立）。
send_denylist_bool_true_test() ->
    ?assertEqual(
        {reject, in_denylist},
        message_policy:send_decision(false, true)
    ).

%% 修正：{false,false}（非好友且未拉黑）→ not_a_friend（原 `false > 0` quirk 已修）。
send_false_false_not_a_friend_test() ->
    ?assertEqual(
        {reject, not_a_friend},
        message_policy:send_decision(false, false)
    ).

%% 拉黑优先：好友但已拉黑（布尔 true）→ in_denylist。
send_friend_but_blocked_test() ->
    ?assertEqual(
        {reject, in_denylist},
        message_policy:send_decision(true, true)
    ).

%% ---- encode_payload（构建）----

encode_map_to_json_test() ->
    Json = message_policy:encode_payload(#{<<"body">> => <<"hi">>}),
    ?assert(is_binary(Json)),
    ?assertEqual(
        #{<<"body">> => <<"hi">>},
        jsone:decode(Json, [{object_format, map}])
    ).

encode_binary_passthrough_test() ->
    ?assertEqual(<<"raw">>, message_policy:encode_payload(<<"raw">>)).

%% ---- reply_mode（暂存）----

reply_none_test() ->
    ?assertEqual(none, message_policy:reply_mode({<<>>, 0, <<>>})).

reply_present_test() ->
    ?assertEqual(
        {reply, <<"m1">>, 42, <<"snip">>},
        message_policy:reply_mode({<<"m1">>, 42, <<"snip">>})
    ).

%% ---- build_server_ack（构建）----

server_ack_shape_test() ->
    Ack = message_policy:build_server_ack(<<"m1">>, 1700000000000),
    ?assertEqual(<<"m1">>, maps:get(<<"id">>, Ack)),
    ?assertEqual(<<"C2C_SERVER_ACK">>, maps:get(<<"type">>, Ack)),
    ?assertEqual(1700000000000, maps:get(<<"server_ts">>, Ack)).

%% 【T15/R9】响应显式标注被响应的请求 id；字段名避开引用回复占用的 reply_to
server_ack_carries_in_reply_to_test() ->
    Ack = message_policy:build_server_ack(<<"m1">>, 1700000000000),
    ?assertEqual(<<"m1">>, maps:get(<<"in_reply_to">>, Ack)),
    ?assertEqual(false, maps:is_key(<<"reply_to">>, Ack)).

plaintext_text_size_test_() ->
    Boundaries = [{65535, ok}, {65536, ok}, {65537, {error, <<"message_text_too_large">>}}],
    Shapes = [payload_map, payload_json, envelope_json, edit_json, legacy_body, raw_text],
    [
        {atom_to_list(Shape) ++ "_" ++ integer_to_list(Size), fun() ->
            Text = boundary_text(Size),
            ?assertEqual(Size, byte_size(Text)),
            {Action, Payload} = text_payload(Shape, Text),
            ?assertEqual(Expected, message_policy:validate_text_size(<<"text">>, Action, Payload, false))
        end}
     || {Size, Expected} <- Boundaries, Shape <- Shapes
    ].

plaintext_text_size_ignores_json_escape_overhead_test() ->
    Text = boundary_text(65536),
    Payload = jsone:encode(#{<<"text">> => Text}, [native_utf8]),
    ?assert(byte_size(Payload) > 65536),
    ?assertEqual(ok, message_policy:validate_text_size(<<"text">>, <<>>, Payload, false)),
    ?assertEqual(Text, maps:get(<<"text">>, jsone:decode(Payload))).

plaintext_text_size_leaves_other_message_types_and_actions_test() ->
    Payload = #{<<"text">> => boundary_text(65537)},
    ?assertEqual(ok, message_policy:validate_text_size(<<"image">>, <<>>, Payload, false)),
    ?assertEqual(ok, message_policy:validate_text_size(<<"file">>, <<>>, Payload, false)),
    ?assertEqual(ok, message_policy:validate_text_size(<<"text">>, <<"message_revoke">>, Payload, false)).

plaintext_text_size_does_not_drop_malformed_or_numeric_raw_text_test() ->
    lists:foreach(fun(Payload) ->
        ?assertEqual({error, <<"message_text_too_large">>},
            message_policy:validate_text_size(<<"text">>, <<>>, Payload, false))
    end, [binary:copy(<<"1">>, 65537), <<"{invalid", (binary:copy(<<"a">>, 65537))/binary>>]).

plaintext_text_size_preserves_raw_numbers_and_json_wrapper_whitespace_test() ->
    ?assertEqual(ok, message_policy:validate_text_size(
        <<"text">>, <<>>, binary:copy(<<"1">>, 65536), false)),
    Json = jsone:encode(#{<<"text">> => boundary_text(65536)}, [native_utf8]),
    ?assertEqual(ok, message_policy:validate_text_size(
        <<"text">>, <<>>, <<" \r\n\t", Json/binary>>, false)).

plaintext_text_size_cannot_hide_an_oversized_legacy_field_test() ->
    Payload = #{<<"text">> => <<"small">>, <<"content">> => boundary_text(65537),
        <<"payload">> => #{<<"text">> => <<"small">>}},
    ?assertEqual({error, <<"message_text_too_large">>},
        message_policy:validate_text_size(<<"text">>, <<>>, Payload, false)).

plaintext_text_size_rejects_malformed_visible_fields_test_() ->
    BadTexts = [{"integer", 7}, {"null", null}, {"list", [1, 2]}, {"invalid_utf8", <<255>>}],
    [{binary_to_list(Key) ++ "_" ++ Name ++ "_" ++ atom_to_list(Encrypted), fun() ->
        ?assertEqual({error, <<"invalid_message_format">>},
            message_policy:validate_text_size(<<"text">>, <<>>, #{Key => Value}, Encrypted))
    end} || Key <- [<<"text">>, <<"content">>], {Name, Value} <- BadTexts,
        Encrypted <- [false, true]].

plaintext_text_size_rejects_invalid_utf8_raw_body_test() ->
    ?assertEqual({error, <<"invalid_message_format">>},
        message_policy:validate_text_size(<<"text">>, <<>>, <<255>>, false)).

plaintext_text_size_preserves_opaque_encrypted_body_test() ->
    Opaque = binary:copy(<<"x">>, 65537),
    ?assertEqual(ok, message_policy:validate_text_size(<<"text">>, <<>>, Opaque, true)),
    ?assertEqual(ok, message_policy:validate_text_size(
        <<"text">>, <<>>, #{<<"ciphertext">> => Opaque}, true)),
    ?assertEqual(ok, message_policy:validate_text_size(
        <<"text">>, <<>>, #{<<"body">> => Opaque}, true)),
    ?assertEqual({error, <<"message_text_too_large">>},
        message_policy:validate_text_size(<<"text">>, <<>>, #{<<"text">> => Opaque}, true)).

boundary_text(Size) ->
    Prefix = <<"中👩‍💻\n"/utf8>>,
    ?assertEqual(15, byte_size(Prefix)),
    <<Prefix/binary, (binary:copy(<<"a">>, Size - 15))/binary>>.

text_payload(payload_map, Text) -> {<<>>, #{<<"text">> => Text}};
text_payload(payload_json, Text) ->
    {<<>>, jsone:encode(#{<<"text">> => Text}, [native_utf8])};
text_payload(envelope_json, Text) ->
    {<<>>, jsone:encode(#{<<"type">> => <<"C2G">>, <<"msg_type">> => <<"text">>,
        <<"meta">> => binary:copy(<<"z">>, 1024), <<"payload">> => #{<<"text">> => Text}}, [native_utf8])};
text_payload(edit_json, Text) ->
    {<<"message_edit">>, jsone:encode(#{<<"content">> => Text}, [native_utf8])};
text_payload(legacy_body, Text) -> {<<>>, #{<<"body">> => Text}};
text_payload(raw_text, Text) -> {<<>>, Text}.

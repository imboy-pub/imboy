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

%% 语义留痕：{false,false} 因 `false > 0`=:=true 落入 in_denylist（镜像现状）。
send_false_false_mirrors_quirk_test() ->
    ?assertEqual(
        {reject, in_denylist},
        message_policy:send_decision(false, false)
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

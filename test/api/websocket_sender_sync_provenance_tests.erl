-module(websocket_sender_sync_provenance_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("imboy_frame.hrl").

actual_websocket_input_controls_plain_sender_sync_test_() ->
    [
        {atom_to_list(Name), sender_sync_case(Format, Kind, Recipients, Echoes)}
     || {Name, Format, Kind, Recipients, Echoes} <- [
        {current_json_null, json_v2, explicit_null, 1, 1},
        {json_missing_is_not_plain_authority, json_v2, missing, 1, 0},
        {json_binary_marker_cannot_authorize, json_v2, forged_marker, 1, 0},
        {legacy_json_defaults_cannot_authorize, json_v2, legacy, 0, 0},
        {current_protobuf_none, protobuf_v2, missing, 1, 1},
        {text_json_missing_is_not_plain_authority, json_text, missing, 1, 0},
        {authenticated_sender_overrides_json_fields, json_v2, spoofed_sender, 1, 1}
    ]
    ].

sender_sync_case(Format, Kind, Recipients, Echoes) ->
    ?WITH_MECKS(
        [
            {elib_log, [{internal_log, 4, fun(_, _, _, _) -> ok end}, {internal_log, 5, fun(_, _, _, _, _) -> ok end}]},
            {auth_ds, [{current_uid, 1, fun(_) -> 100 end}]},
            {throttle, [{check, 2, fun(_, _) -> ok end}]},
            {friend_ds, [{check_relationship, 2, fun(200, 100) -> {true, false} end}]},
            {ai_agent_ds, [{is_agent, 1, fun(200) -> false end}]},
            {bot_ds, [{is_bot, 1, fun(_) -> false end}]},
            {msg_store_ds, [
                {stage_with_sequence, 13, fun(_, _, _, _, _, _, 100, 200, _, _, _, _, _) ->
                    {ok, new, 7}
                end},
                {enqueue, 3, fun(_, _, _) -> ok end}
            ]},
            {elib_async, [
                {async_retry, 3, fun(F, 3, 1000) -> F(), ok end},
                {async, 1, fun(F) -> F(), self() end}
            ]},
            {push_notification_logic, [{maybe_push_for_c2c, 4, fun(_, _, _, _) -> ok end}]},
            {elib_retry_config, [{intervals, 1, fun(<<"c2c">>) -> [0] end}]},
            {imboy_message_helper, [
                {encode_and_send, 4, fun(200, <<"source-bound-mid">>, Msg, <<"c2c">>) ->
                    assert_outbound_envelope(Msg),
                    ok
                end}
            ]},
            {message_ds, [
                {send_next, 6, fun(
                    100, <<"source-bound-mid">>, Bytes, [0], [<<"native-a">>], false
                ) ->
                    assert_outbound_envelope(jsone:decode(Bytes)),
                    ok
                end}
            ]}
        ],
        fun() ->
            State = #{current_uid => 100, did => <<"native-a">>, dtype => <<"macos">>,
                      protocol => protobuf, framing => v2},
            Wire = wire_message(Kind),
            {Frame, ActualState} = case Format of
                json_v2 ->
                    {{binary, imboy_frame:encode(?FRAME_TYPE_MSG_C2C, 0, jsone:encode(Wire))}, State};
                protobuf_v2 ->
                    Payload = imboy_codec:encode_payload(protobuf, <<"text">>, #{body => <<"owned-test">>}),
                    Pb = imboy_codec:encode(protobuf, Wire#{<<"payload">> => Payload}),
                    {{binary, imboy_frame:encode(?FRAME_TYPE_MSG_C2C, 0, Pb)}, State};
                json_text ->
                    {{text, jsone:encode(Wire)}, State#{protocol => json, framing => none}}
            end,
            Result = websocket_handler:websocket_handle(Frame, ActualState),
            case Recipients of
                1 -> ?assertMatch({ok, ActualState, hibernate}, Result);
                0 -> ?assertMatch({reply, _, ActualState, hibernate}, Result)
            end,
            ?assertEqual(Recipients, meck:num_calls(imboy_message_helper, encode_and_send, 4)),
            ?assertEqual(Echoes, meck:num_calls(message_ds, send_next, 6)),
            %% Consume the server ACK created by the real C2C pipeline.
            receive {reply, _} -> ok after 0 -> ok end
        end
    ).

wire_message(Kind) ->
    Base = #{<<"id">> => <<"source-bound-mid">>, <<"type">> => <<"C2C">>,
             <<"from">> => 100, <<"to">> => 200, <<"msg_type">> => <<"text">>,
             <<"action">> => <<>>, <<"payload">> => #{<<"text">> => <<"owned-test">>},
             <<"created_at">> => 1700000000123},
    case Kind of
        explicit_null -> Base#{<<"e2ee">> => null};
        missing -> Base;
        forged_marker -> Base#{<<"sender_sync_plain_eligible">> => true};
        spoofed_sender -> Base#{<<"e2ee">> => null, <<"from">> => 999,
                                <<"sender_did">> => <<"forged-device">>};
        legacy -> (maps:without([<<"msg_type">>, <<"action">>], Base))#{
            <<"payload">> => #{<<"text">> => <<"owned-test">>, <<"msg_type">> => <<"text">>}
        }
    end.

assert_outbound_envelope(Msg) ->
    ?assertEqual(100, maps:get(<<"from">>, Msg)),
    ?assertEqual(200, ec_cnv:to_integer(maps:get(<<"to">>, Msg))),
    ?assertEqual(<<"native-a">>, maps:get(<<"sender_did">>, Msg)),
    ?assertEqual(null, maps:get(<<"e2ee">>, Msg)),
    ?assertEqual(7, maps:get(<<"conv_seq">>, Msg)),
    ?assertNot(maps:is_key(sender_sync_plain_eligible, Msg)),
    ?assertNot(maps:is_key(<<"sender_sync_plain_eligible">>, Msg)).

raw_json_provenance_precedes_compatibility_defaults_test() ->
    lists:foreach(fun({Wire, Expected}) ->
        Decoded = message_ds:decode_websocket_message(jsone:encode(Wire)),
        ?assertEqual(Expected, maps:get(sender_sync_plain_eligible, Decoded)),
        ?assertNot(maps:is_key(<<"sender_sync_plain_eligible">>, Decoded))
    end, [
        {wire_message(explicit_null), true},
        {wire_message(missing), false},
        {wire_message(forged_marker), false},
        {(wire_message(legacy))#{<<"e2ee">> => null}, false},
        {(wire_message(explicit_null))#{<<"e2ee">> => #{<<"version">> => 2}}, false},
        {(wire_message(explicit_null))#{<<"e2ee">> => <<"unknown-legacy">>}, false}
    ]).

-module(client_ack_persistence_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

storage_failure_preserves_retry_and_correlated_error_test_() ->
    [
        ws_case(Mode, Result)
     || Mode <- [text, v2, protobuf],
        Result <- [{error, <<"ack_persistence_failed">>}, raised]
    ].

ws_case(Mode, Result) ->
    ?WITH_MECKS(
        [
            {auth_ds, [{current_uid, 1, fun(_) -> 123 end}]},
            {elib_log, [
                {internal_log, 4, fun(_, _, _, _) -> ok end},
                {internal_log, 5, fun(_, _, _, _, _) -> ok end}
            ]},
            {websocket_logic, [{cancel_timer, 3, fun(_, _, _) -> error(cancelled_on_failure) end}]},
            {msg_c2c_logic, [
                {c2c_client_ack, 3, fun(_, _, _) ->
                    case Result of
                        raised -> error(storage_unavailable);
                        _ -> Result
                    end
                end}
            ]}
        ],
        fun() ->
            State0 = #{did => <<"did">>, current_uid => 123},
            State =
                case Mode of
                    text -> State0;
                    v2 -> State0#{framing => v2, protocol => protobuf};
                    protobuf -> State0#{protocol => protobuf}
                end,
            {reply, {_Kind, Bin}, _, hibernate} = websocket_handler:websocket_handle(
                input(Mode), State
            ),
            Json =
                case Mode of
                    v2 ->
                        {ok, Frame} = imboy_codec:unwrap_v2_frame(Bin),
                        imboy_frame:payload(Frame);
                    _ ->
                        Bin
                end,
            Msg = jsone:decode(Json),
            ?assertEqual(<<"CLIENT_ACK_ERROR">>, maps:get(<<"type">>, Msg)),
            ?assertEqual(<<"mid">>, maps:get(<<"id">>, Msg)),
            ?assertEqual(<<"mid">>, maps:get(<<"in_reply_to">>, Msg)),
            ?assertEqual(<<"ack_persistence_failed">>, maps:get(<<"reason">>, Msg)),
            ?assertEqual(0, meck:num_calls(websocket_logic, cancel_timer, 3))
        end
    ).

input(text) ->
    {text, <<"CLIENT_ACK,C2C,mid,did">>};
input(v2) ->
    {binary, imboy_codec:wrap_v2_frame(16#22, 0, <<"CLIENT_ACK,C2C,mid,did">>)};
input(protobuf) ->
    Payload = imboy_pb:encode_msg(
        #{
            msg_direction => 'C2C',
            msg_id => <<"mid">>,
            did => <<"did">>
        },
        'PayloadClientAck'
    ),
    {binary,
        imboy_codec:encode(protobuf, #{
            <<"id">> => <<"mid">>,
            <<"type">> => <<"CLIENT_ACK">>,
            <<"msg_type">> => <<"client_ack">>,
            <<"payload">> => Payload
        })}.

logic_failure_has_no_success_metric_test_() ->
    [
        logic_case(Type, Fun)
     || {Type, Fun} <-
            [{<<"c2c">>, ack_c2c_msg}, {<<"s2c">>, ack_s2c_msg}]
    ].

logic_case(Type, Fun) ->
    ?WITH_MECKS(
        [
            {msg_operation_ds, [
                {Fun, 3, fun(_, _, _) -> {error, <<"ack_persistence_failed">>} end}
            ]},
            {elib_metric, [{increment, 1, fun(_) -> error(success_metric_on_failure) end}]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"ack_persistence_failed">>},
                msg_ack_logic:client_ack(Type, <<"mid">>, 123, <<"did">>)
            ),
            ?assertEqual(0, meck:num_calls(elib_metric, increment, 1))
        end
    ).

offline_failure_is_not_reported_as_processed_test_() ->
    [
        offline_case(Type, Fun)
     || {Type, Fun} <-
            [{<<"c2c">>, ack_c2c_batch}, {<<"s2c">>, ack_s2c_batch}]
    ].

offline_case(Type, Fun) ->
    ?WITH_MECK(
        msg_operation_ds,
        [{Fun, 3, fun(_, _, _) -> {error, <<"ack_persistence_failed">>} end}],
        fun() ->
            ?assertEqual(
                {error, <<"ack_persistence_failed">>},
                messaging_logic:offline_ack(123, Type, [<<"mid">>], <<"did">>)
            )
        end
    ).

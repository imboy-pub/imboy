-module(talk_record_card_codec_tests).
-include_lib("eunit/include/eunit.hrl").
-include("imboy_frame.hrl").

card_transport_test_() ->
    [
        ?_test(card_transport(Direction, Framing))
     || Direction <- [<<"C2C">>, <<"C2G">>], Framing <- [none, v2]
    ].

card_transport(Direction, Framing) ->
    Payload = #{
        <<"card_type">> => <<"talk_record_card">>,
        <<"record_id">> => 9001,
        <<"title">> => <<"meeting">>
    },
    Msg = #{
        <<"id">> => <<"synthetic-card-1">>,
        <<"type">> => Direction,
        <<"msg_type">> => <<"talk_record_card">>,
        <<"from">> => 1,
        <<"to">> => 2,
        <<"payload">> => Payload
    },
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
    ?assertEqual(Msg, jsone:decode(Json, [{object_format, map}])).

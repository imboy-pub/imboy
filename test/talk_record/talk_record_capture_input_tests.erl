-module(talk_record_capture_input_tests).
-include_lib("eunit/include/eunit.hrl").
-define(UUID, <<"11111111-2222-4333-8444-555555555555">>).

capture_defaults_test() ->
    ?assertEqual(
        {ok, #{device_session_id => ?UUID, device_id => <<"desktop">>}},
        talk_record_logic:parse_capture_create(#{
            <<"device_session_id">> => ?UUID, <<"device_id">> => <<"desktop">>
        })
    ).
capture_ascii_boundary_test() ->
    Base = #{<<"device_session_id">> => ?UUID},
    ?assertMatch(
        {ok, _},
        talk_record_logic:parse_capture_create(Base#{<<"device_id">> => binary:copy(<<"a">>, 128)})
    ),
    ?assertEqual(
        {error, invalid_device_id},
        talk_record_logic:parse_capture_create(Base#{<<"device_id">> => binary:copy(<<"a">>, 129)})
    ),
    ?assertEqual(
        {error, invalid_device_id},
        talk_record_logic:parse_capture_create(Base#{<<"device_id">> => <<"中"/utf8>>})
    ).
reported_at_contract_test() ->
    Base = #{<<"device_session_id">> => ?UUID, <<"device_id">> => <<"d">>},
    ?assertEqual(
        {ok, #{
            device_session_id => ?UUID,
            device_id => <<"d">>,
            reported_at => <<"opaque-existing-contract">>
        }},
        talk_record_logic:parse_capture_create(Base#{
            <<"reported_at">> => <<"opaque-existing-contract">>
        })
    ),
    ?assertEqual(
        {error, invalid_reported_at},
        talk_record_logic:parse_capture_create(Base#{<<"reported_at">> => binary:copy(<<"a">>, 65)})
    ).
event_normalization_test() ->
    ?assertEqual(
        {ok, #{
            client_event_id => ?UUID,
            device_session_id => ?UUID,
            seq => 0,
            event => <<"pause">>,
            position_ms => 42
        }},
        talk_record_logic:parse_event(event(#{<<"seq">> => <<"0">>, <<"position_ms">> => <<"42">>}))
    ).
event_error_precedence_test() ->
    ?assertEqual({error, invalid_client_event_id}, talk_record_logic:parse_event(#{})),
    ?assertEqual(
        {error, invalid_event_seq},
        talk_record_logic:parse_event(event(#{<<"seq">> => -1, <<"event">> => <<"bad">>}))
    ),
    ?assertEqual(
        {error, invalid_event}, talk_record_logic:parse_event(event(#{<<"event">> => <<"bad">>}))
    ).
part_normalization_test() ->
    ?assertEqual(
        {ok, #{
            seq => 0,
            start_ms => 10,
            end_ms => 20,
            capture_offset_ms => 10,
            sha256 => binary:copy(<<"a">>, 64),
            byte_size => 5,
            codec => <<"aac">>,
            sample_rate => 48000,
            channels => 1
        }},
        talk_record_logic:parse_audio_part(
            part(
                #{<<"seq">> => <<"0">>, <<"sample_rate">> => 48000, <<"channels">> => 1}
            )
        )
    ).
part_time_axis_test() ->
    ?assertEqual(
        {error, invalid_part_time_axis},
        talk_record_logic:parse_audio_part(part(#{<<"end_ms">> => 10}))
    ),
    ?assertEqual(
        {error, invalid_capture_offset},
        talk_record_logic:parse_audio_part(part(#{<<"capture_offset_ms">> => -1}))
    ),
    ?assertMatch(
        {ok, #{capture_offset_ms := 0}},
        talk_record_logic:parse_audio_part(part(#{<<"capture_offset_ms">> => <<"0">>}))
    ).
part_optional_contract_test() ->
    {ok, P} = talk_record_logic:parse_audio_part(
        part(#{<<"sample_rate">> => 0, <<"channels">> => <<"1">>})
    ),
    ?assertEqual(false, maps:is_key(sample_rate, P)),
    ?assertEqual(false, maps:is_key(channels, P)),
    ?assertEqual(
        {error, invalid_codec},
        talk_record_logic:parse_audio_part(part(#{<<"codec">> => binary:copy(<<"a">>, 33)}))
    ),
    ?assertEqual(
        {error, invalid_byte_size},
        talk_record_logic:parse_audio_part(part(#{<<"byte_size">> => <<"5">>}))
    ).
confirm_receipt_contract_test() ->
    ?assertEqual({ok, #{}}, talk_record_logic:parse_confirm_receipt(#{})),
    Sha = binary:copy(<<"A">>, 64),
    ?assertEqual(
        {ok, #{sha256 => Sha, byte_size => 5}},
        talk_record_logic:parse_confirm_receipt(#{<<"sha256">> => Sha, <<"byte_size">> => 5})
    ),
    ?assertEqual(
        {error, invalid_confirm_receipt},
        talk_record_logic:parse_confirm_receipt(#{<<"byte_size">> => 0})
    ).
session_and_abandon_test() ->
    ?assertEqual(
        {ok, #{device_session_id => ?UUID}},
        talk_record_logic:parse_device_session(#{<<"device_session_id">> => ?UUID})
    ),
    ?assertEqual(
        {error, abandon_confirm_required},
        talk_record_logic:parse_abandon(#{<<"confirm">> => false})
    ),
    ?assertEqual({ok, #{}}, talk_record_logic:parse_abandon(#{<<"confirm">> => true})).
non_map_inputs_test() ->
    lists:foreach(
        fun(F) -> ?assertEqual({error, invalid_body}, apply(talk_record_logic, F, [undefined])) end,
        [
            parse_capture_create,
            parse_event,
            parse_audio_part,
            parse_confirm_receipt,
            parse_device_session,
            parse_abandon
        ]
    ).
event_enumeration_test() ->
    ?assertEqual(
        [<<"start">>, <<"pause">>, <<"resume">>, <<"interrupt">>, <<"end">>],
        talk_record_logic:valid_events()
    ).

event(Extra) ->
    maps:merge(
        #{
            <<"client_event_id">> => ?UUID,
            <<"device_session_id">> => ?UUID,
            <<"seq">> => 0,
            <<"event">> => <<"pause">>,
            <<"position_ms">> => 0
        },
        Extra
    ).
part(Extra) ->
    maps:merge(
        #{
            <<"seq">> => 0,
            <<"start_ms">> => 10,
            <<"end_ms">> => 20,
            <<"sha256">> => binary:copy(<<"A">>, 64),
            <<"byte_size">> => 5,
            <<"codec">> => <<"aac">>
        },
        Extra
    ).

part_codec_rejection_test_() ->
    Values = [
        undefined,
        null,
        <<>>,
        <<"aac_lc">>,
        <<"mp4">>,
        <<"opus">>,
        <<"mpeg">>,
        <<"wav">>,
        <<"unknown">>,
        1
    ],
    [
        ?_assertEqual(
            {error, invalid_codec},
            talk_record_logic:parse_audio_part(
                case V of
                    undefined -> maps:remove(<<"codec">>, part(#{}));
                    _ -> part(#{<<"codec">> => V})
                end
            )
        )
     || V <- Values
    ].

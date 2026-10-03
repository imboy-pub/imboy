-module(messaging_own_device_history_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% Local mocked archive only: proves history routing, not storage/decryption.
own_device_history_preserves_logical_recipient_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {conv_key_c2c, 2, fun(100, 200) -> <<"c2c:100:200">> end},
                {history, 3, fun(<<"c2c:100:200">>, 0, 3) ->
                    {ok, [row(1, 100, 200), row(2, 200, 100)]}
                end}
            ]}
        ],
        fun() ->
            {ok, Res} = messaging_logic:history(
                100, <<"c2c">>, <<"200">>, 0, 2, <<"sender-other-device">>
            ),
            %% Sent row has the self-device envelope; received row lacks it.
            [Msg] = maps:get(<<"messages">>, Res),
            ?assertEqual(<<"100">>, maps:get(<<"from">>, Msg)),
            ?assertEqual(<<"200">>, maps:get(<<"to">>, Msg)),
            ?assertEqual(<<"sender-original-device">>, maps:get(<<"sender_did">>, Msg)),
            ?assertEqual(2, maps:get(<<"next_seq">>, Res)),
            ?assertEqual(false, maps:get(<<"has_more">>, Res)),
            Meta = maps:get(<<"e2ee">>, Msg),
            ?assert(maps:is_key(<<"sender-other-device">>, maps:get(<<"devices">>, Meta)))
        end
    ).

row(Seq, From, To) ->
    #{
        <<"conv_seq">> => Seq,
        <<"msg_id">> => integer_to_binary(Seq),
        <<"from_id">> => From,
        <<"to_id">> => To,
        <<"sender_did">> => <<"sender-original-device">>,
        <<"e2ee">> => #{
            <<"meta_version">> => 3,
            <<"fan_out">> => <<"per_device">>,
            <<"devices">> => #{
                case From of
                    100 -> <<"sender-other-device">>;
                    _ -> <<"different-device">>
                end =>
                    #{<<"ciphertext">> => <<"structural-fixture-only">>}
            }
        }
    }.

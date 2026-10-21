-module(talk_record_codec_boundary_tests).
-include_lib("eunit/include/eunit.hrl").

invalid_codec_has_no_io_test_() ->
    [
        ?_test(begin
            %% Neither PG nor attachment modules are mocked: crossing the pure guard
            %% reaches missing runtime configuration and fails instead of faking I/O.
            ?assertEqual(
                {error, invalid_codec},
                talk_record_parts_ds:register_part(1, 2, 3, #{codec => C}, <<"key">>, <<"hash">>)
            ),
            ?assertEqual(
                {error, invalid_codec},
                talk_record_part_upload:prepare(1, 2, #{<<"codec">> => C})
            )
        end)
     || C <- [
            undefined,
            null,
            <<>>,
            <<"aac_lc">>,
            <<"mp4">>,
            <<"opus">>,
            <<"mpeg">>,
            <<"wav">>,
            <<"unknown">>
        ]
    ].

missing_codec_has_no_io_test() ->
    ?assertEqual(
        {error, invalid_codec},
        talk_record_parts_ds:register_part(1, 2, 3, #{}, <<"key">>, <<"hash">>)
    ),
    ?assertEqual({error, invalid_codec}, talk_record_part_upload:prepare(1, 2, #{})).

aac_presign_contract_test() ->
    meck:new(attach_logic, [non_strict]),
    try
        meck:expect(attach_logic, presign, fun(
            1,
            <<"part-4.aac">>,
            <<"audio/aac">>,
            <<"talk_record">>,
            <<"2">>,
            Cid
        ) ->
            ?assertEqual(talk_record_part_upload:client_upload_id(4), Cid),
            {ok, #{scoped => true}}
        end),
        ?assertEqual(
            {ok, #{scoped => true}},
            talk_record_part_upload:prepare(
                1,
                2,
                #{<<"part_id">> => 4, <<"codec">> => <<"aac">>}
            )
        ),
        ?assertEqual(1, meck:num_calls(attach_logic, presign, '_')),
        ?assert(meck:validate(attach_logic))
    after
        meck:unload(attach_logic)
    end.

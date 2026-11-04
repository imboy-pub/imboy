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

%% V3-18/V3-20：录音分片上传切 direct s3_put（presign/gateway put_url 通道已
%% 退役）。义务逐字保留：codec→定名 part-4.aac、mime audio/aac、Cid 幂等、
%% 走（现行的）attach_direct_logic register→declare→grant 上传通道。
aac_direct_upload_contract_test() ->
    meck:new(attach_direct_logic, [non_strict]),
    try
        meck:expect(attach_direct_logic, register, fun(
            1,
            <<"talk_record">>,
            <<"2">>,
            Cid,
            <<"part-4.aac">>
        ) ->
            ?assertEqual(talk_record_part_upload:client_upload_id(4), Cid),
            {ok, #{object_key => <<"talk_record/2/part-4.aac">>}}
        end),
        meck:expect(attach_direct_logic, declare, fun(
            <<"talk_record/2/part-4.aac">>,
            <<>>,
            0,
            <<"audio/aac">>
        ) ->
            ok
        end),
        meck:expect(attach_direct_logic, grant, fun(
            <<"talk_record/2/part-4.aac">>,
            <<>>
        ) ->
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
        ?assertEqual(1, meck:num_calls(attach_direct_logic, register, 5)),
        ?assert(meck:validate(attach_direct_logic))
    after
        meck:unload(attach_direct_logic)
    end.

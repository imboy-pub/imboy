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

%% V3-18/V3-20 退役后 prepare_aac 走 direct s3_put 通道（register→declare→
%% grant），不再经 attach_logic:presign——mock 对齐新合同：register 的
%% Cid 幂等锚（T02）+ grant 投影直传。Storage/DB 在单测域外，全部 mock。
aac_direct_grant_contract_test() ->
    meck:new(attach_direct_logic, [non_strict]),
    try
        meck:expect(attach_direct_logic, register, fun(
            1, <<"talk_record">>, <<"2">>, Cid, <<"part-4.aac">>
        ) ->
            ?assertEqual(talk_record_part_upload:client_upload_id(4), Cid),
            {ok, #{object_key => <<"direct-c/u1/talk_record/2/part-4.aac">>}}
        end),
        meck:expect(attach_direct_logic, declare, fun(_Key, _Sha, _Size, _Mime) -> ok end),
        Granted = #{grant => #{url => <<"https://s3.test/put">>}, attempt_id => 1},
        meck:expect(attach_direct_logic, grant, fun(_Key, _Sha) -> {ok, Granted} end),
        ?assertEqual(
            {ok, Granted},
            talk_record_part_upload:prepare(
                1,
                2,
                #{<<"part_id">> => 4, <<"codec">> => <<"aac">>}
            )
        ),
        ?assertEqual(1, meck:num_calls(attach_direct_logic, register, '_')),
        ?assertEqual(1, meck:num_calls(attach_direct_logic, declare, '_')),
        ?assertEqual(1, meck:num_calls(attach_direct_logic, grant, '_')),
        ?assert(meck:validate(attach_direct_logic))
    after
        meck:unload(attach_direct_logic)
    end.

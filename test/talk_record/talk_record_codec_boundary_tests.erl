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

%% V3-18：录音分片上传切 direct s3_put（正文不过 BEAM）——prepare 边界 =
%% attach_direct_logic（register→declare→grant），旧 attach_logic:presign
%% 通道已退役（继续 mock 它只会得到 fail-closed upload_root_unavailable，
%% 不再触达）。本卡断言 aac codec 的路由合同：Name/Mime/Cid/scope 正确
%% 落入 direct 会话，granted descriptor 原样上抛。
aac_presign_contract_test() ->
    meck:new(attach_direct_logic, [non_strict]),
    try
        meck:expect(attach_direct_logic, register, fun(
            1, <<"talk_record">>, <<"2">>, Cid, <<"part-4.aac">>
        ) ->
            ?assertEqual(talk_record_part_upload:client_upload_id(4), Cid),
            {ok, #{object_key => <<"dv1/part-4-key">>}}
        end),
        meck:expect(attach_direct_logic, declare, fun(
            <<"dv1/part-4-key">>, <<>>, 0, <<"audio/aac">>
        ) ->
            ok
        end),
        meck:expect(attach_direct_logic, grant, fun(<<"dv1/part-4-key">>, <<>>) ->
            {ok, #{grant => <<"g1">>, attempt_id => 1}}
        end),
        ?assertEqual(
            {ok, #{grant => <<"g1">>, attempt_id => 1}},
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

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

%% aac_presign_contract_test 已随 V3-18/V3-20 退役（9f18d2b93）：prepare 切
%% direct s3_put 通道（register→declare→grant），不再调用 attach_logic:presign；
%% scoped 上传合同由 talk_record_part_upload_direct_tests（真 PG+真 Garage
%% 端到端，断言 s3_put descriptor transport/url/required_headers）覆盖。

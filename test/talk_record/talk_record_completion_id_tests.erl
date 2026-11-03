-module(talk_record_completion_id_tests).
-include_lib("eunit/include/eunit.hrl").

integer_capture_boundaries_test() ->
    lists:foreach(
        fun(Id) ->
            ?assertMatch({ok, _}, talk_record_job_logic:parse_complete(body(Id)))
        end,
        [1, 119258348645517312, 9223372036854775807]
    ),
    lists:foreach(
        fun(Id) ->
            ?assertEqual(
                {error, invalid_capture_id}, talk_record_job_logic:parse_complete(body(Id))
            )
        end,
        [0, -1, 1.5, true, <<"123">>, <<"">>, 9223372036854775808]
    ).

invalid_capture_maps_http_422_test() ->
    meck:new(elib_response, [passthrough, no_link]),
    try
        meck:expect(
            elib_response,
            error_with_status,
            fun(Req, Status, _, Code) -> #{req => Req, status => Status, code => Code} end
        ),
        ?assertMatch(#{status := 422}, talk_record_http:respond_error(#{}, invalid_capture_id))
    after
        meck:unload(elib_response)
    end.

body(Id) ->
    #{
        <<"captures">> => [
            #{
                <<"capture_id">> => Id,
                <<"last_seq">> => 0,
                <<"manifest_hash">> => hash64()
            }
        ],
        <<"final_audio">> => final_audio_decl()
    }.

%% no-backend-ffmpeg V1：complete 必填 final_audio（形态校验级）。
final_audio_decl() ->
    #{
        <<"object_key">> => <<"u1/talk_record/r1/final.aac">>,
        <<"client_upload_id">> => <<"0f0e0d0c-0b0a-49f8-8a7b-6c5d4e3f2a1b">>,
        <<"sha256">> => hash64(),
        <<"byte_size">> => 1024,
        <<"sample_rate">> => 16000,
        <<"channels">> => 1,
        <<"sample_count">> => 1024,
        <<"duration_ms">> => 64
    }.

hash64() -> list_to_binary(lists:duplicate(64, $a)).

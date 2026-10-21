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

body(Id) -> #{<<"captures">> => [#{<<"capture_id">> => Id, <<"last_seq">> => 0}]}.

-module(elib_uuid_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

gen_v7_returns_36_char_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_uuid:gen_v7(),
        ?assertEqual(36, byte_size(Result)),
        ?assertMatch(
            {match, _},
            re:run(
                Result, <<"^[0-9a-f]{8}-[0-9a-f]{4}-7[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$">>
            )
        )
    end).

gen_v7_bin_returns_16_bytes_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_uuid:gen_v7_bin(),
        ?assertEqual(16, byte_size(Result))
    end).

gen_v7_bin_timestamp_is_recent_test_() ->
    ?TEST_SIMPLE(fun() ->
        <<TsMs:48, _/bits>> = elib_uuid:gen_v7_bin(),
        NowMs = erlang:system_time(millisecond),
        ?assert(NowMs - TsMs < 1000)
    end).

gen_v7_unique_across_calls_test_() ->
    ?TEST_SIMPLE(fun() ->
        Ids = lists:usort([elib_uuid:gen_v7() || _ <- lists:seq(1, 1000)]),
        ?assertEqual(1000, length(Ids))
    end).

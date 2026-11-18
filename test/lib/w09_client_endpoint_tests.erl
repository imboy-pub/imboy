-module(w09_client_endpoint_tests).
-include_lib("eunit/include/eunit.hrl").

client_endpoint_validation_test() ->
    Internal = <<"http://internal.test:3900">>,
    ?assertEqual({ok, Internal}, elib_s3_sign:client_endpoint(#{endpoint => Internal})),
    Valid = [
        <<"http://public.test">>,
        <<"https://api.example.test/s3">>,
        <<"https://public.test:19484">>,
        <<"http://127.0.0.1:3900">>,
        <<"http://[::1]:3900">>
    ],
    lists:foreach(
        fun(E) ->
            ?assertEqual(
                {ok, E}, elib_s3_sign:client_endpoint(#{endpoint => Internal, public_endpoint => E})
            )
        end,
        Valid
    ),
    Invalid = [
        <<>>,
        <<"ftp://public.test">>,
        <<"http://">>,
        <<"http://host:">>,
        <<"http://host:0">>,
        <<"http://host:65536">>,
        <<"http://host:03900">>,
        <<"http://host:80">>,
        <<"https://host:443">>,
        <<"http://user@host">>,
        <<"http://host/a/../b">>,
        <<"http://host/%2e">>,
        <<"http://host//a">>,
        <<"http://host/">>,
        <<"http://host/s3/">>,
        <<"http://host?x">>,
        <<"http://host#x">>,
        <<"http://UPPER.test">>,
        <<"http://-bad.test">>,
        <<"http://a_b.test">>,
        <<"http://999.1.1.1">>
    ],
    lists:foreach(
        fun(E) ->
            ?assertEqual(
                {error, invalid_public_endpoint},
                elib_s3_sign:client_endpoint(#{endpoint => Internal, public_endpoint => E})
            )
        end,
        Invalid
    ).

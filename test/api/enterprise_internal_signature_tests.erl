-module(enterprise_internal_signature_tests).
-include_lib("eunit/include/eunit.hrl").

sha256_round_trip_test() ->
    round_trip(sha256).

round_trip(Algorithm) ->
    Signed = fixture(Algorithm),
    Body = <<"{\"name\":\"synthetic\"}">>,
    Hex = enterprise_internal_signature:sign(
        <<"synthetic-key">>,
        <<"POST">>,
        <<"/api/internal/v1/directory/sync">>,
        <<"a=1&b=%2F">>,
        Body,
        Signed
    ),
    Headers = (maps:get(headers, Signed))#{
        <<"x-imboy-signature">> =>
            <<(atom_to_binary(Algorithm))/binary, "=", Hex/binary>>
    },
    {ok, Parsed} = enterprise_internal_signature:parse(Headers, 1800000000),
    Verify = fun(M, P, Q, B, S) ->
        enterprise_internal_signature:verify(
            <<"synthetic-key">>, M, P, Q, B, S
        )
    end,
    Request = Parsed#{headers => Headers},
    Path = <<"/api/internal/v1/directory/sync">>,
    Query = <<"a=1&b=%2F">>,
    ?assert(Verify(<<"POST">>, Path, Query, Body, Request)),
    ?assertNot(Verify(<<"PUT">>, Path, Query, Body, Request)),
    ?assertNot(Verify(<<"POST">>, <<Path/binary, "/other">>, Query, Body, Request)),
    ?assertNot(Verify(<<"POST">>, Path, <<"b=%2F&a=1">>, Body, Request)),
    ?assertNot(Verify(<<"POST">>, Path, Query, <<Body/binary, " ">>, Request)),
    ?assertNot(
        Verify(
            <<"POST">>,
            Path,
            Query,
            Body,
            Request#{headers => Headers#{<<"idempotency-key">> => <<"different">>}}
        )
    ),
    ?assertNot(
        enterprise_internal_signature:verify(
            <<"wrong-secret">>,
            <<"POST">>,
            Path,
            Query,
            Body,
            Request
        )
    ).

malformed_and_stale_test() ->
    H = maps:get(headers, fixture(sha256)),
    ?assertMatch({error, invalid_credential}, enterprise_internal_signature:parse(H, 1800000301)),
    ?assertMatch({error, invalid_credential}, enterprise_internal_signature:parse(H, 1799999699)),
    [
        ?assertMatch(
            {error, invalid_credential},
            enterprise_internal_signature:parse(
                H#{Key => Value}, 1800000000
            )
        )
     || {Key, Value} <- [
            {<<"x-imboy-signature">>, <<"md5=0000000000000000">>},
            {<<"x-imboy-signature">>, <<"sha256=INVALID">>},
            {<<"x-imboy-nonce">>, <<"short">>},
            {<<"x-imboy-timestamp">>, <<"01800000000">>},
            {<<"x-imboy-timestamp">>, <<"1800000000\n">>},
            {<<"x-imboy-credential">>, <<"ib_int_1.secret">>}
        ]
    ],
    ?assertMatch({error, invalid_credential}, enterprise_internal_signature:parse(#{}, 1800000000)).

fixture(Algorithm) ->
    H = #{
        <<"x-imboy-credential">> => <<"ib_int_1001">>,
        <<"x-imboy-timestamp">> => <<"1800000000">>,
        <<"x-imboy-nonce">> => <<"synthetic_nonce_123">>,
        <<"x-imboy-signature">> => <<"sha256=", (binary:copy(<<"0">>, 64))/binary>>,
        <<"content-type">> => <<"application/json">>,
        <<"idempotency-key">> => <<"synthetic-operation">>
    },
    #{
        prefix => <<"ib_int_1001">>,
        timestamp => <<"1800000000">>,
        nonce => <<"synthetic_nonce_123">>,
        algorithm => Algorithm,
        headers => H
    }.

known_wire_vectors_test() ->
    [
        ?assertEqual(
            Expected,
            enterprise_internal_signature:sign(
                <<"synthetic-key">>,
                <<"POST">>,
                <<"/api/internal/v1/directory/sync">>,
                <<"a=1&b=%2F">>,
                <<"{\"name\":\"synthetic\"}">>,
                fixture(Algorithm)
            )
        )
     || {Algorithm, Expected} <- [
            {sha256, <<"c82fc3eaf85886142fb3543e2076f30ed1549b075bd6230bdbc5fd467da27389">>}
        ]
    ].

sha512_rejected_test() ->
    Signed = fixture(sha256),
    Canonical = enterprise_internal_signature:canonical(
        <<"GET">>, <<"/api/internal/v1/application">>, <<>>, <<>>, Signed
    ),
    LegacyCanonical = binary:replace(Canonical, <<"\nsha256\n">>, <<"\nsha512\n">>),
    Hex = binary:encode_hex(
        crypto:mac(hmac, sha512, <<"synthetic-key">>, LegacyCanonical), lowercase
    ),
    Headers = (maps:get(headers, Signed))#{<<"x-imboy-signature">> => <<"sha512=", Hex/binary>>},
    ?assertEqual(
        {error, invalid_credential}, enterprise_internal_signature:parse(Headers, 1800000000)
    ),
    ?assertError(
        function_clause,
        enterprise_internal_signature:sign(
            <<"synthetic-key">>, <<"GET">>, <<"/">>, <<>>, <<>>, Signed#{algorithm => sha512}
        )
    ).

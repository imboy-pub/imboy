-module(enterprise_internal_signature_tests).
-include_lib("eunit/include/eunit.hrl").

both_algorithms_test() ->
    [round_trip(A) || A <- [sha256, sha512]].

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
            {sha256, <<"c82fc3eaf85886142fb3543e2076f30ed1549b075bd6230bdbc5fd467da27389">>},
            {sha512,
                <<"c66f3cda7d80c5151a27d194bc1e8cc7e2f4ea51aec6f006ee3f35e941e2760a89b83f88223e71b91127b72f982015ecb4a9b49a86f81822907dc8ad5e01f55a">>}
        ]
    ].

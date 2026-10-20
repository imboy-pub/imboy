-module(enterprise_internal_self_auth_tests).
-include_lib("eunit/include/eunit.hrl").

hmac_only_admission_test_() ->
    {setup,
        fun() ->
            meck:new(enterprise_internal_rate, [passthrough, no_link]),
            meck:expect(enterprise_internal_rate, check, fun(_, _) -> {ok, 100} end)
        end,
        fun(_) -> meck:unload(enterprise_internal_rate) end, fun(_) ->
            [
                ?_test(begin
                    R = hd(enterprise_internal_routes:routes()),
                    M = maps:get(method, R),
                    P = maps:get(path, R),
                    Auth = fun() -> {ok, #{authentication => hmac, application_id => 1}} end,
                    H = intbe02_http_support:signed_headers(
                        <<"ib_int_1.synthetic-secret">>, M, P, <<>>, #{}, sha256
                    ),
                    ?assertMatch({ok, _}, enterprise_internal_auth:decide(M, P, H, Auth)),
                    ?assertEqual(
                        {error, invalid_credential},
                        enterprise_internal_auth:decide(M, P, #{}, Auth)
                    ),
                    ?assertEqual(
                        {error, invalid_credential},
                        enterprise_internal_auth:decide(
                            M,
                            P,
                            #{<<"authorization">> => <<"Bearer ib_int_1.synthetic-secret">>},
                            Auth
                        )
                    ),
                    ?assertEqual(
                        {error, invalid_credential},
                        enterprise_internal_auth:decide(
                            M,
                            P,
                            H#{<<"authorization">> => <<"Bearer synthetic">>},
                            Auth
                        )
                    ),
                    ?assertEqual(
                        {error, invalid_credential},
                        enterprise_internal_auth:decide(
                            M,
                            P,
                            maps:remove(<<"x-imboy-nonce">>, H),
                            Auth
                        )
                    )
                end),
                ?_test(begin
                    H = intbe02_http_support:signed_headers(
                        <<"ib_int_1.synthetic-secret">>,
                        <<"POST">>,
                        <<"/api/internal/v1/groups">>,
                        <<>>,
                        #{},
                        sha512
                    ),
                    Auth = fun() -> {ok, #{authentication => hmac, application_id => 1}} end,
                    ?assertEqual(
                        {error, invalid_request},
                        enterprise_internal_auth:decide(
                            <<"POST">>,
                            <<"/api/internal/v1/groups">>,
                            H,
                            Auth
                        )
                    ),
                    ?assertMatch(
                        {ok, _},
                        enterprise_internal_auth:decide(
                            <<"POST">>,
                            <<"/api/internal/v1/groups">>,
                            H#{<<"idempotency-key">> => <<"synthetic-key">>},
                            Auth
                        )
                    )
                end),
                ?_test(begin
                    Exports = enterprise_internal_auth:module_info(exports),
                    ?assertNot(lists:member({parse_bearer, 1}, Exports)),
                    ?assertNot(lists:member({authenticate, 2}, Exports)),
                    ?assertNot(lists:member({authenticate_tx, 3}, Exports))
                end)
            ]
        end}.

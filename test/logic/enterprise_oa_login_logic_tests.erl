-module(enterprise_oa_login_logic_tests).
-include_lib("eunit/include/eunit.hrl").
pkce_rfc7636_test() ->
    ?assertEqual(
        <<"E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM">>,
        enterprise_oa_login_logic:pkce(<<"dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk">>)
    ).
issue_input_test() ->
    P = #{
        <<"external_user_id">> => <<"employee-1">>,
        <<"intent">> => <<"login">>,
        <<"code_challenge_method">> => <<"S256">>,
        <<"code_challenge">> => enterprise_oa_login_logic:pkce(<<"verifier">>)
    },
    ?assertEqual(ok, enterprise_oa_login_logic:validate_issue(P)),
    lists:foreach(
        fun(Bad) ->
            ?assertEqual(
                {error, invalid_request},
                enterprise_oa_login_logic:validate_issue(Bad)
            )
        end,
        [
            P#{<<"external_user_id">> => <<>>},
            P#{<<"intent">> => <<"admin">>},
            P#{<<"code_challenge_method">> => <<"plain">>},
            P#{<<"code_challenge">> => <<"x">>}
        ]
    ).
no_implicit_scope_test() ->
    ?assertEqual(
        {error, insufficient_scope},
        enterprise_internal_scope:authorize(
            <<"oa:login:issue">>, [<<"identities:write">>, <<"sso:exchange">>]
        )
    ),
    ?assertEqual(
        ok, enterprise_internal_scope:authorize(<<"oa:login:issue">>, [<<"oa:login:issue">>])
    ).

login_start_origin_test() ->
    Old = application:get_env(imboy, oa_login_start_uris),
    Key = <<"test-oa">>,
    try
        lists:foreach(
            fun({Uri, Expected}) ->
                application:set_env(imboy, oa_login_start_uris, #{Key => Uri}),
                ?assertEqual(
                    Expected,
                    enterprise_oa_sso_logic:login_start_fields(
                        Key, <<"https://oa.example/callback">>
                    )
                )
            end,
            [
                {<<"https://oa.example:443/login">>, #{
                    <<"login_start_uri">> => <<"https://oa.example:443/login">>
                }},
                {<<"https://other.example/login">>, #{}},
                {<<"http://oa.example/login">>, #{}},
                {<<"https://oa.example:444/login">>, #{}},
                {<<"https://user@oa.example/login">>, #{}},
                {<<"https://oa.example/login#fragment">>, #{}},
                {undefined, #{}}
            ]
        )
    after
        case Old of
            {ok, V} -> application:set_env(imboy, oa_login_start_uris, V);
            undefined -> application:unset_env(imboy, oa_login_start_uris)
        end
    end.

invalid_registration_never_allocates_test() ->
    Mods = [elib_pg, enterprise_oa_login_repo, account_ds, passport_logic],
    lists:foreach(fun(M) -> meck:new(M, [non_strict]) end, Mods),
    try
        meck:expect(elib_pg, with_tx, fun(F) ->
            try
                F(test_connection)
            catch
                throw:{abort_tx, R} -> {error, R}
            end
        end),
        meck:expect(
            enterprise_oa_login_repo,
            lock_tx,
            fun(_, _, _) -> {error, invalid_ticket} end
        ),
        meck:expect(account_ds, allocate, fun() -> error(unexpected_allocation) end),
        meck:expect(passport_logic, quota_guard, fun() -> error(unexpected_quota) end),
        P = #{
            <<"confirm_create">> => true,
            <<"pending_token">> => binary:copy(<<"a">>, 43),
            <<"code_verifier">> => binary:copy(<<"b">>, 43),
            <<"did">> => <<"test-device">>,
            <<"application_key">> => <<"expected">>
        },
        ?assertEqual({error, invalid_ticket}, enterprise_oa_login_logic:register(P)),
        ?assertEqual(
            {error, invalid_request},
            enterprise_oa_login_logic:register(P#{<<"pending_token">> => <<>>})
        ),
        ?assertEqual(0, meck:num_calls(account_ds, allocate, [])),
        ?assertEqual(0, meck:num_calls(passport_logic, quota_guard, []))
    after
        lists:foreach(fun meck:unload/1, Mods)
    end.

-module(identity_lookup_failure_tests).
-include_lib("eunit/include/eunit.hrl").

lookup_result_test_() ->
    [
        isolated(fun() -> repository_result(Repo, Field, Result) end)
     || Repo <- [user_repo, adm_user_repo],
        Field <- [id, account, email, mobile],
        Result <- [{ok, #{}}, {ok, #{<<"id">> => 7}}, {error, synthetic_database_failure}]
    ].

repository_result(Repo, Field, Result) ->
    Identity =
        case Field of
            id -> 7;
            _ -> <<"synthetic-identity">>
        end,
    pure_mock_fixture:with_mocks(
        [
            {elib_pg, [
                {one, 2, fun(_Sql, [Value]) ->
                    ?assertEqual(Identity, Value),
                    Result
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(Result, Repo:lookup(Field, Identity, <<"id">>)),
            Legacy =
                case Result of
                    {ok, Row} -> Row;
                    {error, _} -> #{}
                end,
            Function =
                case Field of
                    id -> find_by_id;
                    account -> find_by_account;
                    email -> find_by_email;
                    mobile -> find_by_mobile
                end,
            ?assertEqual(Legacy, apply(Repo, Function, [Identity, <<"id">>]))
        end
    ).

lookup_exception_test_() ->
    [
        isolated(fun() ->
            pure_mock_fixture:with_mocks(
                [
                    {elib_pg, [
                        {one, 2, fun(_, _) -> exit(synthetic_pool_unavailable) end}
                    ]}
                ],
                fun() ->
                    ?assertEqual(
                        {error, {identity_lookup_failed, exit, synthetic_pool_unavailable}},
                        Repo:lookup(id, 7, <<"id">>)
                    )
                end
            )
        end)
     || Repo <- [user_repo, adm_user_repo]
    ].

login_database_failure_test_() ->
    [
        isolated(fun() -> login_failure(Mode, Type) end)
     || Mode <- [password, code],
        Type <- [<<"account">>, <<"email">>, <<"mobile">>]
    ].

found_user_login_test_() ->
    [
        isolated(fun() ->
            User = (maps:from_list([
                {Field, <<>>}
             || Field <- [
                    <<"account">>,
                    <<"email">>,
                    <<"nickname">>,
                    <<"avatar">>,
                    <<"gender">>,
                    <<"region">>,
                    <<"sign">>
                ]
            ]))#{
                <<"id">> => 7, <<"status">> => 1, <<"password">> => <<"synthetic-hash">>
            },
            pure_mock_fixture:with_mocks(
                [
                    {user_ds, [{lookup, 3, fun(_, _, _) -> {ok, User} end}]},
                    {login_attempt_ds, [
                        {begin_attempt, 3, fun({user, 7}, _, _) -> {ok, synthetic_attempt} end},
                        {end_attempt, 2, fun(synthetic_attempt, reset) -> ok end}
                    ]},
                    {login_security_logic, [
                        {check_login_allowed, 1, fun({user, 7}) -> {ok, true} end}
                    ]},
                    {elib_password, [
                        {verify, 2, fun(_, _) -> {ok, true} end},
                        {verify_and_plan, 2, fun(_, _) -> {error, no_upgrade} end}
                    ]},
                    {verification_code_ds, [{consume, 2, fun(_, _) -> {ok, <<"verified">>} end}]},
                    {token_ds, [
                        {encrypt_token, 2, fun(7, _) -> <<"synthetic-token">> end},
                        {encrypt_refreshtoken, 2, fun(7, _) -> <<"synthetic-refresh">> end}
                    ]},
                    {user_device_logic, [
                        {validate_device_type, 1, fun(_) -> false end},
                        {check_login_conflict, 2, fun(_, _) -> {ok, no_conflict} end}
                    ]},
                    {elib_async, [{async, 1, fun(_) -> self() end}]}
                ],
                fun() ->
                    Account =
                        case Type of
                            <<"email">> -> <<"synthetic@invalid.test">>;
                            _ -> <<"synthetic">>
                        end,
                    Result =
                        case Mode of
                            password ->
                                passport_logic:do_login(Type, Account, <<"password">>);
                            code ->
                                passport_logic:do_login_by_code(
                                    Type, Account, <<"000000">>, <<>>, <<>>
                                )
                        end,
                    ?assertMatch(
                        {ok, #{<<"uid">> := 7, <<"token">> := <<"synthetic-token">>}}, Result
                    ),
                    ?assertEqual(
                        1, meck:num_calls(login_attempt_ds, end_attempt, [synthetic_attempt, reset])
                    )
                end
            )
        end)
     || Mode <- [password, code], Type <- [<<"account">>, <<"email">>, <<"mobile">>]
    ].

login_failure(Mode, Type) ->
    Configs = [
        {user_ds, failed_user_queries()},
        {login_attempt_ds, [
            {begin_attempt, 3, fun(_, _, _) -> error(credential_gate_must_not_run) end}
        ]}
    ],
    pure_mock_fixture:with_mocks(Configs, fun() ->
        Account =
            case Type of
                <<"email">> -> <<"synthetic@invalid.test">>;
                _ -> <<"synthetic">>
            end,
        Result =
            case Mode of
                password ->
                    passport_logic:do_login(Type, Account, <<"synthetic-password">>, <<>>, <<>>);
                code ->
                    passport_logic:do_login_by_code(Type, Account, <<"000000">>, <<>>, <<>>)
            end,
        ?assertEqual({error, service_unavailable}, Result),
        ?assertEqual(1, meck:num_calls(user_ds, lookup, 3)),
        ?assertEqual(0, meck:num_calls(login_attempt_ds, begin_attempt, 3))
    end).

account_fallback_failure_test_() ->
    isolated(fun() ->
        pure_mock_fixture:with_mocks(
            [
                {user_ds, [
                    {lookup, 3, fun
                        (account, _, _) -> {ok, #{}};
                        (mobile, _, _) -> {error, synthetic_database_failure}
                    end},
                    {find_by_account, 2, fun(_, _) -> #{} end},
                    {find_by_mobile, 2, fun(_, _) -> #{} end}
                ]},
                {login_attempt_ds, [{begin_attempt, 3, fun(_, _, _) -> error(must_not_run) end}]}
            ],
            fun() ->
                ?assertEqual(
                    {error, service_unavailable},
                    passport_logic:do_login(<<"account">>, <<"synthetic">>, <<"password">>)
                ),
                ?assertEqual(0, meck:num_calls(login_attempt_ds, begin_attempt, 3))
            end
        )
    end).

compat_signup_queries_before_cleanup_test_() ->
    isolated(fun() ->
        pure_mock_fixture:with_mocks(
            [
                {user_ds, [
                    {lookup, 3, fun
                        (mobile, _, _) -> {ok, #{<<"id">> => 7, <<"status">> => -1}};
                        (email, _, _) -> {error, synthetic_database_failure}
                    end},
                    {delete_all_related_data, 1, fun(_) -> error(cleanup_must_not_run) end}
                ]}
            ],
            fun() ->
                ?assertEqual(
                    {error, <<"服务暂不可用，请稍后重试"/utf8>>, 503},
                    passport_logic:signup(
                        <<"synthetic">>,
                        <<"Synthetic-password-9">>,
                        <<"synthetic@invalid.test">>,
                        #{}
                    )
                ),
                ?assertEqual(0, meck:num_calls(user_ds, delete_all_related_data, 1))
            end
        )
    end).

signup_and_reset_do_not_consume_code_test_() ->
    [
        isolated(fun() ->
            pure_mock_fixture:with_mocks(
                [
                    {user_ds, failed_user_queries()},
                    {verification_code_ds, [
                        {consume, 2, fun(_, _) -> error(code_must_not_be_consumed) end}
                    ]}
                ],
                fun() ->
                    Result = apply(
                        passport_logic,
                        Operation,
                        [
                            Type,
                            <<"synthetic@invalid.test">>,
                            <<"Synthetic-password-9">>,
                            <<"000000">>,
                            #{<<"nickname">> => <<"synthetic">>}
                        ]
                    ),
                    ?assertEqual({error, service_unavailable}, Result),
                    ?assertEqual(0, meck:num_calls(verification_code_ds, consume, 2))
                end
            )
        end)
     || Operation <- [do_signup, find_password], Type <- [<<"email">>, <<"mobile">>]
    ].

admin_database_failure_test_() ->
    isolated(fun() ->
        pure_mock_fixture:with_mocks(
            [
                {adm_user_ds, [
                    {lookup, 3, fun(_, _, _) -> {error, synthetic_database_failure} end},
                    {find_by_account, 2, fun(_, _) -> #{} end}
                ]},
                {elib_password, [{verify, 2, fun(_, _) -> error(password_must_not_be_checked) end}]}
            ],
            fun() ->
                ?assertEqual(
                    {error, service_unavailable},
                    adm_passport_logic:do_login(<<"synthetic-admin">>, <<"password">>)
                ),
                ?assertEqual(0, meck:num_calls(elib_password, verify, 2))
            end
        )
    end).

quick_login_database_failure_test_() ->
    isolated(fun() ->
        pure_mock_fixture:with_mocks(
            [
                {user_ds, failed_user_queries()},
                {imboy_sms, [{jverification, 1, fun(_) -> {ok, <<"synthetic">>} end}]}
            ],
            fun() ->
                ?assertEqual(
                    {error, service_unavailable},
                    passport_logic:quick_login(
                        <<"jverify">>, undefined, <<"synthetic-token">>, #{}
                    )
                )
            end
        )
    end).

alipay_identity_failure_has_no_side_effect_test_() ->
    isolated(fun() ->
        pure_mock_fixture:with_mocks(
            [
                {user_ds, failed_user_queries()},
                {sso_identity_ds, [{find_uid, 2, fun(_, _) -> {ok, 7} end}]},
                {alipay_openapi, [
                    {oauth_token, 2, fun(_, _) ->
                        {ok, #{
                            access_token => <<"synthetic-token">>,
                            user_id => <<"synthetic-subject">>
                        }}
                    end},
                    {user_info_share, 2, fun(_, _) -> {ok, #{}} end}
                ]},
                {user_setting_ds, [
                    {find_by_uid, 1, fun(_) -> error(setting_write_must_not_run) end}
                ]},
                {token_ds, [{encrypt_token, 2, fun(_, _) -> error(token_must_not_be_issued) end}]}
            ],
            fun() ->
                application:set_env(imboy, alipay_app_id, <<"synthetic-app">>),
                application:set_env(imboy, alipay_private_key, <<"synthetic-key">>),
                ?assertEqual(
                    {error, service_unavailable},
                    passport_logic:alipay_login(<<"synthetic-code">>, #{})
                ),
                ?assertEqual(0, meck:num_calls(user_setting_ds, find_by_uid, 1)),
                ?assertEqual(0, meck:num_calls(token_ds, encrypt_token, 2))
            end
        )
    end).

admin_loader_does_not_cache_failure_test_() ->
    isolated(fun() ->
        pure_mock_fixture:with_mocks(
            [
                {adm_user_ds, [
                    {lookup, 3, fun(_, _, _) -> {error, synthetic_database_failure} end},
                    {find_by_id, 2, fun(_, _) -> #{} end}
                ]},
                {imboy_cache, [
                    {memo, 3, fun(F, _, _) ->
                        case F() of
                            {uncached, Value} -> Value;
                            Value -> Value
                        end
                    end},
                    {uncached, 1, fun(Value) -> {uncached, Value} end}
                ]}
            ],
            fun() ->
                ?assertEqual(#{}, adm_user_logic:find(7, <<"id,role_id">>, {synthetic, 7})),
                ?assertEqual(1, meck:num_calls(imboy_cache, uncached, 1))
            end
        )
    end).

password_change_database_failure_test_() ->
    [
        isolated(fun() ->
            pure_mock_fixture:with_mocks(
                [
                    {user_ds, failed_user_queries()},
                    {elib_param, [{post, 1, fun(_) -> #{} end}]}
                ],
                fun() ->
                    ?assertEqual(
                        {error, service_unavailable},
                        apply(user_logic, Operation, [7, synthetic_request])
                    )
                end
            )
        end)
     || Operation <- [set_password, change_password]
    ].

passport_http_service_unavailable_test_() ->
    [
        isolated(fun() ->
            pure_mock_fixture:with_mocks(
                [
                    {passport_logic, [
                        {do_login, 5, fun(_, _, _, _, _) -> {error, service_unavailable} end},
                        {quick_login, 4, fun(_, _, _, _) -> {error, service_unavailable} end},
                        {alipay_login, 2, fun(_, _) -> {error, service_unavailable} end},
                        {do_signup, 5, fun(_, _, _, _, _) -> {error, service_unavailable} end},
                        {find_password, 5, fun(_, _, _, _, _) -> {error, service_unavailable} end},
                        {mobile_registered, 1, fun(_) -> {error, service_unavailable} end}
                    ]},
                    {elib_param, [
                        {post, 1, fun(_) ->
                            #{
                                <<"auth_code">> => <<"synthetic-code">>,
                                <<"type">> => <<"sms">>,
                                <<"scene">> => <<"signup">>
                            }
                        end}
                    ]},
                    {elib_req, [{get_client_ip, 1, fun(_) -> <<"127.0.0.1">> end}]},
                    {elib_cipher, [
                        {safe_rsa_decrypt, 2, fun(_, _) -> <<"synthetic-password">> end}
                    ]},
                    {cowboy_req, [
                        {header, 3, fun(_, _, Default) -> Default end},
                        {reply, 4, fun(Status, _Headers, Body, Req) ->
                            Req#{status => Status, body => Body}
                        end}
                    ]}
                ],
                fun() ->
                    {ok, Req, _} = passport_handler:init(#{}, #{action => Action}),
                    assert_service_response(Req)
                end
            )
        end)
     || Action <- [login, quick_login, alipay_login, signup, find_password, getcode]
    ].

admin_http_service_unavailable_test_() ->
    isolated(fun() ->
        pure_mock_fixture:with_mocks(
            [
                {adm_passport_logic, [{do_login, 2, fun(_, _) -> {error, service_unavailable} end}]},
                {elib_req, [{cookie, 2, fun(_, _) -> <<"synthetic-cookie">> end}]},
                {elib_param, [{post, 1, fun(_) -> #{<<"captcha">> => <<"synthetic-captcha">>} end}]},
                {imboy_cache, [{get, 1, fun(_) -> {ok, 1} end}]},
                {simple_captcha, [{check, 2, fun(_, _) -> true end}]},
                {elib_cipher, [{rsa_decrypt, 1, fun(_) -> <<"synthetic-password">> end}]},
                {cowboy_req, [
                    {method, 1, fun(_) -> <<"POST">> end},
                    {reply, 4, fun(Status, _, Body, Req) ->
                        Req#{status => Status, body => Body}
                    end}
                ]}
            ],
            fun() ->
                {ok, Req, _} = adm_passport_handler:init(#{}, #{action => login}),
                assert_service_response(Req)
            end
        )
    end).

oidc_http_service_unavailable_test_() ->
    isolated(fun() ->
        pure_mock_fixture:with_mocks(
            [
                {auth_oidc_logic, [{callback, 2, fun(_, _) -> {error, service_unavailable} end}]},
                {login_attempt_ds, [{admit, 2, fun(_, _) -> {ok, #{}} end}]},
                {elib_req, [{get_client_ip, 1, fun(_) -> <<"127.0.0.1">> end}]},
                {cowboy_req, [
                    {method, 1, fun(_) -> <<"GET">> end},
                    {parse_qs, 1, fun(_) -> [] end},
                    {reply, 4, fun(Status, _, Body, Req) ->
                        Req#{status => Status, body => Body}
                    end}
                ]}
            ],
            fun() ->
                {stop, Req, _} = auth_oidc_handler:handle_request(#{}, #{action => callback}),
                assert_service_response(Req)
            end
        )
    end).

user_identity_http_service_unavailable_test_() ->
    [
        isolated(fun() ->
            pure_mock_fixture:with_mocks(
                [
                    {user_logic, [
                        {set_password, 2, fun(_, _) -> {error, service_unavailable} end},
                        {change_password, 2, fun(_, _) -> {error, service_unavailable} end},
                        {update, 4, fun(_, _, _, _) ->
                            {error, {503, <<>>, <<"服务暂不可用，请稍后重试"/utf8>>}}
                        end}
                    ]},
                    {auth_ds, [{current_uid, 1, fun(_) -> 7 end}]},
                    {elib_param, [{post, 1, fun(_) -> #{} end}]},
                    {cowboy_req, [
                        {reply, 4, fun(Status, _, Body, Req) ->
                            Req#{status => Status, body => Body}
                        end}
                    ]}
                ],
                fun() ->
                    {ok, Req, _} = user_handler:init(#{}, #{action => Action}),
                    assert_service_response(Req)
                end
            )
        end)
     || Action <- [set_password, change_password, update]
    ].

admin_governance_http_service_unavailable_test_() ->
    [
        isolated(fun() ->
            pure_mock_fixture:with_mocks(
                [
                    {adm_user_logic, [
                        {find, 3, fun(_, _, _) -> #{<<"role_id">> => [2]} end},
                        {save, 1, fun(_) -> {error, service_unavailable} end},
                        {update_status, 2, fun(_, _) -> {error, service_unavailable} end}
                    ]},
                    {adm_index_handler, [
                        {role_acl, 1, fun(_) ->
                            {<<"synthetic-role">>, [<<"admins:create">>, <<"admins:delete">>], []}
                        end}
                    ]},
                    {elib_cipher, [{rsa_decrypt, 1, fun(_) -> <<"Synthetic-password-9">> end}]},
                    {elib_param, [
                        {post, 1, fun(_) ->
                            #{
                                <<"account">> => <<"synthetic">>,
                                <<"pwd">> => <<"encrypted">>,
                                <<"role_id">> => 2,
                                <<"admin_id">> => 9
                            }
                        end}
                    ]},
                    {imboy_cache, [
                        {invalidate, 1, fun(_) -> error(cache_must_not_be_invalidated) end}
                    ]},
                    {cowboy_req, [
                        {method, 1, fun(_) -> <<"POST">> end},
                        {reply, 4, fun(Status, _, Body, Req) ->
                            Req#{status => Status, body => Body}
                        end}
                    ]}
                ],
                fun() ->
                    {ok, Req, _} = adm_admin_handler:init(#{}, #{action => Action, adm_user_id => 7}),
                    assert_service_response(Req),
                    ?assertEqual(0, meck:num_calls(imboy_cache, invalidate, 1))
                end
            )
        end)
     || Action <- [create, disable]
    ].

binding_database_failure_does_not_consume_code_test_() ->
    [
        isolated(fun() ->
            pure_mock_fixture:with_mocks(
                [
                    {user_ds, failed_user_queries()},
                    {verification_code_ds, [{consume, 2, fun(_, _) -> error(must_not_consume) end}]}
                ],
                fun() ->
                    ?assertEqual(
                        {error, {503, <<>>, <<"服务暂不可用，请稍后重试"/utf8>>}},
                        user_logic:update(7, Field, <<"synthetic@invalid.test">>, <<"000000">>)
                    ),
                    ?assertEqual(0, meck:num_calls(verification_code_ds, consume, 2))
                end
            )
        end)
     || Field <- [<<"email">>, <<"mobile">>]
    ].

assert_service_response(#{status := Status, body := Body}) ->
    ?assertEqual(503, Status),
    Payload = jsone:decode(Body),
    ?assertEqual(503, maps:get(<<"code">>, Payload)),
    ?assertEqual(<<"服务暂不可用，请稍后重试"/utf8>>, maps:get(<<"msg">>, Payload)),
    ?assertEqual(#{}, maps:get(<<"payload">>, Payload)).

admin_governance_failure_test_() ->
    [
        isolated(fun() ->
            pure_mock_fixture:with_mocks(
                [
                    {adm_user_ds, [
                        {lookup, 3, fun(_, _, _) -> {error, synthetic_database_failure} end},
                        {find_by_account, 2, fun(_, _) -> #{} end},
                        {find_by_id, 2, fun(_, _) -> #{} end},
                        {save, 1, fun(_) -> error(write_must_not_run) end},
                        {update, 2, fun(_, _) -> error(write_must_not_run) end},
                        {delete, 1, fun(_) -> error(write_must_not_run) end}
                    ]}
                ],
                fun() ->
                    ?assertEqual(
                        {error, service_unavailable}, apply(adm_user_logic, Operation, Args)
                    ),
                    ?assertEqual(0, meck:num_calls(adm_user_ds, save, 1)),
                    ?assertEqual(0, meck:num_calls(adm_user_ds, update, 2)),
                    ?assertEqual(0, meck:num_calls(adm_user_ds, delete, 1))
                end
            )
        end)
     || {Operation, Args} <- [
            {save, [#{<<"account">> => <<"synthetic-admin">>, <<"password">> => <<"password">>}]},
            {update, [7, #{<<"account">> => <<"synthetic-admin">>}]},
            {delete, [7]},
            {update_status, [7, 0]}
        ]
    ].

failed_user_queries() ->
    [{lookup, 3, fun(_, _, _) -> {error, synthetic_database_failure} end}] ++
        [
            {Function, 2, fun(_, _) -> #{} end}
         || Function <- [find_by_email, find_by_mobile, find_by_account, find_by_id]
        ].

isolated(Fun) ->
    {timeout, 30, fun() -> pure_mock_fixture:run(Fun) end}.

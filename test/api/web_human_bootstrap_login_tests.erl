-module(web_human_bootstrap_login_tests).
-include_lib("eunit/include/eunit.hrl").

%% Direct boundary mocks: no application, database or service boot.
-define(WEB, #{jwt_purpose => web_human}).
-define(UID, 111174215241304065).

bootstrap_public_allowlist_test() ->
    with_mocks(
        [
            {cowboy_req, [{header, fun(_, _, Default) -> Default end}]},
            {config_ds, [
                {env, fun(login_rsa_pub_key) -> <<"public-key">> end},
                {env, fun
                    (ws_url, _) -> <<"wss://trusted.test/old?credential=redacted#fragment">>;
                    (login_pwd_rsa_encrypt, _) -> <<"on">>;
                    (_, Default) -> Default
                end}
            ]},
            {elib_oss, [{public_base_url, fun() -> <<"https://assets.test">> end}]},
            {imboy_policy, [{effective_features, fun() -> #{core => true} end}]},
            {product_experience, [
                {effective_binary, fun() -> <<"community">> end},
                {config_version, fun() -> <<"test-version">> end}
            ]},
            {product_terminology, [{public, fun(_) -> #{} end}]},
            {elib_response, response_mocks()}
        ],
        fun() ->
            {ok, Req, _} = index_handler:init(#{}, #{action => web_bootstrap}),
            Payload = maps:get(payload, Req),
            ?assertEqual(
                lists:sort([
                    <<"ws_url">>,
                    <<"login_pwd_rsa_encrypt">>,
                    <<"login_rsa_pub_key">>,
                    <<"public_base_url">>,
                    <<"attachment_download_targets">>,
                    <<"features">>,
                    <<"effective_product_experience">>,
                    <<"config_version">>,
                    <<"terminology">>
                ]),
                lists:sort(maps:keys(Payload))
            ),
            ?assertEqual(<<"wss://trusted.test/api/v1/web/ws">>, maps:get(<<"ws_url">>, Payload)),
            ?assertEqual(<<"1">>, maps:get(<<"login_pwd_rsa_encrypt">>, Payload))
        end
    ).

web_login_registration_failure_hides_credentials_test() ->
    login_case({error, service_unavailable}, fun(Req) ->
        ?assertEqual(503, maps:get(status, Req)),
        ?assertNot(maps:is_key(payload, Req)),
        ?assertEqual(0, login_cast_count())
    end).

web_login_registers_fixed_metadata_test() ->
    login_case(ok, fun(Req) ->
        ?assertEqual(200, maps:get(status, Req)),
        ?assertEqual(<<"synthetic-tk">>, maps:get(<<"token">>, maps:get(payload, Req))),
        ?assertEqual(1, login_cast_count())
    end).

web_credential_settings_failure_precedes_session_side_effects_test() ->
    credential_case(ok, fun() ->
        meck:expect(user_setting_ds, find_web_by_uid, fun(_) -> {error, unavailable} end),
        ?assertEqual(
            {error, service_unavailable},
            passport_logic:do_web_login(
                <<"email">>, <<"synthetic@example.test">>, <<"pwd">>, web_values()
            )
        ),
        ?assertEqual(0, meck:num_calls(user_device_ds, register_web, '_')),
        ?assertEqual(0, meck:num_calls(user_device_logic, kick_device, '_')),
        ?assertEqual(0, meck:num_calls(token_ds, issue_web_session, '_')),
        ?assertEqual(0, meck:num_calls(token_ds, encrypt_token, '_')),
        ?assertEqual(0, meck:num_calls(elib_async, async, '_'))
    end).

web_credential_login_registration_failure_does_not_kick_or_sign_test() ->
    credential_case({error, db_down}, fun() ->
        ?assertEqual(
            {error, service_unavailable},
            passport_logic:do_web_login(
                <<"email">>, <<"synthetic@example.test">>, <<"pwd">>, web_values()
            )
        ),
        ?assertEqual(
            {error, service_unavailable},
            passport_logic:do_web_login_by_code(
                <<"email">>, <<"synthetic@example.test">>, <<"123456">>, web_values()
            )
        ),
        ?assertEqual(2, meck:num_calls(user_device_ds, register_web, '_')),
        ?assertEqual(0, meck:num_calls(user_device_logic, kick_device, '_')),
        ?assertEqual(0, meck:num_calls(token_ds, encrypt_token, '_')),
        ?assertEqual(0, meck:num_calls(token_ds, encrypt_refreshtoken, '_'))
    end).

web_credential_login_rejects_nonhuman_and_disabled_before_registration_test() ->
    lists:foreach(
        fun({Type, Status}) ->
            credential_case(ok, fun() ->
                meck:expect(user_ds, find_by_id, fun(?UID, <<"status,account_type">>) ->
                    #{<<"status">> => Status, <<"account_type">> => Type}
                end),
                ?assertMatch(
                    {error, _},
                    passport_logic:do_web_login(
                        <<"email">>, <<"synthetic@example.test">>, <<"pwd">>, web_values()
                    )
                ),
                ?assertEqual(0, meck:num_calls(user_device_ds, register_web, '_')),
                ?assertEqual(0, meck:num_calls(user_device_logic, kick_device, '_')),
                ?assertEqual(0, meck:num_calls(token_ds, encrypt_token, '_'))
            end)
        end,
        [{1, 1}, {2, 1}, {3, 1}, {0, 0}, {0, -1}]
    ).

web_credential_login_registers_then_kicks_then_signs_test() ->
    credential_case(ok, fun() ->
        %% Exercise real token_ds epoch selection, mock only its signing/config boundaries.
        meck:unload(token_ds),
        with_mocks(
            [
                {auth_session_ds, [
                    {current_epoch, fun(?UID) ->
                        ?assert(
                            meck:called(
                                user_device_logic,
                                kick_device,
                                [?UID, <<"web">>, <<"old-web-did">>]
                            )
                        ),
                        {ok, 7}
                    end}
                ]},
                {config_ds, [{env, fun(jwt_key, _) -> <<"synthetic-key">> end}]},
                {imboy_jwt, [
                    {sign, fun(Claims, <<"synthetic-key">>) ->
                        ?assertEqual(7, maps:get(<<"ep">>, Claims)),
                        ?assertEqual(456, maps:get(<<"dsid">>, Claims)),
                        ?assertEqual(<<"web-did">>, maps:get(<<"did">>, Claims)),
                        case maps:get(<<"sub">>, Claims) of
                            <<"tk">> -> <<"after-kick-tk">>;
                            <<"rtk">> -> <<"after-kick-rtk">>
                        end
                    end}
                ]}
            ],
            fun() ->
                {ok, Data} = passport_logic:do_web_login(
                    <<"email">>, <<"synthetic@example.test">>, <<"pwd">>, web_values()
                ),
                ?assertEqual(<<"after-kick-tk">>, maps:get(<<"token">>, Data)),
                ?assertEqual(<<"after-kick-rtk">>, maps:get(<<"refreshtoken">>, Data))
            end
        )
    end).

web_initial_login_epoch_unknown_never_signs_or_reports_success_test_() ->
    [
        ?_test(
            credential_case(ok, fun() ->
                with_mocks(
                    [{auth_session_ds, [{current_epoch, fun(?UID) -> EpochResult end}]}],
                    fun() ->
                        Result =
                            case Method of
                                password ->
                                    passport_logic:do_web_login(
                                        <<"email">>,
                                        <<"synthetic@example.test">>,
                                        <<"pwd">>,
                                        web_values()
                                    );
                                code ->
                                    passport_logic:do_web_login_by_code(
                                        <<"email">>,
                                        <<"synthetic@example.test">>,
                                        <<"123456">>,
                                        web_values()
                                    )
                            end,
                        ?assertEqual({error, service_unavailable}, Result),
                        ?assertEqual(0, meck:num_calls(token_ds, encrypt_token, '_')),
                        ?assertEqual(0, meck:num_calls(token_ds, encrypt_refreshtoken, '_')),
                        ?assertEqual(0, meck:num_calls(token_ds, issue_web_session, '_')),
                        ?assertEqual(0, meck:num_calls(elib_async, async, '_'))
                    end
                )
            end)
        )
     || EpochResult <- [{error, db_down}, undefined, {ok, 0}, {ok, <<"7">>}, {ok, 1.0}],
        Method <- [password, code]
    ].

web_login_invalid_did_rejected_before_authentication_test() ->
    lists:foreach(
        fun(Did) ->
            with_mocks(
                [
                    {cowboy_req, [
                        {header, fun
                            (<<"did">>, _, _) -> Did;
                            (_, _, Default) -> Default
                        end}
                    ]},
                    {passport_logic, []},
                    {elib_param, []},
                    {elib_response, response_mocks()}
                ],
                fun() ->
                    {ok, Req, _} = passport_handler:init(#{}, ?WEB#{action => login}),
                    ?assertEqual(400, maps:get(code, Req)),
                    ?assertEqual(0, meck:num_calls(passport_logic, do_login, '_')),
                    ?assertEqual(0, meck:num_calls(elib_param, post, '_'))
                end
            )
        end,
        [<<>>, 42, <<255>>, binary:copy(<<"x">>, 129)]
    ).

web_refresh_uses_only_verified_refresh_identity_test() ->
    with_mocks(
        [
            {cowboy_req, [{header, fun(<<"imboy-refreshtoken">>, _) -> <<"synthetic-rtk">> end}]},
            {throttle, [{check, fun(refreshtoken, <<"synthetic-rtk">>) -> ok end}]},
            {auth_ds, [
                {verify_web_refresh_token, fun(<<"synthetic-rtk">>) ->
                    {ok, ?UID, <<"verified-web-did">>, 7, 9999999999, 456}
                end}
            ]},
            {token_ds, [
                {encrypt_web_token, fun(?UID, <<"verified-web-did">>, 7, 456) -> <<"new-tk">> end}
            ]},
            {elib_response, response_mocks()}
        ],
        fun() ->
            {ok, Req, _} = passport_handler:init(#{}, ?WEB#{action => refreshtoken}),
            ?assertEqual(#{<<"token">> => <<"new-tk">>}, maps:get(payload, Req)),
            ?assertEqual(0, meck:num_calls(token_ds, decrypt_token, '_'))
        end
    ).

web_refresh_rejection_never_signs_test() ->
    with_mocks(
        [
            {cowboy_req, [{header, fun(<<"imboy-refreshtoken">>, _) -> <<"wrong-purpose">> end}]},
            {throttle, [{check, fun(_, _) -> ok end}]},
            {auth_ds, [{verify_web_refresh_token, fun(_) -> {error, 706, <<"invalid">>} end}]},
            {token_ds, []},
            {elib_response, response_mocks()}
        ],
        fun() ->
            {ok, Req, _} = passport_handler:init(#{}, ?WEB#{action => refreshtoken}),
            ?assertEqual(706, maps:get(code, Req)),
            ?assertEqual(401, maps:get(status, Req)),
            ?assertEqual(0, meck:num_calls(token_ds, encrypt_token, '_')),
            ?assertEqual(0, meck:num_calls(token_ds, encrypt_web_token, '_'))
        end
    ).

web_refresh_does_not_upgrade_verified_epoch_after_revocation_test() ->
    with_mocks(
        [
            {cowboy_req, [
                {header, fun(<<"imboy-refreshtoken">>, _) -> <<"synthetic-old-rtk">> end}
            ]},
            {throttle, [{check, fun(_, _) -> ok end}]},
            {auth_ds, [
                {verify_web_refresh_token, fun(_) ->
                    {ok, ?UID, <<"web-did">>, 2, 9999999999, 456}
                end}
            ]},
            {auth_session_ds, [{current_epoch, fun(?UID) -> {ok, 3} end}]},
            {config_ds, [{env, fun(jwt_key, _) -> <<"synthetic-key">> end}]},
            {imboy_jwt, [
                {sign, fun(Claims, _) ->
                    ?assertEqual(2, maps:get(<<"ep">>, Claims)),
                    ?assertEqual(456, maps:get(<<"dsid">>, Claims)),
                    ?assertEqual(<<"web-did">>, maps:get(<<"did">>, Claims)),
                    <<"still-epoch-two-tk">>
                end}
            ]},
            {elib_response, response_mocks()}
        ],
        fun() ->
            {ok, Req, _} = passport_handler:init(#{}, ?WEB#{action => refreshtoken}),
            ?assertEqual(#{<<"token">> => <<"still-epoch-two-tk">>}, maps:get(payload, Req)),
            ?assertEqual(0, meck:num_calls(auth_session_ds, current_epoch, '_'))
        end
    ).

%% BE-22:LOCAL:02 —— 缺头与存储失败都是确定性拒绝，绝不签发（无假成功）。
web_refresh_missing_header_yields_deterministic_rejection_test() ->
    with_mocks(
        [
            {cowboy_req, [{header, fun(<<"imboy-refreshtoken">>, _) -> undefined end}]},
            {throttle, [{check, fun(refreshtoken, undefined) -> ok end}]},
            {auth_ds, [
                {verify_web_refresh_token, fun(undefined) -> {error, 706, <<"invalid">>} end}
            ]},
            {token_ds, []},
            {elib_response, response_mocks()}
        ],
        fun() ->
            {ok, Req, _} = passport_handler:init(#{}, ?WEB#{action => refreshtoken}),
            ?assertEqual(401, maps:get(status, Req)),
            ?assertNot(maps:is_key(payload, Req)),
            ?assertEqual(0, meck:num_calls(token_ds, encrypt_web_token, '_')),
            ?assertEqual(0, meck:num_calls(token_ds, encrypt_token, '_')),
            ?assertEqual(0, meck:num_calls(token_ds, encrypt_refreshtoken, '_'))
        end
    ).

web_refresh_storage_failure_is_rejection_not_fake_success_test() ->
    lists:foreach(
        fun(Failure) ->
            with_mocks(
                [
                    {cowboy_req, [
                        {header, fun(<<"imboy-refreshtoken">>, _) -> <<"synthetic-rtk">> end}
                    ]},
                    {throttle, [{check, fun(_, _) -> ok end}]},
                    {auth_ds, [{verify_web_refresh_token, fun(_) -> Failure end}]},
                    {token_ds, []},
                    {elib_response, response_mocks()}
                ],
                fun() ->
                    {ok, Req, _} = passport_handler:init(#{}, ?WEB#{action => refreshtoken}),
                    ?assertEqual(401, maps:get(status, Req)),
                    ?assertNot(maps:is_key(payload, Req)),
                    ?assertEqual(0, meck:num_calls(token_ds, encrypt_web_token, '_')),
                    ?assertEqual(0, meck:num_calls(token_ds, encrypt_token, '_')),
                    ?assertEqual(0, meck:num_calls(token_ds, encrypt_refreshtoken, '_'))
                end
            )
        end,
        [{error, 705, <<"device_storage_unavailable">>}, {error, 503, <<"refresh_db_down">>}]
    ).

%% BE-22:LOCAL:05 —— features 全关（发布配置漂移）时 bootstrap 载荷键集稳定：
%% 无身份/凭据残留键，ws_url 改写与 RSA 开关归一不漂移，关闭能力无从旁路重开。
web_bootstrap_payload_stable_under_feature_drift_test() ->
    with_mocks(
        [
            {cowboy_req, [{header, fun(_, _, Default) -> Default end}]},
            {config_ds, [
                {env, fun(login_rsa_pub_key) -> <<"public-key">> end},
                {env, fun
                    (ws_url, _) -> <<"wss://trusted.test/old?credential=redacted#fragment">>;
                    (login_pwd_rsa_encrypt, _) -> <<"on">>;
                    (_, Default) -> Default
                end}
            ]},
            {elib_oss, [{public_base_url, fun() -> <<"https://assets.test">> end}]},
            {imboy_policy, [{effective_features, fun() -> #{} end}]},
            {product_experience, [
                {effective_binary, fun() -> <<"community">> end},
                {config_version, fun() -> <<"test-version">> end}
            ]},
            {product_terminology, [{public, fun(_) -> #{} end}]},
            {elib_response, response_mocks()}
        ],
        fun() ->
            {ok, Req, _} = index_handler:init(#{}, #{action => web_bootstrap}),
            ?assertEqual(200, maps:get(status, Req)),
            Payload = maps:get(payload, Req),
            ?assertEqual(
                lists:sort([
                    <<"ws_url">>,
                    <<"login_pwd_rsa_encrypt">>,
                    <<"login_rsa_pub_key">>,
                    <<"public_base_url">>,
                    <<"attachment_download_targets">>,
                    <<"features">>,
                    <<"effective_product_experience">>,
                    <<"config_version">>,
                    <<"terminology">>
                ]),
                lists:sort(maps:keys(Payload))
            ),
            ?assertEqual(#{}, maps:get(<<"features">>, Payload)),
            ?assertEqual(<<"wss://trusted.test/api/v1/web/ws">>, maps:get(<<"ws_url">>, Payload)),
            ?assertEqual(<<"1">>, maps:get(<<"login_pwd_rsa_encrypt">>, Payload)),
            lists:foreach(
                fun(Key) -> ?assertNot(maps:is_key(Key, Payload)) end,
                [<<"uid">>, <<"token">>, <<"refreshtoken">>, <<"did">>, <<"device_session_id">>]
            )
        end
    ).

web_qr_create_rejects_seat_before_storage_test() ->
    with_qr(
        cache,
        #{<<"device_id">> => <<"web-did">>, <<"purpose">> => <<"seat">>},
        fun() ->
            {stop, Req, _} = qr_login_handler:handle_request(
                #{method => <<"POST">>},
                ?WEB#{action => create}
            ),
            ?assertEqual(400, maps:get(code, Req)),
            ?assertEqual(0, meck:num_calls(imboy_cache, set, '_'))
        end
    ).

web_qr_create_fixes_platform_and_bounds_device_fields_test() ->
    with_qr(
        cache,
        #{
            <<"device_id">> => binary:copy(<<"中"/utf8>>, 128),
            <<"device_name">> => binary:copy(<<"中"/utf8>>, 80),
            <<"platform">> => <<"ios">>
        },
        fun() ->
            meck:expect(qr_login_session_ds, create, fun(Session) ->
                ?assertEqual(<<"web">>, maps:get(platform, Session)),
                ?assertEqual(<<"human">>, maps:get(purpose, Session)),
                ok
            end),
            {stop, Req, _} = qr_login_handler:handle_request(
                #{method => <<"POST">>},
                ?WEB#{action => create}
            ),
            ?assertEqual(200, maps:get(status, Req))
        end
    ),
    lists:foreach(
        fun(Body) ->
            with_qr(cache, Body, fun() ->
                {stop, Req, _} = qr_login_handler:handle_request(
                    #{method => <<"POST">>},
                    ?WEB#{action => create}
                ),
                ?assertEqual(400, maps:get(code, Req)),
                ?assertEqual(0, meck:num_calls(imboy_cache, set, '_'))
            end)
        end,
        [
            #{<<"device_id">> => binary:copy(<<"x">>, 129)},
            #{<<"device_id">> => <<"web-did">>, <<"device_name">> => 42},
            #{<<"device_id">> => <<"web-did">>, <<"device_name">> => binary:copy(<<"x">>, 81)},
            {raw_json, <<"{\"device_id\":\"web-did\",\"device_name\":\"", 255, "\"}">>}
        ]
    ).

web_qr_consumers_reject_mixed_session_test() ->
    lists:foreach(
        fun(Authority) ->
            lists:foreach(
                fun({Purpose, Platform}) ->
                    with_qr(Authority, #{<<"session_token">> => <<"session">>}, fun() ->
                        mock_session(Authority, qr_session(Purpose, Platform)),
                        {stop, Status, _} = qr_login_handler:handle_request(
                            #{method => <<"GET">>},
                            ?WEB#{action => status}
                        ),
                        ?assertEqual(403, maps:get(code, Status)),
                        {stop, Cancel, _} = qr_login_handler:handle_request(
                            #{method => <<"POST">>},
                            ?WEB#{action => cancel}
                        ),
                        ?assertEqual(403, maps:get(code, Cancel)),
                        ?assertMatch({ok, _, _}, qr_login_sse_handler:init(#{}, ?WEB)),
                        ?assertEqual(
                            0, meck:num_calls(qr_login_session_ds, claim_login_token, '_')
                        ),
                        ?assertEqual(
                            0, meck:num_calls(qr_login_session_ds, transition_cancel, '_')
                        ),
                        ?assertEqual(0, meck:num_calls(cowboy_req, stream_reply, '_'))
                    end)
                end,
                [{<<"seat">>, <<"web">>}, {<<"human">>, <<"ios">>}]
            )
        end,
        [cache, pg]
    ).

qr_registration_failure_precedes_signing_despite_authority_setting_test() ->
    lists:foreach(
        fun(Authority) ->
            with_qr(Authority, #{<<"qr_token">> => <<"qr">>}, fun() ->
                mock_session(Authority, qr_session(<<"human">>, <<"web">>)),
                meck:expect(qr_login_logic, log_device_login, fun(_, _, _, _) ->
                    {error, db_down}
                end),
                {stop, Req, _} = qr_login_handler:handle_request(
                    #{method => <<"POST">>},
                    #{action => confirm, current_uid => ?UID}
                ),
                ?assertEqual(503, maps:get(status, Req)),
                ?assertEqual(0, meck:num_calls(token_ds, encrypt_token, '_')),
                ?assertEqual(0, meck:num_calls(token_ds, encrypt_web_token, '_')),
                ?assertEqual(0, meck:num_calls(qr_login_session_ds, transition_confirm, '_')),
                ?assertEqual(0, meck:num_calls(qr_login_event_ds, notify, '_')),
                ?assertEqual(0, meck:num_calls(imboy_cache, set, '_'))
            end)
        end,
        [cache, pg]
    ).

web_qr_epoch_unknown_never_signs_confirms_or_notifies_test() ->
    lists:foreach(
        fun(Authority) ->
            lists:foreach(
                fun(EpochResult) ->
                    with_qr(Authority, #{<<"qr_token">> => <<"qr">>}, fun() ->
                        mock_session(Authority, qr_session(<<"human">>, <<"web">>)),
                        meck:expect(qr_login_logic, log_device_login, fun(_, _, _, _) ->
                            {ok, 456}
                        end),
                        with_mocks(
                            [{auth_session_ds, [{current_epoch, fun(?UID) -> EpochResult end}]}],
                            fun() ->
                                {stop, Req, _} = qr_login_handler:handle_request(
                                    #{method => <<"POST">>},
                                    #{action => confirm, current_uid => ?UID}
                                ),
                                ?assertEqual(503, maps:get(status, Req)),
                                ?assertEqual(0, meck:num_calls(token_ds, encrypt_token, '_')),
                                ?assertEqual(0, meck:num_calls(token_ds, encrypt_web_token, '_')),
                                ?assertEqual(
                                    0, meck:num_calls(qr_login_session_ds, transition_confirm, '_')
                                ),
                                ?assertEqual(0, meck:num_calls(qr_login_event_ds, notify, '_')),
                                ?assertEqual(0, meck:num_calls(imboy_cache, set, '_'))
                            end
                        )
                    end)
                end,
                [{error, db_down}, undefined, {ok, 0}, {ok, <<"7">>}]
            )
        end,
        [cache, pg]
    ).

%% Web and legacy consumers share the PG claim even with a stale cache setting.
%% These DS mocks verify handler routing; PG atomicity is covered by
%% qr_login_claim_acceptance_tests and the multi-node LEE-5 acceptance gate.
web_sse_claims_pg_credentials_once_for_every_authority_test() ->
    lists:foreach(
        fun(Authority) ->
            with_qr(Authority, #{}, fun() ->
                mock_session(Authority, confirmed_web_session()),
                Calls = atomics:new(1, []),
                meck:expect(qr_login_session_ds, claim_login_token, fun(<<"session">>) ->
                    case atomics:add_get(Calls, 1, 1) of
                        1 -> {ok, #{login_token => <<"authoritative-tk">>, scanned_by => ?UID}};
                        _ -> {error, not_claimable}
                    end
                end),
                meck:expect(cowboy_req, stream_body, fun(_, _, _) -> ok end),
                Event = #{
                    <<"status">> => <<"confirmed">>,
                    <<"token">> => <<"untrusted-event-token">>,
                    <<"uid">> => 999
                },
                State = #{session_token => <<"session">>, web_human => true},
                ?assertMatch({stop, _, _}, qr_login_sse_handler:info(Event, #{}, State)),
                ?assertMatch({stop, _, _}, qr_login_sse_handler:info(Event, #{}, State)),
                [Payload] = web_sse_payloads(),
                ?assertEqual(<<"authoritative-tk">>, maps:get(<<"token">>, Payload)),
                ?assertEqual(?UID, maps:get(<<"uid">>, Payload)),
                ?assertEqual(2, atomics:get(Calls, 1)),
                ?assertEqual(0, meck:num_calls(imboy_cache, get, '_')),
                ?assertEqual(0, meck:num_calls(imboy_cache, set, '_'))
            end)
        end,
        [cache, pg, postgres, invalid]
    ).

web_sse_cancelled_session_does_not_emit_old_credentials_test() ->
    with_qr(cache, #{}, fun() ->
        Session = (confirmed_web_session())#{<<"status">> => <<"cancelled">>},
        mock_session(cache, Session),
        meck:expect(qr_login_session_ds, claim_login_token, fun(_) -> {error, not_claimable} end),
        meck:expect(cowboy_req, stream_body, fun(_, _, _) -> ok end),
        ?assertMatch(
            {stop, _, _},
            qr_login_sse_handler:info(
                #{<<"status">> => <<"confirmed">>, <<"token">> => <<"old-token">>},
                #{},
                #{session_token => <<"session">>, web_human => true}
            )
        ),
        ?assertEqual([], web_sse_payloads())
    end).

web_sse_pg_claim_failure_does_not_emit_credentials_test() ->
    lists:foreach(
        fun(Failure) ->
            with_qr(cache, #{}, fun() ->
                mock_session(cache, confirmed_web_session()),
                meck:expect(qr_login_session_ds, claim_login_token, fun(_) -> Failure end),
                meck:expect(cowboy_req, stream_body, fun(_, _, _) -> ok end),
                ?assertMatch(
                    {stop, _, _},
                    qr_login_sse_handler:info(
                        #{<<"status">> => <<"confirmed_ready">>, <<"token">> => <<"old-token">>},
                        #{},
                        #{session_token => <<"session">>, web_human => true}
                    )
                ),
                ?assertEqual([], web_sse_payloads()),
                ?assertEqual(1, meck:num_calls(qr_login_session_ds, claim_login_token, '_')),
                ?assertEqual(0, meck:num_calls(imboy_cache, get, '_'))
            end)
        end,
        [{error, {pg_error, closed}}, {error, {claim_gate, unavailable}}]
    ).

web_qr_confirm_binds_device_row_before_pg_transition_test() ->
    with_qr(cache, #{<<"qr_token">> => <<"qr">>}, fun() ->
        mock_session(cache, qr_session(<<"human">>, <<"web">>)),
        meck:expect(qr_login_logic, log_device_login, fun(?UID, <<"web-did">>, _, <<"web">>) ->
            {ok, 456}
        end),
        with_mocks([{auth_session_ds, [{current_epoch, fun(?UID) -> {ok, 7} end}]}], fun() ->
            meck:expect(token_ds, encrypt_web_token, fun(?UID, <<"web-did">>, 7, 456) ->
                ?assertEqual(1, meck:num_calls(qr_login_logic, log_device_login, '_')),
                <<"bound-web-token">>
            end),
            meck:expect(
                qr_login_session_ds,
                transition_confirm,
                fun(<<"session">>, ?UID, <<"bound-web-token">>) ->
                    ?assertEqual(1, meck:num_calls(token_ds, encrypt_web_token, '_')),
                    {ok, #{<<"status">> => <<"confirmed">>}}
                end
            ),
            meck:expect(
                qr_login_event_ds,
                notify,
                fun(<<"session">>, #{<<"status">> := <<"confirmed_ready">>} = Event) ->
                    ?assertEqual(1, map_size(Event)),
                    ?assertEqual(1, meck:num_calls(qr_login_session_ds, transition_confirm, '_')),
                    {ok, 1}
                end
            ),
            {stop, Req, _} = qr_login_handler:handle_request(
                #{method => <<"POST">>}, #{action => confirm, current_uid => ?UID}
            ),
            ?assertEqual(#{<<"status">> => <<"confirmed">>}, maps:get(payload, Req)),
            ?assertEqual(0, meck:num_calls(token_ds, encrypt_token, '_')),
            ?assertEqual(0, meck:num_calls(imboy_cache, set, '_'))
        end)
    end).

web_qr_missing_device_row_never_signs_test() ->
    with_qr(pg, #{<<"qr_token">> => <<"qr">>}, fun() ->
        mock_session(pg, qr_session(<<"human">>, <<"web">>)),
        meck:expect(qr_login_logic, log_device_login, fun(_, _, _, _) -> ok end),
        {stop, Req, _} = qr_login_handler:handle_request(
            #{method => <<"POST">>}, #{action => confirm, current_uid => ?UID}
        ),
        ?assertEqual(503, maps:get(status, Req)),
        ?assertEqual(0, meck:num_calls(token_ds, encrypt_web_token, '_')),
        ?assertEqual(0, meck:num_calls(qr_login_session_ds, transition_confirm, '_'))
    end).

web_sse_and_polling_share_one_pg_claim_test() ->
    with_qr(cache, #{}, fun() ->
        mock_session(cache, confirmed_web_session()),
        Table = ets:new(web_pg_claim, [set, public]),
        ets:insert(Table, {claim, #{login_token => <<"one-token">>, scanned_by => ?UID}}),
        try
            meck:expect(qr_login_session_ds, claim_login_token, fun(<<"session">>) ->
                case ets:take(Table, claim) of
                    [{claim, Claim}] -> {ok, Claim};
                    [] -> {error, not_claimable}
                end
            end),
            meck:expect(cowboy_req, stream_body, fun(_, _, _) -> ok end),
            Parent = self(),
            Ref = make_ref(),
            Pids = [
                spawn_link(fun() ->
                    receive
                        {go, Ref} -> Parent ! {Ref, self(), web_pg_consumer(Kind)}
                    end
                end)
             || Kind <- [sse, web_status, native_status]
            ],
            [Pid ! {go, Ref} || Pid <- Pids],
            Results = [
                receive
                    {Ref, Pid, Result} -> Result
                after 3000 -> error(pg_claim_routing_timeout)
                end
             || Pid <- Pids
            ],
            PollPayloads = [Payload || {stop, #{payload := Payload}, _} <- Results],
            [Winner] = web_sse_payloads() ++ PollPayloads,
            ?assertEqual(<<"one-token">>, maps:get(<<"token">>, Winner)),
            ?assertEqual(?UID, maps:get(<<"uid">>, Winner)),
            ?assertEqual(3, meck:num_calls(qr_login_session_ds, claim_login_token, '_')),
            ?assertEqual(0, meck:num_calls(imboy_cache, get, '_'))
        after
            ets:delete(Table)
        end
    end).

web_pg_consumer(sse) ->
    qr_login_sse_handler:info(
        #{<<"status">> => <<"confirmed_ready">>},
        #{},
        #{session_token => <<"session">>, web_human => true}
    );
web_pg_consumer(web_status) ->
    qr_login_handler:handle_request(#{method => <<"GET">>}, ?WEB#{action => status});
web_pg_consumer(native_status) ->
    qr_login_handler:handle_request(#{method => <<"GET">>}, #{action => status}).

confirmed_web_session() ->
    (qr_session(<<"human">>, <<"web">>))#{<<"status">> => <<"confirmed">>}.

web_sse_payloads() ->
    [
        jsone:decode(binary:part(Json, 0, byte_size(Json) - 2))
     || {_, {cowboy_req, stream_body, [<<"data: ", Json/binary>>, _, _]}, _} <-
            meck:history(cowboy_req)
    ].

qr_logic_maps_device_fields_and_propagates_errors_test() ->
    with_mocks(
        [
            {elib_dt, [{now, fun() -> <<"synthetic-now">> end}]},
            {user_ds, [
                {find_by_id, fun(?UID, <<"status,account_type">>) ->
                    #{<<"status">> => 1, <<"account_type">> => 0}
                end}
            ]},
            {user_device_ds, [
                {register_web, fun(<<"synthetic-now">>, ?UID, <<"web-did">>, Values) ->
                    ?assertEqual(<<"web">>, maps:get(<<"cos">>, Values)),
                    ?assertEqual(<<"Browser">>, maps:get(<<"dname">>, Values)),
                    {error, db_down}
                end},
                {save, fun(_, _, _, Values) ->
                    ?assertEqual(<<"ios">>, maps:get(<<"cos">>, Values)),
                    ?assertEqual(<<"Phone">>, maps:get(<<"dname">>, Values)),
                    {error, db_down}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, db_down},
                qr_login_logic:log_device_login(
                    ?UID, <<"web-did">>, <<"Browser">>, <<"web">>
                )
            ),
            ?assertEqual(
                {error, db_down},
                qr_login_logic:log_device_login(
                    ?UID, <<"native-did">>, <<"Phone">>, <<"ios">>
                )
            )
        end
    ).

login_case(Registration, Assert) ->
    with_mocks(
        [
            {cowboy_req, [
                {header, fun
                    (<<"did">>, _, _) -> <<"web-did">>;
                    (<<"cos">>, _, _) -> <<"ios">>;
                    (<<"dname">>, _, _) -> <<"Browser">>;
                    (_, _, Default) -> Default
                end}
            ]},
            {elib_param, [
                {post, fun(_) ->
                    #{
                        <<"type">> => <<"email">>,
                        <<"account">> => <<"synthetic@example.test">>,
                        <<"pwd">> => <<"pwd">>
                    }
                end}
            ]},
            {elib_req, [{get_client_ip, fun(_) -> <<"127.0.0.1">> end}]},
            {elib_cipher, [{safe_rsa_decrypt, fun(Pwd, _) -> Pwd end}]},
            {passport_logic, [
                {do_web_login, fun(<<"email">>, _, <<"pwd">>, Values) ->
                    ?assertEqual(<<"web-did">>, maps:get(<<"did">>, Values)),
                    ?assertEqual(<<"web">>, maps:get(<<"cos">>, Values)),
                    ?assertEqual(<<"web">>, maps:get(<<"dtype">>, Values)),
                    case Registration of
                        ok ->
                            {ok, #{
                                <<"uid">> => ?UID,
                                <<"token">> => <<"synthetic-tk">>,
                                <<"refreshtoken">> => <<"synthetic-rtk">>,
                                <<"setting">> => #{}
                            }};
                        Error ->
                            Error
                    end
                end},
                {find_user_setting, fun(?UID) -> #{} end}
            ]},
            {elib_response, response_mocks()}
        ],
        fun() ->
            %% This suite owns the user_server name for the test scope. In the
            %% app-booted shared eunit VM the real user_server may already be
            %% registered — take over the name (never assert it absent) and
            %% restore the original registration afterwards, so login_success
            %% casts always land on this process, never on the real server.
            RealUs =
                case whereis(user_server) of
                    undefined ->
                        none;
                    Pid when is_pid(Pid) ->
                        unregister(user_server),
                        Pid
                end,
            true = register(user_server, self()),
            try
                {ok, Req, _} = passport_handler:init(#{}, ?WEB#{action => login}),
                ?assertEqual(0, meck:num_calls(passport_logic, record_login_device, '_')),
                Assert(Req)
            after
                unregister(user_server),
                case RealUs of
                    none -> ok;
                    RealPid -> catch register(user_server, RealPid)
                end,
                _ = login_cast_count()
            end
        end
    ).

login_cast_count() ->
    receive
        {'$gen_cast', {login_success, ?UID, _}} -> 1 + login_cast_count()
    after 0 -> 0
    end.

web_values() ->
    #{
        <<"did">> => <<"web-did">>,
        <<"cos">> => <<"web">>,
        <<"dtype">> => <<"web">>,
        <<"dname">> => <<"Browser">>
    }.

credential_case(Registration, Test) ->
    User = #{
        <<"id">> => ?UID,
        <<"password">> => <<"synthetic-hash">>,
        <<"email">> => <<"synthetic@example.test">>,
        <<"nickname">> => <<"test">>,
        <<"avatar">> => <<>>,
        <<"account">> => <<"synthetic">>,
        <<"gender">> => 0,
        <<"region">> => <<>>,
        <<"sign">> => <<>>,
        <<"status">> => 1
    },
    with_mocks(
        [
            {elib_type, [{is_email, fun(_) -> true end}]},
            {elib_async, [{async, fun(_) -> ok end}]},
            {user_ds, [
                {lookup, fun(email, <<"synthetic@example.test">>, _) -> {ok, User} end},
                {find_by_id, fun(?UID, <<"status,account_type">>) ->
                    #{<<"status">> => 1, <<"account_type">> => 0}
                end}
            ]},
            {login_attempt_ds, [
                {begin_attempt, fun(_, _, default) -> {ok, synthetic_ref} end},
                {end_attempt, fun(synthetic_ref, _) -> ok end}
            ]},
            {login_security_logic, [{check_login_allowed, fun(_) -> {ok, true} end}]},
            {verification_code_ds, [{consume, fun(_, <<"123456">>) -> {ok, <<"123456">>} end}]},
            {elib_password, [
                {verify, fun(<<"pwd">>, <<"synthetic-hash">>) -> {ok, true} end},
                {verify_and_plan, fun(_, _) -> {ok, #{version => v2}} end}
            ]},
            {elib_dt, [{now, fun() -> <<"synthetic-now">> end}]},
            {user_setting_ds, [
                {find_web_by_uid, fun(?UID) -> {ok, #{<<"nested">> => #{<<"keep">> => true}}} end}
            ]},
            {user_device_ds, [
                {register_web, fun(_, ?UID, <<"web-did">>, Values) ->
                    ?assertEqual(<<"web">>, maps:get(<<"cos">>, Values)),
                    case Registration of
                        ok -> {ok, 456};
                        Other -> Other
                    end
                end}
            ]},
            {user_device_logic, [
                {check_login_conflict, fun(?UID, <<"web">>) ->
                    ?assertEqual(0, meck:num_calls(user_device_logic, kick_device, '_')),
                    {ok, conflict, #{<<"device_id">> => <<"old-web-did">>}}
                end},
                {kick_device, fun(?UID, <<"web">>, <<"old-web-did">>) ->
                    ?assert(
                        meck:called(user_device_ds, register_web, ['_', ?UID, <<"web-did">>, '_'])
                    ),
                    ok
                end}
            ]},
            {token_ds, [
                {encrypt_token, fun(?UID, <<"web-did">>) ->
                    ?assert(
                        meck:called(user_device_logic, kick_device, [
                            ?UID, <<"web">>, <<"old-web-did">>
                        ])
                    ),
                    <<"after-kick-tk">>
                end},
                {encrypt_refreshtoken, fun(?UID, <<"web-did">>) ->
                    ?assert(
                        meck:called(user_device_logic, kick_device, [
                            ?UID, <<"web">>, <<"old-web-did">>
                        ])
                    ),
                    <<"after-kick-rtk">>
                end}
            ]}
        ],
        Test
    ).

with_qr(Authority, Body, Test) ->
    Old = application:get_env(imboy, qr_login_authority),
    application:set_env(imboy, qr_login_authority, Authority),
    try
        with_mocks(
            [
                {cowboy_req, [
                    {method, fun(Req) -> maps:get(method, Req) end},
                    {read_body, fun(Req) ->
                        Raw =
                            case Body of
                                {raw_json, Bytes} -> Bytes;
                                _ -> jsone:encode(Body)
                            end,
                        {ok, Raw, Req}
                    end},
                    {parse_qs, fun(_) -> [{<<"session_token">>, <<"session">>}] end},
                    {reply, fun(Status, _, _, Req) -> Req#{status => Status} end}
                ]},
                {imboy_cache, [
                    {get, fun(_) -> error(unexpected_cache_read) end},
                    {set, fun(_, _, _) -> error(unexpected_cache_write) end}
                ]},
                {qr_login_session_ds, [
                    {ensure_claim_compatible, fun() -> ok end},
                    {find_by_qr_token, fun(<<"qr">>) ->
                        #{<<"session_token">> => <<"session">>}
                    end}
                ]},
                {qr_login_logic, []},
                {qr_login_event_ds, []},
                {token_ds, []},
                {elib_response, response_mocks()}
            ],
            Test
        )
    after
        case Old of
            undefined -> application:unset_env(imboy, qr_login_authority);
            {ok, V} -> application:set_env(imboy, qr_login_authority, V)
        end
    end.

mock_session(_ConfiguredAuthority, Session) ->
    meck:expect(qr_login_session_ds, find_by_session_token, fun(<<"session">>) -> Session end).

qr_session(Purpose, Platform) ->
    #{
        <<"status">> => <<"scanned">>,
        <<"scanned_by">> => ?UID,
        <<"purpose">> => Purpose,
        <<"platform">> => Platform,
        <<"device_id">> => <<"web-did">>,
        <<"device_name">> => <<"Browser">>,
        <<"expires_at">> => erlang:system_time(millisecond) + 60000
    }.

response_mocks() ->
    [
        {success, fun(Req, Payload) -> Req#{status => 200, payload => Payload} end},
        {success, fun(Req, Payload, _) -> Req#{status => 200, payload => Payload} end},
        {error, fun(Req, _, Code) -> Req#{status => 200, code => Code} end},
        {error_with_status, fun(Req, Status, _, Code) -> Req#{status => Status, code => Code} end}
    ].

with_mocks(Mocks, Test) ->
    try
        lists:foreach(
            fun({Module, Expectations}) ->
                Options =
                    case Module of
                        qr_login_logic -> [passthrough, no_link, non_strict, unstick];
                        qr_login_event_ds -> [passthrough, no_link, non_strict, unstick];
                        _ -> [no_link, non_strict, unstick]
                    end,
                meck:new(Module, Options),
                lists:foreach(fun({Name, Fun}) -> meck:expect(Module, Name, Fun) end, Expectations)
            end,
            Mocks
        ),
        Test()
    after
        lists:foreach(fun({Module, _}) -> catch meck:unload(Module) end, lists:reverse(Mocks))
    end.

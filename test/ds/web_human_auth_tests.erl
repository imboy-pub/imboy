-module(web_human_auth_tests).
-include_lib("eunit/include/eunit.hrl").
-include("error_code.hrl").
-include("common.hrl").

%% 独立 mock fixture：不 boot 应用、不访问数据库、不运行设备清理。
fixture(Modules, Test) ->
    {setup, fun() -> lists:foreach(fun(M) -> ok = meck:new(M, [no_link]) end, Modules) end,
        fun(_) -> lists:foreach(fun meck:unload/1, Modules) end, fun(_) -> ?_test(Test()) end}.

web_token_requires_purpose_device_epoch_test_() ->
    fixture([token_ds], fun() ->
        Invalid = [
            {123, <<"rtk">>, <<"web-1">>, 2},
            {123, <<"seat_tk">>, <<"web-1">>, 2},
            {123, <<"application">>, <<"web-1">>, 2},
            {123, <<"tk">>, <<>>, 2},
            {123, <<"tk">>, <<"web-1">>, undefined},
            {123, <<"tk">>, <<"web-1">>, 0},
            {123, <<"tk">>, <<"web-1">>, malformed},
            {0, <<"tk">>, <<"web-1">>, 2}
        ],
        lists:foreach(
            fun({Uid, Purpose, Did, Ep}) ->
                meck:expect(token_ds, decrypt_web_token, fun(_, _) ->
                    {ok, Uid, 2000000000, Purpose, Did, Ep, 456}
                end),
                ?assertMatch({error, ?ERR_TOKEN_INVALID, _}, auth_ds:verify_web_token(<<"token">>))
            end,
            Invalid
        ),
        meck:expect(token_ds, decrypt_web_token, fun(_, _) ->
            {ok, 123, 2000000000, <<"tk">>, <<"web-1">>, 2, 456}
        end),
        ?assertMatch(
            {error, ?ERR_TOKEN_INVALID, _},
            auth_ds:verify_web_refresh_token(<<"token">>)
        ),
        meck:expect(token_ds, decrypt_web_token, fun(_, _) -> {error, 705, <<"expired">>, #{}} end),
        ?assertEqual({error, 705, <<"expired">>}, auth_ds:verify_web_token(<<"token">>)),
        meck:expect(token_ds, decrypt_web_token, fun(_, _) -> erlang:error(unavailable) end),
        ?assertMatch({error, ?ERR_TOKEN_INVALID, _}, auth_ds:verify_web_token(<<"token">>))
    end).

web_session_revocation_account_and_fail_closed_test_() ->
    fixture([token_ds, user_ds, user_device_ds, auth_session_ds], fun() ->
        meck:expect(token_ds, decrypt_web_token, fun(Token, _) ->
            Purpose =
                case Token of
                    <<"refresh">> -> <<"rtk">>;
                    _ -> <<"tk">>
                end,
            {ok, 123, 2000000000, Purpose, <<"web-1">>, 2, 456}
        end),
        meck:expect(user_ds, find_by_id, fun(123, _) ->
            #{<<"status">> => 1, <<"account_type">> => 0}
        end),
        meck:expect(user_device_ds, is_active_web, fun(123, _, 456) -> true end),
        meck:expect(auth_session_ds, current_epoch, fun(123) -> {ok, 2} end),
        lists:foreach(
            fun(Did) ->
                ?assert(auth_ds:web_session_valid(123, Did, 2, 456))
            end,
            valid_schema_dids()
        ),
        lists:foreach(
            fun(Did) ->
                ?assertNot(auth_ds:web_session_valid(123, Did, 2, 456))
            end,
            invalid_schema_dids()
        ),
        ?assertEqual(
            {ok, 123, <<"web-1">>, 2, 2000000000, 456},
            auth_ds:verify_web_token(<<"Bearer token">>)
        ),
        ?assertEqual(
            {ok, 123, <<"web-1">>, 2, 2000000000, 456},
            auth_ds:verify_web_refresh_token(<<"refresh">>)
        ),
        Env = #{handler_opts => #{jwt_purpose => web_human}},
        ?assertMatch(
            {ok, #{}, #{
                handler_opts := #{
                    current_uid := 123,
                    current_did := <<"web-1">>,
                    session_epoch := 2,
                    token_expire_at := 2000000000,
                    device_session_id := 456
                }
            }},
            auth_ds:condition(false, false, <<"token">>, #{}, Env)
        ),
        lists:foreach(
            fun(Status) ->
                meck:expect(user_ds, find_by_id, fun(123, _) ->
                    #{<<"status">> => Status, <<"account_type">> => 0}
                end),
                ?assertEqual(
                    Status =:= 1 orelse Status =:= 2,
                    auth_ds:web_session_valid(123, <<"web-1">>, 2, 456)
                )
            end,
            [-2, -1, 0, 1, 2, 3]
        ),
        lists:foreach(
            fun(User) ->
                meck:expect(user_ds, find_by_id, fun(123, _) -> User end),
                ?assertNot(auth_ds:web_session_valid(123, <<"web-1">>, 2, 456))
            end,
            [#{}, #{<<"status">> => 1}, #{<<"status">> => 1, <<"account_type">> => 1}]
        ),
        meck:expect(user_ds, find_by_id, fun(123, _) ->
            #{<<"status">> => 1, <<"account_type">> => 0}
        end),
        %% Native、未知、已删除/非活跃设备均由精确 Web 点查返回 false。
        meck:expect(user_device_ds, is_active_web, fun(_, _, _) -> false end),
        ?assertNot(auth_ds:web_session_valid(123, <<"web-1">>, 2, 456)),
        meck:expect(user_device_ds, is_active_web, fun(_, _, _) -> true end),
        lists:foreach(
            fun(Epoch) ->
                meck:expect(auth_session_ds, current_epoch, fun(_) -> Epoch end),
                ?assertNot(auth_ds:web_session_valid(123, <<"web-1">>, 2, 456))
            end,
            [{ok, 3}, {ok, 1}, {error, unavailable}]
        ),
        meck:expect(auth_session_ds, current_epoch, fun(_) -> {ok, 2} end),
        meck:expect(user_ds, find_by_id, fun(_, _) -> erlang:error(db_down) end),
        ?assertNot(auth_ds:web_session_valid(123, <<"web-1">>, 2, 456)),
        ?assertNot(auth_ds:web_session_valid(123, <<>>, 2, 456)),
        ?assertNot(auth_ds:web_session_valid(123, <<"web-1">>, undefined, 456))
    end).

web_device_ds_fail_closed_and_registration_test_() ->
    fixture([user_device_repo], fun() ->
        lists:foreach(
            fun(Result) ->
                meck:expect(user_device_repo, is_active_web, fun(123, <<"web-1">>, 456) ->
                    Result
                end),
                ?assertEqual(
                    Result =:= {ok, true}, user_device_ds:is_active_web(123, <<"web-1">>, 456)
                )
            end,
            [{ok, true}, {ok, false}, {error, db_down}, unexpected]
        ),
        meck:expect(user_device_repo, is_active_web, fun(_, _, _) -> erlang:error(db_down) end),
        ?assertNot(user_device_ds:is_active_web(123, <<"web-1">>, 456)),
        meck:expect(user_device_repo, save_web, fun(Now, 123, <<"web-1">>, Values) ->
            ?assertEqual(<<"now">>, Now),
            ?assertEqual(<<"web">>, maps:get(<<"cos">>, Values)),
            ?assertEqual(<<"Browser">>, maps:get(<<"dname">>, Values)),
            {ok, 456}
        end),
        meck:expect(user_device_repo, is_active_web, fun(_, _, _) -> {ok, true} end),
        ?assertEqual(
            {ok, 456},
            user_device_ds:register_web(
                <<"now">>,
                123,
                <<"web-1">>,
                #{<<"cos">> => <<"ios">>, <<"device_name">> => <<"Browser">>}
            )
        ),
        meck:expect(user_device_repo, is_active_web, fun(_, _, _) -> {ok, false} end),
        ?assertEqual(
            {error, unavailable},
            user_device_ds:register_web(
                <<"now">>,
                123,
                <<"web-1">>,
                #{<<"device_name">> => <<"Browser">>}
            )
        ),
        meck:expect(user_device_repo, save_web, fun(_, _, _, _) -> {error, device_conflict} end),
        ?assertEqual(
            {error, device_conflict},
            user_device_ds:register_web(<<"now">>, 123, <<"native-1">>, #{})
        ),
        meck:expect(user_device_repo, save_web, fun(_, _, _, _) -> erlang:error(db_down) end),
        ?assertEqual(
            {error, unavailable}, user_device_ds:register_web(<<"now">>, 123, <<"web-1">>, #{})
        ),
        ?assertEqual(
            {error, invalid_device}, user_device_ds:register_web(<<"now">>, 123, <<>>, #{})
        )
    end).

web_device_repo_exact_query_and_atomic_collision_test_() ->
    fixture([elib_pg, elib_pg_sql, elib_tsid], fun() ->
        meck:expect(elib_pg_sql, public_tablename, fun(<<"user_device">>) ->
            <<"public.user_device">>
        end),
        meck:expect(elib_tsid, generate, fun(user_device) -> 456 end),
        meck:expect(elib_pg, query, fun(Sql, [123, <<"web-1">>, 456]) ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"status = 1">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"id = $3">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"device_type = 'web'">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"user_id = $1 AND device_id = $2">>)),
            {ok, [#{<<"active">> => 1}]}
        end),
        ?assertEqual({ok, true}, user_device_repo:is_active_web(123, <<"web-1">>, 456)),
        meck:expect(elib_pg, query, fun(_, _) -> {ok, []} end),
        ?assertEqual({ok, false}, user_device_repo:is_active_web(123, <<"unknown">>, 456)),
        lists:foreach(
            fun(Result) ->
                meck:expect(elib_pg, query, fun(_, _) -> Result end),
                ?assertMatch({error, _}, user_device_repo:is_active_web(123, <<"web-1">>, 456))
            end,
            [{error, db_down}, {ok, [#{}]}, unexpected]
        ),
        lists:foreach(
            fun(Result) ->
                meck:expect(elib_pg, query, fun(Sql, Args) ->
                    ?assertNotEqual(
                        nomatch, binary:match(Sql, <<"ON CONFLICT (user_id, device_id)">>)
                    ),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"existing.device_type = 'web'">>)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"existing.status = 1">>)),
                    ?assertEqual(
                        [456, 123, <<"web-1">>, <<"1">>, <<"Browser">>, <<>>, <<"now">>], Args
                    ),
                    Result
                end),
                Expected =
                    case Result of
                        {ok, [_]} -> {ok, 456};
                        {ok, []} -> {error, device_conflict};
                        _ -> Result
                    end,
                ?assertEqual(
                    Expected,
                    user_device_repo:save_web(
                        <<"now">>,
                        123,
                        <<"web-1">>,
                        #{<<"dvsn">> => <<"1">>, <<"dname">> => <<"Browser">>}
                    )
                )
            end,
            [{ok, [#{<<"id">> => 456}]}, {ok, []}, {error, db_down}]
        ),
        meck:expect(elib_pg, query, fun(_, _) -> erlang:error(db_down) end),
        ?assertEqual({error, unavailable}, user_device_repo:is_active_web(123, <<"web-1">>, 456)),
        ?assertEqual(
            {error, unavailable}, user_device_repo:save_web(<<"now">>, 123, <<"web-1">>, #{})
        )
    end).

web_raw_claims_are_strict_without_changing_native_test_() ->
    fixture([config_ds, imboy_jwt], fun() ->
        meck:expect(config_ds, env, fun(jwt_key, <<>>) -> <<"unit-web-key">> end),
        Claims = #{
            <<"uid">> => 123,
            <<"exp">> => 2000000000,
            <<"sub">> => <<"tk">>,
            <<"did">> => <<"web-1">>,
            <<"ep">> => 2,
            <<"dsid">> => 456
        },
        meck:expect(imboy_jwt, verify, fun(_, <<"unit-web-key">>) -> {ok, Claims} end),
        ?assertEqual(
            {ok, 123, 2000000000, <<"tk">>, <<"web-1">>, 2, 456},
            token_ds:decrypt_web_token(<<"token">>, <<"tk">>)
        ),
        lists:foreach(
            fun(Did) ->
                Payload = Claims#{<<"did">> => Did},
                meck:expect(imboy_jwt, verify, fun(_, _) -> {ok, Payload} end),
                ?assertMatch(
                    {ok, 123, _, <<"tk">>, Did, 2, 456},
                    token_ds:decrypt_web_token(<<"token">>, <<"tk">>)
                )
            end,
            valid_schema_dids()
        ),
        lists:foreach(
            fun(Did) ->
                Payload = Claims#{<<"did">> => Did},
                meck:expect(imboy_jwt, verify, fun(_, _) -> {ok, Payload} end),
                ?assertMatch({error, 706, _, _}, token_ds:decrypt_web_token(<<"token">>, <<"tk">>))
            end,
            invalid_schema_dids()
        ),
        Invalid = [
            maps:remove(<<"sub">>, Claims),
            maps:remove(<<"did">>, Claims),
            maps:remove(<<"ep">>, Claims),
            maps:remove(<<"dsid">>, Claims),
            Claims#{<<"dsid">> => 0},
            Claims#{<<"dsid">> => <<"456">>},
            Claims#{<<"sub">> => <<"rtk">>},
            Claims#{<<"sub">> => <<"seat_tk">>},
            Claims#{<<"sub">> => "tk"},
            Claims#{<<"did">> => "web-1"},
            Claims#{<<"did">> => <<>>},
            Claims#{<<"ep">> => <<"2">>},
            Claims#{<<"ep">> => 0},
            Claims#{<<"uid">> => <<"123">>},
            Claims#{<<"uid">> => 0}
        ],
        lists:foreach(
            fun(Payload) ->
                meck:expect(imboy_jwt, verify, fun(_, _) -> {ok, Payload} end),
                ?assertMatch({error, 706, _, _}, token_ds:decrypt_web_token(<<"token">>, <<"tk">>))
            end,
            Invalid
        ),
        Legacy = maps:without([<<"sub">>, <<"dsid">>], Claims),
        meck:expect(imboy_jwt, verify, fun(_, _) -> {ok, Legacy} end),
        ?assertEqual(
            {ok, 123, 2000000000, <<"tk">>, <<"web-1">>, 2},
            token_ds:decrypt_token(<<"token">>)
        ),
        Refresh = Claims#{<<"sub">> => <<"rtk">>},
        meck:expect(imboy_jwt, verify, fun(_, _) -> {ok, Refresh} end),
        ?assertEqual(
            {ok, 123, 2000000000, <<"rtk">>, <<"web-1">>, 2, 456},
            token_ds:decrypt_web_token(<<"refresh">>, <<"rtk">>)
        ),
        ?assertMatch({error, 706, _, _}, token_ds:decrypt_web_token(<<"token">>, <<"seat_tk">>))
    end).

web_authorization_failures_always_use_401_test_() ->
    fixture([token_ds, elib_response], fun() ->
        meck:expect(token_ds, decrypt_web_token, fun(_, _) ->
            {error, ?ERR_TOKEN_REFRESH_NOT_ALLOWED, <<"wrong purpose">>, #{}}
        end),
        meck:expect(elib_response, error_with_status, fun(#{}, 401, _, _) -> denied end),
        ?assertEqual(
            {stop, denied},
            auth_ds:condition(
                false,
                false,
                <<"token">>,
                #{},
                #{handler_opts => #{jwt_purpose => web_human}}
            )
        )
    end).

%% 同时覆盖超过旧40上限与PG varchar(128)码点上限，Unicode字节数可大于128。
valid_schema_dids() ->
    [
        binary:copy(<<"a">>, 41),
        binary:copy(<<"a">>, 128),
        binary:copy(<<"中"/utf8>>, 41),
        binary:copy(<<"中"/utf8>>, 128)
    ].

invalid_schema_dids() ->
    [
        <<>>,
        binary:copy(<<"a">>, 129),
        binary:copy(<<"中"/utf8>>, 129),
        <<255>>,
        <<16#e4, 16#b8>>
    ].

web_registration_schema_boundary_test_() ->
    fixture([user_device_repo], fun() ->
        meck:expect(user_device_repo, save_web, fun(_, 123, _, _) -> {ok, 456} end),
        meck:expect(user_device_repo, is_active_web, fun(123, _, 456) -> {ok, true} end),
        lists:foreach(
            fun(Did) ->
                ?assertEqual({ok, 456}, user_device_ds:register_web(<<"now">>, 123, Did, #{})),
                ?assert(user_device_ds:is_active_web(123, Did, 456))
            end,
            valid_schema_dids()
        ),
        lists:foreach(
            fun(Did) ->
                ?assertEqual(
                    {error, invalid_device},
                    user_device_ds:register_web(<<"now">>, 123, Did, #{})
                ),
                ?assertNot(user_device_ds:is_active_web(123, Did, 456))
            end,
            invalid_schema_dids()
        ),
        ?assertEqual(length(valid_schema_dids()), meck:num_calls(user_device_repo, save_web, '_'))
    end).

web_repo_schema_boundary_test_() ->
    fixture([elib_pg, elib_pg_sql, elib_tsid], fun() ->
        meck:expect(elib_pg_sql, public_tablename, fun(_) -> <<"public.user_device">> end),
        meck:expect(elib_tsid, generate, fun(user_device) -> 456 end),
        meck:expect(elib_pg, query, fun(_, _) -> {ok, [#{<<"id">> => 456}]} end),
        lists:foreach(
            fun(Did) ->
                ?assertEqual({ok, 456}, user_device_repo:save_web(<<"now">>, 123, Did, #{}))
            end,
            valid_schema_dids()
        ),
        lists:foreach(
            fun(Did) ->
                ?assertEqual(
                    {error, invalid_device}, user_device_repo:save_web(<<"now">>, 123, Did, #{})
                )
            end,
            invalid_schema_dids()
        ),
        ?assertEqual(length(valid_schema_dids()), meck:num_calls(elib_pg, query, '_'))
    end).

web_refresh_preserves_verified_epoch_after_revoke_test_() ->
    fixture([config_ds, imboy_jwt, auth_session_ds], fun() ->
        meck:expect(config_ds, env, fun(jwt_key, <<>>) -> <<"unit-web-key">> end),
        RefreshClaims = #{
            <<"uid">> => 123,
            <<"did">> => <<"web-1">>,
            <<"ep">> => 2,
            <<"sub">> => <<"rtk">>,
            <<"exp">> => 2000000000,
            <<"dsid">> => 456
        },
        meck:expect(imboy_jwt, verify, fun(_, _) -> {ok, RefreshClaims} end),
        {ok, Uid, _, <<"rtk">>, Did, VerifiedEp, DeviceRowId} =
            token_ds:decrypt_web_token(<<"refresh">>, <<"rtk">>),
        %% 验证后发生撤销；签票边界只捕获真实构造的claims，不mock Token DS。
        meck:expect(auth_session_ds, current_epoch, fun(123) -> {ok, 3} end),
        meck:expect(imboy_jwt, sign, fun(Claims, <<"unit-web-key">>) -> term_to_binary(Claims) end),
        Before = erlang:system_time(second),
        SignedClaims = binary_to_term(
            token_ds:encrypt_web_token(Uid, Did, VerifiedEp, DeviceRowId)
        ),
        After = erlang:system_time(second),
        ?assertMatch(
            #{
                <<"uid">> := 123,
                <<"did">> := <<"web-1">>,
                <<"ep">> := 2,
                <<"sub">> := <<"tk">>
            },
            SignedClaims
        ),
        ?assert(maps:get(<<"exp">>, SignedClaims) >= Before + ?TOKEN_VALID),
        ?assert(maps:get(<<"exp">>, SignedClaims) =< After + ?TOKEN_VALID),
        ?assertEqual(0, meck:num_calls(auth_session_ds, current_epoch, '_')),
        lists:foreach(
            fun(InvalidDid) ->
                ?assertError(
                    badarg, token_ds:encrypt_web_token(Uid, InvalidDid, VerifiedEp, DeviceRowId)
                )
            end,
            invalid_schema_dids()
        ),
        ?assertError(badarg, token_ds:encrypt_web_token(Uid, Did, 0, DeviceRowId)),
        ?assertEqual(1, meck:num_calls(imboy_jwt, sign, '_'))
    end).

-module(user_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

title_returns_nickname_when_present_test_() ->
    ?WITH_MOCK(
        user_repo,
        [
            {find_by_id, 2, fun(_Uid, _Columns) ->
                #{
                    <<"account">> => <<"testuser">>,
                    <<"nickname">> => <<"Test Nickname">>
                }
            end}
        ],
        fun() ->
            ?assertEqual(<<"Test Nickname">>, user_ds:title(12345))
        end
    ).

title_returns_account_when_nickname_empty_test_() ->
    ?WITH_MOCK(
        user_repo,
        [
            {find_by_id, 2, fun(_Uid, _Columns) ->
                #{
                    <<"account">> => <<"testuser">>,
                    <<"nickname">> => <<>>
                }
            end}
        ],
        fun() ->
            ?assertEqual(<<"testuser">>, user_ds:title(12345))
        end
    ).

title_decodes_binary_uid_test_() ->
    %% Ensure ec_cnv is on code path so meck can mock it
    EcCnvEbin = filename:join([
        filename:dirname(code:lib_dir(imboy)), "deps", "erlware_commons", "ebin"
    ]),
    case filelib:is_dir(EcCnvEbin) of
        false ->
            %% ec_cnv not available, skip this test
            [];
        true ->
            code:add_patha(EcCnvEbin),
            ?WITH_MECKS(
                [
                    {ec_cnv, [
                        {'to_integer', 1, fun(<<"12345">>) -> 12345 end}
                    ]},
                    {user_repo, [
                        {'find_by_id', 2, fun(12345, <<"account,nickname">>) ->
                            #{
                                <<"account">> => <<"testuser">>,
                                <<"nickname">> => <<"Decoded Nickname">>
                            }
                        end}
                    ]}
                ],
                fun() ->
                    ?assertEqual(<<"Decoded Nickname">>, user_ds:title(<<"12345">>))
                end
            )
    end.

title_mode2_returns_tuple_test_() ->
    ?WITH_MOCK(
        user_repo,
        [
            {find_by_id, 2, fun(_Uid, _Columns) ->
                #{
                    <<"account">> => <<"testuser">>,
                    <<"nickname">> => <<"Test Nickname">>
                }
            end}
        ],
        fun() ->
            ?assertEqual(
                {<<"Test Nickname">>, <<"Test Nickname">>},
                user_ds:title(12345, 2)
            )
        end
    ).

title_mode2_keeps_empty_nickname_test_() ->
    ?WITH_MOCK(
        user_repo,
        [
            {find_by_id, 2, fun(_Uid, _Columns) ->
                #{
                    <<"account">> => <<"testuser">>,
                    <<"nickname">> => <<>>
                }
            end}
        ],
        fun() ->
            ?assertEqual(
                {<<"testuser">>, <<>>},
                user_ds:title(12345, 2)
            )
        end
    ).

webrtc_credential_returns_expected_payload_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 2, fun
                    (eturnal_turn_urls, _D) -> [<<"turn:example.org">>];
                    (eturnal_stun_urls, _D) -> [<<"stun:example.org">>];
                    (eturnal_secret, _D) -> <<"test_secret">>
                end}
            ]},
            {elib_dt, [
                {'utc', 1, fun(second) -> 100 end}
            ]}
        ],
        fun() ->
            Result = user_ds:webrtc_credential(12345),
            Username = <<"86500:12345">>,
            ExpectedCredential = base64:encode(
                crypto:mac(hmac, sha, <<"test_secret">>, Username)
            ),
            ?assertEqual(86400, maps:get(<<"ttl">>, Result)),
            ?assertEqual([<<"turn:example.org">>], maps:get(<<"turn_urls">>, Result)),
            ?assertEqual([<<"stun:example.org">>], maps:get(<<"stun_urls">>, Result)),
            ?assertEqual(Username, maps:get(<<"username">>, Result)),
            ?assertEqual(ExpectedCredential, maps:get(<<"credential">>, Result))
        end
    ).

auth_webrtc_credential_valid_credential_test_() ->
    ?WITH_MECK(
        config_ds,
        [
            {'env', 2, fun(eturnal_secret, _D) -> <<"test_secret">> end}
        ],
        fun() ->
            Username = <<"1728610200:12345">>,
            Credential = base64:encode(
                crypto:mac(hmac, sha, <<"test_secret">>, Username)
            ),
            ?assertEqual(true, user_ds:auth_webrtc_credential(Username, Credential))
        end
    ).

auth_webrtc_credential_invalid_credential_test_() ->
    ?WITH_MECK(
        config_ds,
        [
            {'env', 2, fun(eturnal_secret, _D) -> <<"test_secret">> end}
        ],
        fun() ->
            ?assertEqual(
                false,
                user_ds:auth_webrtc_credential(
                    <<"1728610200:12345">>,
                    <<"InvalidCredential">>
                )
            )
        end
    ).

auth_webrtc_credential_empty_username_test_() ->
    ?WITH_MECK(
        config_ds,
        [
            {'env', 2, fun(eturnal_secret, _D) -> <<"test_secret">> end}
        ],
        fun() ->
            ?assertEqual(false, user_ds:auth_webrtc_credential(<<>>, <<"whatever">>))
        end
    ).

title_with_utf8_nickname_test_() ->
    ?WITH_MOCK(
        user_repo,
        [
            {find_by_id, 2, fun(_Uid, _Columns) ->
                #{
                    <<"account">> => <<"user123">>,
                    <<"nickname">> => <<"昵称 😊"/utf8>>
                }
            end}
        ],
        fun() ->
            ?assertEqual(<<"昵称 😊"/utf8>>, user_ds:title(12345))
        end
    ).

%% 注销级联：e2ee_* 表必须被清理（含作为他人代理的 proxy_uid 维度），
%% 防止历史代理合谋重建已注销用户的私钥（被遗忘权）
delete_all_related_data_cascades_e2ee_tables_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end},
                {'execute', 3, fun(fake_conn, Sql, Params) ->
                    case Sql of
                        <<"SELECT to_regclass", _/binary>> ->
                            {ok, cols, [{true}]};
                        <<"DELETE FROM ", Rest/binary>> ->
                            [Table | _] = binary:split(Rest, <<" ">>),
                            self() ! {deleted_table, Table, Params},
                            {ok, 1}
                    end
                end}
            ]}
        ],
        fun() ->
            ok = user_ds:delete_all_related_data(42),
            Deleted = collect_deleted_tables([]),
            E2ee = [T || T <- Deleted, binary:match(T, <<"e2ee_">>) =/= nomatch],
            %% shards×2(uid/proxy_uid) + trusted_contacts×2(uid/contact_uid)
            %% + transfer_sessions×2(from/to) + local_backups×1
            ?assertEqual(7, length(E2ee)),
            ?assert(lists:member(<<"public.e2ee_social_shards">>, E2ee)),
            ?assert(lists:member(<<"public.e2ee_local_backups">>, E2ee))
        end
    ).

collect_deleted_tables(Acc) ->
    receive
        {deleted_table, Table, _Params} -> collect_deleted_tables([Table | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

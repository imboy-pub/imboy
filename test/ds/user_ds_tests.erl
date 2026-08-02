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
            %% 自研 social/transfer 表已下线；剩云端加密备份两张
            %% （local_backups = 客户端本地备份上传；key_backups = 迁移 36 的密钥备份）
            ?assertEqual(2, length(E2ee)),
            ?assert(lists:member(<<"public.e2ee_local_backups">>, E2ee)),
            ?assert(lists:member(<<"public.e2ee_key_backups">>, E2ee))
        end
    ).

%% 注销级联：Olm 三表必须被清理，且只按本人 uid 维度删。
%%
%% 为什么要单独一条：注销路径删 user_device 用的是本函数内的直接 SQL，**不经**
%% user_device_ds:delete/2，所以 P3-3 加的设备吊销级联在这里根本不会触发。少了它，
%% 账号注销后别人仍能 claim 到它的 one-time key，与一个不存在的账号建立 Olm 会话。
%%
%% 空测反证：删掉 user_ds 里任一 olm_* 的 delete_from_table_if_exists 调用，必红。
delete_all_related_data_cascades_olm_tables_test_() ->
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
            Pairs = collect_deleted_pairs([]),
            Tables = [T || {T, _P} <- Pairs],
            ?assert(lists:member(<<"public.olm_identity">>, Tables)),
            ?assert(lists:member(<<"public.olm_one_time_key">>, Tables)),
            ?assert(lists:member(<<"public.olm_fallback_key">>, Tables)),
            %% 参数维度：每张 olm 表都必须只用本人 uid 作条件。
            %% 写错维度（比如误用 device_id 或漏传）会清掉别人的密钥。
            OlmPairs = [{T, P} || {T, P} <- Pairs, binary:match(T, <<"olm_">>) =/= nomatch],
            ?assertEqual(3, length(OlmPairs)),
            ?assertEqual([], [{T, P} || {T, P} <- OlmPairs, P =/= [42]])
        end
    ).

%% 与 collect_deleted_tables/1 同源，但保留 Params 以便断言删除维度。
collect_deleted_pairs(Acc) ->
    receive
        {deleted_table, Table, Params} -> collect_deleted_pairs([{Table, Params} | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

collect_deleted_tables(Acc) ->
    receive
        {deleted_table, Table, _Params} -> collect_deleted_tables([Table | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

-module(e2ee_backup_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc E2EE 加密密钥备份 Logic 层测试（P0-B B2）
%%%
%%% 零信任契约：客户端本地用恢复口令派生密钥加密密钥包，服务端只存密文。
%%% 守护线：① PEM 明文拒收；② 密文 put→get 逐字节透传；
%%% ③ elib_cipher 服务端加解密函数零调用（meck 注入 zerotrust_violation）；
%%% ④ info 响应不含密文与盐值。
%%%===================================================================

valid_params() ->
    #{
        <<"backup_version">> => 1,
        <<"kdf_salt">> => <<"c2FsdC1zYWx0LXNhbHQ=">>,
        <<"kdf_iterations">> => 310000,
        <<"encrypted_payload">> => base64:encode(<<"opaque-cipher-bytes-v1">>),
        <<"payload_hash">> => <<"deadbeefcafe">>
    }.

backup_row(Version) ->
    #{
        <<"id">> => 111222333,
        <<"uid">> => 9999,
        <<"backup_version">> => Version,
        <<"algo">> => <<"pbkdf2-sha256/aes-256-gcm">>,
        <<"kdf_salt">> => <<"c2FsdC1zYWx0LXNhbHQ=">>,
        <<"kdf_iterations">> => 310000,
        <<"encrypted_payload">> => base64:encode(<<"opaque-cipher-bytes-v1">>),
        <<"payload_hash">> => <<"deadbeefcafe">>,
        <<"created_at">> => <<"2026-07-12 00:00:00">>
    }.

%% ===================================================================
%% put：首版本 / 递增版本 / 版本冲突
%% ===================================================================

put_first_version_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_backup_ds, [
                {'latest', 1, fun(9999) -> {error, not_found} end},
                {'save', 1, fun(Record) ->
                    %% 落库记录必须携带调用方 uid 与透传密文
                    ?assertEqual(9999, maps:get(<<"uid">>, Record)),
                    ?assertEqual(1, maps:get(<<"backup_version">>, Record)),
                    {ok, 111}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{<<"backup_version">> => 1}},
                e2ee_backup_logic:put_backup(9999, valid_params())
            )
        end
    ).

put_next_version_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_backup_ds, [
                {'latest', 1, fun(9999) -> {ok, backup_row(3)} end},
                {'save', 1, fun(_) -> {ok, 222} end}
            ]}
        ],
        fun() ->
            Params = maps:put(<<"backup_version">>, 4, valid_params()),
            ?assertEqual(
                {ok, #{<<"backup_version">> => 4}},
                e2ee_backup_logic:put_backup(9999, Params)
            )
        end
    ).

put_version_conflict_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_backup_ds, [
                {'latest', 1, fun(9999) -> {ok, backup_row(3)} end}
            ]}
        ],
        fun() ->
            %% 重放当前版本、跳版本均拒（必须 = 当前 + 1）
            lists:foreach(
                fun(BadVersion) ->
                    Params = maps:put(<<"backup_version">>, BadVersion, valid_params()),
                    ?assertEqual(
                        {error, <<"version_conflict">>, 409},
                        e2ee_backup_logic:put_backup(9999, Params)
                    )
                end,
                [1, 3, 5]
            )
        end
    ).

%% 并发 put 撞版本：读检查通过但 UNIQUE(uid, backup_version) 拦下 → 409
put_unique_violation_maps_to_conflict_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_backup_ds, [
                {'latest', 1, fun(9999) -> {error, not_found} end},
                {'save', 1, fun(_) ->
                    {error, {pgsql_error, #{code => <<"23505">>}}}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"version_conflict">>, 409},
                e2ee_backup_logic:put_backup(9999, valid_params())
            )
        end
    ).

%% ===================================================================
%% put：参数校验与明文拦截（零信任守护线 ①）
%% ===================================================================

put_rejects_plaintext_pem_test_() ->
    ?TEST_SIMPLE(fun() ->
        Pem =
            <<"-----BEGIN RSA PRIVATE KEY-----\nMIIEpAIBAAKCAQEA\n-----END RSA PRIVATE KEY-----">>,
        %% 裸 PEM 与 base64 包裹的 PEM 都必须拒收
        RawParams = maps:put(<<"encrypted_payload">>, Pem, valid_params()),
        ?assertEqual(
            {error, <<"plaintext_payload_rejected">>, 400},
            e2ee_backup_logic:put_backup(9999, RawParams)
        ),
        B64Params = maps:put(<<"encrypted_payload">>, base64:encode(Pem), valid_params()),
        ?assertEqual(
            {error, <<"plaintext_payload_rejected">>, 400},
            e2ee_backup_logic:put_backup(9999, B64Params)
        )
    end).

put_rejects_invalid_base64_test_() ->
    ?TEST_SIMPLE(fun() ->
        Params = maps:put(<<"encrypted_payload">>, <<"!!!not-base64!!!">>, valid_params()),
        ?assertEqual(
            {error, <<"invalid_base64_payload">>, 400},
            e2ee_backup_logic:put_backup(9999, Params)
        )
    end).

put_accepts_unpadded_base64_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_backup_ds, [
                {'latest', 1, fun(9999) -> {error, not_found} end},
                {'save', 1, fun(_) -> {ok, 333} end}
            ]}
        ],
        fun() ->
            %% vodozemac 输出 unpadded base64，服务端须容忍
            Unpadded = binary:replace(base64:encode(<<"ab">>), <<"=">>, <<>>, [global]),
            Params = maps:put(<<"encrypted_payload">>, Unpadded, valid_params()),
            ?assertMatch({ok, _}, e2ee_backup_logic:put_backup(9999, Params))
        end
    ).

put_rejects_bad_params_test_() ->
    ?TEST_SIMPLE(fun() ->
        Cases = [
            {<<"backup_version">>, 0, <<"invalid_backup_version">>},
            {<<"backup_version">>, <<"1">>, <<"invalid_backup_version">>},
            {<<"kdf_salt">>, <<>>, <<"invalid_kdf_salt">>},
            %% salt 超长 = 存储放大 DoS 面（security-reviewer M1）
            {<<"kdf_salt">>, binary:copy(<<"s">>, 257), <<"invalid_kdf_salt">>},
            %% 迭代次数过低 = 离线爆破降级攻击面；过高 = int4 溢出 500
            {<<"kdf_iterations">>, 1000, <<"invalid_kdf_iterations">>},
            {<<"kdf_iterations">>, 2147483648, <<"invalid_kdf_iterations">>},
            {<<"payload_hash">>, <<>>, <<"invalid_payload_hash">>},
            {<<"payload_hash">>, binary:copy(<<"a">>, 129), <<"invalid_payload_hash">>},
            {<<"algo">>, binary:copy(<<"x">>, 41), <<"invalid_algo">>},
            {<<"encrypted_payload">>, <<>>, <<"encrypted_payload_required">>}
        ],
        lists:foreach(
            fun({Key, BadValue, ExpectMsg}) ->
                Params = maps:put(Key, BadValue, valid_params()),
                ?assertEqual(
                    {error, ExpectMsg, 400},
                    e2ee_backup_logic:put_backup(9999, Params)
                )
            end,
            Cases
        )
    end).

put_rejects_oversize_payload_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 1048580 字节 > 1MB 上限
        Big = binary:copy(<<"QUJD">>, 262145),
        Params = maps:put(<<"encrypted_payload">>, Big, valid_params()),
        ?assertEqual(
            {error, <<"encrypted_payload_too_large">>, 400},
            e2ee_backup_logic:put_backup(9999, Params)
        )
    end).

%% ===================================================================
%% get / info / delete
%% ===================================================================

%% 零信任守护线 ②：get 返回的密文与落库值逐字节一致，且不泄漏内部 id/uid
get_returns_ciphertext_verbatim_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_backup_ds, [
                {'latest', 1, fun(9999) -> {ok, backup_row(2)} end}
            ]}
        ],
        fun() ->
            {ok, Payload} = e2ee_backup_logic:get_backup(9999),
            ?assertEqual(
                base64:encode(<<"opaque-cipher-bytes-v1">>),
                maps:get(<<"encrypted_payload">>, Payload)
            ),
            ?assertEqual(2, maps:get(<<"backup_version">>, Payload)),
            ?assertNot(maps:is_key(<<"id">>, Payload)),
            ?assertNot(maps:is_key(<<"uid">>, Payload))
        end
    ).

get_not_found_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_backup_ds, [
                {'latest', 1, fun(9999) -> {error, not_found} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"backup_not_found">>, 404},
                e2ee_backup_logic:get_backup(9999)
            )
        end
    ).

%% 零信任守护线 ④：info 是探测端点，不得下发密文与盐值
info_excludes_ciphertext_and_salt_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_backup_ds, [
                {'latest', 1, fun(9999) -> {ok, backup_row(2)} end}
            ]}
        ],
        fun() ->
            {ok, Info} = e2ee_backup_logic:info(9999),
            ?assertEqual(true, maps:get(<<"has_backup">>, Info)),
            ?assertEqual(2, maps:get(<<"backup_version">>, Info)),
            ?assertNot(maps:is_key(<<"encrypted_payload">>, Info)),
            ?assertNot(maps:is_key(<<"kdf_salt">>, Info)),
            ?assertNot(maps:is_key(<<"payload_hash">>, Info))
        end
    ).

info_no_backup_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_backup_ds, [
                {'latest', 1, fun(9999) -> {error, not_found} end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, #{<<"has_backup">> => false}}, e2ee_backup_logic:info(9999))
        end
    ).

delete_returns_count_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_backup_ds, [
                {'delete_by_uid', 1, fun(9999) -> {ok, 3} end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, #{<<"deleted">> => 3}}, e2ee_backup_logic:delete_backup(9999))
        end
    ).

%% ===================================================================
%% 零信任守护线 ③：备份链路对 elib_cipher 服务端加解密函数零调用
%% ===================================================================

zerotrust_no_server_side_crypto_test_() ->
    ?WITH_MECKS(
        [
            {elib_cipher, [
                {'derive_master_password', 2, fun(_, _) ->
                    erlang:error(zerotrust_violation)
                end},
                {'decrypt_private_key', 2, fun(_, _) ->
                    erlang:error(zerotrust_violation)
                end},
                {'encrypt_private_key', 2, fun(_, _) ->
                    erlang:error(zerotrust_violation)
                end}
            ]},
            {e2ee_backup_ds, [
                {'latest', 1, fun(9999) -> {error, not_found} end},
                {'save', 1, fun(_) -> {ok, 444} end},
                {'delete_by_uid', 1, fun(9999) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            %% put / get / info / delete 全链路走一遍，任何服务端解密调用会崩测试
            ?assertMatch({ok, _}, e2ee_backup_logic:put_backup(9999, valid_params())),
            ?assertMatch({error, <<"backup_not_found">>, 404}, e2ee_backup_logic:get_backup(9999)),
            ?assertMatch({ok, _}, e2ee_backup_logic:info(9999)),
            ?assertMatch({ok, _}, e2ee_backup_logic:delete_backup(9999)),
            ?assertEqual(0, meck:num_calls(elib_cipher, derive_master_password, 2)),
            ?assertEqual(0, meck:num_calls(elib_cipher, decrypt_private_key, 2)),
            ?assertEqual(0, meck:num_calls(elib_cipher, encrypt_private_key, 2))
        end
    ).

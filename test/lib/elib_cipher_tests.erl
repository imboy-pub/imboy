-module(elib_cipher_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% elib_cipher 模块的 EUnit 测试
%%%
%%% 目标：验证加密解密工具功能
%%% 覆盖：AES加密、解密、随机数生成
%%%===================================================================

%% ===================================================================
%% AES 加密解密测试
%% ===================================================================

encrypt_and_decrypt_roundtrip_test_() ->
    ?TEST_WITH_APP(fun() ->
        PlainText = <<"secret message">>,
        Key = <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
        IV = <<"aaaaaaaaaaaaaaaa">>,
        % 加密
        CipherText = elib_cipher:aes_encrypt(PlainText, Key, IV),
        ?assertNotEqual(PlainText, CipherText),
        ?assertMatch(<<_/binary>>, CipherText),
        ?assert(byte_size(CipherText) > 0),
        % 解密
        DecryptedText = elib_cipher:aes_decrypt(CipherText, Key, IV),
        ?assertEqual(PlainText, DecryptedText)
    end).

encrypt_with_empty_text_test_() ->
    ?TEST_WITH_APP(fun() ->
        PlainText = <<>>,
        Key = <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
        IV = <<"aaaaaaaaaaaaaaaa">>,
        % 测试空文本加密
        CipherText = elib_cipher:aes_encrypt(PlainText, Key, IV),
        ?assertMatch(<<_/binary>>, CipherText),
        ?assert(byte_size(CipherText) > 0),
        % 空文本解密应该返回空文本
        DecryptedText = elib_cipher:aes_decrypt(CipherText, Key, IV),
        ?assertEqual(PlainText, DecryptedText)
    end).

decrypt_with_invalid_data_test_() ->
    ?TEST_WITH_APP(fun() ->
        InvalidCipherText = <<"invalid_data_not_base64">>,
        Key = <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
        IV = <<"aaaaaaaaaaaaaaaa">>,
        % 测试无效数据解密应该抛出异常
        ?assertError(_, elib_cipher:aes_decrypt(InvalidCipherText, Key, IV))
    end).

%% ===================================================================
%% 随机数生成测试
%% ===================================================================

num_random_generates_integer_test_() ->
    ?TEST_WITH_APP(fun() ->
        Length = 40,
        Result = elib_cipher:num_random(Length),
        % 验证返回的是整数
        ?assert(is_integer(Result)),
        % 验证数字在合理范围内
        ?assert(Result >= 0),
        % 验证数字长度符合要求（对于40位数字，应该在10^39到10^40-1之间）
        ?assert(Result >= trunc(math:pow(10, Length-1)))
    end).

num_random_with_different_lengths_test_() ->
    ?TEST_WITH_APP(fun() ->
        Length1 = 6,
        Length2 = 20,
        Result1 = elib_cipher:num_random(Length1),
        Result2 = elib_cipher:num_random(Length2),
        % 验证不同长度生成的数字位数不同
        ?assert(Result1 < trunc(math:pow(10, Length1))),
        ?assert(Result2 >= trunc(math:pow(10, Length2-1))),
        % 验证两个结果不相等（极小概率相等，但测试中可忽略）
        ?assert(Result1 =/= Result2)
    end).

num_random_six_digits_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 验证6位随机数（验证码场景）
        Code = elib_cipher:num_random(6),
        ?assert(is_integer(Code)),
        ?assert(Code >= 100000),
        ?assert(Code =< 999999)
    end).

num_random_forty_digits_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 验证40位随机数（盐值场景）
        Salt = elib_cipher:num_random(40),
        ?assert(is_integer(Salt)),
        ?assert(Salt >= trunc(math:pow(10, 39))),
        ?assert(Salt < trunc(math:pow(10, 40)))
    end).

%% ===================================================================
%% AES 加密/解密 - 指定加密类型 (4参数版本)
%% ===================================================================

aes_encrypt_with_type_test_() ->
    ?TEST_WITH_APP(fun() ->
        PlainText = <<"test message">>,
        Key = <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
        IV = <<"aaaaaaaaaaaaaaaa">>,
        % 使用 aes_256_cbc 类型
        CipherText = elib_cipher:aes_encrypt(aes_256_cbc, PlainText, Key, IV),
        ?assertNotEqual(PlainText, CipherText),
        ?assertMatch(<<_/binary>>, CipherText),
        % 解密验证
        Decrypted = elib_cipher:aes_decrypt(aes_256_cbc, CipherText, Key, IV),
        ?assertEqual(PlainText, Decrypted)
    end).

aes_encrypt_with_binary_key_test_() ->
    ?TEST_WITH_APP(fun() ->
        PlainText = <<"binary key test">>,
        Key = <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>,
        IV = <<"cccccccccccccccc">>,
        % 测试二进制密钥
        CipherText = elib_cipher:aes_encrypt(aes_256_cbc, PlainText, Key, IV),
        ?assertMatch(<<_/binary>>, CipherText),
        Decrypted = elib_cipher:aes_decrypt(aes_256_cbc, CipherText, Key, IV),
        ?assertEqual(PlainText, Decrypted)
    end).

aes_decrypt_removes_padding_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 测试 PKCS#7 填充移除
        PlainText = <<"x">>,  % 1字节，需要填充15字节
        Key = <<"dddddddddddddddddddddddddddddddd">>,
        IV = <<"eeeeeeeeeeeeeeee">>,
        CipherText = elib_cipher:aes_encrypt(aes_256_cbc, PlainText, Key, IV),
        Decrypted = elib_cipher:aes_decrypt(aes_256_cbc, CipherText, Key, IV),
        ?assertEqual(PlainText, Decrypted)
    end).

%% ===================================================================
%% RSA 加密/解密测试
%% ===================================================================

rsa_encrypt_with_binary_input_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(config_ds, [no_link]),
        meck:new(public_key, [passthrough, no_link]),
        try
            TestPubKey = <<"-----BEGIN PUBLIC KEY-----\ntest\n-----END PUBLIC KEY-----">>,
            meck:expect(config_ds, env, fun(login_rsa_pub_key) -> TestPubKey end),
            meck:expect(public_key, pem_decode, fun(_) -> [mock_pub_entry] end),
            meck:expect(public_key, pem_entry_decode, fun(mock_pub_entry) -> mock_public_key end),
            meck:expect(public_key, encrypt_public, fun(<<"test password">>, mock_public_key, Opts) ->
                ?assertEqual([
                    {rsa_padding, rsa_pkcs1_oaep_padding},
                    {rsa_oaep_md, sha256},
                    {rsa_mgf1_md, sha256}
                ], Opts),
                <<"encrypted_payload">>
            end),

            Result = elib_cipher:rsa_encrypt(<<"test password">>),

            ?assertEqual(base64:encode(<<"encrypted_payload">>), Result),
            ?assert(meck:called(config_ds, env, 1)),
            ?assert(meck:called(public_key, encrypt_public, 3))
        after
            meck:unload(config_ds),
            meck:unload(public_key)
        end
    end).

rsa_encrypt_with_list_input_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(config_ds, [no_link]),
        meck:new(public_key, [passthrough, no_link]),
        try
            TestPubKey = <<"-----BEGIN PUBLIC KEY-----\ntest\n-----END PUBLIC KEY-----">>,
            meck:expect(config_ds, env, fun(login_rsa_pub_key) -> TestPubKey end),
            meck:expect(public_key, pem_decode, fun(_) -> [mock_pub_entry] end),
            meck:expect(public_key, pem_entry_decode, fun(mock_pub_entry) -> mock_public_key end),
            meck:expect(public_key, encrypt_public, fun(<<"password">>, mock_public_key, _Opts) -> <<"encrypted">> end),

            Result = elib_cipher:rsa_encrypt("password"),
            ?assertEqual(base64:encode(<<"encrypted">>), Result)
        after
            meck:unload(config_ds),
            meck:unload(public_key)
        end
    end).

rsa_encrypt_with_custom_key_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(public_key, [passthrough, no_link]),
        try
            CustomKey = <<"-----BEGIN PUBLIC KEY-----\ntest key\n-----END PUBLIC KEY-----">>,
            meck:expect(public_key, pem_decode, fun(_) -> [mock_pub_entry] end),
            meck:expect(public_key, pem_entry_decode, fun(mock_pub_entry) -> mock_public_key end),
            meck:expect(public_key, encrypt_public, fun(<<"data">>, mock_public_key, _Opts) -> <<"custom_encrypted">> end),

            Result = elib_cipher:rsa_encrypt(<<"data">>, CustomKey),

            ?assertEqual(base64:encode(<<"custom_encrypted">>), Result)
        after
            meck:unload(public_key)
        end
    end).

rsa_encrypt_error_handling_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(public_key, [passthrough, no_link]),
        try
            CustomKey = <<"invalid key">>,
            meck:expect(public_key, pem_decode, fun(_) -> [mock_pub_entry] end),
            meck:expect(public_key, pem_entry_decode, fun(mock_pub_entry) -> mock_public_key end),
            meck:expect(public_key, encrypt_public, fun(_Data, _Key, _Opts) ->
                error(badarg)
            end),

            Result = elib_cipher:rsa_encrypt(<<"data">>, CustomKey),

            ?assertMatch({error, encrypt_failed}, Result)
        after
            meck:unload(public_key)
        end
    end).

rsa_decrypt_with_config_key_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(config_ds, [no_link]),
        meck:new(public_key, [passthrough, no_link]),
        try
            TestPrivKey = <<"-----BEGIN RSA PRIVATE KEY-----\ntest\n-----END RSA PRIVATE KEY-----">>,
            meck:expect(config_ds, env, fun(login_rsa_priv_key) -> TestPrivKey end),
            meck:expect(public_key, pem_decode, fun(_) -> [mock_priv_entry] end),
            meck:expect(public_key, pem_entry_decode, fun(mock_priv_entry) -> mock_private_key end),
            meck:expect(public_key, decrypt_private, fun(<<"encrypted">>, mock_private_key, Opts) ->
                ?assertEqual([
                    {rsa_padding, rsa_pkcs1_oaep_padding},
                    {rsa_oaep_md, sha256},
                    {rsa_mgf1_md, sha256}
                ], Opts),
                <<"decrypted_password">>
            end),

            CipherText = base64:encode(<<"encrypted">>),
            Result = elib_cipher:rsa_decrypt(CipherText),

            ?assertEqual(<<"decrypted_password">>, Result)
        after
            meck:unload(config_ds),
            meck:unload(public_key)
        end
    end).

rsa_decrypt_missing_config_key_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(config_ds, [no_link]),
        try
            meck:expect(config_ds, env, fun(login_rsa_priv_key) -> undefined end),

            Result = elib_cipher:rsa_decrypt(<<"encrypted">>),

            ?assertEqual(undefined, Result)
        after
            meck:unload(config_ds)
        end
    end).

rsa_decrypt_with_custom_key_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(public_key, [passthrough, no_link]),
        try
            CustomKey = <<"-----BEGIN RSA PRIVATE KEY-----\nkey\n-----END RSA PRIVATE KEY-----">>,
            meck:expect(public_key, pem_decode, fun(_) -> [mock_priv_entry] end),
            meck:expect(public_key, pem_entry_decode, fun(mock_priv_entry) -> mock_private_key end),
            meck:expect(public_key, decrypt_private, fun(<<"data">>, mock_private_key, _Opts) -> <<"custom_decrypted">> end),

            Result = elib_cipher:rsa_decrypt(base64:encode(<<"data">>), CustomKey),

            ?assertEqual(<<"custom_decrypted">>, Result)
        after
            meck:unload(public_key)
        end
    end).

rsa_decrypt_url_safe_base64_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(public_key, [passthrough, no_link]),
        try
            CustomKey = <<"-----BEGIN RSA PRIVATE KEY-----\nkey\n-----END RSA PRIVATE KEY-----">>,
            meck:expect(public_key, pem_decode, fun(_) -> [mock_priv_entry] end),
            meck:expect(public_key, pem_entry_decode, fun(mock_priv_entry) -> mock_private_key end),
            meck:expect(public_key, decrypt_private, fun(<<251, 255>>, mock_private_key, _Opts) -> <<"url_safe_decrypted">> end),

            Result = elib_cipher:rsa_decrypt(<<"-_8">>, CustomKey),

            ?assertEqual(<<"url_safe_decrypted">>, Result)
        after
            meck:unload(public_key)
        end
    end).

rsa_decrypt_url_encoded_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(public_key, [passthrough, no_link]),
        try
            CustomKey = <<"-----BEGIN RSA PRIVATE KEY-----\nkey\n-----END RSA PRIVATE KEY-----">>,
            meck:expect(public_key, pem_decode, fun(_) -> [mock_priv_entry] end),
            meck:expect(public_key, pem_entry_decode, fun(mock_priv_entry) -> mock_private_key end),
            meck:expect(public_key, decrypt_private, fun(<<251, 255>>, mock_private_key, _Opts) -> <<"url_decoded_decrypted">> end),

            Result = elib_cipher:rsa_decrypt(<<"%2B%2F8%3D">>, CustomKey),

            ?assertEqual(<<"url_decoded_decrypted">>, Result)
        after
            meck:unload(public_key)
        end
    end).

rsa_decrypt_space_replaced_plus_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(public_key, [passthrough, no_link]),
        try
            CustomKey = <<"-----BEGIN RSA PRIVATE KEY-----\nkey\n-----END RSA PRIVATE KEY-----">>,
            meck:expect(public_key, pem_decode, fun(_) -> [mock_priv_entry] end),
            meck:expect(public_key, pem_entry_decode, fun(mock_priv_entry) -> mock_private_key end),
            meck:expect(public_key, decrypt_private, fun(<<251, 255>>, mock_private_key, _Opts) -> <<"space_fixed">> end),

            Result = elib_cipher:rsa_decrypt(<<" /8=">>, CustomKey),

            ?assertEqual(<<"space_fixed">>, Result)
        after
            meck:unload(public_key)
        end
    end).

rsa_decrypt_missing_padding_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(public_key, [passthrough, no_link]),
        try
            CustomKey = <<"-----BEGIN RSA PRIVATE KEY-----\nkey\n-----END RSA PRIVATE KEY-----">>,
            meck:expect(public_key, pem_decode, fun(_) -> [mock_priv_entry] end),
            meck:expect(public_key, pem_entry_decode, fun(mock_priv_entry) -> mock_private_key end),
            meck:expect(public_key, decrypt_private, fun(<<"hello">>, mock_private_key, _Opts) -> <<"padding_added">> end),

            Result = elib_cipher:rsa_decrypt(<<"aGVsbG8">>, CustomKey),

            ?assertEqual(<<"padding_added">>, Result)
        after
            meck:unload(public_key)
        end
    end).

%% ===================================================================
%% safe_rsa_decrypt/2 测试
%% ===================================================================

safe_rsa_decrypt_version_1_success_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(config_ds, [no_link]),
        meck:new(public_key, [passthrough, no_link]),
        try
            PrivateKeyPem = <<"-----BEGIN RSA PRIVATE KEY-----\nkey\n-----END RSA PRIVATE KEY-----">>,
            meck:expect(config_ds, env, fun(login_rsa_priv_key) -> PrivateKeyPem end),
            meck:expect(public_key, pem_decode, fun(_) -> [mock_priv_entry] end),
            meck:expect(public_key, pem_entry_decode, fun(mock_priv_entry) -> mock_private_key end),
            meck:expect(public_key, decrypt_private, fun(<<"encrypted">>, mock_private_key, _Opts) ->
                <<"decrypted_value">>
            end),

            Result = elib_cipher:safe_rsa_decrypt(base64:encode(<<"encrypted">>), <<"1">>),

            ?assertEqual(<<"decrypted_value">>, Result)
        after
            meck:unload(config_ds),
            meck:unload(public_key)
        end
    end).

safe_rsa_decrypt_version_1_error_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(config_ds, [no_link]),
        try
            meck:expect(config_ds, env, fun(login_rsa_priv_key) -> undefined end),

            Result = elib_cipher:safe_rsa_decrypt(<<"invalid_encrypted">>, <<"1">>),

            ?assertEqual(<<>>, Result)
        after
            meck:unload(config_ds)
        end
    end).

safe_rsa_decrypt_version_1_throw_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(config_ds, [no_link]),
        try
            meck:expect(config_ds, env, fun(login_rsa_priv_key) -> error(bad_config) end),

            Result = elib_cipher:safe_rsa_decrypt(<<"bad_encrypted">>, <<"1">>),

            ?assertEqual(<<>>, Result)
        after
            meck:unload(config_ds)
        end
    end).

safe_rsa_decrypt_version_1_non_binary_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(config_ds, [no_link]),
        try
            meck:expect(config_ds, env, fun(login_rsa_priv_key) -> undefined end),

            Result = elib_cipher:safe_rsa_decrypt(<<"encrypted">>, <<"1">>),

            ?assertEqual(<<>>, Result)
        after
            meck:unload(config_ds)
        end
    end).

safe_rsa_decrypt_other_version_test_() ->
    ?TEST_WITH_APP(fun() ->
        Password = <<"plaintext_password">>,

        % 非 "1" 版本应直接返回原密码
        Result = elib_cipher:safe_rsa_decrypt(Password, <<"2">>),

        ?assertEqual(Password, Result)
    end).

safe_rsa_decrypt_empty_version_test_() ->
    ?TEST_WITH_APP(fun() ->
        Password = <<"any_password">>,

        % 空版本应直接返回原密码
        Result = elib_cipher:safe_rsa_decrypt(Password, <<>>),

        ?assertEqual(Password, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

aes_encrypt_large_data_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 测试大数据加密（超过16字节）
        LargeText = list_to_binary(lists:seq(1, 100)),
        Key = <<"ffffffffffffffffffffffffffffffff">>,
        IV = <<"eeeeeeeeeeeeeeee">>,

        CipherText = elib_cipher:aes_encrypt(LargeText, Key, IV),
        Decrypted = elib_cipher:aes_decrypt(CipherText, Key, IV),

        ?assertEqual(LargeText, Decrypted)
    end).

num_random_min_value_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 测试最小随机数生成（1位）
        Result = elib_cipher:num_random(1),
        ?assert(is_integer(Result)),
        ?assert(Result >= 1),
        ?assert(Result =< 10)
    end).

num_random_max_value_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 测试较大随机数生成
        Result = elib_cipher:num_random(20),
        ?assert(is_integer(Result)),
        ?assert(Result >= trunc(math:pow(10, 19)))
    end).

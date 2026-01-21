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
        meck:new(config_ds, [passthrough, no_link]),
        meck:new(public_key, [passthrough, no_link]),
        try
            % Mock 配置获取公钥
            TestPubKey = <<"-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA test\n-----END PUBLIC KEY-----">>,
            meck:expect(config_ds, get, fun(<<"login_rsa_pub_key">>) -> TestPubKey end),

            % Mock RSA 加密
            meck:expect(public_key, encrypt_public, fun(_Data, _Key) ->
                <<"encrypted_base64_data">>
            end),

            PlainText = <<"test password">>,
            Result = elib_cipher:rsa_encrypt(PlainText),

            % 验证返回 Base64 编码的数据
            ?assertMatch(<<_/binary>>, Result),
            ?assert(meck:called(config_ds, get, 1)),
            ?assert(meck:called(public_key, encrypt_public, 2))
        after
            meck:unload(config_ds),
            meck:unload(public_key)
        end
    end).

rsa_encrypt_with_list_input_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(config_ds, [passthrough, no_link]),
        meck:new(public_key, [passthrough, no_link]),
        try
            TestPubKey = <<"-----BEGIN PUBLIC KEY-----\ntest\n-----END PUBLIC KEY-----">>,
            meck:expect(config_ds, get, fun(<<"login_rsa_pub_key">>) -> TestPubKey end),
            meck:expect(public_key, encrypt_public, fun(_Data, _Key) -> <<"encrypted">> end),

            % 测试列表输入
            Result = elib_cipher:rsa_encrypt("password"),
            ?assertMatch(<<_/binary>>, Result)
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
            meck:expect(public_key, encrypt_public, fun(_Data, _Key) -> <<"custom_encrypted">> end),

            BinData = <<"data">>,
            Result = elib_cipher:rsa_encrypt(BinData, CustomKey),

            ?assertMatch(<<_/binary>>, Result)
        after
            meck:unload(public_key)
        end
    end).

rsa_encrypt_error_handling_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(public_key, [passthrough, no_link]),
        try
            CustomKey = <<"invalid key">>,
            % Mock 加密抛出异常
            meck:expect(public_key, encrypt_public, fun(_Data, _Key) ->
                error(badarg)
            end),
            meck:expect(public_key, pem_decode, fun(_Key) -> [] end),

            BinData = <<"data">>,
            Result = elib_cipher:rsa_encrypt(BinData, CustomKey),

            ?assertMatch({error, encrypt_failed}, Result)
        after
            meck:unload(public_key)
        end
    end).

rsa_decrypt_with_config_key_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(config_ds, [passthrough, no_link]),
        meck:new(public_key, [passthrough, no_link]),
        try
            TestPrivKey = <<"-----BEGIN RSA PRIVATE KEY-----\ntest\n-----END RSA PRIVATE KEY-----">>,
            meck:expect(config_ds, get, fun(<<"login_rsa_priv_key">>) -> TestPrivKey end),
            meck:expect(public_key, decrypt_private, fun(_Data, _Key) -> <<"decrypted_password">> end),

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
        meck:new(config_ds, [passthrough, no_link]),
        try
            % Mock 配置未设置私钥
            meck:expect(config_ds, get, fun(<<"login_rsa_priv_key">>) -> {error, not_found} end),

            CipherText = <<"encrypted">>,
            Result = elib_cipher:rsa_decrypt(CipherText),

            ?assertMatch({error, not_found}, Result)
        after
            meck:unload(config_ds)
        end
    end).

rsa_decrypt_with_custom_key_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(public_key, [passthrough, no_link]),
        try
            CustomKey = <<"-----BEGIN RSA PRIVATE KEY-----\nkey\n-----END RSA PRIVATE KEY-----">>,
            meck:expect(public_key, decrypt_private, fun(_Data, _Key) -> <<"custom_decrypted">> end),

            CipherText = base64:encode(<<"data">>),
            Result = elib_cipher:rsa_decrypt(CipherText, CustomKey),

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
            meck:expect(public_key, decrypt_private, fun(_Data, _Key) -> <<"url_safe_decrypted">> end),

            % 测试 URL-safe Base64 格式
            UrlSafeCipher = <<"abc-def_xyz">>,
            Result = elib_cipher:rsa_decrypt(UrlSafeCipher, CustomKey),

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
            meck:expect(public_key, decrypt_private, fun(_Data, _Key) -> <<"url_decoded_decrypted">> end),

            % 测试 URL 编码
            UrlEncodedCipher = <<"abc%2Fdef">>,
            Result = elib_cipher:rsa_decrypt(UrlEncodedCipher, CustomKey),

            ?assertEqual(<<"url_decoded_decrypted">>, Result)
        after
            meck:unload(public_key)
        end
    end).

rsa_decrypt_missing_padding_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(public_key, [passthrough, no_link]),
        try
            CustomKey = <<"-----BEGIN RSA PRIVATE KEY-----\nkey\n-----END RSA PRIVATE KEY-----">>,
            meck:expect(public_key, decrypt_private, fun(_Data, _Key) -> <<"padding_added">> end),

            % 测试缺失 Base64 填充
            NoPaddingCipher = <<"abc">>,
            Result = elib_cipher:rsa_decrypt(NoPaddingCipher, CustomKey),

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
        meck:new(elib_cipher, [passthrough, no_link]),
        try
            Password = <<"encrypted_password">>,
            Decrypted = <<"decrypted_value">>,

            meck:expect(elib_cipher, rsa_decrypt, fun(_Password) -> Decrypted end),

            Result = elib_cipher:safe_rsa_decrypt(Password, <<"1">>),

            ?assertEqual(Decrypted, Result)
        after
            meck:unload(elib_cipher)
        end
    end).

safe_rsa_decrypt_version_1_error_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_cipher, [passthrough, no_link]),
        try
            Password = <<"invalid_encrypted">>,

            meck:expect(elib_cipher, rsa_decrypt, fun(_Password) ->
                error(decrypt_failed)
            end),

            Result = elib_cipher:safe_rsa_decrypt(Password, <<"1">>),

            % 解密失败应返回空二进制
            ?assertEqual(<<>>, Result)
        after
            meck:unload(elib_cipher)
        end
    end).

safe_rsa_decrypt_version_1_throw_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_cipher, [passthrough, no_link]),
        try
            Password = <<"bad_encrypted">>,

            meck:expect(elib_cipher, rsa_decrypt, fun(_Password) ->
                throw({error, bad_key})
            end),

            Result = elib_cipher:safe_rsa_decrypt(Password, <<"1">>),

            ?assertEqual(<<>>, Result)
        after
            meck:unload(elib_cipher)
        end
    end).

safe_rsa_decrypt_version_1_non_binary_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_cipher, [passthrough, no_link]),
        try
            Password = <<"encrypted">>,

            meck:expect(elib_cipher, rsa_decrypt, fun(_Password) ->
                {error, invalid}
            end),

            Result = elib_cipher:safe_rsa_decrypt(Password, <<"1">>),

            ?assertEqual(<<>>, Result)
        after
            meck:unload(elib_cipher)
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
        ?assert(Result =< 9)
    end).

num_random_max_value_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 测试较大随机数生成
        Result = elib_cipher:num_random(20),
        ?assert(is_integer(Result)),
        ?assert(Result >= trunc(math:pow(10, 19)))
    end).

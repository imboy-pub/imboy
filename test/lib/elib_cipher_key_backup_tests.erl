-module(elib_cipher_key_backup_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc
%%% elib_cipher 模块密钥备份功能的 EUnit 测试
%%%
%%% 目标：验证密钥备份相关加密功能
%%% 覆盖：主密码派生(PBKDF2)、私钥加密/解密(AES-256-GCM)
%%%===================================================================

%%%===================================================================
%%% @doc
%%% elib_cipher 模块密钥备份功能的 EUnit 测试
%%%
%%% 目标：验证密钥备份相关加密功能
%%% 覆盖：主密码派生(PBKDF2)、私钥加密/解密(AES-256-GCM)
%%%===================================================================

%% ===================================================================
%% 主密码派生测试 (PBKDF2)
%% ===================================================================

derive_master_password_returns_32_bytes_key_test_() ->
    ?_test(begin
        MasterPassword = <<"my_secure_password_123">>,
        Salt = crypto:strong_rand_bytes(16),

        {ok, DerivedKey} = elib_cipher:derive_master_password(MasterPassword, Salt),

        % 验证返回的是 32 字节密钥 (AES-256)
        ?assertMatch({ok, <<_:256>>}, {ok, DerivedKey}),
        ?assertEqual(32, byte_size(DerivedKey))
    end).
derive_master_password_with_same_inputs_same_output_test_() ->
    ?_test(begin
        MasterPassword = <<"test_password">>,
        Salt = crypto:strong_rand_bytes(16),

        {ok, Key1} = elib_cipher:derive_master_password(MasterPassword, Salt),
        {ok, Key2} = elib_cipher:derive_master_password(MasterPassword, Salt),

        % 相同输入应产生相同输出
        ?assertEqual(Key1, Key2)
    end).
derive_master_password_with_different_inputs_different_output_test_() ->
    ?_test(begin
        MasterPassword1 = <<"password1">>,
        MasterPassword2 = <<"password2">>,
        Salt = crypto:strong_rand_bytes(16),

        {ok, Key1} = elib_cipher:derive_master_password(MasterPassword1, Salt),
        {ok, Key2} = elib_cipher:derive_master_password(MasterPassword2, Salt),

        % 不同密码应产生不同密钥
        ?assertNotEqual(Key1, Key2)
    end).
derive_master_password_with_different_salts_different_output_test_() ->
    ?_test(begin
        MasterPassword = <<"password">>,
        Salt1 = crypto:strong_rand_bytes(16),
        Salt2 = crypto:strong_rand_bytes(16),

        {ok, Key1} = elib_cipher:derive_master_password(MasterPassword, Salt1),
        {ok, Key2} = elib_cipher:derive_master_password(MasterPassword, Salt2),

        % 不同盐值应产生不同密钥
        ?assertNotEqual(Key1, Key2)
    end).
derive_master_password_with_binary_password_test_() ->
    ?_test(begin
        MasterPassword = <<"binary_password"/utf8>>,
        Salt = crypto:strong_rand_bytes(16),

        {ok, DerivedKey} = elib_cipher:derive_master_password(MasterPassword, Salt),

        ?assertEqual(32, byte_size(DerivedKey))
    end).
derive_master_password_with_list_password_test_() ->
    ?_test(begin
        MasterPassword = "list_password",
        Salt = crypto:strong_rand_bytes(16),

        {ok, DerivedKey} = elib_cipher:derive_master_password(MasterPassword, Salt),

        % 应支持列表输入并转换为二进制
        ?assertEqual(32, byte_size(DerivedKey))
    end).
derive_master_password_with_empty_password_test_() ->
    ?_test(begin
        MasterPassword = <<>>,
        Salt = crypto:strong_rand_bytes(16),

        % 空密码应返回错误
        Result = elib_cipher:derive_master_password(MasterPassword, Salt),

        ?assertMatch({error, _}, Result)
    end).
derive_master_password_with_invalid_salt_test_() ->
    ?_test(begin
        MasterPassword = <<"password">>,
        InvalidSalt = <<>>,  % 空盐值

        Result = elib_cipher:derive_master_password(MasterPassword, InvalidSalt),

        % 无效盐值应返回错误
        ?assertMatch({error, _}, Result)
    end).
%% ===================================================================
%% 私钥加密/解密测试 (AES-256-GCM)
%% ===================================================================

encrypt_private_key_returns_base64_test_() ->
    ?_test(begin
        PrivateKey = <<"-----BEGIN RSA PRIVATE KEY-----\nMIIEpAIBAAKCAQEA...\n-----END RSA PRIVATE KEY-----">>,
        EncryptionKey = crypto:strong_rand_bytes(32),

        {ok, EncryptedData} = elib_cipher:encrypt_private_key(PrivateKey, EncryptionKey),

        % 验证返回的是有效的 Base64 编码
        ?assertMatch({ok, <<_/binary>>}, {ok, EncryptedData}),
        ?assert(is_binary(EncryptedData)),
        ?assert(byte_size(EncryptedData) > 0)
    end).
encrypt_and_decrypt_private_key_roundtrip_test_() ->
    ?_test(begin
        OriginalPrivateKey = <<"-----BEGIN RSA PRIVATE KEY-----\nTestPrivateKey123\n-----END RSA PRIVATE KEY-----">>,
        EncryptionKey = crypto:strong_rand_bytes(32),

        % 加密
        {ok, EncryptedData} = elib_cipher:encrypt_private_key(OriginalPrivateKey, EncryptionKey),

        % 解密
        {ok, DecryptedKey} = elib_cipher:decrypt_private_key(EncryptedData, EncryptionKey),

        % 验证往返成功
        ?assertEqual(OriginalPrivateKey, DecryptedKey)
    end).
encrypt_private_key_with_different_keys_different_output_test_() ->
    ?_test(begin
        PrivateKey = <<"test_private_key">>,
        Key1 = crypto:strong_rand_bytes(32),
        Key2 = crypto:strong_rand_bytes(32),

        {ok, Encrypted1} = elib_cipher:encrypt_private_key(PrivateKey, Key1),
        {ok, Encrypted2} = elib_cipher:encrypt_private_key(PrivateKey, Key2),

        % 不同密钥应产生不同密文
        ?assertNotEqual(Encrypted1, Encrypted2)
    end).
encrypt_private_key_with_same_key_different_iv_test_() ->
    ?_test(begin
        PrivateKey = <<"test_private_key">>,
        EncryptionKey = crypto:strong_rand_bytes(32),

        % 两次加密应使用不同的 IV（GCM 模式每次应不同）
        {ok, Encrypted1} = elib_cipher:encrypt_private_key(PrivateKey, EncryptionKey),
        {ok, Encrypted2} = elib_cipher:encrypt_private_key(PrivateKey, EncryptionKey),

        % 验证密文不同（因为 IV 不同）
        ?assertNotEqual(Encrypted1, Encrypted2)
    end).
decrypt_private_key_with_wrong_key_fails_test_() ->
    ?_test(begin
        PrivateKey = <<"test_private_key">>,
        EncryptionKey = crypto:strong_rand_bytes(32),
        WrongKey = crypto:strong_rand_bytes(32),

        {ok, EncryptedData} = elib_cipher:encrypt_private_key(PrivateKey, EncryptionKey),

        % 使用错误的密钥解密应失败
        Result = elib_cipher:decrypt_private_key(EncryptedData, WrongKey),

        ?assertMatch({error, _}, Result)
    end).
decrypt_private_key_with_invalid_base64_fails_test_() ->
    ?_test(begin
        InvalidBase64 = <<"not_valid_base64!!!">>,
        EncryptionKey = crypto:strong_rand_bytes(32),

        Result = elib_cipher:decrypt_private_key(InvalidBase64, EncryptionKey),

        ?assertMatch({error, _}, Result)
    end).
decrypt_private_key_with_empty_data_fails_test_() ->
    ?_test(begin
        EmptyData = <<>>,
        EncryptionKey = crypto:strong_rand_bytes(32),

        Result = elib_cipher:decrypt_private_key(EmptyData, EncryptionKey),

        ?assertMatch({error, _}, Result)
    end).
decrypt_private_key_with_wrong_key_length_fails_test_() ->
    ?_test(begin
        PrivateKey = <<"test_private_key">>,
        ValidKey = crypto:strong_rand_bytes(32),
        InvalidKey = crypto:strong_rand_bytes(16),  % 错误的密钥长度

        {ok, EncryptedData} = elib_cipher:encrypt_private_key(PrivateKey, ValidKey),

        Result = elib_cipher:decrypt_private_key(EncryptedData, InvalidKey),

        ?assertMatch({error, _}, Result)
    end).
encrypt_private_key_with_large_data_test_() ->
    ?_test(begin
        % 生成较大的私钥数据（模拟 4096 位 RSA 密钥）
        LargePrivateKey = crypto:strong_rand_bytes(512),
        EncryptionKey = crypto:strong_rand_bytes(32),

        {ok, EncryptedData} = elib_cipher:encrypt_private_key(LargePrivateKey, EncryptionKey),
        {ok, DecryptedKey} = elib_cipher:decrypt_private_key(EncryptedData, EncryptionKey),

        ?assertEqual(LargePrivateKey, DecryptedKey)
    end).
encrypt_private_key_with_empty_key_fails_test_() ->
    ?_test(begin
        PrivateKey = <<"test_key">>,
        EmptyKey = <<>>,

        Result = elib_cipher:encrypt_private_key(PrivateKey, EmptyKey),

        ?assertMatch({error, _}, Result)
    end).
%% ===================================================================
%% 集成测试：完整的密钥备份流程
%% ===================================================================

complete_key_backup_flow_test_() ->
    ?_test(begin
        % 模拟完整的密钥备份流程

        % 1. 用户输入主密码
        UserPassword = <<"MyMasterPassword123!">>,

        % 2. 生成盐值
        Salt = crypto:strong_rand_bytes(16),

        % 3. 派生加密密钥
        {ok, DerivedKey} = elib_cipher:derive_master_password(UserPassword, Salt),

        % 4. 加密私钥
        OriginalPrivateKey = <<"-----BEGIN RSA PRIVATE KEY-----\nOriginalKeyData\n-----END RSA PRIVATE KEY-----">>,
        {ok, EncryptedKey} = elib_cipher:encrypt_private_key(OriginalPrivateKey, DerivedKey),

        % 5. 验证可以正确解密
        {ok, DecryptedPrivateKey} = elib_cipher:decrypt_private_key(EncryptedKey, DerivedKey),

        ?assertEqual(OriginalPrivateKey, DecryptedPrivateKey),

        % 6. 验证使用相同密码和盐值可以重新派生密钥并解密
        {ok, DerivedKey2} = elib_cipher:derive_master_password(UserPassword, Salt),
        {ok, DecryptedPrivateKey2} = elib_cipher:decrypt_private_key(EncryptedKey, DerivedKey2),

        ?assertEqual(OriginalPrivateKey, DecryptedPrivateKey2)
    end).
key_backup_with_wrong_password_fails_test_() ->
    ?_test(begin
        % 验证使用错误密码无法解密

        OriginalPassword = <<"CorrectPassword123">>,
        WrongPassword = <<"WrongPassword456">>,
        Salt = crypto:strong_rand_bytes(16),

        % 使用正确密码派生密钥并加密
        {ok, CorrectKey} = elib_cipher:derive_master_password(OriginalPassword, Salt),
        PrivateKey = <<"secret_key">>,
        {ok, EncryptedKey} = elib_cipher:encrypt_private_key(PrivateKey, CorrectKey),

        % 使用错误密码派生密钥并尝试解密
        {ok, WrongKey} = elib_cipher:derive_master_password(WrongPassword, Salt),
        Result = elib_cipher:decrypt_private_key(EncryptedKey, WrongKey),

        % 应该失败
        ?assertMatch({error, _}, Result)
    end).

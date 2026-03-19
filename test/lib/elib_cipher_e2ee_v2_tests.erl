-module(elib_cipher_e2ee_v2_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

%%%===================================================================
%%% @doc
%%% elib_cipher 模块 E2EE v2.0 格式的 EUnit 测试
%%%
%%% 目标：验证端到端加密 v2.0 消息格式和边界条件
%%% 覆盖：E2EE 消息结构、RSA-OAEP 加密、AES-256-GCM 加密
%%%===================================================================

%% ===================================================================
%% E2EE v2.0 消息格式测试
%% ===================================================================

e2ee_message_structure_v2_test_() ->
    ?_test(begin
        % 构建 v2.0 格式的 E2EE 消息
        Msg = #{
            <<"id">> => <<"msg123">>,
            <<"type">> => <<"C2C">>,
            <<"from">> => <<"user1">>,
            <<"to">> => <<"user2">>,
            <<"msg_type">> => <<"text">>,
            <<"action">> => <<>>,
            <<"e2ee">> => #{
                <<"e2ee">> => true,
                <<"e2ee_ver">> => 1,
                <<"e2ee_suite">> => <<"RSA-OAEP-256+AES-256-GCM">>,
                <<"nonce">> => base64:encode(crypto:strong_rand_bytes(12)),
                <<"keys">> => [
                    #{
                        <<"recipient">> => <<"user2_device1">>,
                        <<"ephemeral_key">> => base64:encode(crypto:strong_rand_bytes(32))
                    }
                ]
            },
            <<"payload">> => base64:encode(<<"encrypted_content">>)
        },

        % 验证顶层字段
        ?assertEqual(<<"text">>, maps:get(<<"msg_type">>, Msg)),
        ?assertEqual(<<>>, maps:get(<<"action">>, Msg)),
        ?assert(is_map(maps:get(<<"e2ee">>, Msg))),
        ?assert(is_binary(maps:get(<<"payload">>, Msg))),

        % 验证 E2EE 元数据
        E2EE = maps:get(<<"e2ee">>, Msg),
        ?assertEqual(true, maps:get(<<"e2ee">>, E2EE)),
        ?assertEqual(1, maps:get(<<"e2ee_ver">>, E2EE)),
        ?assert(is_list(maps:get(<<"keys">>, E2EE)))
    end).

e2ee_message_with_null_e2ee_field_test_() ->
    ?_test(begin
        % 非 E2EE 消息的 e2ee 字段应为 null
        Msg = #{
            <<"id">> => <<"msg124">>,
            <<"type">> => <<"C2C">>,
            <<"msg_type">> => <<"text">>,
            <<"action">> => <<>>,
            <<"e2ee">> => null,
            <<"payload">> => #{<<"content">> => <<"Hello">>}
        },

        ?assertEqual(null, maps:get(<<"e2ee">>, Msg)),
        ?assert(is_map(maps:get(<<"payload">>, Msg)))
    end).

%% ===================================================================
%% RSA-OAEP 加密/解密测试
%% ===================================================================

encrypt_rsa_oaep_and_decrypt_rsa_oaep_roundtrip_test_() ->
    ?_test(begin
        % 生成 RSA 密钥对
        PrivateKeyPEM = generate_rsa_private_key(),
        PublicKeyPEM = derive_public_key(PrivateKeyPEM),

        PlainText = <<"secret message for e2ee">>,

        % 使用公钥加密
        {ok, Encrypted} = elib_cipher:encrypt_rsa_oaep(PlainText, PublicKeyPEM),

        % 验证加密结果
        ?assertMatch({ok, <<_/binary>>}, {ok, Encrypted}),
        ?assertNotEqual(PlainText, Encrypted),

        % 使用私钥解密
        {ok, Decrypted} = elib_cipher:decrypt_rsa_oaep(Encrypted, PrivateKeyPEM),

        % 验证解密结果
        ?assertEqual(PlainText, Decrypted)
    end).

encrypt_rsa_oaep_with_large_data_test_() ->
    ?_test(begin
        PrivateKeyPEM = generate_rsa_private_key(),
        PublicKeyPEM = derive_public_key(PrivateKeyPEM),

        % RSA-OAEP 可以加密较大的数据（取决于密钥大小）
        PlainText = <<"This is a longer message that tests the RSA-OAEP encryption capabilities. "
                       "It should handle reasonable message sizes for E2EE key exchange.">>,

        {ok, Encrypted} = elib_cipher:encrypt_rsa_oaep(PlainText, PublicKeyPEM),
        {ok, Decrypted} = elib_cipher:decrypt_rsa_oaep(Encrypted, PrivateKeyPEM),

        ?assertEqual(PlainText, Decrypted)
    end).

encrypt_rsa_oaep_with_empty_data_test_() ->
    ?_test(begin
        PrivateKeyPEM = generate_rsa_private_key(),
        PublicKeyPEM = derive_public_key(PrivateKeyPEM),

        % 测试空数据
        PlainText = <<>>,

        {ok, Encrypted} = elib_cipher:encrypt_rsa_oaep(PlainText, PublicKeyPEM),
        {ok, Decrypted} = elib_cipher:decrypt_rsa_oaep(Encrypted, PrivateKeyPEM),

        ?assertEqual(PlainText, Decrypted)
    end).

encrypt_rsa_oaep_with_invalid_key_test_() ->
    ?_test(begin
        InvalidKey = <<"invalid_key_content">>,
        PlainText = <<"test">>,

        % 无效密钥应返回错误
        Result = elib_cipher:encrypt_rsa_oaep(PlainText, InvalidKey),

        ?assertMatch({error, _}, Result)
    end).

decrypt_rsa_oaep_with_invalid_data_test_() ->
    ?_test(begin
        PrivateKeyPEM = generate_rsa_private_key(),

        InvalidCipher = <<"not_valid_base64_or_encrypted_data">>,

        % 无效数据应返回错误
        Result = elib_cipher:decrypt_rsa_oaep(InvalidCipher, PrivateKeyPEM),

        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% E2EE 完整流程测试
%% ===================================================================

e2ee_full_flow_simulation_test_() ->
    ?_test(begin
        % 模拟完整的 E2EE 流程
        SenderKey = generate_rsa_private_key(),
        SenderPublicKey = derive_public_key(SenderKey),

        RecipientKey = generate_rsa_private_key(),
        RecipientPublicKey = derive_public_key(RecipientKey),

        % 1. 发送方生成 AES 密钥
        AESKey = crypto:strong_rand_bytes(32),
        Nonce = crypto:strong_rand_bytes(12),

        % 2. 加密消息内容
        Plaintext = <<"Secret E2EE message">>,
        {CipherText, GCMTag} = crypto:crypto_one_time_aead(
            aes_256_gcm, AESKey, Nonce, Plaintext, Nonce, 16, true
        ),
        CombinedCipher = <<Nonce/binary, CipherText/binary, GCMTag/binary>>,

        % 3. 用接收方公钥加密 AES 密钥
        {ok, EncryptedAESKey} = elib_cipher:encrypt_rsa_oaep(AESKey, RecipientPublicKey),

        % 4. 构建 E2EE 消息
        E2EEMsg = #{
            <<"e2ee">> => true,
            <<"e2ee_ver">> => 1,
            <<"e2ee_suite">> => <<"RSA-OAEP-256+AES-256-GCM">>,
            <<"nonce">> => base64:encode(Nonce),
            <<"keys">> => [
                #{
                    <<"recipient">> => <<"recipient_device">>,
                    <<"ephemeral_key">> => EncryptedAESKey
                }
            ]
        },

        % 5. 接收方解密
        KeysList = maps:get(<<"keys">>, E2EEMsg),
        [#{<<"ephemeral_key">> := EncryptedKey} | _] = KeysList,
        {ok, DecryptedAESKey} = elib_cipher:decrypt_rsa_oaep(EncryptedKey, RecipientKey),

        % 6. 解密消息内容
        <<DecryptedNonce:12/binary, Rest/binary>> = CombinedCipher,
        CipherPartSize = byte_size(Rest) - 16,
        <<DecryptedCipher:CipherPartSize/binary, DecryptedTag:16/binary>> = Rest,

        case crypto:crypto_one_time_aead(
            aes_256_gcm, DecryptedAESKey, DecryptedNonce, DecryptedCipher, DecryptedNonce, DecryptedTag, false
        ) of
            error ->
                ?assert(false, "GCM authentication failed");
            DecryptedPlaintext ->
                ?assertEqual(Plaintext, DecryptedPlaintext)
        end
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

e2ee_with_unicode_content_test_() ->
    ?_test(begin
        PrivateKeyPEM = generate_rsa_private_key(),
        PublicKeyPEM = derive_public_key(PrivateKeyPEM),

        % 测试 Unicode 内容
        UnicodeText = <<"你好世界！Hello World! 🌍🔐">>,

        {ok, Encrypted} = elib_cipher:encrypt_rsa_oaep(UnicodeText, PublicKeyPEM),
        {ok, Decrypted} = elib_cipher:decrypt_rsa_oaep(Encrypted, PrivateKeyPEM),

        ?assertEqual(UnicodeText, Decrypted)
    end).

e2ee_with_special_characters_test_() ->
    ?_test(begin
        PrivateKeyPEM = generate_rsa_private_key(),
        PublicKeyPEM = derive_public_key(PrivateKeyPEM),

        % 测试特殊字符
        SpecialText = <<"\n\r\t\\\"'">>,

        {ok, Encrypted} = elib_cipher:encrypt_rsa_oaep(SpecialText, PublicKeyPEM),
        {ok, Decrypted} = elib_cipher:decrypt_rsa_oaep(Encrypted, PrivateKeyPEM),

        ?assertEqual(SpecialText, Decrypted)
    end).

e2ee_with_binary_data_test_() ->
    ?_test(begin
        PrivateKeyPEM = generate_rsa_private_key(),
        PublicKeyPEM = derive_public_key(PrivateKeyPEM),

        % 测试二进制数据
        BinaryData = crypto:strong_rand_bytes(64),

        {ok, Encrypted} = elib_cipher:encrypt_rsa_oaep(BinaryData, PublicKeyPEM),
        {ok, Decrypted} = elib_cipher:decrypt_rsa_oaep(Encrypted, PrivateKeyPEM),

        ?assertEqual(BinaryData, Decrypted)
    end).

%% ===================================================================
%% 辅助函数
%% ===================================================================

%% @private 生成 RSA 私钥 (2048 位)
generate_rsa_private_key() ->
    PrivateKey = public_key:generate_key({rsa, 2048, 65537}),
    public_key:pem_encode([
        public_key:pem_entry_encode('RSAPrivateKey', PrivateKey)
    ]).

%% @private 从私钥推导公钥
derive_public_key(PrivateKeyPEM) ->
    [PemEntry] = public_key:pem_decode(PrivateKeyPEM),
    PrivateKey = public_key:pem_entry_decode(PemEntry),
    PublicKey = #'RSAPublicKey'{
        modulus = PrivateKey#'RSAPrivateKey'.modulus,
        publicExponent = PrivateKey#'RSAPrivateKey'.publicExponent
    },
    public_key:pem_encode([
        public_key:pem_entry_encode('RSAPublicKey', PublicKey)
    ]).

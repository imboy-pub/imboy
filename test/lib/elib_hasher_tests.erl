-module(elib_hasher_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

md5_with_string_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            <<"098f6bcd4621d373cade4e832627b4f6">>,
            elib_hasher:md5("test")
        )
    end).

md5_with_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_hasher:md5(<<"test">>),
        ?assertEqual(32, byte_size(Result)),
        ?assertEqual(Result, elib_hasher:md5("test"))
    end).

md5_with_empty_input_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            <<"d41d8cd98f00b204e9800998ecf8427e">>,
            elib_hasher:md5(<<>>)
        )
    end).

hmac_sha512_returns_base64_mac_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = <<"test">>,
        Key = <<"secret">>,
        Expected = base64:encode(crypto:mac(hmac, sha512, Key, Input)),
        Result = elib_hasher:hmac_sha512(Input, Key),

        ?assertEqual(Expected, Result),
        ?assertEqual(88, byte_size(Result))
    end).

hmac_sha256_returns_base64_mac_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = <<"test">>,
        Key = <<"secret">>,
        Expected = base64:encode(crypto:mac(hmac, sha256, Key, Input)),
        Result = elib_hasher:hmac_sha256(Input, Key),

        ?assertEqual(Expected, Result),
        ?assertEqual(44, byte_size(Result))
    end).

hmac_with_different_keys_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = <<"test">>,
        Result1 = elib_hasher:hmac_sha256(Input, <<"key1">>),
        Result2 = elib_hasher:hmac_sha256(Input, <<"key2">>),

        ?assertNotEqual(Result1, Result2)
    end).

encoded_val_with_binary_test_() ->
    ?WITH_MECK(config_ds, [
        {'env', 1, fun(postgre_aes_key) -> <<"aes-key">> end}
    ], fun() ->
        Input = <<"plain-text">>,
        Base64Input = base64:encode(Input),
        Expected = <<"encode(encrypt('", Base64Input/binary, "', 'aes-key', 'aes-cbc/pad:pkcs'), 'base64')">>,

        ?assertEqual(Expected, elib_hasher:encoded_val(Input))
    end).

encoded_val_with_map_uses_json_encode_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(postgre_aes_key) -> <<"aes-key">> end}
        ]},
        {jsone, [
            {'encode', 2, fun(#{<<"hello">> := <<"world">>}, [native_utf8]) -> <<"{\"hello\":\"world\"}">> end}
        ]}
    ], fun() ->
        Json = <<"{\"hello\":\"world\"}">>,
        Base64Json = base64:encode(Json),
        Expected = <<"encode(encrypt('", Base64Json/binary, "', 'aes-key', 'aes-cbc/pad:pkcs'), 'base64')">>,

        ?assertEqual(Expected, elib_hasher:encoded_val(#{<<"hello">> => <<"world">>}))
    end).

decoded_payload_uses_payload_field_test_() ->
    ?WITH_MECK(config_ds, [
        {'env', 1, fun(postgre_aes_key) -> <<"aes-key">> end}
    ], fun() ->
        Expected = <<"decode(encode(decrypt(decode(replace(payload, 'aes_cbc_', ''),'base64'), 'aes-key', 'aes-cbc/pad:pkcs') , 'escape'), 'base64') as payload">>,
        ?assertEqual(Expected, elib_hasher:decoded_payload())
    end).

decoded_field_supports_custom_field_test_() ->
    ?WITH_MECK(config_ds, [
        {'env', 1, fun(postgre_aes_key) -> <<"aes-key">> end}
    ], fun() ->
        Field = <<"custom_field">>,
        Expected = <<"decode(encode(decrypt(decode(replace(custom_field, 'aes_cbc_', ''),'base64'), 'aes-key', 'aes-cbc/pad:pkcs') , 'escape'), 'base64') as custom_field">>,
        ?assertEqual(Expected, elib_hasher:decoded_field(Field))
    end).

hash_unicode_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_hasher:md5(<<"测试中文"/utf8>>),
        ?assertEqual(32, byte_size(Result)),
        ?assert(re:run(Result, "^[0-9a-f]+$") =/= nomatch)
    end).

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

%% ===================================================================
%% A-05：encoded_val/1 改应用层加密（审计缺陷 #2）
%% ===================================================================

%% 32 字节测试密钥
-define(K1, <<"0123456789abcdef0123456789abcdef">>).
-define(K2, <<"ffffffffffffffffffffffffffffffff">>).

-define(WITH_KEY(Key, TestFun),
    ?WITH_MECK(
        config_ds,
        [{'env', 1, fun(postgre_aes_key) -> Key end}],
        TestFun
    )
).

%% 新写入格式：aesg1_ 前缀 + base64 密文，且不含任何 SQL 表达式/密钥痕迹
encoded_val_produces_app_layer_cipher_test_() ->
    ?WITH_KEY(?K1, fun() ->
        Cipher = elib_hasher:encoded_val(<<"plain-text">>),

        ?assertMatch(<<"aesg1_", _/binary>>, Cipher),
        ?assertEqual(nomatch, binary:match(Cipher, <<"encrypt(">>)),
        ?assertEqual(nomatch, binary:match(Cipher, ?K1)),
        ?assertEqual(nomatch, binary:match(Cipher, <<"plain-text">>))
    end).

encoded_val_round_trip_test_() ->
    ?WITH_KEY(?K1, fun() ->
        Input = <<"plain-text">>,
        ?assertEqual(Input, elib_hasher:decoded_val(elib_hasher:encoded_val(Input)))
    end).

encoded_val_with_map_uses_json_encode_test_() ->
    ?WITH_KEY(?K1, fun() ->
        Cipher = elib_hasher:encoded_val(#{<<"hello">> => <<"world">>}),
        ?assertEqual(<<"{\"hello\":\"world\"}">>, elib_hasher:decoded_val(Cipher))
    end).

%% 随机 IV：同一明文两次加密密文必须不同（审计 #26 的确定性密文问题）
encoded_val_uses_random_iv_test_() ->
    ?WITH_KEY(?K1, fun() ->
        ?assertNotEqual(
            elib_hasher:encoded_val(<<"same">>),
            elib_hasher:encoded_val(<<"same">>)
        )
    end).

%% fail-closed：密钥缺失时必须 error，绝不回落明文落库
encoded_val_fails_closed_without_key_test_() ->
    ?WITH_MECK(
        config_ds,
        [{'env', 1, fun(postgre_aes_key) -> undefined end}],
        fun() ->
            ?assertError(invalid_postgre_aes_key, elib_hasher:encoded_val(<<"x">>))
        end
    ).

encoded_val_fails_closed_on_short_key_test_() ->
    ?WITH_KEY(<<"too-short">>, fun() ->
        ?assertError(invalid_postgre_aes_key, elib_hasher:encoded_val(<<"x">>))
    end).

%% ===================================================================
%% decoded_val/1 历史形态兼容
%% ===================================================================

%% 形态 2：A-05 之前的脏数据 —— 从未真正加密，内层是 base64(明文)
decoded_val_reads_legacy_sql_literal_test_() ->
    ?WITH_KEY(?K1, fun() ->
        Plain = <<"{\"text\":\"hello\"}">>,
        B64 = base64:encode(Plain),
        Legacy =
            <<"encode(encrypt('", B64/binary,
                "', 'rPWaWDXmXwmNBCes8Dm94RhFBkwZFtnX', 'aes-cbc/pad:pkcs'), 'base64')">>,

        ?assertEqual(Plain, elib_hasher:decoded_val(Legacy))
    end).

decoded_val_tolerates_corrupt_legacy_literal_test_() ->
    ?WITH_KEY(?K1, fun() ->
        %% 内层不是合法 base64 → 空，不抛异常
        ?assertEqual(<<>>, elib_hasher:decoded_val(<<"encode(encrypt('%%%not-base64%%%'">>)),
        %% 长度不是 4 的倍数 → 空
        ?assertEqual(<<>>, elib_hasher:decoded_val(<<"encode(encrypt('truncated">>))
    end).

%% 截断行（无收尾单引号）必须与迁移 00000053 的 split_part 语义一致：
%% 迁移会把它转成明文，读取侧就不能判定失败，否则出现「迁移前读不出、
%% 迁移后读得出」的窗口期不一致，`--apply` 也会跳过迁移能处理的行。
decoded_val_matches_migration_on_truncated_literal_test_() ->
    ?WITH_KEY(?K1, fun() ->
        Plain = <<"{\"t\":1}">>,
        B64 = base64:encode(Plain),
        ?assertEqual(Plain, elib_hasher:decoded_val(<<"encode(encrypt('", B64/binary>>))
    end).

%% 形态 3：迁移 00000053 清洗后的明文，原样返回
decoded_val_passes_through_plaintext_test_() ->
    ?WITH_KEY(?K1, fun() ->
        ?assertEqual(<<"{\"a\":1}">>, elib_hasher:decoded_val(<<"{\"a\":1}">>)),
        ?assertEqual(<<>>, elib_hasher:decoded_val(<<>>))
    end).

%% 密钥不匹配（轮换后拿旧密文）必须返回空，不能把密文回落给客户端
decoded_val_fails_closed_on_wrong_key_test_() ->
    Cipher = elib_cipher:aes_gcm_encrypt(<<"secret">>, ?K1),
    {ok, B64} = Cipher,
    ?WITH_KEY(?K2, fun() ->
        ?assertEqual(<<>>, elib_hasher:decoded_val(<<"aesg1_", B64/binary>>))
    end).

%% ===================================================================
%% decode_list_field/2
%% ===================================================================

decode_list_field_decrypts_each_row_test_() ->
    ?WITH_KEY(?K1, fun() ->
        C1 = elib_hasher:encoded_val(<<"one">>),
        C2 = elib_hasher:encoded_val(<<"two">>),
        List = [
            #{<<"id">> => 1, <<"info">> => C1},
            #{<<"id">> => 2, <<"info">> => C2}
        ],

        ?assertEqual(
            [
                #{<<"id">> => 1, <<"info">> => <<"one">>},
                #{<<"id">> => 2, <<"info">> => <<"two">>}
            ],
            elib_hasher:decode_list_field(List, <<"info">>)
        )
    end).

decode_list_field_leaves_rows_without_field_untouched_test_() ->
    ?WITH_KEY(?K1, fun() ->
        List = [#{<<"id">> => 1}, #{<<"info">> => 123}, not_a_map],
        ?assertEqual(List, elib_hasher:decode_list_field(List, <<"info">>)),
        ?assertEqual(<<"not-a-list">>, elib_hasher:decode_list_field(<<"not-a-list">>, <<"info">>))
    end).

hash_unicode_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_hasher:md5(<<"测试中文"/utf8>>),
        ?assertEqual(32, byte_size(Result)),
        ?assert(re:run(Result, "^[0-9a-f]+$") =/= nomatch)
    end).

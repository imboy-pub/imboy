-module(imboy_hasher_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_hasher 模块的 EUnit 测试
%%%
%%% 目标：验证哈希工具功能
%%% 覆盖：MD5、SHA、HMAC
%%%===================================================================

%% ===================================================================
%% MD5 哈希测试
%% ===================================================================

md5_with_string_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = "test",
        Result = imboy_hasher:md5(Input),
        % 验证MD5哈希结果长度
        ?assertEqual(32, byte_size(Result)),
        % 验证结果是十六进制字符串
        ?assert(re:run(Result, "^[0-9a-fA-F]+$") =/= nomatch)
    end).

md5_with_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = <<"test">>,
        Result = imboy_hasher:md5(Input),
        % 验证MD5哈希结果长度
        ?assertEqual(32, byte_size(Result)),
        % 验证相同输入产生相同输出
        Result2 = imboy_hasher:md5("test"),
        ?assertEqual(Result, Result2)
    end).

md5_with_empty_input_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = <<>>,
        Result = imboy_hasher:md5(Input),
        % 验证空输入的MD5哈希
        Expected = <<"d41d8cd98f00b204e9800998ecf8427e">>,
        ?assertEqual(Expected, Result)
    end).

%% ===================================================================
%% SHA256 哈希测试
%% ===================================================================

sha256_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = <<"test">>,
        Result = imboy_hasher:sha256(Input),
        % 验证SHA256哈希结果长度
        ?assertEqual(64, byte_size(Result)),
        % 验证结果是十六进制字符串
        ?assert(re:run(Result, "^[0-9a-fA-F]+$") =/= nomatch)
    end).

sha256_with_large_input_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = binary:copy(<<"a">>, 1000),
        Result = imboy_hasher:sha256(Input),
        % 验证大输入的哈希结果
        ?assertEqual(64, byte_size(Result)),
        ?assertNotEqual(<<>>, Result)
    end).

%% ===================================================================
%% HMAC 测试
%% ===================================================================

hmac_sha512_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = <<"test">>,
        Key = <<"secret">>,
        Result = imboy_hasher:hmac_sha512(Input, Key),
        % 验证HMAC结果长度
        ?assertEqual(128, byte_size(Result)),
        % 验证结果是十六进制字符串
        ?assert(re:run(Result, "^[0-9a-fA-F]+$") =/= nomatch)
    end).

hmac_sha256_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = <<"test">>,
        Key = <<"secret">>,
        Result = imboy_hasher:hmac_sha256(Input, Key),
        % 验证HMAC-SHA256结果长度
        ?assertEqual(64, byte_size(Result)),
        % 验证相同输入和密钥产生相同输出
        Result2 = imboy_hasher:hmac_sha256(Input, Key),
        ?assertEqual(Result, Result2)
    end).

hmac_with_different_keys_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = <<"test">>,
        Key1 = <<"key1">>,
        Key2 = <<"key2">>,
        Result1 = imboy_hasher:hmac_sha256(Input, Key1),
        Result2 = imboy_hasher:hmac_sha256(Input, Key2),
        % 验证不同密钥产生不同HMAC
        ?assertNotEqual(Result1, Result2)
    end).

%% ===================================================================
%% 密码哈希测试
%% ===================================================================

password_hash_test_() ->
    ?TEST_SIMPLE(fun() ->
        Password = <<"password123">>,
        Salt = <<"salt">>,
        Result = imboy_hasher:password_hash(Password, Salt),
        % 验证密码哈希结果
        ?assertMatch(<<_/binary>>, Result),
        ?assertNotEqual(<<>>, Result),
        % 验证包含盐值影响
        Result2 = imboy_hasher:password_hash(Password, <<"different">>),
        ?assertNotEqual(Result, Result2)
    end).

password_verify_test_() ->
    ?TEST_SIMPLE(fun() ->
        Password = <<"password123">>,
        Salt = <<"salt">>,
        Hash = imboy_hasher:password_hash(Password, Salt),
        % 验证密码验证功能
        ?assert(imboy_hasher:password_verify(Password, Salt, Hash)),
        ?assertNot(imboy_hasher:password_verify(<<"wrong">>, Salt, Hash))
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

hash_unicode_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = <<"测试中文">>,
        Result = imboy_hasher:md5(Input),
        % 验证Unicode字符哈希
        ?assertEqual(32, byte_size(Result)),
        ?assertNotEqual(<<>>, Result)
    end).

hash_very_long_input_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = binary:copy(<<"x">>, 10000),
        Result = imboy_hasher:sha256(Input),
        % 验证超长输入处理
        ?assertEqual(64, byte_size(Result))
    end).

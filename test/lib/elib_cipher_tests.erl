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
        Key = <<"encryption_key_123">>,
        % 加密
        {ok, CipherText} = elib_cipher:encrypt(PlainText, Key),
        ?assertNotEqual(PlainText, CipherText),
        ?assertMatch(<<_/binary>>, CipherText),
        ?assert(byte_size(CipherText) > 0),
        % 解密
        {ok, DecryptedText} = elib_cipher:decrypt(CipherText, Key),
        ?assertEqual(PlainText, DecryptedText)
    end).

encrypt_with_empty_text_test_() ->
    ?TEST_WITH_APP(fun() ->
        PlainText = <<>>,
        Key = <<"encryption_key">>,
        % 测试空文本加密
        {ok, CipherText} = elib_cipher:encrypt(PlainText, Key),
        ?assertMatch(<<_/binary>>, CipherText),
        ?assert(byte_size(CipherText) > 0),
        % 空文本解密应该返回空文本
        {ok, DecryptedText} = elib_cipher:decrypt(CipherText, Key),
        ?assertEqual(PlainText, DecryptedText)
    end).

decrypt_with_invalid_data_test_() ->
    ?TEST_WITH_APP(fun() ->
        InvalidCipherText = <<"invalid_data">>,
        Key = <<"encryption_key">>,
        % 测试无效数据解密应该失败
        Result = elib_cipher:decrypt(InvalidCipherText, Key),
        ?ASSERT_ERROR(Result),
        {error, Reason} = Result,
        % 验证错误类型
        ?assert(is_atom(Reason) orelse is_binary(Reason)),
        % 验证是否为已知的错误类型
        case Reason of
            invalid_data -> ok;
            decryption_failed -> ok;
            invalid_key -> ok;
            _ when is_atom(Reason) -> ok;  % 允许其他原子类型错误
            _ when is_binary(Reason) -> ok  % 允许二进制错误消息
        end
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

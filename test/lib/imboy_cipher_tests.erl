-module(imboy_cipher_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_cipher 模块的 EUnit 测试
%%%
%%% 目标：验证加密解密工具功能
%%% 覆盖：AES加密、解密
%%%===================================================================

%% ===================================================================
%% AES 加密解密测试
%% ===================================================================

encrypt_and_decrypt_roundtrip_test_() ->
    ?TEST_WITH_APP(fun() ->
        PlainText = <<"secret message">>,
        Key = <<"encryption_key_123">>,
        % 加密
        {ok, CipherText} = imboy_cipher:encrypt(PlainText, Key),
        ?assertNotEqual(PlainText, CipherText),
        ?assertMatch(<<_/binary>>, CipherText),
        ?assert(byte_size(CipherText) > 0),
        % 解密
        {ok, DecryptedText} = imboy_cipher:decrypt(CipherText, Key),
        ?assertEqual(PlainText, DecryptedText)
    end).

encrypt_with_empty_text_test_() ->
    ?TEST_WITH_APP(fun() ->
        PlainText = <<>>,
        Key = <<"encryption_key">>,
        % 测试空文本加密
        {ok, CipherText} = imboy_cipher:encrypt(PlainText, Key),
        ?assertMatch(<<_/binary>>, CipherText),
        ?assert(byte_size(CipherText) > 0),
        % 空文本解密应该返回空文本
        {ok, DecryptedText} = imboy_cipher:decrypt(CipherText, Key),
        ?assertEqual(PlainText, DecryptedText)
    end).

decrypt_with_invalid_data_test_() ->
    ?TEST_WITH_APP(fun() ->
        InvalidCipherText = <<"invalid_data">>,
        Key = <<"encryption_key">>,
        % 测试无效数据解密应该失败
        Result = imboy_cipher:decrypt(InvalidCipherText, Key),
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

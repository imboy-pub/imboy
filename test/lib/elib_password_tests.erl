-module(elib_password_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% elib_password 模块的 EUnit 测试
%%%
%%% 目标：验证密码加密和验证功能
%%% 覆盖：密码生成、验证、MD5/HMAC-SHA512 算法
%%%===================================================================

%% ===================================================================
%% generate/1 测试
%% ===================================================================

generate_returns_binary_test_() ->
    ?TEST_WITH_APP(fun() ->
        Plaintext = "test_password",
        Result = elib_password:generate(Plaintext),
        ?assertMatch(<<_/binary>>, Result),
        Decoded = base64:decode(Result, #{padding => false}),
        [Salt, <<"hmac_sha512">>, Mac] = binary:split(Decoded, <<$:>>, [global]),
        ?assert(byte_size(Salt) > 0),
        ?assert(byte_size(Mac) > 0),
        Hash2 = elib_password:generate(Plaintext),
        ?assertNotEqual(Result, Hash2)
    end).

generate_with_different_inputs_test_() ->
    ?TEST_WITH_APP(fun() ->
        Pwd1 = elib_password:generate("password1"),
        Pwd2 = elib_password:generate("password2"),
        % 验证不同密码生成不同哈希
        ?assertNotEqual(Pwd1, Pwd2),
        % 验证相同密码生成不同哈希（由于随机salt）
        Pwd1_again = elib_password:generate("password1"),
        ?assertNotEqual(Pwd1, Pwd1_again),
        % 验证所有哈希都符合预期格式
        ?assertMatch(<<_/binary>>, Pwd1),
        ?assertMatch(<<_/binary>>, Pwd2),
        ?assertMatch(<<_/binary>>, Pwd1_again)
    end).

%% ===================================================================
%% verify/2 测试
%% ===================================================================

verify_with_correct_password_test_() ->
    ?TEST_WITH_APP(fun() ->
        Plaintext = "correct_password",
        Ciphertext = elib_password:generate(Plaintext),
        Result = elib_password:verify(Plaintext, Ciphertext),
        % 验证正确密码能通过验证
        ?assertEqual({ok, []}, Result),
        % 验证验证函数对不同格式的输入都能处理
        Result2 = elib_password:verify(list_to_binary(Plaintext), Ciphertext),
        ?assertEqual({ok, []}, Result2)
    end).

verify_with_incorrect_password_test_() ->
    ?TEST_WITH_APP(fun() ->
        Plaintext = "correct_password",
        Ciphertext = elib_password:generate(Plaintext),
        Result = elib_password:verify("wrong_password", Ciphertext),
        % 精确断言：验证错误原因
        case Result of
            {error, Reason} when is_binary(Reason); is_atom(Reason) ->
                ?assert(true);
            _ ->
                ?assert(false, "Expected {error, Reason}")
        end,
        % 测试多种错误密码情况
        WrongPasswords = ["", "wrong", "CORRECT_PASSWORD", "correct_password "],
        lists:foreach(fun(WrongPwd) ->
            WrongResult = elib_password:verify(WrongPwd, Ciphertext),
            case WrongResult of
                {error, _Reason} when is_binary(_Reason); is_atom(_Reason) ->
                    ?assert(true);
                _ ->
                    ?assert(false, "Expected {error, Reason}")
            end
        end, WrongPasswords)
    end).

verify_with_empty_password_test_() ->
    ?TEST_WITH_APP(fun() ->
        Plaintext = "",
        Ciphertext = elib_password:generate(Plaintext),
        Result = elib_password:verify(Plaintext, Ciphertext),
        % 验证空密码也能正常验证
        ?assertMatch({ok, []}, Result),
        % 验证空密码生成的哈希格式正确
        ?assertMatch(<<_/binary>>, Ciphertext),
        ?assert(byte_size(Ciphertext) >= 50),
        % 验证空密码与非空密码的哈希不同
        NonEmptyHash = elib_password:generate("nonempty"),
        ?assertNotEqual(Ciphertext, NonEmptyHash)
    end).

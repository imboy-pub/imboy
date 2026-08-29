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
        % 契约（main C-1 后实现语义）：generate 入参即明文，内部 sha256 预哈希
        % 后 hmac 存储；verify 对称解出 → generate↔verify 明文往返闭环
        Plaintext = <<"correct_password">>,
        Sha256Ciphertext = elib_password:generate(Plaintext),
        ?assertEqual({ok, []}, elib_password:verify(Plaintext, Sha256Ciphertext)),
        % iodata 健壮性：列表输入不得崩溃（历史 badarg 回归钉子）
        ?assertEqual(
            {error, <<"errorPassword">>},
            elib_password:verify("wrong_list_input", Sha256Ciphertext)
        )
    end).

verify_with_incorrect_password_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 错误密码必须精确返回 errorPassword，不得误通过任一预哈希回退路径
        Plaintext = <<"correct_password">>,
        Ciphertext = elib_password:generate(crypto:hash(sha256, Plaintext)),
        WrongPasswords = [<<"">>, <<"wrong">>, <<"CORRECT_PASSWORD">>, <<"correct_password ">>],
        lists:foreach(
            fun(WrongPwd) ->
                ?assertEqual(
                    {error, <<"errorPassword">>}, elib_password:verify(WrongPwd, Ciphertext)
                )
            end,
            WrongPasswords
        )
    end).

verify_with_empty_password_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 空密码按明文往返契约：generate(<<>>) ↔ verify(<<>>)
        Ciphertext = elib_password:generate(<<>>),
        ?assertEqual({ok, []}, elib_password:verify(<<>>, Ciphertext)),
        % 验证空密码生成的哈希格式正确
        ?assertMatch(<<_/binary>>, Ciphertext),
        ?assert(byte_size(Ciphertext) >= 50),
        % 验证空密码与非空密码的哈希不同
        NonEmptyHash = elib_password:generate(crypto:hash(sha256, <<"nonempty">>)),
        ?assertNotEqual(Ciphertext, NonEmptyHash)
    end).

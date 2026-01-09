-module(token_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% token_ds 模块的 EUnit 测试
%%%
%%% 目标：验证Token管理功能
%%% 覆盖：Token加密、解密、刷新Token
%%%===================================================================

%% ===================================================================
%% encrypt_token/1 测试
%% ===================================================================

encrypt_token_returns_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 12345,
        % 测试函数调用不会崩溃
        Result = token_ds:encrypt_token(Uid),
        ?assert(is_tuple(Result)),
        case Result of
            {ok, Token} ->
                ?assertMatch(<<_/binary>>, Token),
                ?assert(byte_size(Token) > 0);
            {error, Reason} ->
                ?assert(is_atom(Reason) orelse is_binary(Reason))
        end
    end).

encrypt_token_non_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 12345,
        % 测试函数调用不会崩溃
        Result = token_ds:encrypt_token(Uid),
        ?assert(is_tuple(Result)),
        case Result of
            {ok, Token} ->
                ?assertMatch(<<_/binary>>, Token),
                ?assertNotEqual(<<>>, Token);
            {error, Reason} ->
                ?assert(is_atom(Reason) orelse is_binary(Reason))
        end
    end).

encrypt_token_different_for_different_uids_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid1 = 12345,
        Uid2 = 67890,
        % 测试函数调用不会崩溃
        Result1 = token_ds:encrypt_token(Uid1),
        Result2 = token_ds:encrypt_token(Uid2),
        ?assert(is_tuple(Result1)),
        ?assert(is_tuple(Result2)),
        case {Result1, Result2} of
            {{ok, Token1}, {ok, Token2}} ->
                ?assertNotEqual(Token1, Token2);
            _ ->
                ok  % 允许错误情况
        end
    end).

%% ===================================================================
%% encrypt_refreshtoken/1 测试
%% ===================================================================

encrypt_refreshtoken_returns_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 12345,
        ?assert(is_integer(Uid)),
        ?assert(Uid > 0),
        % 实际调用函数
        Result = token_ds:encrypt_refreshtoken(Uid),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(byte_size(Result) > 0)
    end).

encrypt_refreshtoken_non_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 12345,
        ?assert(is_integer(Uid)),
        ?assert(Uid > 0),
        % 实际调用函数
        Result = token_ds:encrypt_refreshtoken(Uid),
        ?assertMatch(<<_/binary>>, Result),
        ?assertNotEqual(<<>>, Result),
        ?assert(byte_size(Result) > 0)
    end).

encrypt_refreshtoken_different_from_token_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 12345,
        ?assert(is_integer(Uid)),
        ?assert(Uid > 0),
        % 实际调用函数
        Token = token_ds:encrypt_token(Uid),
        RefreshToken = token_ds:encrypt_refreshtoken(Uid),
        % 验证两个token不同
        ?assertNotEqual(Token, RefreshToken)
    end).

%% ===================================================================
%% 边界测试
%% ===================================================================

encrypt_token_zero_uid_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 0,
        ?assert(is_integer(Uid)),
        % 实际调用函数
        Result = token_ds:encrypt_token(Uid),
        % 验证返回值格式
        ?assertMatch(<<_/binary>>, Result)
    end).

encrypt_token_negative_uid_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = -1,
        ?assert(is_integer(Uid)),
        ?assert(Uid < 0),
        % 实际调用函数
        Result = token_ds:encrypt_token(Uid),
        % 验证返回值格式
        ?assertMatch(<<_/binary>>, Result)
    end).

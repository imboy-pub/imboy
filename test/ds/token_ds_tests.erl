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
        ?assertMatch(<<_/binary>>, Result),
        ?assert(byte_size(Result) > 0)
    end).

encrypt_token_non_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 12345,
        % 测试函数调用不会崩溃
        Result = token_ds:encrypt_token(Uid),
        ?assertMatch(<<_/binary>>, Result),
        ?assertNotEqual(<<>>, Result)
    end).

encrypt_token_different_for_different_uids_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid1 = 12345,
        Uid2 = 67890,
        % 测试函数调用不会崩溃
        Token1 = token_ds:encrypt_token(Uid1),
        Token2 = token_ds:encrypt_token(Uid2),
        ?assertMatch(<<_/binary>>, Token1),
        ?assertMatch(<<_/binary>>, Token2),
        ?assertNotEqual(Token1, Token2)
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

%% ===================================================================
%% E2EE-013：DID 绑定 token 往返
%% ===================================================================

%% 绑定设备 DID 的 token → decrypt 返回同一 DID + ep（6 元组）。
encrypt_token_binds_did_roundtrip_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 12345,
        Did = <<"device-abc-123">>,
        Token = token_ds:encrypt_token(Uid, Did),
        ?assertMatch(
            {ok, 12345, _Exp, <<"tk">>, <<"device-abc-123">>, _Ep},
            token_ds:decrypt_token(Token)
        )
    end).

%% legacy token（encrypt_token/1，无 DID）→ decrypt 返回 Did=<<>>（向后兼容）。
legacy_token_has_empty_did_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 12345,
        Token = token_ds:encrypt_token(Uid),
        ?assertMatch(
            {ok, 12345, _Exp, <<"tk">>, <<>>, undefined},
            token_ds:decrypt_token(Token)
        )
    end).

%% refresh token 也绑定 DID。
encrypt_refreshtoken_binds_did_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 12345,
        Did = <<"device-xyz">>,
        Rtk = token_ds:encrypt_refreshtoken(Uid, Did),
        ?assertMatch(
            {ok, 12345, _Exp, <<"rtk">>, <<"device-xyz">>, _Ep},
            token_ds:decrypt_token(Rtk)
        )
    end).

%% ===================================================================
%% 错误码映射（等价覆盖 websocket/api CT 的 expired/invalid 用例）
%% ===================================================================

%% 签名有效但 exp 已过 → 705（可刷新，客户端走 refresh 流程）。
%% exp = now - 400：签名有效且确定过期（imboy_jwt exp 严格判定）。
expired_token_returns_705_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 12345,
        Now = erlang:system_time(second),
        JwtKey = config_ds:env(jwt_key, <<>>),
        Token = imboy_jwt:sign(
            #{<<"sub">> => <<"tk">>, <<"exp">> => Now - 400, <<"uid">> => Uid}, JwtKey
        ),
        ?assertMatch(
            {error, 705, "Please refresh token", _},
            token_ds:decrypt_token(Token)
        )
    end).

%% 畸形 token → 706（无效）。
garbage_token_returns_706_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertMatch(
            {error, 706, _, _},
            token_ds:decrypt_token(<<"not.a.jwt">>)
        )
    end).

%% 错误密钥签发的 token → 706（验签失败）。
wrong_key_token_returns_706_test_() ->
    ?TEST_SIMPLE(fun() ->
        Token = imboy_jwt:sign(
            #{
                <<"sub">> => <<"tk">>,
                <<"exp">> => erlang:system_time(second) + 600,
                <<"uid">> => 12345
            },
            <<"a_totally_different_key_0123456789">>
        ),
        ?assertMatch(
            {error, 706, _, _},
            token_ds:decrypt_token(Token)
        )
    end).

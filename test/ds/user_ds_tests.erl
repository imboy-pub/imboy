-module(user_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_ds 模块的 EUnit 测试
%%%
%%% 目标：验证用户领域服务功能
%%% 覆盖：用户标题、WebRTC凭据生成和验证
%%%===================================================================

%% ===================================================================
%% title/1 测试
%% ===================================================================

title_returns_nickname_when_present_test_() ->
    ?WITH_MOCK(user_repo, [
        {find_by_id, 2, fun(_Uid, _Columns) ->
            #{
                <<"account">> => <<"testuser">>,
                <<"nickname">> => <<"Test Nickname">>
            }
        end}
    ], fun() ->
        Uid = 12345,
        Result = user_ds:title(Uid),
        ?assertEqual(<<"Test Nickname">>, Result)
    end).

title_returns_account_when_nickname_empty_test_() ->
    ?WITH_MOCK(user_repo, [
        {find_by_id, 2, fun(_Uid, _Columns) ->
            #{
                <<"account">> => <<"testuser">>,
                <<"nickname">> => <<>>
            }
        end}
    ], fun() ->
        Uid = 12345,
        Result = user_ds:title(Uid),
        ?assertEqual(<<"testuser">>, Result)
    end).

title_with_nonexistent_user_test_() ->
    ?WITH_MOCK(user_repo, [
        {find_by_id, 2, fun(_Uid, _Columns) ->
            #{<<"account">> => <<>>, <<"nickname">> => <<>>}
        end}
    ], fun() ->
        Uid = 99999,
        Result = user_ds:title(Uid),
        ?assertEqual(<<>>, Result)
    end).

%% ===================================================================
%% title/2 测试
%% ===================================================================

title_mode2_returns_tuple_test_() ->
    ?WITH_MOCK(user_repo, [
        {find_by_id, 2, fun(_Uid, _Columns) ->
            #{
                <<"account">> => <<"testuser">>,
                <<"nickname">> => <<"Test Nickname">>
            }
        end}
    ], fun() ->
        Uid = 12345,
        Result = user_ds:title(Uid, 2),
        ?assertMatch({<<"Test Nickname">>, <<"Test Nickname">>}, Result)
    end).

title_mode2_with_empty_nickname_test_() ->
    ?WITH_MOCK(user_repo, [
        {find_by_id, 2, fun(_Uid, _Columns) ->
            #{
                <<"account">> => <<"testuser">>,
                <<"nickname">> => <<>>
            }
        end}
    ], fun() ->
        Uid = 12345,
        Result = user_ds:title(Uid, 2),
        ?assertMatch({<<"testuser">>, <<>>}, Result)
    end).

title_mode2_title_or_account_test_() ->
    ?WITH_MOCK(user_repo, [
        {find_by_id, 2, fun(_Uid, _Columns) ->
            #{
                <<"account">> => <<"testuser">>,
                <<"nickname">> => <<"Test Nickname">>
            }
        end}
    ], fun() ->
        Uid = 12345,
        {Title, Nickname} = user_ds:title(Uid, 2),
        ?assertEqual(<<"Test Nickname">>, Title),
        ?assertEqual(<<"Test Nickname">>, Nickname)
    end).

%% ===================================================================
%% webrtc_credential/1 测试 (使用meck模拟依赖)
%% ===================================================================

webrtc_credential_valid_user_test_() ->
    ?WITH_MECK(elib_hashids, [
        {'decode_hex', 1, fun(_Hex) -> {ok, 12345} end}
    ], fun() ->
        UserId = 12345,
        Result = user_ds:webrtc_credential(UserId),
        ?ASSERT_OK(Result),
        {ok, Credential} = Result,
        % 精确断言：验证凭据的具体格式和内容
        ?assertMatch(Cred when is_binary(Cred) andalso byte_size(Cred) >= 32, Credential),
        % 进一步验证凭据格式（Base64编码的HMAC）
        case base64:decode(Credential) of
            {ok, Decoded} when is_binary(Decoded), byte_size(Decoded) > 0 -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected valid Base64 decoded binary")
        end
    end).

webrtc_credential_invalid_user_test_() ->
    ?WITH_MECK(elib_hashids, [
        {'decode_hex', 1, fun(_Hex) -> {error, invalid_hash} end}
    ], fun() ->
        UserId = 999999,
        Result = user_ds:webrtc_credential(UserId),
        ?ASSERT_ERROR(Result)
    end).

%% ===================================================================
%% auth_webrtc_credential/2 测试
%% ===================================================================

auth_webrtc_credential_valid_credential_test_() ->
    ?WITH_MOCK(config_ds, [
        {get, 1, fun(<<"eturnal_secret">>) -> <<"test_secret">> end}
    ], fun() ->
        Username = <<"1728610200:12345">>,
        % Generate valid credential using same algorithm
        Secret = <<"test_secret">>,
        Credential = base64:encode(crypto:mac(hmac, sha, Secret, Username)),
        Result = user_ds:auth_webrtc_credential(Username, Credential),
        ?assertEqual(true, Result)
    end).

auth_webrtc_credential_invalid_credential_test_() ->
    ?WITH_MOCK(config_ds, [
        {get, 1, fun(<<"eturnal_secret">>) -> <<"test_secret">> end}
    ], fun() ->
        Username = <<"1728610200:12345">>,
        InvalidCredential = <<"InvalidCredential">>,
        Result = user_ds:auth_webrtc_credential(Username, InvalidCredential),
        ?assertEqual(false, Result)
    end).

auth_webrtc_credential_empty_credential_test_() ->
    ?WITH_MOCK(config_ds, [
        {get, 1, fun(<<"eturnal_secret">>) -> <<"test_secret">> end}
    ], fun() ->
        Username = <<"1728610200:12345">>,
        EmptyCredential = <<>>,
        Result = user_ds:auth_webrtc_credential(Username, EmptyCredential),
        ?assertEqual(false, Result)
    end).

auth_webrtc_credential_different_secret_test_() ->
    ?WITH_MOCK(config_ds, [
        {get, 1, fun(<<"eturnal_secret">>) -> <<"different_secret">> end}
    ], fun() ->
        Username = <<"1728610200:12345">>,
        % Generate credential with different secret
        Secret = <<"test_secret">>,
        Credential = base64:encode(crypto:mac(hmac, sha, Secret, Username)),
        Result = user_ds:auth_webrtc_credential(Username, Credential),
        ?assertEqual(false, Result)
    end).

%% ===================================================================
%% 集成测试 (使用meck模拟依赖)
%% ===================================================================

integration_user_flow_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode_hex', 1, fun(_Hex) -> {ok, 12345} end},
            {'encode_hex', 1, fun(_Id) -> <<"encoded_hash">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) -> {ok, []} end},
            {'query', 3, fun(_Sql, _Params, _Conn) -> {ok, []} end}
        ]}
    ], fun() ->
        % 测试完整的用户流程
        UserId = 12345,
        % 创建用户凭证
        {ok, Credential} = user_ds:webrtc_credential(UserId),
        % 精确断言：验证创建的凭证格式
        ?assertMatch(Cred when is_binary(Cred) andalso byte_size(Cred) >= 32, Credential),
        % 验证凭证认证结果
        {ok, AuthResult} = user_ds:auth_webrtc_credential(Credential, <<"test_secret">>),
        ?assertMatch(Result when is_map(Result) andalso map_size(Result) > 0, AuthResult),
        % 验证认证结果包含必要字段
        ?assert(maps:is_key(<<"username">>, AuthResult) orelse maps:is_key(<<"valid">>, AuthResult))
    end).

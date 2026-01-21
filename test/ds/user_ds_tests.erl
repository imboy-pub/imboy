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
        ?assert(is_binary(Credential)),
        ?assert(byte_size(Credential) >= 32)
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
        UserId = 12345,
        {ok, Credential} = user_ds:webrtc_credential(UserId),
        ?assert(is_binary(Credential)),
        ?assert(byte_size(Credential) >= 32),
        {ok, AuthResult} = user_ds:auth_webrtc_credential(Credential, <<"test_secret">>),
        ?assert(is_map(AuthResult) orelse is_boolean(AuthResult))
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

title_with_non_existent_user_test_() ->
    ?WITH_MOCK(user_repo, [
        {find_by_id, 2, fun(_Uid, _Columns) ->
            #{<<"account">> => <<>>, <<"nickname">> => <<>>}
        end}
    ], fun() ->
        Uid = 999999,
        Result = user_ds:title(Uid),
        ?assertEqual(<<>>, Result)
    end).

title_with_zero_uid_test_() ->
    ?WITH_MOCK(user_repo, [
        {find_by_id, 2, fun(_Uid, _Columns) ->
            #{<<"account">> => <<"user0">>, <<"nickname">> => <<>>}
        end}
    ], fun() ->
        Uid = 0,
        Result = user_ds:title(Uid),
        ?assert(is_binary(Result))
    end).

title_with_negative_uid_test_() ->
    ?WITH_MOCK(user_repo, [
        {find_by_id, 2, fun(_Uid, _Columns) ->
            #{<<"account">> => <<>>, <<"nickname">> => <<>>}
        end}
    ], fun() ->
        Uid = -1,
        Result = user_ds:title(Uid),
        ?assert(is_binary(Result))
    end).

webrtc_credential_with_zero_uid_test_() ->
    ?WITH_MECK(elib_hashids, [
        {'decode_hex', 1, fun(_Hex) -> {ok, 0} end}
    ], fun() ->
        UserId = 0,
        Result = user_ds:webrtc_credential(UserId),
        case Result of
            {ok, _} -> ok;
            {error, _} -> ok
        end
    end).

auth_webrtc_credential_with_empty_username_test_() ->
    ?WITH_MOCK(config_ds, [
        {get, 1, fun(<<"eturnal_secret">>) -> <<"test_secret">> end}
    ], fun() ->
        Username = <<>>,
        Credential = base64:encode(crypto:mac(hmac, sha, <<"test_secret">>, Username)),
        Result = user_ds:auth_webrtc_credential(Username, Credential),
        ?assert(is_boolean(Result))
    end).

auth_webrtc_credential_with_invalid_base64_test_() ->
    ?WITH_MOCK(config_ds, [
        {get, 1, fun(<<"eturnal_secret">>) -> <<"test_secret">> end}
    ], fun() ->
        Username = <<"1728610200:12345">>,
        InvalidCredential = <<"NotValidBase64!!!">>,
        Result = user_ds:auth_webrtc_credential(Username, InvalidCredential),
        ?assertEqual(false, Result)
    end).

%% ===================================================================
%% UTF-8 编码测试
%% ===================================================================

title_with_chinese_nickname_test_() ->
    ?WITH_MOCK(user_repo, [
        {find_by_id, 2, fun(_Uid, _Columns) ->
            #{
                <<"account">> => <<"user123">>,
                <<"nickname">> => <<"中文昵称"/utf8>>
            }
        end}
    ], fun() ->
        Uid = 12345,
        Result = user_ds:title(Uid),
        ?assertEqual(<<"中文昵称"/utf8>>, Result)
    end).

title_with_emoji_nickname_test_() ->
    ?WITH_MOCK(user_repo, [
        {find_by_id, 2, fun(_Uid, _Columns) ->
            #{
                <<"account">> => <<"user123">>,
                <<"nickname">> => <<"昵称 😊"/utf8>>
            }
        end}
    ], fun() ->
        Uid = 12345,
        Result = user_ds:title(Uid),
        ?assertEqual(<<"昵称 😊"/utf8>>, Result)
    end).

title_with_chinese_account_test_() ->
    ?WITH_MOCK(user_repo, [
        {find_by_id, 2, fun(_Uid, _Columns) ->
            #{
                <<"account">> => <<"中文账号"/utf8>>,
                <<"nickname">> => <<>>
            }
        end}
    ], fun() ->
        Uid = 12345,
        Result = user_ds:title(Uid),
        ?assertEqual(<<"中文账号"/utf8>>, Result)
    end).

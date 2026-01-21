-module(auth_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc
%%% auth_ds 模块的 EUnit 测试
%%%
%%% 目标：验证认证服务功能
%%% 覆盖：Token获取和管理
%%%===================================================================

%% ===================================================================
%% get_token/3 测试
%% ===================================================================

get_token_with_assets_resource_test_() ->
    ?WITH_MECK(config_ds, [
        {'get', 1, fun(_Key) -> <<"test_upload_key">> end}
    ], fun() ->
        ?WITH_MECK(elib_hasher, [
            {'md5', 1, fun(_Input) -> <<"1234567890abcdef1234567890abcdef">> end}
        ], fun() ->
            ResourceType = assets,
            Scene = <<"test_scene">>,
            ResourceId = 12345,
            
            Result = auth_ds:get_token(ResourceType, Scene, ResourceId),
            % 验证返回的是16字节的二进制token
            ?assertEqual(<<"567890abcdef1234">>, Result),
            ?assertEqual(16, byte_size(Result))
        end)
    end).

get_token_with_string_resource_id_test_() ->
    ?WITH_MECK(config_ds, [
        {'get', 1, fun(_Key) -> <<"test_upload_key">> end}
    ], fun() ->
        ?WITH_MECK(elib_hasher, [
            {'md5', 1, fun(_Input) -> <<"1234567890abcdef1234567890abcdef">> end}
        ], fun() ->
            ResourceType = assets,
            Scene = <<"test_scene">>,
            ResourceId = "/img/2023/12/test.png",
            
            Result = auth_ds:get_token(ResourceType, Scene, ResourceId),
            ?assertEqual(<<"567890abcdef1234">>, Result),
            ?assertMatch(<<_/binary>>, Result)
        end)
    end).

get_token_different_upload_keys_test_() ->
    ?WITH_MECK(config_ds, [
        {'get', 1, fun(Key) -> 
            case Key of
                <<"upload_key">> -> <<"key1">>;
                _ -> <<"default_key">>
            end
        end}
    ], fun() ->
        ?WITH_MECK(elib_hasher, [
            {'md5', 1, fun(Input) ->
                case Input of
                    <<"key1test123">> -> <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>;
                    _ -> <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>
                end
            end}
        ], fun() ->
            ResourceType = assets,
            Scene = <<"test_scene">>,
            ResourceId = <<"test123">>,
            
            Result1 = auth_ds:get_token(ResourceType, Scene, ResourceId),
            % 验证不同的上传密钥生成不同的token
            ?assertEqual(<<"aaaaaaaaaaaaaaaa">>, Result1),
            ?assertEqual(16, byte_size(Result1))
        end)
    end).

get_token_with_empty_upload_key_test_() ->
    ?WITH_MECK(config_ds, [
        {'get', 1, fun(_Key) -> <<>> end}
    ], fun() ->
        ?WITH_MECK(elib_hasher, [
            {'md5', 1, fun(_Input) -> <<"1234567890abcdef1234567890abcdef">> end}
        ], fun() ->
            ResourceType = assets,
            Scene = <<"test_scene">>,
            ResourceId = <<"test123">>,
            
            Result = auth_ds:get_token(ResourceType, Scene, ResourceId),
            % 即使上传密钥为空，也应该能生成token
            ?assertEqual(<<"567890abcdef1234">>, Result),
            ?assertMatch(<<_/binary>>, Result)
        end)
    end).

get_token_consistency_test_() ->
    ?WITH_MECK(config_ds, [
        {'get', 1, fun(_Key) -> <<"consistent_key">> end}
    ], fun() ->
        ?WITH_MECK(elib_hasher, [
            {'md5', 1, fun(_Input) -> <<"1234567890abcdef1234567890abcdef">> end}
        ], fun() ->
            ResourceType = assets,
            Scene = <<"test_scene">>,
            ResourceId = <<"consistent_resource">>,

            % 验证相同参数生成相同的token
            Result1 = auth_ds:get_token(ResourceType, Scene, ResourceId),
            Result2 = auth_ds:get_token(ResourceType, Scene, ResourceId),
            ?assertEqual(Result1, Result2),
            ?assertEqual(<<"567890abcdef1234">>, Result1)
        end)
    end).

%% ===================================================================
%% verify_sign/2 测试
%% ===================================================================

verify_sign_with_valid_sign_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'header', 2, fun(<<"vsn">>, _Req) -> <<"1.0.0">>;
                               (<<"pkg">>, _Req) -> <<"pub.imboy.apk">>;
                               (<<"did">>, _Req) -> <<"device123">>;
                               (<<"cos">>, _Req) -> <<"android">>;
                               (<<"sign">>, _Req) -> <<"valid_sign">>;
                               (<<"method">>, _Req) -> <<"sha256">>;
                               (<<"sk">>, _Req) -> <<"1.0.0">>
                            end}
        ]},
        {app_version_ds, [
            {'sign_key', 3, fun(_ClientOS, _Vsn, _Pkg) -> <<"test_key">> end}
        ]},
        {elib_hasher, [
            {'hmac_sha256', 2, fun(_PlainText, _Key) -> <<"valid_sign">> end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, _Code) -> error_req end}
        ]}
    ], fun() ->
        Req = #{},
        Env = #{},
        Result = auth_ds:verify_sign(Req, Env),
        ?assertMatch({ok, _, _}, Result)
    end).

verify_sign_with_invalid_sign_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'header', 2, fun(<<"vsn">>, _Req) -> <<"1.0.0">>;
                               (<<"pkg">>, _Req) -> <<"pub.imboy.apk">>;
                               (<<"did">>, _Req) -> <<"device123">>;
                               (<<"cos">>, _Req) -> <<"android">>;
                               (<<"sign">>, _Req) -> <<"invalid_sign">>;
                               (<<"method">>, _Req) -> <<"sha256">>;
                               (<<"sk">>, _Req) -> <<"1.0.0">>
                            end}
        ]},
        {app_version_ds, [
            {'sign_key', 3, fun(_ClientOS, _Vsn, _Pkg) -> <<"test_key">> end}
        ]},
        {elib_hasher, [
            {'hmac_sha256', 2, fun(_PlainText, _Key) -> <<"valid_sign">> end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, _Code) -> error_req end}
        ]}
    ], fun() ->
        Req = #{},
        Env = #{},
        Result = auth_ds:verify_sign(Req, Env),
        ?assertMatch({stop, _}, Result)
    end).

verify_sign_with_missing_sign_header_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'header', 2, fun(<<"vsn">>, _Req) -> <<"1.0.0">>;
                               (<<"pkg">>, _Req) -> <<"pub.imboy.apk">>;
                               (<<"did">>, _Req) -> <<"device123">>;
                               (<<"cos">>, _Req) -> <<"android">>;
                               (<<"sign">>, _Req) -> undefined;
                               (<<"method">>, _Req) -> undefined;
                               (<<"sk">>, _Req) -> <<"1.0.0">>
                            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, _Code) -> error_req end}
        ]}
    ], fun() ->
        Req = #{},
        Env = #{},
        Result = auth_ds:verify_sign(Req, Env),
        ?assertMatch({stop, _}, Result)
    end).

%% ===================================================================
%% do_verify_sign/4 测试
%% ===================================================================

do_verify_sign_with_sha256_test_() ->
    ?WITH_MECK(elib_hasher, [
        {'hmac_sha256', 2, fun(_PlainText, _Key) -> <<"correct_sha256">> end}
    ], fun() ->
        Sign = <<"correct_sha256">>,
        PlainText = <<"plaintext">>,
        Key = <<"key">>,
        Method = <<"sha256">>,
        Result = auth_ds:do_verify_sign(Sign, PlainText, Key, Method),
        ?assertEqual(true, Result)
    end).

do_verify_sign_with_sha512_test_() ->
    ?WITH_MECK(elib_hasher, [
        {'hmac_sha512', 2, fun(_PlainText, _Key) -> <<"correct_sha512">> end}
    ], fun() ->
        Sign = <<"correct_sha512">>,
        PlainText = <<"plaintext">>,
        Key = <<"key">>,
        Method = <<"sha512">>,
        Result = auth_ds:do_verify_sign(Sign, PlainText, Key, Method),
        ?assertEqual(true, Result)
    end).

do_verify_sign_with_invalid_sign_test_() ->
    ?WITH_MECK(elib_hasher, [
        {'hmac_sha256', 2, fun(_PlainText, _Key) -> <<"correct_sha256">> end}
    ], fun() ->
        Sign = <<"wrong_sha256">>,
        PlainText = <<"plaintext">>,
        Key = <<"key">>,
        Method = <<"sha256">>,
        Result = auth_ds:do_verify_sign(Sign, PlainText, Key, Method),
        ?assertEqual(false, Result)
    end).

do_verify_sign_with_undefined_sign_test_() ->
    Result = auth_ds:do_verify_sign(undefined, <<"plaintext">>, <<"key">>, <<"sha256">>),
    ?assertEqual(false, Result).

do_verify_sign_with_undefined_key_test_() ->
    Result = auth_ds:do_verify_sign(<<"sign">>, <<"plaintext">>, undefined, <<"sha256">>),
    ?assertEqual(false, Result).

do_verify_sign_with_unsupported_method_test_() ->
    Result = auth_ds:do_verify_sign(<<"sign">>, <<"plaintext">>, <<"key">>, <<"md5">>),
    ?assertEqual(false, Result).

%% ===================================================================
%% verify_token/1 测试
%% ===================================================================

verify_token_with_valid_token_test_() ->
    ?WITH_MECKS([
        {token_ds, [
            {'decrypt_token', 1, fun(_Token) -> {ok, 123, <<"2024-01-01">>, <<"tk">>} end}
        ]}
    ], fun() ->
        Authorization = <<"Bearer valid_token">>,
        Result = auth_ds:verify_token(Authorization),
        ?assertMatch({ok, 123}, Result)
    end).

verify_token_with_refresh_token_test_() ->
    ?WITH_MECKS([
        {token_ds, [
            {'decrypt_token', 1, fun(_Token) -> {ok, 123, <<"2024-01-01">>, <<"rtk">>} end}
        ]}
    ], fun() ->
        Authorization = <<"Bearer refresh_token">>,
        Result = auth_ds:verify_token(Authorization),
        ?assertMatch({error, ?ERR_TOKEN_REFRESH_NOT_ALLOWED, _}, Result)
    end).

verify_token_with_invalid_token_test_() ->
    ?WITH_MECKS([
        {token_ds, [
            {'decrypt_token', 1, fun(_Token) -> {error, ?ERR_TOKEN_INVALID, <<"Token invalid"/utf8>>, #{}} end}
        ]}
    ], fun() ->
        Authorization = <<"Bearer invalid_token">>,
        Result = auth_ds:verify_token(Authorization),
        ?assertMatch({error, ?ERR_TOKEN_INVALID, _}, Result)
    end).

%% ===================================================================
%% parse_authorization_header/1 测试
%% ===================================================================

parse_authorization_header_with_bearer_prefix_test_() ->
    Authorization = <<"Bearer my_token">>,
    Result = auth_ds:parse_authorization_header(Authorization),
    ?assertEqual(<<"my_token">>, Result).

parse_authorization_header_without_bearer_prefix_test_() ->
    Authorization = <<"my_token">>,
    Result = auth_ds:parse_authorization_header(Authorization),
    ?assertEqual(<<"my_token">>, Result).

parse_authorization_header_with_empty_value_test_() ->
    Authorization = <<>>,
    Result = auth_ds:parse_authorization_header(Authorization),
    ?assertEqual(<<>>, Result).

parse_authorization_header_with_non_binary_test_() ->
    Result = auth_ds:parse_authorization_header(undefined),
    ?assertEqual(<<>>, Result).

parse_authorization_header_with_lowercase_bearer_test_() ->
    % 注意：当前实现区分大小写，"bearer" 不会被识别
    Authorization = <<"bearer my_token">>,
    Result = auth_ds:parse_authorization_header(Authorization),
    ?assertEqual(<<"bearer my_token">>, Result).

%% ===================================================================
%% remove_last_forward_slash/1 测试
%% ===================================================================

remove_last_forward_slash_with_trailing_slash_test_() ->
    Path = <<"/api/v1/user/">>,
    Result = auth_ds:remove_last_forward_slash(Path),
    ?assertEqual(<<"/api/v1/user">>, Result).

remove_last_forward_slash_without_trailing_slash_test_() ->
    Path = <<"/api/v1/user">>,
    Result = auth_ds:remove_last_forward_slash(Path),
    ?assertEqual(<<"/api/v1/user">>, Result).

remove_last_forward_slash_with_root_path_test_() ->
    Path = <<"/">>,
    Result = auth_ds:remove_last_forward_slash(Path),
    ?assertEqual(<<"/">>, Result).

remove_last_forward_slash_with_empty_path_test_() ->
    Path = <<>>,
    Result = auth_ds:remove_last_forward_slash(Path),
    ?assertEqual(<<"/">>, Result).

remove_last_forward_slash_with_multiple_trailing_slashes_test_() ->
    Path = <<"/api/v1/user//">>,
    Result = auth_ds:remove_last_forward_slash(Path),
    ?assertEqual(<<"/api/v1/user/">>, Result).

%% ===================================================================
%% strip_version_prefix/2 测试
%% ===================================================================

strip_version_prefix_with_v1_prefix_test_() ->
    Path = <<"/v1/user/info">>,
    Prefix = <<"/v1">>,
    Result = auth_ds:strip_version_prefix(Path, Prefix),
    ?assertEqual(<<"/user/info">>, Result).

strip_version_prefix_without_version_prefix_test_() ->
    Path = <<"/user/info">>,
    Prefix = <<"/v1">>,
    Result = auth_ds:strip_version_prefix(Path, Prefix),
    ?assertEqual(<<"/user/info">>, Result).

strip_version_prefix_with_different_prefix_test_() ->
    Path = <<"/v2/user/info">>,
    Prefix = <<"/v1">>,
    Result = auth_ds:strip_version_prefix(Path, Prefix),
    ?assertEqual(<<"/v2/user/info">>, Result).

strip_version_prefix_with_empty_prefix_test_() ->
    Path = <<"/user/info">>,
    Prefix = <<>>,
    Result = auth_ds:strip_version_prefix(Path, Prefix),
    ?assertEqual(<<"/user/info">>, Result).

strip_version_prefix_with_exact_match_test_() ->
    Path = <<"/v1">>,
    Prefix = <<"/v1">>,
    Result = auth_ds:strip_version_prefix(Path, Prefix),
    ?assertEqual(<<>>, Result).

%% ===================================================================
%% condition/5 测试
%% ===================================================================

condition_in_option_list_without_auth_test_() ->
    % 在 option 列表中且没有 Authorization
    Req = #{},
    Env = #{},
    Result = auth_ds:condition(true, false, undefined, Req, Env),
    ?assertMatch({ok, _, _}, Result).

condition_in_option_list_with_empty_auth_test_() ->
    % 在 option 列表中且 Authorization 为空
    Req = #{},
    Env = #{},
    Result = auth_ds:condition(true, false, <<>>, Req, Env),
    ?assertMatch({ok, _, _}, Result).

condition_in_option_list_with_auth_test_() ->
    ?WITH_MECKS([
        {token_ds, [
            {'decrypt_token', 1, fun(_Token) -> {ok, 123, <<"2024-01-01">>, <<"tk">>} end}
        ]}
    ], fun() ->
        Req = #{},
        Env = #{handler_opts => #{}},
        Authorization = <<"Bearer valid_token">>,
        Result = auth_ds:condition(true, false, Authorization, Req, Env),
        ?assertMatch({ok, _, #{handler_opts := #{current_uid := 123}}}, Result)
    end).

condition_in_open_list_test_() ->
    % 在 open 列表中，不需要验证 token
    Req = #{},
    Env = #{},
    Result = auth_ds:condition(false, true, <<"Bearer any_token">>, Req, Env),
    ?assertMatch({ok, _, _}, Result).

condition_requires_auth_with_valid_token_test_() ->
    ?WITH_MECKS([
        {token_ds, [
            {'decrypt_token', 1, fun(_Token) -> {ok, 123, <<"2024-01-01">>, <<"tk">>} end}
        ]}
    ], fun() ->
        Req = #{},
        Env = #{handler_opts => #{}},
        Authorization = <<"Bearer valid_token">>,
        Result = auth_ds:condition(false, false, Authorization, Req, Env),
        ?assertMatch({ok, _, #{handler_opts := #{current_uid := 123}}}, Result)
    end).

condition_requires_auth_with_invalid_token_test_() ->
    ?WITH_MECKS([
        {token_ds, [
            {'decrypt_token', 1, fun(_Token) -> {error, ?ERR_TOKEN_INVALID, <<"Token invalid"/utf8>>, #{}} end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, _Code) -> error_req end}
        ]}
    ], fun() ->
        Req = #{},
        Env = #{handler_opts => #{}},
        Authorization = <<"Bearer invalid_token">>,
        Result = auth_ds:condition(false, false, Authorization, Req, Env),
        ?assertMatch({stop, _}, Result)
    end).

%% ===================================================================
%% do_authorization/3 测试
%% ===================================================================

do_authorization_with_valid_token_test_() ->
    ?WITH_MECKS([
        {token_ds, [
            {'decrypt_token', 1, fun(_Token) -> {ok, 123, <<"2024-01-01">>, <<"tk">>} end}
        ]}
    ], fun() ->
        Req = #{},
        Env = #{handler_opts => #{existing_key => existing_value}},
        Authorization = <<"Bearer valid_token">>,
        Result = auth_ds:do_authorization(Authorization, Req, Env),
        ?assertMatch({ok, _, #{handler_opts := #{current_uid := 123, existing_key := existing_value}}}, Result)
    end).

do_authorization_with_undefined_auth_test_() ->
    Req = #{},
    Env = #{},
    Result = auth_ds:do_authorization(undefined, Req, Env),
    ?assertMatch({stop, _}, Result).

do_authorization_with_invalid_token_test_() ->
    ?WITH_MECKS([
        {token_ds, [
            {'decrypt_token', 1, fun(_Token) -> {error, ?ERR_TOKEN_INVALID, <<"Token invalid"/utf8>>, #{}} end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, _Code) -> error_req end}
        ]}
    ], fun() ->
        Req = #{},
        Env = #{handler_opts => #{}},
        Authorization = <<"Bearer invalid_token">>,
        Result = auth_ds:do_authorization(Authorization, Req, Env),
        ?assertMatch({stop, _}, Result)
    end).

%% ===================================================================
%% current_uid/1 测试
%% ===================================================================

current_uid_with_existing_uid_test_() ->
    State = #{current_uid => 123, other_key => other_value},
    Result = auth_ds:current_uid(State),
    ?assertEqual(123, Result).

current_uid_without_uid_test_() ->
    State = #{other_key => other_value},
    Result = auth_ds:current_uid(State),
    ?assertEqual(0, Result).

current_uid_with_empty_map_test_() ->
    State = #{},
    Result = auth_ds:current_uid(State),
    ?assertEqual(0, Result).

current_uid_with_zero_uid_test_() ->
    State = #{current_uid => 0},
    Result = auth_ds:current_uid(State),
    ?assertEqual(0, Result).

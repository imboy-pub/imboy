-module(passport_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% passport_logic 模块的 EUnit 测试
%%%
%%% 目标：验证登录注册业务逻辑功能
%%% 覆盖：注册、登录、验证码发送、用户验证、快速登录
%%%===================================================================

%% ===================================================================
%% do_signup/5 测试
%% ===================================================================

do_signup_with_valid_data_succeeds_test_() ->
    ?WITH_MECKS([
        {user_repo, [
            {'find_by_mobile', 2, fun(_Mobile, _Fields) -> {error, not_found} end},
            {'insert', 1, fun(_Data) -> {ok, 1} end}
        ]},
        {user_setting_ds, [
            {'init', 1, fun(_Uid) -> ok end}
        ]},
        {user_device_ds, [
            {'is_activated', 2, fun(_Uid, _DID) -> false end}
        ]},
        {user_log_ds, [
            {'add_password_change_log', 4, fun(_Conn, _Uid, _Req0, _Type) -> {ok, ok} end}
        ]}
    ], fun() ->
        Mobile = <<"13800138000">>,
        Password = <<"Test@123456">>,
        Email = <<"test@example.com">>,
        Req0 = #{},
        Ip = <<"127.0.0.1">>,

        Result = passport_logic:do_signup(Mobile, Password, Email, Req0, Ip),
        ?assertMatch({ok, _Map}, Result)
    end).

do_signup_with_existing_mobile_fails_test_() ->
    ?WITH_MECK(user_repo, [
        {'find_by_mobile', 2, fun(_Mobile, _Fields) ->
            {ok, #{<<"id">> => 1}}
        end}
    ], fun() ->
        Mobile = <<"13800138000">>,
        Password = <<"Test@123456">>,
        Email = <<"test@example.com">>,
        Req0 = #{},
        Ip = <<"127.0.0.1">>,

        Result = passport_logic:do_signup(Mobile, Password, Email, Req0, Ip),
        ?assertMatch({error, _, _}, Result)
    end).

do_signup_with_invalid_password_fails_test_() ->
    ?WITH_MECK(user_repo, [
        {'find_by_mobile', 2, fun(_Mobile, _Fields) -> {error, not_found} end}
        ], fun() ->
        Mobile = <<"13800138000">>,
        Password = <<"123">>,  % 太短
        Email = <<"test@example.com">>,
        Req0 = #{},
        Ip = <<"127.0.0.1">>,

        Result = passport_logic:do_signup(Mobile, Password, Email, Req0, Ip),
        ?assertMatch({error, _, _}, Result)
    end).

%% ===================================================================
%% do_login/3 测试
%% ===================================================================

do_login_with_valid_credentials_succeeds_test_() ->
    ?WITH_MECKS([
        {user_repo, [
            {'find_by_mobile', 2, fun(_Mobile, _Fields) ->
                {ok, #{<<"id">> => 123, <<"password">> => <<"$2a$12$hash">>}}
            end}
        ]},
        {user_device_ds, [
            {'is_activated', 2, fun(_Uid, _DID) -> false end}
        ]},
        {user_setting_ds, [
            {'find_by_uid', 1, fun(_Uid) -> {ok, #{}} end}
        ]},
        {token_ds, [
            {'encrypt_token', 1, fun(_Uid) -> <<"encrypted_token">> end}
        ]},
        {user_device_ds, [
            {'save', 5, fun(_Now, _Uid, _DID, _PostMap) -> ok end}
        ]},
        {user_log_ds, [
            {'add_password_change_log', 4, fun(_Conn, _Uid, _Req0, _Type) -> {ok, ok} end}
        ]}
    ], fun() ->
        Mobile = <<"13800138000">>,
        Password = <<"Test@123456">>,
        PostMap = #{<<"did">> => <<"device_1">>},

        Result = passport_logic:do_login(Mobile, Password, PostMap),
        ?assertMatch({ok, #{<<"uid">> := _, <<"token">> := _}}, Result)
    end).

do_login_with_invalid_password_fails_test_() ->
    ?WITH_MECK(user_repo, [
        {'find_by_mobile', 2, fun(_Mobile, _Fields) ->
            {ok, #{<<"id">> => 123, <<"password">> => <<"$2a$12$hash">>}}
            end}
        ], fun() ->
        Mobile = <<"13800138000">>,
        Password = <<"WrongPassword">>,
        PostMap = #{},

        Result = passport_logic:do_login(Mobile, Password, PostMap),
        ?assertMatch({error, _, _}, Result)
    end).

do_login_with_nonexistent_user_fails_test_() ->
    ?WITH_MECK(user_repo, [
        {'find_by_mobile', 2, fun(_Mobile, _Fields) -> {error, not_found} end}
        ], fun() ->
        Mobile = <<"13900139999">>,
        Password = <<"Test@123456">>,
        PostMap = #{},

        Result = passport_logic:do_login(Mobile, Password, PostMap),
        ?assertMatch({error, _, _}, Result)
    end).

%% ===================================================================
%% verify_user/2 测试
%% ===================================================================

verify_user_with_valid_credentials_succeeds_test_() ->
    ?WITH_MECK(user_repo, [
        {'find_by_mobile', 2, fun(_Mobile, _Fields) ->
            {ok, #{<<"id">> => 123, <<"password">> => <<"$2a$12$hash">>}}
        end}
    ], fun() ->
        Mobile = <<"13800138000">>,
        Password = <<"Test@123456">>,

        Result = passport_logic:verify_user(Mobile, Password),
        ?assertMatch({ok, 123}, Result)
    end).

verify_user_with_invalid_password_fails_test_() ->
    ?WITH_MECK(user_repo, [
        {'find_by_mobile', 2, fun(_Mobile, _Fields) ->
            {ok, #{<<"id">> => 123, <<"password">> => <<"$2a$12$hash">>}}
        end}
        ], fun() ->
        Mobile = <<"13800138000">>,
        Password = <<"WrongPassword">>,

        Result = passport_logic:verify_user(Mobile, Password),
        ?assertMatch({error, invalid_password, _}, Result)
    end).

verify_user_with_nonexistent_user_fails_test_() ->
    ?WITH_MECK(user_repo, [
        {'find_by_mobile', 2, fun(_Mobile, _Fields) -> {error, not_found} end}
        ], fun() ->
        Mobile = <<"13900139999">>,
        Password = <<"Test@123456">>,

        Result = passport_logic:verify_user(Mobile, Password),
        ?assertMatch({error, user_not_found, _}, Result)
    end).

%% ===================================================================
%% quick_login/4 测试
%% ===================================================================

quick_login_with_valid_token_succeeds_test_() ->
    ?WITH_MECKS([
        {token_ds, [
            {'decrypt_token', 1, fun(_Token) -> {ok, #{<<"uid">> => 123}} end}
        ]},
        {user_repo, [
            {'find_by_id', 2, fun(_Uid, _Fields) ->
                {ok, #{<<"id">> => 123, <<"status">> => 1}}
            end}
        ]},
        {user_setting_ds, [
            {'find_by_uid', 1, fun(_Uid) -> {ok, #{}} end}
        ]},
        {token_ds, [
            {'encrypt_token', 1, fun(_Uid) -> <<"new_token">> end}
        ]}
    ], fun() ->
        Token = <<"valid_token">>,
        PostMap = #{<<"did">> => <<"device_1">>},

        Result = passport_logic:quick_login(Token, PostMap),
        ?assertMatch({ok, #{<<"uid">> := _, <<"token">> := _}}, Result)
    end).

quick_login_with_invalid_token_fails_test_() ->
    ?WITH_MECK(token_ds, [
        {'decrypt_token', 1, fun(_Token) -> {error, invalid_token} end}
        ], fun() ->
        Token = <<"invalid_token">>,
        PostMap = #{},

        Result = passport_logic:quick_login(Token, PostMap),
        ?assertMatch({error, invalid_token, _}, Result)
    end).

quick_login_with_inactive_user_fails_test_() ->
    ?WITH_MECKS([
        {token_ds, [
            {'decrypt_token', 1, fun(_Token) -> {ok, #{<<"uid">> => 123}} end}
        ]},
        {user_repo, [
            {'find_by_id', 2, fun(_Uid, _Fields) ->
                {ok, #{<<"id">> => 123, <<"status">> => 0}}
            end}
        ]}
    ], fun() ->
        Token = <<"valid_token">>,
        PostMap = #{},

        Result = passport_logic:quick_login(Token, PostMap),
        ?assertMatch({error, user_inactive, _}, Result)
    end).

%% ===================================================================
%% send_code/2 测试
%% ===================================================================

send_code_with_valid_mobile_succeeds_test_() ->
    ?WITH_MECK(verification_code_ds, [
        {'send', 3, fun(_Mobile, _Code, _Scene) -> {ok, sent} end}
    ], fun() ->
        Mobile = <<"13800138000">>,
        Scene = <<"login">>,

        Result = passport_logic:send_code(Mobile, Scene),
        ?assertMatch({ok, _Map}, Result)
    end).

send_code_with_empty_mobile_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        Mobile = <<>>,
        Scene = <<"login">>,

        Result = passport_logic:send_code(Mobile, Scene),
        ?assertMatch({error, _, _}, Result)
    end).

send_code_with_invalid_mobile_format_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        Mobile = <<"invalid">>,
        Scene = <<"login">>,

        Result = passport_logic:send_code(Mobile, Scene),
        ?assertMatch({error, _, _}, Result)
    end).

send_code_unavailable_scene_fails_test_() ->
    ?WITH_MECK(verification_code_ds, [
        {'send', 3, fun(_Mobile, _Code, _Scene) -> {error, rate_limited} end}
    ], fun() ->
        Mobile = <<"13800138000">>,
        Scene = <<"invalid_scene">>,

        Result = passport_logic:send_code(Mobile, Scene),
        ?assertMatch({error, _, _}, Result)
    end).

%% ===================================================================
%% find_password/5 测试
%% ===================================================================

find_password_with_valid_mobile_succeeds_test_() ->
    ?WITH_MECK(user_repo, [
        {'find_by_mobile', 2, fun(_Mobile, _Fields) ->
            {ok, #{<<"id">> => 123, <<"email">> => <<"user@example.com">>}}
        end}
    ], fun() ->
        Mobile = <<"13800138000">>,
        Ip = <<"127.0.0.1">>,

        Result = passport_logic:find_password(Mobile, Ip, #{}, <<>>),
        ?assertMatch({ok, #{<<"email">> := _}}, Result)
    end).

find_password_with_nonexistent_mobile_fails_test_() ->
    ?WITH_MECK(user_repo, [
        {'find_by_mobile', 2, fun(_Mobile, _Fields) -> {error, not_found} end}
        ], fun() ->
        Mobile = <<"13900139999">>,
        Ip = <<"127.0.0.1">>,

        Result = passport_logic:find_password(Mobile, Ip, #{}, <<>>),
        ?assertMatch({error, _, _}, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

do_signup_with_empty_mobile_fails_test_() ->
    ?WITH_MECK(user_repo, [
        {'find_by_mobile', 2, fun(_Mobile, _Fields) -> {error, not_found} end}
        ], fun() ->
        Mobile = <<>>,
        Password = <<"Test@123456">>,
        Email = <<"test@example.com">>,
        Req0 = #{},
        Ip = <<"127.0.0.1">>,

        Result = passport_logic:do_signup(Mobile, Password, Email, Req0, Ip),
        ?assertMatch({error, _, _}, Result)
    end).

do_login_with_empty_credentials_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        Mobile = <<>>,
        Password = <<>>,
        PostMap = #{},

        Result = passport_logic:do_login(Mobile, Password, PostMap),
        ?assertMatch({error, _, _}, Result)
    end).

quick_login_with_empty_token_fails_test_() ->
    ?WITH_MECK(token_ds, [
        {'decrypt_token', 1, fun(_Token) -> {error, invalid_token} end}
        ], fun() ->
        Token = <<>>,
        PostMap = #{},

        Result = passport_logic:quick_login(Token, PostMap),
        ?assertMatch({error, _, _}, Result)
    end).

verify_user_with_empty_credentials_fails_test_() ->
    ?WITH_MECK(user_repo, [
        {'find_by_mobile', 2, fun(_Mobile, _Fields) -> {error, not_found} end}
        ], fun() ->
        Mobile = <<>>,
        Password = <<>>,

        Result = passport_logic:verify_user(Mobile, Password),
        ?assertMatch({error, _, _}, Result)
    end).

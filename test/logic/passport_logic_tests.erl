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

%% @doc 测试 verify_user 返回的数据包含 id 字段
%% Bug: {badkey,<<"id">>} 错误表明 login_resp 没有返回 id 字段
%% 这个测试验证修复后的行为
verify_user_returns_id_field_test_() ->
    ?WITH_MECKS([
        {elib_password, [
            {'verify', 2, fun(_Pwd, _Hash) -> {ok, true} end}
        ]},
        {elib_hashids, [
            {'encode', 1, fun(Id) ->
                UidBin = integer_to_binary(Id),
                <<"uid-", UidBin/binary>>
            end}
        ]},
        {token_ds, [
            {'encrypt_token', 1, fun(_Id) -> <<"encrypted_token">> end},
            {'encrypt_refreshtoken', 1, fun(_Id) -> <<"refresh_token">> end}
        ]}
    ], fun() ->
        % 模拟用户数据（来自 user_ds:find_by_xxx）
        User = #{
            <<"id">> => 12345,
            <<"password">> => <<"$2a$12$hash">>,
            <<"email">> => <<"test@example.com">>,
            <<"nickname">> => <<"Test User">>,
            <<"avatar">> => <<"https://example.com/avatar.jpg">>,
            <<"account">> => <<"test_account">>,
            <<"gender">> => 1,
            <<"region">> => <<"Beijing">>,
            <<"sign">> => <<"Hello">>,
            <<"status">> => 1
        },
        Password = <<"Test@123456">>,

        % 调用 verify_user
        Result = passport_logic:verify_user(Password, User),

        % 验证返回的数据包含 id 字段（这是 Bug 的核心）
        ?assertMatch({ok, #{<<"id">> := 12345, <<"uid">> := _}}, Result)
    end).

%% @doc 测试 do_login_verify 返回的数据包含 id 字段
%% 这是完整登录流程的测试
do_login_verify_returns_id_field_test_() ->
    ?WITH_MECKS([
        {elib_password, [
            {'verify', 2, fun(_Pwd, _Hash) -> {ok, true} end}
        ]},
        {elib_hashids, [
            {'encode', 1, fun(Id) ->
                UidBin = integer_to_binary(Id),
                <<"uid-", UidBin/binary>>
            end}
        ]},
        {token_ds, [
            {'encrypt_token', 1, fun(_Id) -> <<"encrypted_token">> end},
            {'encrypt_refreshtoken', 1, fun(_Id) -> <<"refresh_token">> end}
        ]},
        {user_device_logic, [
            {'validate_device_type', 1, fun(_DType) -> true end},
            {'check_login_conflict', 2, fun(_Uid, _DType) -> {ok, no_conflict} end}
        ]}
    ], fun() ->
        % 模拟用户数据
        User = #{
            <<"id">> => 99999,
            <<"password">> => <<"$2a$12$hash">>,
            <<"email">> => <<"login@test.com">>,
            <<"nickname">> => <<"Login User">>,
            <<"avatar">> => <<"">>,
            <<"account">> => <<"login_test">>,
            <<"gender">> => 0,
            <<"region">> => <<"">>,
            <<"sign">> => <<"">>,
            <<"status">> => 1
        },
        Password = <<"Password123">>,
        DType = <<"mobile">>,

        % 调用 do_login_verify
        Result = passport_logic:do_login_verify(Password, User, DType, <<>>),

        % 验证返回的数据包含 id 和 uid 字段
        ?assertMatch({ok, #{<<"id">> := 99999, <<"uid">> := _, <<"token">> := _, <<"refreshtoken">> := _}}, Result)
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

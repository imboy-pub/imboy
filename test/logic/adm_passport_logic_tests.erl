-module(adm_passport_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(ADM_LOGIN_COLUMN, <<"id,account,mobile,password,email,nickname,avatar,role_id,status">>).

do_login_with_empty_password_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = adm_passport_logic:do_login(<<"admin">>, <<>>),
        ?assertEqual({error, <<"密码有误"/utf8>>}, Result)
    end).

do_login_with_mobile_success_test_() ->
    ?WITH_MECKS([
        {elib_type, [
            {'is_mobile', 1, fun(<<"13800138000">>) -> true end}
        ]},
        {adm_user_ds, [
            {'find_by_mobile', 2, fun(<<"13800138000">>, ?ADM_LOGIN_COLUMN) ->
                #{
                    <<"id">> => 1,
                    <<"account">> => <<"admin">>,
                    <<"mobile">> => <<"13800138000">>,
                    <<"password">> => <<"hash">>,
                    <<"email">> => <<"admin@example.com">>,
                    <<"nickname">> => <<"Admin">>,
                    <<"avatar">> => <<"avatar.png">>,
                    <<"role_id">> => 9,
                    <<"status">> => 1
                }
            end}
        ]},
        {elib_password, [
            {'verify', 2, fun(<<"password">>, <<"hash">>) -> {ok, valid} end}
        ]}
    ], fun() ->
        Result = adm_passport_logic:do_login(<<"13800138000">>, <<"password">>),
        ?assertEqual({ok, #{
            <<"id">> => <<"encoded-1">>,
            <<"mobile">> => <<"13800138000">>,
            <<"email">> => <<"admin@example.com">>,
            <<"nickname">> => <<"Admin">>,
            <<"avatar">> => <<"avatar.png">>,
            <<"account">> => <<"admin">>,
            <<"role_id">> => 9
        }}, Result)
    end).

do_login_with_account_success_and_default_role_test_() ->
    ?WITH_MECKS([
        {elib_type, [
            {'is_mobile', 1, fun(<<"admin">>) -> false end}
        ]},
        {adm_user_ds, [
            {'find_by_account', 2, fun(<<"admin">>, ?ADM_LOGIN_COLUMN) ->
                #{
                    <<"id">> => 2,
                    <<"account">> => <<"admin">>,
                    <<"mobile">> => <<"13800138001">>,
                    <<"password">> => <<"hash2">>,
                    <<"email">> => <<"ops@example.com">>,
                    <<"nickname">> => <<"Ops">>,
                    <<"avatar">> => <<>>,
                    <<"status">> => 1
                }
            end}
        ]},
        {elib_password, [
            {'verify', 2, fun(<<"password">>, <<"hash2">>) -> {ok, valid} end}
        ]}
    ], fun() ->
        Result = adm_passport_logic:do_login(<<"admin">>, <<"password">>),
        ?assertEqual({ok, #{
            <<"id">> => <<"encoded-2">>,
            <<"mobile">> => <<"13800138001">>,
            <<"email">> => <<"ops@example.com">>,
            <<"nickname">> => <<"Ops">>,
            <<"avatar">> => <<>>,
            <<"account">> => <<"admin">>,
            <<"role_id">> => 0
        }}, Result)
    end).

do_login_with_missing_account_test_() ->
    ?WITH_MECKS([
        {elib_type, [
            {'is_mobile', 1, fun(<<"ghost">>) -> false end}
        ]},
        {adm_user_ds, [
            {'find_by_account', 2, fun(<<"ghost">>, ?ADM_LOGIN_COLUMN) -> #{} end}
        ]},
        {elib_password, [
            {'verify', 2, fun(_, <<>>) -> {ok, valid} end}
        ]}
    ], fun() ->
        Result = adm_passport_logic:do_login(<<"ghost">>, <<"password">>),
        ?assertEqual({error, <<"账号不存在"/utf8>>}, Result)
    end).

do_login_with_wrong_password_test_() ->
    ?WITH_MECKS([
        {elib_type, [
            {'is_mobile', 1, fun(<<"13800138000">>) -> true end}
        ]},
        {adm_user_ds, [
            {'find_by_mobile', 2, fun(<<"13800138000">>, ?ADM_LOGIN_COLUMN) ->
                #{
                    <<"id">> => 3,
                    <<"account">> => <<"admin">>,
                    <<"mobile">> => <<"13800138000">>,
                    <<"password">> => <<"wrong-hash">>,
                    <<"status">> => 1
                }
            end}
        ]},
        {elib_password, [
            {'verify', 2, fun(<<"bad-password">>, <<"wrong-hash">>) -> {error, <<"密码错误"/utf8>>} end}
        ]}
    ], fun() ->
        Result = adm_passport_logic:do_login(<<"13800138000">>, <<"bad-password">>),
        ?assertEqual({error, <<"密码错误"/utf8>>}, Result)
    end).

-module(adm_passport_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_passport_logic 模块的 EUnit 测试
%%%
%%% 目标：验证管理员认证功能
%%% 覆盖：管理员登录、密码验证
%%%===================================================================

%% ===================================================================
%% do_login/2 测试
%% ===================================================================

%% @doc 测试空密码登录失败
do_login_with_empty_password_fails_test_() ->
    ?WITH_MECKS([
        {imboy_func, [
            {'is_mobile', 1, fun(_Mobile) -> true end}
        ]}
    ], fun() ->
        Mobile = <<"13800138000">>,
        Pwd = <<>>,
        Result = adm_passport_logic:do_login(Mobile, Pwd),
        % 空密码应该返回 {error, "密码有误"}
        ?assertEqual({error, "密码有误"}, Result)
    end).

%% @doc 测试使用手机号登录（需要 Mock）
do_login_with_mobile_and_mock_test_() ->
    % 使用固定的密码哈希，避免在测试中调用复杂函数
    % 这是一个伪造的哈希值，仅用于测试
    PwdHash = <<"fake_hash_for_testing_only">>,
    ?WITH_MECKS([
        {imboy_func, [
            {'is_mobile', 1, fun(_Mobile) -> true end}
        ]},
        {adm_user_repo, [
            {'find_by_mobile', 2, fun(_Mobile, _Column) ->
                #{
                    <<"id">> => 1,
                    <<"account">> => <<"admin">>,
                    <<"mobile">> => <<"13800138000">>,
                    <<"password">> => PwdHash,
                    <<"nickname">> => <<"Admin">>,
                    <<"avatar">> => <<>>,
                    <<"role_id">> => 1,
                    <<"email">> => <<"admin@example.com">>,
                    <<"status">> => 1
                }
            end}
        ]},
        {elib_password, [
            {'verify', 2, fun(_Pwd, _Hash) -> {ok, valid} end}
        ]}
    ], fun() ->
        Mobile = <<"13800138000">>,
        Pwd = <<"password">>,
        Result = adm_passport_logic:do_login(Mobile, Pwd),
        ?assertMatch({ok, _}, Result),
        {ok, UserInfo} = Result,
        ?assert(maps:is_key(<<"id">>, UserInfo)),
        ?assert(maps:get(<<"id">>, UserInfo) > 0)
    end).

%% @doc 测试使用账号登录（需要 Mock）
do_login_with_account_and_mock_test_() ->
    PwdHash = <<"fake_hash_for_testing_only">>,
    ?WITH_MECKS([
        {imboy_func, [
            {'is_mobile', 1, fun(_Account) -> false end}
        ]},
        {adm_user_repo, [
            {'find_by_account', 2, fun(_Account, _Column) ->
                #{
                    <<"id">> => 1,
                    <<"account">> => <<"admin">>,
                    <<"mobile">> => <<"13800138000">>,
                    <<"password">> => PwdHash,
                    <<"nickname">> => <<"Admin">>,
                    <<"avatar">> => <<>>,
                    <<"role_id">> => 1,
                    <<"email">> => <<"admin@example.com">>,
                    <<"status">> => 1
                }
            end}
        ]},
        {elib_password, [
            {'verify', 2, fun(_Pwd, _Hash) -> {ok, valid} end}
        ]}
    ], fun() ->
        Account = <<"admin">>,
        Pwd = <<"password">>,
        Result = adm_passport_logic:do_login(Account, Pwd),
        ?assertMatch({ok, _}, Result),
        {ok, UserInfo} = Result,
        ?assertEqual(<<"admin">>, maps:get(<<"account">>, UserInfo))
    end).

%% @doc 测试使用不存在账号登录失败
do_login_with_nonexistent_account_test_() ->
    ?WITH_MECKS([
        {imboy_func, [
            {'is_mobile', 1, fun(_Account) -> false end}
        ]},
        {adm_user_repo, [
            {'find_by_account', 2, fun(_Account, _Column) ->
                #{}
            end}
        ]},
        {elib_password, [
            {'verify', 2, fun(_Pwd, _Hash) ->
                {error, "密码验证失败"}
            end}
        ]}
    ], fun() ->
        Account = <<"nonexistent">>,
        Pwd = <<"wrong_password">>,
        Result = adm_passport_logic:do_login(Account, Pwd),
        ?assertMatch({error, _}, Result)
    end).

%% @doc 测试使用错误密码登录失败
do_login_with_wrong_password_test_() ->
    ?WITH_MECKS([
        {imboy_func, [
            {'is_mobile', 1, fun(_Mobile) -> true end}
        ]},
        {adm_user_repo, [
            {'find_by_mobile', 2, fun(_Mobile, _Column) ->
                #{
                    <<"id">> => 1,
                    <<"account">> => <<"admin">>,
                    <<"password">> => <<"some_hash">>,
                    <<"status">> => 1
                }
            end}
        ]},
        {elib_password, [
            {'verify', 2, fun(_Pwd, _Hash) ->
                {error, "密码错误"}
            end}
        ]}
    ], fun() ->
        Mobile = <<"13800138000">>,
        Pwd = <<"wrong_password">>,
        Result = adm_passport_logic:do_login(Mobile, Pwd),
        ?assertMatch({error, _}, Result)
    end).

%% @doc 测试禁用用户登录失败
do_login_with_disabled_account_test_() ->
    PwdHash = <<"fake_hash_for_testing_only">>,
    ?WITH_MECKS([
        {imboy_func, [
            {'is_mobile', 1, fun(_Mobile) -> true end}
        ]},
        {adm_user_repo, [
            {'find_by_mobile', 2, fun(_Mobile, _Column) ->
                #{
                    <<"id">> => 1,
                    <<"account">> => <<"admin">>,
                    <<"password">> => PwdHash,
                    <<"status">> => 0
                }
            end}
        ]},
        {elib_password, [
            {'verify', 2, fun(_Pwd, _Hash) -> {ok, valid} end}
        ]}
    ], fun() ->
        Mobile = <<"13800138000">>,
        Pwd = <<"password">>,
        Result = adm_passport_logic:do_login(Mobile, Pwd),
        ?assertMatch({error, _}, Result),
        {error, Reason} = Result,
        ?assertEqual(<<"账号被禁用">>, Reason)
    end).

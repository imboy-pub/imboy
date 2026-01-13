-module(adm_passport_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc adm_passport_logic 模块测试
login_success_test_() ->
    ?WITH_MECKS([
        {adm_user_ds, [
            {'find_by_account_and_password', 2, fun(_Account, _PasswordHash) ->
                #{<<"id">> => 1, <<"nickname">> => <<"管理员"/utf8>>}
            end}
        ]},
        {elib_hasher, [
            {'md5', 1, fun(_Pass) -> <<"hash">> end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {adm_user_ds, [
            {'update_login_count', 2, fun(_Id, _Count) -> ok end}
        ]}
    ], fun() ->
        Result = adm_passport_logic:login(<<"admin">>, <<"password">>),
        ?assertMatch(#{<<"id">> := 1}, Result)
    end).

login_invalid_credentials_test_() ->
    ?WITH_MECKS([
        {adm_user_ds, [
            {'find_by_account_and_password', 2, fun(_Account, _PasswordHash) ->
                #{}
            end}
        ]},
        {elib_hasher, [
            {'md5', 1, fun(_Pass) -> <<"hash">> end}
        ]}
    ], fun() ->
        Result = adm_passport_logic:login(<<"admin">>, <<"wrong">>),
        ?assertEqual({error, <<"账号或密码有误"/utf8>>}, Result)
    end).

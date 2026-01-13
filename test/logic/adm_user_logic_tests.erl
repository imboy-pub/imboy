-module(adm_user_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc adm_user_logic 模块测试
save_success_test_() ->
    ?WITH_MECKS([
        {adm_user_ds, [
            {'find_by_account', 2, fun(_Account, _Column) -> #{} end},
            {'save', 1, fun(Data) -> {ok, maps:get(<<"account">>, Data)} end}
        ]},
        {elib_password, [
            {'generate', 1, fun(_Hash) -> <<"password_hash">> end}
        ]},
        {elib_hasher, [
            {'md5', 1, fun(_Pass) -> <<"md5_hash">> end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]}
    ], fun() ->
        Result = adm_user_logic:save(#{<<"account">> => <<"admin">>, <<"password">> => <<"123456">>}),
        ?assertMatch({ok, _}, Result)
    end).

save_duplicate_account_test_() ->
    ?WITH_MECK(adm_user_ds, [
        {'find_by_account', 2, fun(_Account, _Column) ->
            #{<<"id">> => 1}
        end}
    ], fun() ->
        Result = adm_user_logic:save(#{<<"account">> => <<"admin">>, <<"password">> => <<"123456">>}),
        ?assertEqual({error, <<"账号已存在"/utf8>>}, Result)
    end).

update_success_test_() ->
    ?WITH_MECK(adm_user_ds, [
        {'find_by_account', 2, fun(_Account, _Column) -> #{<<"id">> => 1} end},
        {'update', 2, fun(_Id, _Data) -> {ok, 1} end}
    ], fun() ->
        Result = adm_user_logic:update(1, #{<<"nickname">> => <<"管理员"/utf8>>}),
        ?assertEqual({ok, 1}, Result)
    end).

delete_superuser_fails_test_() ->
    ?WITH_MECK(adm_user_ds, [
        {'find_by_id', 2, fun(_Id, _Column) ->
            #{<<"role_id">> => [1, 2]}
        end}
    ], fun() ->
        Result = adm_user_logic:delete(1),
        ?assertEqual({error, <<"不能删除超级管理员"/utf8>>}, Result)
    end).

assign_roles_success_test_() ->
    ?WITH_MECK(adm_user_ds, [
        {'update', 2, fun(_Id, _Data) -> {ok, 1} end}
    ], fun() ->
        Result = adm_user_logic:assign_roles(1, [2, 3]),
        ?assertEqual(ok, Result)
    end).

update_status_superuser_disabled_fails_test_() ->
    ?WITH_MECK(adm_user_ds, [
        {'find_by_id', 2, fun(_Id, _Column) ->
            #{<<"role_id">> => [1]}
        end}
    ], fun() ->
        Result = adm_user_logic:update_status(1, 0),
        ?assertEqual({error, <<"不能禁用超级管理员"/utf8>>}, Result)
    end).

reset_password_success_test_() ->
    ?WITH_MECKS([
        {elib_password, [
            {'generate', 1, fun(_Hash) -> <<"new_hash">> end}
        ]},
        {elib_hasher, [
            {'md5', 1, fun(_Pass) -> <<"md5_hash">> end}
        ]},
        {adm_user_ds, [
            {'update', 2, fun(_Id, _Data) -> {ok, 1} end}
        ]}
    ], fun() ->
        Result = adm_user_logic:reset_password(1, <<"newpass123">>),
        ?assertEqual(ok, Result)
    end).

reset_password_empty_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = adm_user_logic:reset_password(1, <<>>),
        ?assertEqual({error, <<"密码不能为空"/utf8>>}, Result)
    end).

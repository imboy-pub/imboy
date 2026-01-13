-module(adm_user_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc adm_user_ds 模块测试
find_by_id_success_test_() ->
    ?WITH_MECK(adm_user_repo, [
        {'find_by_id', 2, fun(_Id, _Column) -> #{<<"id">> => 1, <<"account">> => <<"admin">>} end}
    ], fun() ->
        Result = adm_user_ds:find_by_id(1, <<"*">>),
        ?assertMatch(#{<<"id">> := 1}, Result)
    end).

find_by_account_success_test_() ->
    ?WITH_MECK(adm_user_repo, [
        {'find_by_account', 2, fun(_Account, _Column) -> #{<<"id">> => 1} end}
    ], fun() ->
        Result = adm_user_ds:find_by_account(<<"admin">>, <<"id">>),
        ?assertMatch(#{<<"id">> := 1}, Result)
    end).

list_pagination_test_() ->
    ?WITH_MECK(adm_user_repo, [
        {'page', 3, fun(_Page, _Size, _Offset) -> {ok, []} end}
    ], fun() ->
        Result = adm_user_ds:list(1, 10),
        ?assertEqual({ok, []}, Result)
    end).

count_success_test_() ->
    ?WITH_MECK(adm_user_repo, [
        {'count', 0, fun() -> 5 end}
    ], fun() ->
        Result = adm_user_ds:count(),
        ?assertEqual(5, Result)
    end).

update_status_test_() ->
    ?WITH_MECK(adm_user_repo, [
        {'update', 2, fun(_Id, _Data) -> {ok, 1} end}
    ], fun() ->
        Result = adm_user_ds:update(1, #{<<"status">> => 1}),
        ?assertEqual({ok, 1}, Result)
    end).

reset_password_test_() ->
    ?WITH_MECKS([
        {elib_password, [
            {'generate', 1, fun(_Hash) -> <<"new_hash">> end}
        ]},
        {elib_hasher, [
            {'md5', 1, fun(_Pass) -> <<"md5_hash">> end}
        ]},
        {adm_user_repo, [
            {'update', 2, fun(_Id, _Data) -> {ok, 1} end}
        ]}
    ], fun() ->
        Result = adm_user_ds:reset_password(1, <<"newpass">>),
        ?assertEqual({ok, 1}, Result)
    end).

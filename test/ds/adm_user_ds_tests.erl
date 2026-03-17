-module(adm_user_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

find_by_id_success_test_() ->
    ?WITH_MECK(adm_user_repo, [
        {'find_by_id', 2, fun(1, <<"*">>) ->
            #{<<"id">> => 1, <<"account">> => <<"admin">>}
        end}
    ], fun() ->
        Result = adm_user_ds:find_by_id(1, <<"*">>),
        ?assertMatch(#{<<"id">> := 1}, Result)
    end).

find_by_account_success_test_() ->
    ?WITH_MECK(adm_user_repo, [
        {'find_by_account', 2, fun(<<"admin">>, <<"id">>) ->
            #{<<"id">> => 1}
        end}
    ], fun() ->
        Result = adm_user_ds:find_by_account(<<"admin">>, <<"id">>),
        ?assertMatch(#{<<"id">> := 1}, Result)
    end).

list_success_test_() ->
    ?WITH_MECKS([
        {adm_user_repo, [
            {'tablename', 0, fun() ->
                <<"adm_user">>
            end}
        ]},
        {elib_pg, [
            {'page', 6, fun(<<"adm_user">>, _Column, #{}, <<"created_at DESC">>, 1, 10) ->
                {ok, [#{<<"id">> => 1}]}
            end}
        ]}
    ], fun() ->
        ?assertEqual({ok, [#{<<"id">> => 1}]}, adm_user_ds:list(1, 10))
    end).

count_success_test_() ->
    ?WITH_MECKS([
        {adm_user_repo, [
            {'tablename', 0, fun() ->
                <<"adm_user">>
            end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, []) ->
                {ok, [#{<<"count">> => 5}]}
            end}
        ]}
    ], fun() ->
        ?assertEqual({ok, 5}, adm_user_ds:count())
    end).

update_success_test_() ->
    ?WITH_MECK(adm_user_repo, [
        {'update', 2, fun(1, Data) ->
            ?assertEqual(1, maps:get(<<"status">>, Data)),
            {ok, 1}
        end}
    ], fun() ->
        ?assertEqual({ok, 1}, adm_user_ds:update(1, #{<<"status">> => 1}))
    end).

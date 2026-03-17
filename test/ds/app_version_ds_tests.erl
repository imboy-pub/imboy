-module(app_version_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

sign_key_returns_config_value_test_() ->
    ?WITH_MECKS([
        {elib_log, [
            {'internal_log', 4, fun(debug, _Msg, app_version_ds, _Line) ->
                ok
            end}
        ]},
        {config_ds, [
            {'get', 2, fun(<<"pkg_android_1.0.0">>, <<>>) ->
                <<"configured_sign_key">>
            end}
        ]}
    ], fun() ->
        Result = app_version_ds:sign_key(<<"android">>, <<"1.0.0">>, <<"pkg">>),
        ?assertEqual(<<"configured_sign_key">>, Result)
    end).

sign_key_with_missing_config_returns_empty_test_() ->
    ?WITH_MECKS([
        {elib_log, [
            {'internal_log', 4, fun(debug, _Msg, app_version_ds, _Line) ->
                ok
            end}
        ]},
        {config_ds, [
            {'get', 2, fun(_Key, <<>>) ->
                <<>>
            end}
        ]}
    ], fun() ->
        ?assertEqual(<<>>, app_version_ds:sign_key(<<"ios">>, <<"1">>, <<"pkg">>))
    end).

get_sign_key_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'one', 2, fun(_Sql, [<<"1.0.0">>, <<"pkg">>, <<"ios">>]) ->
            {ok, #{<<"sign_key">> => <<"db_sign_key">>}}
        end}
    ], fun() ->
        ?assertEqual({ok, <<"db_sign_key">>}, app_version_ds:get_sign_key(<<"ios">>, <<"1.0.0">>, <<"pkg">>, <<"sign_key">>))
    end).

save_add_success_test_() ->
    ?WITH_MECKS([
        {ec_cnv, [
            {'to_integer', 1, fun(0) ->
                0
            end}
        ]},
        {app_version_repo, [
            {'add', 1, fun(Data) ->
                ?assert(maps:is_key(<<"created_at">>, Data)),
                {ok, #{<<"id">> => 123}}
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() ->
                <<"2026-03-16T00:00:00Z">>
            end}
        ]}
    ], fun() ->
        Data = #{<<"id">> => 0, <<"vsn">> => <<"1.0.0">>, <<"package_name">> => <<"pkg">>},
        ?assertMatch({ok, _}, app_version_ds:save(Data))
    end).

save_update_success_test_() ->
    ?WITH_MECKS([
        {ec_cnv, [
            {'to_integer', 1, fun(123) ->
                123
            end}
        ]},
        {app_version_repo, [
            {'tablename', 0, fun() ->
                <<"app_version">>
            end}
        ]},
        {elib_pg, [
            {'update', 4, fun(<<"app_version">>, Data, <<"id = $1">>, [123]) ->
                ?assert(maps:is_key(<<"updated_at">>, Data)),
                {ok, 1}
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() ->
                <<"2026-03-16T00:00:00Z">>
            end}
        ]}
    ], fun() ->
        Data = #{<<"id">> => 123, <<"vsn">> => <<"1.0.1">>, <<"package_name">> => <<"pkg">>},
        ?assertEqual({ok, 1}, app_version_ds:save(Data))
    end).

delete_success_test_() ->
    ?WITH_MECKS([
        {app_version_repo, [
            {'tablename', 0, fun() ->
                <<"app_version">>
            end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(<<"DELETE FROM app_version WHERE vsn = '1.0.0'">>, []) ->
                ok
            end}
        ]}
    ], fun() ->
        ?assertEqual(ok, app_version_ds:delete(<<"vsn = '1.0.0'">>))
    end).

delete_by_id_valid_test_() ->
    ?WITH_MECK(app_version_repo, [
        {'delete_by_id', 1, fun(123) ->
            {ok, 1}
        end}
    ], fun() ->
        ?assertEqual({ok, 1}, app_version_ds:delete_by_id(123))
    end).

delete_by_id_zero_raises_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertError(function_clause, app_version_ds:delete_by_id(0))
    end).

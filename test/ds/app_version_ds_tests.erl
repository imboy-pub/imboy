-module(app_version_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% app_version_ds 模块的 EUnit 测试
%%%
%%% 目标：验证应用版本管理功能
%%% 覆盖：签名密钥获取和设置
%%%===================================================================

%% ===================================================================
%% sign_key/3 测试
%% ===================================================================

sign_key_returns_binary_test_() ->
    ?TEST_WITH_DB(fun() ->
        Platform = <<"ios">>,
        Version = <<"1.0.0">>,
        Build = <<"1">>,
        Result = app_version_ds:sign_key(Platform, Version, Build),
        ?assertMatch(Key when is_binary(Key) andalso byte_size(Key) > 0 orelse Key =:= undefined, Result)
    end).

%% ===================================================================
%% get_sign_key/4 测试
%% ===================================================================

get_sign_key_returns_binary_test_() ->
    ?TEST_WITH_DB(fun() ->
        Platform = <<"ios">>,
        Version = <<"1.0.0">>,
        Build = <<"1">>,
        Default = <<"default_key">>,
        Result = app_version_ds:get_sign_key(Platform, Version, Build, Default),
        ?assertMatch(Key when is_binary(Key) andalso byte_size(Key) > 0, Result)
    end).

%% ===================================================================
%% set_sign_key/4 测试
%% ===================================================================

set_sign_key_saves_key_test_() ->
    ?TEST_WITH_DB(fun() ->
        Platform = <<"ios">>,
        Version = <<"1.0.0">>,
        Build = <<"1">>,
        Key = <<"test_key">>,
        Result = app_version_ds:set_sign_key(Platform, Version, Build, Key),
        case Result of
            {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount >= 0);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, AffectedCount}")
        end
    end).

%% ===================================================================
%% save/1 测试
%% ===================================================================

save_creates_new_version_test_() ->
    ?WITH_MECKS([
        {ec_cnv, [
            {'to_integer', 1, fun(_Id) -> 0 end}
        ]},
        {app_version_repo, [
            {'add', 1, fun(Data) ->
                ?assert(maps:is_key(<<"created_at">>, Data)),
                {ok, #{<<"id">> => 123}}
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2024-01-01T00:00:00Z">> end}
        ]}
    ], fun() ->
        Data = #{
            <<"vsn">> => <<"1.0.0">>,
            <<"package_name">> => <<"com.example.app">>,
            <<"type">> => <<"ios">>
        },
        Result = app_version_ds:save(Data),
        ?assertMatch({ok, _}, Result)
    end).

save_updates_existing_version_test_() ->
    ?WITH_MECKS([
        {ec_cnv, [
            {'to_integer', 1, fun(_Id) -> 123 end}
        ]},
        {elib_pg, [
            {'update', 4, fun(_Table, Data, _Where, _Params) ->
                ?assert(maps:is_key(<<"updated_at">>, Data)),
                ?assertNot(maps:is_key(<<"id">>, Data)),
                {ok, 1}
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2024-01-01T00:00:00Z">> end}
        ]}
    ], fun() ->
        Data = #{
            <<"id">> => 123,
            <<"vsn">> => <<"1.0.1">>,
            <<"package_name">> => <<"com.example.app">>
        },
        Result = app_version_ds:save(Data),
        ?assertMatch({ok, _}, Result)
    end).

save_with_zero_id_creates_new_test_() ->
    ?WITH_MECKS([
        {ec_cnv, [
            {'to_integer', 1, fun(_Id) -> 0 end}
        ]},
        {app_version_repo, [
            {'add', 1, fun(_Data) -> {ok, #{<<"id">> => 456}} end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2024-01-01T00:00:00Z">> end}
        ]}
    ], fun() ->
        Data = #{<<"id">> => 0, <<"vsn">> => <<"1.0.0">>},
        Result = app_version_ds:save(Data),
        ?assertMatch({ok, _}, Result)
    end).

save_without_id_creates_new_test_() ->
    ?WITH_MECKS([
        {ec_cnv, [
            {'to_integer', 1, fun(_Id) -> 0 end}
        ]},
        {app_version_repo, [
            {'add', 1, fun(_Data) -> {ok, #{<<"id">> => 789}} end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2024-01-01T00:00:00Z">> end}
        ]}
    ], fun() ->
        Data = #{<<"vsn">> => <<"1.0.0">>, <<"package_name">> => <<"test.app">>},
        Result = app_version_ds:save(Data),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% delete/1 测试
%% ===================================================================

delete_with_where_clause_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'execute', 2, fun(Sql, _Params) ->
                ?assertEqual(<<"DELETE FROM app_version WHERE vsn = '1.0.0'">>, Sql),
                ok
            end}
        ]}
    ], fun() ->
        Where = <<"vsn = '1.0.0'">>,
        Result = app_version_ds:delete(Where),
        ?assertEqual(ok, Result)
    end).

delete_with_complex_where_clause_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> ok end}
        ]}
    ], fun() ->
        Where = <<"vsn = '1.0.0' AND package_name = 'com.example.app'">>,
        Result = app_version_ds:delete(Where),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% delete_by_id/1 测试
%% ===================================================================

delete_by_id_with_valid_id_test_() ->
    ?WITH_MECK(app_version_repo, [
        {'delete_by_id', 1, fun(Id) ->
            ?assertEqual(123, Id),
            {ok, 1}
        end}
    ], fun() ->
        Result = app_version_ds:delete_by_id(123),
        ?assertEqual({ok, 1}, Result)
    end).

delete_by_id_with_nonexistent_id_test_() ->
    ?WITH_MECK(app_version_repo, [
        {'delete_by_id', 1, fun(_Id) -> {ok, 0} end}
    ], fun() ->
        Result = app_version_ds:delete_by_id(999999),
        ?assertEqual({ok, 0}, Result)
    end).

delete_by_id_with_zero_id_test_() ->
    ?WITH_MECK(app_version_repo, [
        {'delete_by_id', 1, fun(Id) ->
            ?assertEqual(0, Id),
            {ok, 0}
        end}
    ], fun() ->
        Result = app_version_ds:delete_by_id(0),
        ?assertEqual({ok, 0}, Result)
    end).

%% ===================================================================
%% sign_key/3 完整测试
%% ===================================================================

sign_key_returns_config_value_test_() ->
    ?WITH_MECK(config_ds, [
        {'get', 1, fun(Key) ->
            ?assertEqual(<<"pub.imboy.apk_android_1.0.0">>, Key),
            <<"configured_sign_key">>
        end}
    ], fun() ->
        Result = app_version_ds:sign_key(<<"android">>, <<"1.0.0">>, <<"pub.imboy.apk">>),
        ?assertEqual(<<"configured_sign_key">>, Result)
    end).

sign_key_with_missing_config_returns_empty_test_() ->
    ?WITH_MECK(config_ds, [
        {'get', 1, fun(_Key) -> <<>> end}
    ], fun() ->
        Result = app_version_ds:sign_key(<<"ios">>, <<"1.0.0">>, <<"com.example.app">>),
        ?assertEqual(<<>>, Result)
    end).

sign_key_with_different_platforms_test_() ->
    ?WITH_MECK(config_ds, [
        {'get', 1, fun(Key) ->
            case Key of
                <<"pkg_ios_1">> -> <<"ios_key">>;
                <<"pkg_android_1">> -> <<"android_key">>;
                <<"pkg_macos_1">> -> <<"macos_key">>
            end
        end}
    ], fun() ->
        ?assertEqual(<<"ios_key">>, app_version_ds:sign_key(<<"ios">>, <<"1">>, <<"pkg">>)),
        ?assertEqual(<<"android_key">>, app_version_ds:sign_key(<<"android">>, <<"1">>, <<"pkg">>)),
        ?assertEqual(<<"macos_key">>, app_version_ds:sign_key(<<"macos">>, <<"1">>, <<"pkg">>))
    end).

%% ===================================================================
%% get_sign_key/4 完整测试
%% ===================================================================

get_sign_key_from_database_test_() ->
    ?WITH_MECK(elib_pg, [
        {'one', 2, fun(_Sql, _Params) ->
            {ok, #{<<"sign_key">> => <<"db_sign_key">>}}
        end}
    ], fun() ->
        Result = app_version_ds:get_sign_key(<<"ios">>, <<"1.0.0">>, <<"com.example.app">>, <<"sign_key">>),
        ?assertEqual({ok, <<"db_sign_key">>}, Result)
    end).

get_sign_key_field_not_exists_test_() ->
    ?WITH_MECK(elib_pg, [
        {'one', 2, fun(_Sql, _Params) ->
            {ok, #{<<"other_field">> => <<"value">>}}
        end}
    ], fun() ->
        Result = app_version_ds:get_sign_key(<<"ios">>, <<"1.0.0">>, <<"com.example.app">>, <<"sign_key">>),
        ?assertEqual({ok, undefined}, Result)
    end).

get_sign_key_record_not_found_test_() ->
    ?WITH_MECK(elib_pg, [
        {'one', 2, fun(_Sql, _Params) ->
            {error, not_found}
        end}
    ], fun() ->
        Result = app_version_ds:get_sign_key(<<"ios">>, <<"9.9.9">>, <<"nonexistent.app">>, <<"sign_key">>),
        ?assertMatch({error, not_found}, Result)
    end).

get_sign_key_with_different_fields_test_() ->
    ?WITH_MECK(elib_pg, [
        {'one', 2, fun(Sql, _Params) ->
            Field = case Sql of
                <<"SELECT solidified_key FROM", _/binary>> -> <<"solidified_key">>;
                <<"SELECT sign_key FROM", _/binary>> -> <<"sign_key">>
            end,
            {ok, #{Field => <<"key_value">>}}
        end}
    ], fun() ->
        Result1 = app_version_ds:get_sign_key(<<"ios">>, <<"1.0.0">>, <<"app">>, <<"sign_key">>),
        ?assertEqual({ok, <<"key_value">>}, Result1),

        Result2 = app_version_ds:get_sign_key(<<"ios">>, <<"1.0.0">>, <<"app">>, <<"solidified_key">>),
        ?assertEqual({ok, <<"key_value">>}, Result2)
    end).

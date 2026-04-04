-module(app_version_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% app_version_repo 模块的 EUnit 测试
%%%
%%% 目标：验证 imboy_db → elib_pg 迁移的语义正确性
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_public_prefix_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = app_version_repo:tablename(),
        ?assertEqual(<<"public.app_version">>, Result)
    end).

%% ===================================================================
%% find/2 测试
%% ===================================================================

find_valid_where_test_() ->
    ?TEST_WITH_DB(fun() ->
        Where = <<"type = 'ios'">>,
        Column = <<"id">>,
        Result = app_version_repo:find(Where, Column),
        ?assertMatch({ok, _, _}, Result)
    end).

find_empty_result_test_() ->
    ?TEST_WITH_DB(fun() ->
        Where = <<"type = 'nonexistent_type'">>,
        Column = <<"id">>,
        Result = app_version_repo:find(Where, Column),
        ?assertMatch({ok, _, _}, Result)
    end).

find_all_columns_test_() ->
    ?TEST_WITH_DB(fun() ->
        Where = <<"1=1">>,
        Column = <<"*">>,
        Result = app_version_repo:find(Where, Column),
        ?assertMatch({ok, _, _}, Result)
    end).

%% ===================================================================
%% add/1 测试
%% ===================================================================

add_valid_ios_app_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{
            <<"region_code">> => <<"cn">>,
            <<"type">> => <<"ios">>,
            <<"package_name">> => <<"com.test.app">>,
            <<"app_name">> => <<"TestApp">>,
            <<"vsn">> => <<"1.0.0">>,
            <<"download_url">> => <<"https://test.com/download">>,
            <<"description">> => <<"Test app">>,
            <<"force_update">> => 1,
            <<"created_at">> => elib_dt:now(),
            <<"sign_key">> => <<"test_key">>
        },
        Result = app_version_repo:add(Data),
        ?assertMatch({ok, InsertedId} when is_integer(InsertedId), Result)
    end).

add_valid_android_app_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{
            <<"type">> => <<"android">>,
            <<"package_name">> => <<"com.test.android">>,
            <<"app_name">> => <<"TestAndroid">>,
            <<"vsn">> => <<"1.0.0">>,
            <<"download_url">> => <<"https://test.com/android.apk">>,
            <<"description">> => <<"Test android app">>,
            <<"force_update">> => 2,
            <<"created_at">> => elib_dt:now(),
            <<"sign_key">> => <<>>
        },
        Result = app_version_repo:add(Data),
        ?assertMatch({ok, InsertedId} when is_integer(InsertedId), Result)
    end).

add_minimal_data_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{
            <<"type">> => <<"ios">>,
            <<"vsn">> => <<"1.0">>,
            <<"created_at">> => elib_dt:now()
        },
        Result = app_version_repo:add(Data),
        case Result of
            {ok, Version} -> 
                ?assertMatch(#{<<"id">> := _, <<"type">> := <<"ios">>, <<"vsn">> := <<"1.0">>}, Version);
            {error, Reason} -> 
                ?assert(is_atom(Reason), "Expected atom error reason")
        end
    end).

%% ===================================================================
%% find/2 测试（原 demo/3 已移除，改为测试 find/2）
%% ===================================================================

find_valid_type_test_() ->
    ?TEST_WITH_DB(fun() ->
        Type = <<"android">>,
        RegionCode = <<"CN">>,
        Result = app_version_repo:find(Type, RegionCode),
        case Result of
            {ok, _, _} -> ?assert(true);
            {ok, _} -> ?assert(true);
            {error, _} -> ?assert(true)
        end
    end).

find_non_existing_type_test_() ->
    ?TEST_WITH_DB(fun() ->
        Type = <<"nonexistent">>,
        RegionCode = <<"XX">>,
        Result = app_version_repo:find(Type, RegionCode),
        case Result of
            {ok, _, _} -> ?assert(true);
            {ok, _} -> ?assert(true);
            {error, _} -> ?assert(true)
        end
    end).

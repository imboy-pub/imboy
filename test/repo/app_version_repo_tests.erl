-module(app_version_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% app_version_repo 模块的 EUnit 测试
%%%
%%% 目标：验证 imboy_db → imboy_pg 迁移的语义正确性
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
            <<"created_at">> => imboy_dt:now(),
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
            <<"created_at">> => imboy_dt:now(),
            <<"sign_key">> => <<"">>
        },
        Result = app_version_repo:add(Data),
        ?assertMatch({ok, InsertedId} when is_integer(InsertedId), Result)
    end).

add_minimal_data_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{
            <<"type">> => <<"ios">>,
            <<"vsn">> => <<"1.0">>,
            <<"created_at">> => imboy_dt:now()
        },
        Result = app_version_repo:add(Data),
        case Result of
            {ok, Version} -> 
                ?ASSERT_MATCH(#{<<"id">> := _, <<"type">> := <<"ios">>, <<"vsn">> := <<"1.0">>}, Version);
            {error, Reason} -> 
                ?assert(is_atom(Reason), "Expected atom error reason")
        end
    end).

%% ===================================================================
%% demo/3 测试
%% ===================================================================

demo_valid_id_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Val1 = <<"val1">>,
        Val2 = <<"val2">>,
        Result = app_version_repo:demo(Uid, Val1, Val2),
        ?assertMatch({ok, _, _}, Result)
    end).

demo_non_existing_id_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 999999,
        Val1 = <<"val1">>,
        Val2 = <<"val2">>,
        Result = app_version_repo:demo(Uid, Val1, Val2),
        ?assertMatch({ok, _, _}, Result)
    end).

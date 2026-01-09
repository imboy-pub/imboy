-module(adm_app_version_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_app_version_logic 模块的 EUnit 测试
%%%
%%% 目标：验证应用版本管理功能
%%% 覆盖：版本保存、删除、版本排序
%%%===================================================================

%% ===================================================================
%% save/1 测试
%% ===================================================================

save_with_new_version_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{
            id => 0,
            app_key => <<"test_app">>,
            platform => <<"ios">>,
            version => <<"1.0.0">>,
            url => <<"https://example.com/app.ipa">>
        },
        Result = adm_app_version_logic:save(Data),
        ?assertMatch({ok, _, _}, Result)
    end).

save_with_existing_version_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{
            id => 1,
            app_key => <<"test_app">>,
            platform => <<"ios">>,
            version => <<"1.0.1">>,
            url => <<"https://example.com/app2.ipa">>
        },
        Result = adm_app_version_logic:save(Data),
        ?assertMatch({ok, _, _}, Result)
    end).

%% ===================================================================
%% delete/1 测试
%% ===================================================================

delete_by_condition_test_() ->
    ?TEST_WITH_DB(fun() ->
        Where = <<"id = 999999">>,
        Result = adm_app_version_logic:delete(Where),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% vsn_sort/1 测试
%% ===================================================================

vsn_sort_simple_version_test_() ->
    ?TEST_WITH_APP(fun() ->
        Vsn = <<"1.0">>,
        Result = adm_app_version_logic:vsn_sort(Vsn),
        ?assert(Result > 0)
    end).

vsn_sort_semantic_version_test_() ->
    ?TEST_WITH_APP(fun() ->
        Vsn = <<"1.2.3">>,
        Result = adm_app_version_logic:vsn_sort(Vsn),
        Expected = 1 * 1_000_000 + 2 * 1_000 + 3,
        ?assertEqual(Expected, Result)
    end).

vsn_sort_complex_version_test_() ->
    ?TEST_WITH_APP(fun() ->
        Vsn = <<"10.102.22">>,
        Result = adm_app_version_logic:vsn_sort(Vsn),
        Expected = 10 * 1_000_000 + 102 * 1_000 + 22,
        ?assertEqual(Expected, Result)
    end).

vsn_sort_invalid_version_test_() ->
    ?TEST_WITH_APP(fun() ->
        Vsn = <<"invalid">>,
        Result = adm_app_version_logic:vsn_sort(Vsn),
        ?assertEqual(0, Result)
    end).

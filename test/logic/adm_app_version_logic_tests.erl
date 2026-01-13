-module(adm_app_version_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc adm_app_version_logic 模块的 EUnit 测试
%%% 目标：验证后台版本管理业务逻辑
%%% 覆盖：保存、删除、版本号排序
%%%===================================================================

%% save/1 测试
save_success_test_() ->
    ?WITH_MECK(app_version_ds, [
        {'save', 1, fun(Data) ->
            ?assertEqual(<<"1.0.0">>, maps:get(<<"vsn">>, Data)),
            {ok, 1}
        end}
    ], fun() ->
        Result = adm_app_version_logic:save(#{<<"vsn">> => <<"1.0.0">>}),
        ?assertEqual({ok, 1}, Result)
    end).

save_with_id_updates_test_() ->
    ?WITH_MECK(app_version_ds, [
        {'save', 1, fun(Data) ->
            ?assertEqual(1, maps:get(<<"id">>, Data)),
            {ok, 1}
        end}
    ], fun() ->
        Result = adm_app_version_logic:save(#{<<"id">> => 1, <<"vsn">> => <<"2.0.0">>}),
        ?assertEqual({ok, 1}, Result)
    end).

%% delete/1 测试
delete_success_test_() ->
    ?WITH_MECK(app_version_ds, [
        {'delete', 1, fun(Where) ->
            ?assertEqual(<<"id = 1">>, Where),
            ok
        end}
    ], fun() ->
        Result = adm_app_version_logic:delete(<<"id = 1">>),
        ?assertEqual(ok, Result)
    end).

%% delete_by_id/1 测试
delete_by_id_success_test_() ->
    ?WITH_MECK(app_version_ds, [
        {'delete_by_id', 1, fun(Id) ->
            ?assertEqual(1, Id),
            {ok, 1}
        end}
    ], fun() ->
        Result = adm_app_version_logic:delete_by_id(1),
        ?assertEqual({ok, 1}, Result)
    end).

delete_by_id_with_zero_id_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = adm_app_version_logic:delete_by_id(0),
        ?assertEqual({error, invalid_id}, Result)
    end).

delete_by_id_with_negative_id_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = adm_app_version_logic:delete_by_id(-1),
        ?assertEqual({error, invalid_id}, Result)
    end).

%% vsn_sort/1 测试
vsn_sort_simple_version_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(1000000, adm_app_version_logic:vsn_sort(<<"1.0.0">>)),
        ?assertEqual(1002000, adm_app_version_logic:vsn_sort(<<"1.2.0">>)),
        ?assertEqual(1002345, adm_app_version_logic:vsn_sort(<<"1.23.45">>))
    end).

vsn_sort_complex_version_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(10000200, adm_app_version_logic:vsn_sort(<<"10.0.200">>)),
        ?assertEqual(101002022, adm_app_version_logic:vsn_sort(<<"10.100.22">>))
    end).

vsn_sort_with_two_parts_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(2000000, adm_app_version_logic:vsn_sort(<<"2.0">>)),
        ?assertEqual(2015000, adm_app_version_logic:vsn_sort(<<"2.15">>))
    end).

vsn_sort_with_major_only_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(5000000, adm_app_version_logic:vsn_sort(<<"5">>))
    end).

vsn_sort_invalid_version_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(0, adm_app_version_logic:vsn_sort(<<"invalid">>)),
        ?assertEqual(0, adm_app_version_logic:vsn_sort(<<>>))
    end).

%% 边界条件测试
vsn_sort_zero_version_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(0, adm_app_version_logic:vsn_sort(<<"0.0.0">>))
    end).

vsn_sort_large_version_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(999000000, adm_app_version_logic:vsn_sort(<<"999.999.999">>))
    end).

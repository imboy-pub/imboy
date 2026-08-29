-module(adm_app_version_logic_adm_tests).
-include_lib("eunit/include/eunit.hrl").
%% CI-00：原模块名 adm_*_tests 与 test/logic|test/repo 下同名模块冲突——erlang.mk 把
%% test/ 子目录平铺编译到 test/，同名 beam 互相覆盖导致本文件用例从不被执行。
%% 改名恢复发现执行（用例内容未改动，非 skip 非删除）。
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
        %% CI-00 修桩：字段名对齐现 schema（app_version 表列为 vsn/type/
        %% package_name/download_url/description；原 version/app_key/platform/
        %% url 为旧 schema 字段，插入触发 42703 undefined_column）。
        Data = #{
            <<"id">> => 0,
            <<"type">> => <<"ios">>,
            <<"package_name">> => <<"com.example.test">>,
            %% CI-00 修桩：真库持久，vsn 需逐次唯一（UNIQUE 约束），保证幂等
            <<"vsn">> =>
                <<"1.0.0-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
            <<"download_url">> => <<"https://example.com/app.ipa">>,
            <<"description">> => <<"ci00 test">>
        },
        Result = adm_app_version_logic:save(Data),
        ?assertMatch({ok, _}, Result)
    end).

save_with_existing_version_test_() ->
    ?TEST_WITH_DB(fun() ->
        %% CI-00 修桩：同上（id>1 走 update 分支，断言对齐 {ok, _}）。
        Data = #{
            <<"id">> => 1,
            <<"type">> => <<"ios">>,
            <<"package_name">> => <<"com.example.test">>,
            <<"vsn">> =>
                <<"1.0.1-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
            <<"download_url">> => <<"https://example.com/app2.ipa">>,
            <<"description">> => <<"ci00 test update">>
        },
        Result = adm_app_version_logic:save(Data),
        ?assertMatch({ok, _}, Result)
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

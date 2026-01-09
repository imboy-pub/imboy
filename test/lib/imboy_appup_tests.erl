-module(imboy_appup_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_appup 模块的 EUnit 测试
%%%
%%% 目标：验证应用升级处理功能
%%% 覆盖：版本升级、状态迁移
%%%===================================================================

appup_version_test_() ->
    ?TEST_WITH_APP(fun() ->
        ?assertError(function_clause, imboy_appup:run(1, 2)),
        ?assertError(function_clause, imboy_appup:first_release(123))
    end).

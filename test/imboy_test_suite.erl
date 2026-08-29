%% @doc ImBoy 测试套件主模块
%% 集成所有测试模块，提供统一的测试入口
-module(imboy_test_suite).

-include_lib("eunit/include/eunit.hrl").

%% 测试套件定义
%% CI-00 重写为「套件完整性守卫」：原聚合用例在全量 eunit 中把所有
%% integration/stress/performance 测试经 fun 引用再执行一遍（与各模块的
%% 独立执行完全重复，全量时长翻倍），且引用了不存在的
%% websocket_performance_tests（undef -> cancelled）。各套件的执行由
%% EUNIT_TEST_MODS 自动发现覆盖；本模块保留 run_all/run_stress/run_perf
%% 手动入口，all_tests_test_ 只验证套件引用的模块都存在。
all_tests_test_() ->
    {timeout, 30, [
        {"套件完整性 - 集成/性能/压力模块全部可加载", fun verify_suite_modules/0}
    ]}.

verify_suite_modules() ->
    Suites = [
        msg_forward_integration_tests,
        msg_reply_integration_tests,
        conversation_pin_delete_integration_tests,
        msg_reaction_integration_tests,
        mention_integration_tests,
        group_member_role_integration_tests,
        group_category_tag_integration_tests,
        msg_send_performance_tests,
        db_query_performance_tests,
        high_concurrency_stress_tests,
        group_member_limit_stress_tests
    ],
    lists:foreach(
        fun(M) ->
            {module, M} = code:ensure_loaded(M)
        end,
        Suites
    ).

%% 运行所有测试
run_all() ->
    io:format("~n========================================~n"),
    io:format("ImBoy 测试套件~n"),
    io:format("========================================~n~n"),

    StartTime = erlang:monotonic_time(millisecond),

    Results = eunit:test(?MODULE, [verbose, {report, eunit_surefire}]),

    EndTime = erlang:monotonic_time(millisecond),
    TotalTime = EndTime - StartTime,

    io:format("~n========================================~n"),
    io:format("测试完成~n"),
    io:format("  总耗时: ~p ms~n", [TotalTime]),
    io:format("========================================~n"),

    Results.

%% 运行集成测试
run_integration() ->
    io:format("~n运行集成测试...~n~n"),
    eunit:test(
        [
            msg_forward_integration_tests,
            msg_reply_integration_tests,
            conversation_pin_delete_integration_tests,
            msg_reaction_integration_tests,
            mention_integration_tests,
            group_member_role_integration_tests,
            group_category_tag_integration_tests
        ],
        [verbose]
    ).

%% 运行性能测试
run_performance() ->
    io:format("~n运行性能测试...~n~n"),
    eunit:test(
        [
            msg_send_performance_tests,
            db_query_performance_tests,
            websocket_performance_tests
        ],
        [verbose]
    ).

%% 运行压力测试
run_stress() ->
    io:format("~n运行压力测试...~n~n"),
    eunit:test(
        [
            high_concurrency_stress_tests,
            group_member_limit_stress_tests
        ],
        [verbose]
    ).

%% 生成覆盖率报告
generate_coverage_report() ->
    io:format("~n生成覆盖率报告...~n"),
    % 使用 cover 工具生成报告
    cover:compile_beam_directory("ebin"),
    cover:analyze_to_file("coverage_report.html", html),
    io:format("覆盖率报告已生成: coverage_report.html~n").

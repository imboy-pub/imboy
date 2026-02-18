%% @doc ImBoy 测试套件主模块
%% 集成所有测试模块，提供统一的测试入口
-module(imboy_test_suite).

-include_lib("eunit/include/eunit.hrl").

%% 测试套件定义
all_tests_test_() ->
    {timeout, 600, [
        % 集成测试
        {"集成测试 - 消息转发", fun msg_forward_integration_tests:test_/0},
        {"集成测试 - 消息引用回复", fun msg_reply_integration_tests:test_/0},
        {"集成测试 - 会话置顶删除", fun conversation_pin_delete_integration_tests:test_/0},
        {"集成测试 - 消息表情回应", fun msg_reaction_integration_tests:test_/0},
        {"集成测试 - 群公告", fun group_notice_integration_tests:test_/0},
        {"集成测试 - @提及", fun mention_integration_tests:test_/0},
        {"集成测试 - 群成员角色", fun group_member_role_integration_tests:test_/0},
        {"集成测试 - 群分组标签", fun group_category_tag_integration_tests:test_/0},

        % 性能测试
        {"性能测试 - 消息发送", fun msg_send_performance_tests:test_/0},
        {"性能测试 - 数据库查询", fun db_query_performance_tests:test_/0},
        {"性能测试 - WebSocket", fun websocket_performance_tests:test_/0},

        % 压力测试
        {"压力测试 - 高并发消息", fun high_concurrency_stress_tests:test_/0},
        {"压力测试 - 群成员上限", fun group_member_limit_stress_tests:test_/0}
    ]}.

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
    eunit:test([
        msg_forward_integration_tests,
        msg_reply_integration_tests,
        conversation_pin_delete_integration_tests,
        msg_reaction_integration_tests,
        group_notice_integration_tests,
        mention_integration_tests,
        group_member_role_integration_tests,
        group_category_tag_integration_tests
    ], [verbose]).

%% 运行性能测试
run_performance() ->
    io:format("~n运行性能测试...~n~n"),
    eunit:test([
        msg_send_performance_tests,
        db_query_performance_tests,
        websocket_performance_tests
    ], [verbose]).

%% 运行压力测试
run_stress() ->
    io:format("~n运行压力测试...~n~n"),
    eunit:test([
        high_concurrency_stress_tests,
        group_member_limit_stress_tests
    ], [verbose]).

%% 生成覆盖率报告
generate_coverage_report() ->
    io:format("~n生成覆盖率报告...~n"),
    % 使用 cover 工具生成报告
    cover:compile_beam_directory("ebin"),
    cover:analyze_to_file("coverage_report.html", html),
    io:format("覆盖率报告已生成: coverage_report.html~n").

#!/bin/bash

# ImBoy 性能基准测试脚本
# 用于测试系统各项性能指标

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

# 项目根目录
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BENCHMARK_DIR="$PROJECT_ROOT/benchmark_results"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")

# 创建基准测试结果目录
mkdir -p "$BENCHMARK_DIR"

# 打印带颜色的消息
print_header() {
    echo -e "${PURPLE}========================================${NC}"
    echo -e "${PURPLE}$1${NC}"
    echo -e "${PURPLE}========================================${NC}"
}

print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_result() {
    echo -e "${CYAN}[RESULT]${NC} $1"
}

# 消息发送基准测试
benchmark_message_send() {
    print_header "消息发送基准测试"

    local result_file="$BENCHMARK_DIR/msg_send_benchmark_$TIMESTAMP.txt"

    print_info "测试消息发送性能..."
    print_info "结果将保存到: $result_file"

    cd "$PROJECT_ROOT"

    # 运行消息发送基准测试
    erl -noshell \
        -pa ebin \
        -eval "
            application:set_env(imboy, env, test),
            io:format(\"~n消息发送基准测试~n\"),
            io:format(\"========================================~n\"),

            % 测试参数
            TestCount = 1000,
            io:format(\"测试消息数: ~p~n\", [TestCount]),

            % 创建测试用户
            {ok, User1} = user_repo:create(#{
                uid => 100001,
                nickname => \"benchmark_user1\",
                account => \"bm1\",
                password => \"pass\"
            }),
            {ok, User2} = user_repo:create(#{
                uid => 100002,
                nickname => \"benchmark_user2\",
                account => \"bm2\",
                password => \"pass\"
            }),
            ok = friend_ds:add_friend(100001, 100002),

            % 预热
            io:format(\"~n预热中...~n\"),
            lists:foreach(fun(N) ->
                MsgId = imboy_hashid:uid(),
                msg_c2c_logic:c2c(MsgId, 100001, #{
                    payload => <<N:32, \"warmup\">>,
                    msg_type => <<\"text\">>,
                    to => elib_hashids:encode(100002),
                    created_at => erlang:system_time(millisecond)
                })
            end, lists:seq(1, 100)),

            % 正式测试
            io:format(\"~n开始正式测试...~n\"),
            StartTime = erlang:monotonic_time(millisecond),

            Times = lists:map(fun(N) ->
                MsgId = imboy_hashid:uid(),
                T1 = erlang:monotonic_time(millisecond),
                msg_c2c_logic:c2c(MsgId, 100001, #{
                    payload => <<N:32, \"benchmark\">>,
                    msg_type => <<\"text\">>,
                    to => elib_hashids:encode(100002),
                    created_at => erlang:system_time(millisecond)
                }),
                T2 = erlang:monotonic_time(millisecond),
                T2 - T1
            end, lists:seq(1, TestCount)),

            EndTime = erlang:monotonic_time(millisecond),
            TotalTime = EndTime - StartTime,

            % 计算统计数据
            AvgTime = lists:sum(Times) / length(Times),
            MaxTime = lists:max(Times),
            MinTime = lists:min(Times),

            % 输出结果
            io:format(\"~n========================================~n\"),
            io:format(\"测试结果:~n\"),
            io:format(\"  总消息数: ~p~n\", [TestCount]),
            io:format(\"  总耗时: ~p ms~n\", [TotalTime]),
            io:format(\"  平均耗时: ~.2f ms~n\", [AvgTime]),
            io:format(\"  最大耗时: ~p ms~n\", [MaxTime]),
            io:format(\"  最小耗时: ~p ms~n\", [MinTime]),
            io:format(\"  吞吐量: ~.2f msg/s~n\", [TestCount * 1000 / TotalTime]),
            io:format(\"========================================~n\")
        " \
        -s init stop 2>&1 | tee "$result_file"

    print_success "消息发送基准测试完成"
    echo ""
}

# 数据库查询基准测试
benchmark_database_query() {
    print_header "数据库查询基准测试"

    local result_file="$BENCHMARK_DIR/db_query_benchmark_$TIMESTAMP.txt"

    print_info "测试数据库查询性能..."
    print_info "结果将保存到: $result_file"

    cd "$PROJECT_ROOT"

    # 运行数据库查询基准测试
    erl -noshell \
        -pa ebin \
        -eval "
            application:set_env(imboy, env, test),
            io:format(\"~n数据库查询基准测试~n\"),
            io:format(\"========================================~n\"),

            % 测试简单查询
            io:format(\"~n1. 简单查询测试~n\"),
            SimpleTimes = lists:map(fun(_) ->
                T1 = erlang:monotonic_time(millisecond),
                {ok, _} = user_repo:find_by_uid(100001),
                T2 = erlang:monotonic_time(millisecond),
                T2 - T1
            end, lists:seq(1, 100)),

            SimpleAvg = lists:sum(SimpleTimes) / length(SimpleTimes),
            io:format(\"  简单查询平均耗时: ~.2f ms~n\", [SimpleAvg]),

            % 测试复杂查询
            io:format(\"~n2. 复杂查询测试~n\"),
            ComplexTimes = lists:map(fun(_) ->
                T1 = erlang:monotonic_time(millisecond),
                {ok, _} = friend_repo:list(100001),
                T2 = erlang:monotonic_time(millisecond),
                T2 - T1
            end, lists:seq(1, 50)),

            ComplexAvg = lists:sum(ComplexTimes) / length(ComplexTimes),
            io:format(\"  复杂查询平均耗时: ~.2f ms~n\", [ComplexAvg]),

            io:format(\"~n========================================~n\"),
            io:format(\"测试完成~n\"),
            io:format(\"========================================~n\")
        " \
        -s init stop 2>&1 | tee "$result_file"

    print_success "数据库查询基准测试完成"
    echo ""
}

# WebSocket 性能基准测试
benchmark_websocket() {
    print_header "WebSocket 性能基准测试"

    local result_file="$BENCHMARK_DIR/websocket_benchmark_$TIMESTAMP.txt"

    print_info "测试 WebSocket 性能..."
    print_info "结果将保存到: $result_file"

    cd "$PROJECT_ROOT"

    # 运行 WebSocket 基准测试
    erl -noshell \
        -pa ebin \
        -eval "
            application:set_env(imboy, env, test),
            io:format(\"~nWebSocket 性能基准测试~n\"),
            io:format(\"========================================~n\"),

            % 测试连接建立
            io:format(\"~n1. 连接建立测试~n\"),
            ConnectTimes = lists:map(fun(_) ->
                Token = token_ds:encrypt_token(100001),
                T1 = erlang:monotonic_time(millisecond),
                {ok, Pid} = websocket_ds:connect(100001, Token, #{}),
                T2 = erlang:monotonic_time(millisecond),
                websocket_ds:disconnect(Pid),
                T2 - T1
            end, lists:seq(1, 20)),

            ConnectAvg = lists:sum(ConnectTimes) / length(ConnectTimes),
            io:format(\"  连接建立平均耗时: ~.2f ms~n\", [ConnectAvg]),

            % 测试消息推送
            io:format(\"~n2. 消息推送测试~n\"),
            Token1 = token_ds:encrypt_token(100001),
            Token2 = token_ds:encrypt_token(100002),
            {ok, Pid1} = websocket_ds:connect(100001, Token1, #{}),
            {ok, Pid2} = websocket_ds:connect(100002, Token2, #{}),

            PushTimes = lists:map(fun(N) ->
                T1 = erlang:monotonic_time(millisecond),
                msg_s2c_ds:send(100001, [100002], <<\"benchmark\">>,
                    imboy_hashid:uid(), null, #{n => N}, nosave),
                T2 = erlang:monotonic_time(millisecond),
                T2 - T1
            end, lists:seq(1, 100)),

            PushAvg = lists:sum(PushTimes) / length(PushTimes),
            io:format(\"  消息推送平均耗时: ~.2f ms~n\", [PushAvg]),

            websocket_ds:disconnect(Pid1),
            websocket_ds:disconnect(Pid2),

            io:format(\"~n========================================~n\"),
            io:format(\"测试完成~n\"),
            io:format(\"========================================~n\")
        " \
        -s init stop 2>&1 | tee "$result_file"

    print_success "WebSocket 性能基准测试完成"
    echo ""
}

# 生成综合报告
generate_summary_report() {
    print_header "生成综合报告"

    local report_file="$BENCHMARK_DIR/summary_report_$TIMESTAMP.txt"

    cat > "$report_file" << EOF
========================================
ImBoy 性能基准测试报告
========================================
测试时间: $(date)
测试环境: $(uname -a)

$(cat "$BENCHMARK_DIR"/*benchmark_$TIMESTAMP.txt 2>/dev/null || echo "暂无测试结果")

========================================
测试结论
========================================
[请根据测试结果填写]

1. 消息发送性能:
   - 平均耗时: [结果]
   - 吞吐量: [结果]

2. 数据库查询性能:
   - 简单查询: [结果]
   - 复杂查询: [结果]

3. WebSocket 性能:
   - 连接建立: [结果]
   - 消息推送: [结果]

========================================
建议与改进
========================================
[请根据测试结果填写]

EOF

    print_success "综合报告已生成: $report_file"
    cat "$report_file"
}

# 主菜单
show_menu() {
    echo ""
    echo "请选择要运行的基准测试:"
    echo "  1. 消息发送基准测试"
    echo "  2. 数据库查询基准测试"
    echo "  3. WebSocket 性能基准测试"
    echo "  4. 运行所有基准测试"
    echo "  5. 生成综合报告"
    echo "  0. 退出"
    echo ""
    read -p "请输入选项 [0-5]: " choice

    case $choice in
        1)
            benchmark_message_send
            show_menu
            ;;
        2)
            benchmark_database_query
            show_menu
            ;;
        3)
            benchmark_websocket
            show_menu
            ;;
        4)
            benchmark_message_send
            benchmark_database_query
            benchmark_websocket
            generate_summary_report
            show_menu
            ;;
        5)
            generate_summary_report
            show_menu
            ;;
        0)
            print_info "退出基准测试"
            exit 0
            ;;
        *)
            print_error "无效选项，请重新选择"
            show_menu
            ;;
    esac
}

# 主函数
main() {
    print_header "ImBoy 性能基准测试工具"
    echo ""
    echo "此工具用于测试系统各项性能指标"
    echo ""

    # 如果有命令行参数，直接运行对应测试
    if [ -n "$1" ]; then
        case "$1" in
            msg|message)
                benchmark_message_send
                ;;
            db|database)
                benchmark_database_query
                ;;
            ws|websocket)
                benchmark_websocket
                ;;
            all)
                benchmark_message_send
                benchmark_database_query
                benchmark_websocket
                generate_summary_report
                ;;
            *)
                print_error "未知选项: $1"
                echo "用法: $0 [msg|db|ws|all]"
                exit 1
                ;;
        esac
    else
        # 交互式菜单
        show_menu
    fi
}

# 执行主函数
main "$@"

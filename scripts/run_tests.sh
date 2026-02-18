#!/bin/bash

# ImBoy 测试运行脚本
# 用法: ./run_tests.sh [选项]
#
# 选项:
#   all         运行所有测试（默认）
#   integration 运行集成测试
#   performance 运行性能测试
#   stress      运行压力测试
#   coverage    生成覆盖率报告
#   help        显示帮助信息

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 项目根目录
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TEST_DIR="$PROJECT_ROOT/test"
REPORT_DIR="$PROJECT_ROOT/test_reports"

# 打印帮助信息
print_help() {
    echo "ImBoy 测试运行脚本"
    echo ""
    echo "用法: $0 [选项]"
    echo ""
    echo "选项:"
    echo "  all         运行所有测试（默认）"
    echo "  integration 运行集成测试"
    echo "  performance 运行性能测试"
    echo "  stress      运行压力测试"
    echo "  coverage    生成覆盖率报告"
    echo "  help        显示帮助信息"
    echo ""
    echo "示例:"
    echo "  $0 all          # 运行所有测试"
    echo "  $0 integration  # 只运行集成测试"
    echo "  $0 coverage     # 生成覆盖率报告"
}

# 打印彩色消息
print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# 检查环境
check_environment() {
    print_info "检查测试环境..."

    # 检查 Erlang
    if ! command -v erl &> /dev/null; then
        print_error "未找到 Erlang，请先安装 Erlang/OTP"
        exit 1
    fi

    # 检查项目是否已编译
    if [ ! -d "$PROJECT_ROOT/ebin" ]; then
        print_warning "项目未编译，正在编译..."
        cd "$PROJECT_ROOT"
        make compile
    fi

    # 创建报告目录
    mkdir -p "$REPORT_DIR"

    print_success "环境检查完成"
}

# 运行集成测试
run_integration_tests() {
    print_info "开始运行集成测试..."
    echo "========================================"

    cd "$PROJECT_ROOT"

    START_TIME=$(date +%s%3N)

    # 运行集成测试
    erl -noshell \
        -pa ebin \
        -pa test/integration \
        -eval "application:set_env(imboy, env, test), eunit:test([
            msg_forward_integration_tests,
            msg_reply_integration_tests,
            conversation_pin_delete_integration_tests,
            msg_reaction_integration_tests,
            group_notice_integration_tests,
            mention_integration_tests,
            group_member_role_integration_tests,
            group_category_tag_integration_tests
        ], [verbose])" \
        -s init stop 2>&1 | tee "$REPORT_DIR/integration_test_report.txt"

    END_TIME=$(date +%s%3N)
    DURATION=$((END_TIME - START_TIME))

    echo ""
    echo "========================================"
    print_success "集成测试完成，耗时: ${DURATION}ms"
    echo "报告已保存到: $REPORT_DIR/integration_test_report.txt"
}

# 运行性能测试
run_performance_tests() {
    print_info "开始运行性能测试..."
    echo "========================================"

    cd "$PROJECT_ROOT"

    START_TIME=$(date +%s%3N)

    # 运行性能测试
    erl -noshell \
        -pa ebin \
        -pa test/performance \
        -eval "application:set_env(imboy, env, test), eunit:test([
            msg_send_performance_tests,
            db_query_performance_tests,
            websocket_performance_tests
        ], [verbose])" \
        -s init stop 2>&1 | tee "$REPORT_DIR/performance_test_report.txt"

    END_TIME=$(date +%s%3N)
    DURATION=$((END_TIME - START_TIME))

    echo ""
    echo "========================================"
    print_success "性能测试完成，耗时: ${DURATION}ms"
    echo "报告已保存到: $REPORT_DIR/performance_test_report.txt"
}

# 运行压力测试
run_stress_tests() {
    print_info "开始运行压力测试..."
    print_warning "压力测试可能需要较长时间..."
    echo "========================================"

    cd "$PROJECT_ROOT"

    START_TIME=$(date +%s%3N)

    # 运行压力测试
    erl -noshell \
        -pa ebin \
        -pa test/stress \
        -eval "application:set_env(imboy, env, test), eunit:test([
            high_concurrency_stress_tests,
            group_member_limit_stress_tests
        ], [verbose, {timeout, 600}])" \
        -s init stop 2>&1 | tee "$REPORT_DIR/stress_test_report.txt"

    END_TIME=$(date +%s%3N)
    DURATION=$((END_TIME - START_TIME))

    echo ""
    echo "========================================"
    print_success "压力测试完成，耗时: ${DURATION}ms"
    echo "报告已保存到: $REPORT_DIR/stress_test_report.txt"
}

# 运行所有测试
run_all_tests() {
    print_info "开始运行所有测试..."
    echo "========================================"

    TOTAL_START_TIME=$(date +%s%3N)

    # 运行集成测试
    run_integration_tests
    echo ""

    # 运行性能测试
    run_performance_tests
    echo ""

    # 运行压力测试
    run_stress_tests
    echo ""

    TOTAL_END_TIME=$(date +%s%3N)
    TOTAL_DURATION=$((TOTAL_END_TIME - TOTAL_START_TIME))

    echo "========================================"
    print_success "所有测试完成，总耗时: ${TOTAL_DURATION}ms"
    echo ""
    echo "测试报告目录: $REPORT_DIR"
    ls -la "$REPORT_DIR"
}

# 生成覆盖率报告
generate_coverage_report() {
    print_info "生成覆盖率报告..."
    echo "========================================"

    cd "$PROJECT_ROOT"

    # 使用 cover 编译
    erl -noshell \
        -pa ebin \
        -eval "
            cover:compile_beam_directory(\"ebin\"),
            cover:analyze_to_file(\"$REPORT_DIR/coverage_report.html\", html),
            cover:stop()
        " \
        -s init stop

    echo ""
    echo "========================================"
    print_success "覆盖率报告已生成: $REPORT_DIR/coverage_report.html"

    # 尝试在浏览器中打开报告
    if command -v open &> /dev/null; then
        open "$REPORT_DIR/coverage_report.html"
    elif command -v xdg-open &> /dev/null; then
        xdg-open "$REPORT_DIR/coverage_report.html"
    fi
}

# 主函数
main() {
    local option="${1:-all}"

    echo ""
    echo "========================================"
    echo "  ImBoy 测试套件"
    echo "========================================"
    echo ""

    case "$option" in
        all)
            check_environment
            run_all_tests
            ;;
        integration)
            check_environment
            run_integration_tests
            ;;
        performance)
            check_environment
            run_performance_tests
            ;;
        stress)
            check_environment
            run_stress_tests
            ;;
        coverage)
            check_environment
            generate_coverage_report
            ;;
        help|--help|-h)
            print_help
            ;;
        *)
            print_error "未知选项: $option"
            print_help
            exit 1
            ;;
    esac
}

# 执行主函数
main "$@"

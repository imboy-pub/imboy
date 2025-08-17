#!/bin/bash

# @doc IMBoy 调试助手脚本
# 用于简化生产环境调试和日志分析
# 使用方法: ./script/debug_helper.sh [command] [options]

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 项目根目录
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
LOG_DIR="$PROJECT_ROOT/log"
REL_BIN="$PROJECT_ROOT/_rel/imboy/bin/imboy"

# 日志函数
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_blue() {
    echo -e "${BLUE}[DEBUG]${NC} $1"
}

# 显示帮助信息
show_help() {
    cat << EOF
IMBoy 调试助手脚本

用法: $0 [command] [options]

命令:
  logs [tail|grep|analyze]     日志相关操作
  monitor [system|app]         系统监控
  debug [recon|observer]       调试工具
  cluster [status|nodes]       集群状态
  crash [analyze|list]         崩溃分析
  help                         显示此帮助信息

示例:
  $0 logs tail                 实时查看日志
  $0 logs grep "error"         搜索错误日志
  $0 logs analyze              分析日志统计
  $0 monitor system            系统资源监控
  $0 debug recon               启动 recon 调试
  $0 cluster status            查看集群状态
  $0 crash analyze             分析崩溃转储

EOF
}

# 检查依赖
check_dependencies() {
    if [ ! -d "$LOG_DIR" ]; then
        mkdir -p "$LOG_DIR"
        log_info "创建日志目录: $LOG_DIR"
    fi
    
    if [ ! -x "$REL_BIN" ]; then
        log_error "未找到 release 脚本: $REL_BIN"
        log_info "请先执行: make rel"
        exit 1
    fi
}

# 日志操作
handle_logs() {
    local action="$1"
    shift
    
    case "$action" in
        "tail")
            log_info "实时查看日志 (Ctrl+C 退出)"
            if [ -f "$LOG_DIR/debug.log" ]; then
                tail -f "$LOG_DIR/debug.log" "$LOG_DIR/error.log" 2>/dev/null || \
                tail -f "$LOG_DIR/debug.log" 2>/dev/null || \
                log_warn "日志文件不存在，请先启动应用"
            else
                log_warn "日志文件不存在，请先启动应用"
            fi
            ;;
        "grep")
            local pattern="$1"
            if [ -z "$pattern" ]; then
                log_error "请提供搜索模式"
                exit 1
            fi
            log_info "搜索日志中的模式: $pattern"
            find "$LOG_DIR" -name "*.log" -type f -exec grep -H "$pattern" {} \; 2>/dev/null || \
            log_warn "未找到匹配的日志内容"
            ;;
        "analyze")
            log_info "分析日志统计信息"
            analyze_logs
            ;;
        *)
            log_error "未知的日志操作: $action"
            show_help
            exit 1
            ;;
    esac
}

# 分析日志统计
analyze_logs() {
    if [ ! -d "$LOG_DIR" ] || [ -z "$(ls -A "$LOG_DIR" 2>/dev/null)" ]; then
        log_warn "日志目录为空或不存在"
        return
    fi
    
    echo "=== 日志文件统计 ==="
    find "$LOG_DIR" -name "*.log" -type f -exec ls -lh {} \; | \
    awk '{print $9 " (" $5 ")"}' | sort
    
    echo "\n=== 错误日志统计 ==="
    if [ -f "$LOG_DIR/error.log" ]; then
        echo "错误总数: $(wc -l < "$LOG_DIR/error.log")"
        echo "最近10条错误:"
        tail -10 "$LOG_DIR/error.log" | sed 's/^/  /'
    else
        echo "无错误日志文件"
    fi
    
    echo "\n=== 调试日志统计 ==="
    if [ -f "$LOG_DIR/debug.log" ]; then
        echo "调试日志总行数: $(wc -l < "$LOG_DIR/debug.log")"
        echo "今日日志行数: $(grep "$(date '+%Y-%m-%d')" "$LOG_DIR/debug.log" | wc -l)"
    else
        echo "无调试日志文件"
    fi
}

# 系统监控
handle_monitor() {
    local type="$1"
    
    case "$type" in
        "system")
            log_info "系统资源监控"
            system_monitor
            ;;
        "app")
            log_info "应用监控"
            app_monitor
            ;;
        *)
            log_error "未知的监控类型: $type"
            show_help
            exit 1
            ;;
    esac
}

# 系统资源监控
system_monitor() {
    echo "=== 系统资源使用情况 ==="
    echo "CPU 使用率:"
    top -l 1 | grep "CPU usage" || echo "无法获取 CPU 信息"
    
    echo "\n内存使用情况:"
    top -l 1 | grep "PhysMem" || echo "无法获取内存信息"
    
    echo "\n磁盘使用情况:"
    df -h | grep -E "(Filesystem|/dev/)"
    
    echo "\n网络连接:"
    netstat -an | grep LISTEN | grep -E ":(9800|9801|9802|4369)" || echo "未发现相关端口监听"
}

# 应用监控
app_monitor() {
    echo "=== Erlang 节点状态 ==="
    
    # 检查 EPMD
    if command -v epmd >/dev/null 2>&1; then
        echo "EPMD 注册的节点:"
        epmd -names 2>/dev/null || echo "EPMD 未运行或无注册节点"
    else
        echo "EPMD 命令不可用"
    fi
    
    echo "\n=== 进程信息 ==="
    ps aux | grep beam | grep -v grep || echo "未发现 Erlang 进程"
}

# 调试工具
handle_debug() {
    local tool="$1"
    
    case "$tool" in
        "recon")
            log_info "启动 recon 调试会话"
            start_recon_debug
            ;;
        "observer")
            log_info "启动 observer 工具"
            start_observer
            ;;
        *)
            log_error "未知的调试工具: $tool"
            show_help
            exit 1
            ;;
    esac
}

# 启动 recon 调试
start_recon_debug() {
    cat << 'EOF'
=== Recon 调试命令参考 ===

常用命令:
  recon:info/0                    - 系统信息概览
  recon:proc_count/2              - 进程统计
  recon:proc_window/3             - 进程窗口监控
  recon:bin_leak/1                - 二进制内存泄漏检测
  recon:node_stats/2              - 节点统计信息
  recon_trace:calls/2             - 函数调用跟踪

示例:
  recon:info().                   % 查看系统信息
  recon:proc_count(memory, 10).   % 内存使用最多的10个进程
  recon:proc_count(message_queue_len, 10). % 消息队列最长的10个进程

EOF
    
    "$REL_BIN" remote_console
}

# 启动 observer
start_observer() {
    log_info "尝试启动 observer (需要图形界面支持)"
    "$REL_BIN" rpc observer start
}

# 集群状态
handle_cluster() {
    local action="$1"
    
    case "$action" in
        "status")
            log_info "集群状态检查"
            cluster_status
            ;;
        "nodes")
            log_info "集群节点信息"
            cluster_nodes
            ;;
        *)
            log_error "未知的集群操作: $action"
            show_help
            exit 1
            ;;
    esac
}

# 集群状态检查
cluster_status() {
    echo "=== 集群状态 ==="
    
    # 检查节点连接
    "$REL_BIN" rpc nodes || log_warn "无法获取集群节点信息"
    
    # 检查应用状态
    echo "\n=== 应用状态 ==="
    "$REL_BIN" rpc application which_applications || log_warn "无法获取应用状态"
}

# 集群节点信息
cluster_nodes() {
    echo "=== 节点详细信息 ==="
    "$REL_BIN" rpc erlang nodes || log_warn "无法获取节点列表"
    
    echo "\n=== 当前节点信息 ==="
    "$REL_BIN" rpc erlang node || log_warn "无法获取当前节点信息"
}

# 崩溃分析
handle_crash() {
    local action="$1"
    
    case "$action" in
        "analyze")
            log_info "分析崩溃转储文件"
            analyze_crash_dump
            ;;
        "list")
            log_info "列出崩溃转储文件"
            list_crash_dumps
            ;;
        *)
            log_error "未知的崩溃分析操作: $action"
            show_help
            exit 1
            ;;
    esac
}

# 分析崩溃转储
analyze_crash_dump() {
    local crash_dump="$PROJECT_ROOT/erl_crash.dump"
    
    if [ -f "$crash_dump" ]; then
        log_info "使用现有的崩溃分析脚本"
        if [ -x "$PROJECT_ROOT/script/erl_crashdump_analyzer.sh" ]; then
            "$PROJECT_ROOT/script/erl_crashdump_analyzer.sh"
        else
            log_warn "崩溃分析脚本不存在或不可执行"
            log_info "手动分析崩溃转储: $crash_dump"
        fi
    else
        log_warn "未找到崩溃转储文件: $crash_dump"
    fi
}

# 列出崩溃转储文件
list_crash_dumps() {
    echo "=== 崩溃转储文件 ==="
    find "$PROJECT_ROOT" -name "erl_crash.dump*" -type f -exec ls -lh {} \; 2>/dev/null || \
    echo "未找到崩溃转储文件"
}

# 主函数
main() {
    local command="$1"
    shift || true
    
    check_dependencies
    
    case "$command" in
        "logs")
            handle_logs "$@"
            ;;
        "monitor")
            handle_monitor "$@"
            ;;
        "debug")
            handle_debug "$@"
            ;;
        "cluster")
            handle_cluster "$@"
            ;;
        "crash")
            handle_crash "$@"
            ;;
        "help"|"")
            show_help
            ;;
        *)
            log_error "未知命令: $command"
            show_help
            exit 1
            ;;
    esac
}

# 执行主函数
main "$@"
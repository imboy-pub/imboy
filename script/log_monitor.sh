#!/bin/bash

# @doc IMBoy 实时日志监控脚本
# 用于生产环境的日志实时监控和分析
# 使用方法: ./script/log_monitor.sh [options]

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# 项目根目录
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
LOG_DIR="$PROJECT_ROOT/log"
TMP_DIR="/tmp/imboy_log_monitor"

# 配置参数
REFRESH_INTERVAL=2
MAX_LINES=50
ERROR_THRESHOLD=10
WARN_THRESHOLD=20

# 创建临时目录
mkdir -p "$TMP_DIR"

# 清理函数
cleanup() {
    rm -rf "$TMP_DIR"
    echo -e "\n${GREEN}监控已停止${NC}"
    exit 0
}

# 设置信号处理
trap cleanup INT TERM

# 显示帮助信息
show_help() {
    cat << EOF
IMBoy 实时日志监控脚本

用法: $0 [options]

选项:
  -i, --interval SECONDS       刷新间隔 (默认: 2秒)
  -l, --lines NUMBER           显示行数 (默认: 50行)
  -e, --error-threshold NUM    错误阈值 (默认: 10)
  -w, --warn-threshold NUM     警告阈值 (默认: 20)
  -f, --filter PATTERN         过滤模式
  -t, --tail                   仅显示尾部日志
  -s, --stats                  显示统计信息
  -h, --help                   显示此帮助信息

示例:
  $0                           默认监控
  $0 -i 5 -l 100              5秒刷新，显示100行
  $0 -f "error"                只显示包含error的日志
  $0 -t                        尾部跟踪模式
  $0 -s                        统计模式

EOF
}

# 解析命令行参数
parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            -i|--interval)
                REFRESH_INTERVAL="$2"
                shift 2
                ;;
            -l|--lines)
                MAX_LINES="$2"
                shift 2
                ;;
            -e|--error-threshold)
                ERROR_THRESHOLD="$2"
                shift 2
                ;;
            -w|--warn-threshold)
                WARN_THRESHOLD="$2"
                shift 2
                ;;
            -f|--filter)
                FILTER_PATTERN="$2"
                shift 2
                ;;
            -t|--tail)
                TAIL_MODE=true
                shift
                ;;
            -s|--stats)
                STATS_MODE=true
                shift
                ;;
            -h|--help)
                show_help
                exit 0
                ;;
            *)
                echo "未知选项: $1"
                show_help
                exit 1
                ;;
        esac
    done
}

# 检查日志文件
check_log_files() {
    if [ ! -d "$LOG_DIR" ]; then
        echo -e "${RED}错误: 日志目录不存在: $LOG_DIR${NC}"
        echo -e "${YELLOW}提示: 请先启动应用生成日志文件${NC}"
        exit 1
    fi
    
    local log_files=("$LOG_DIR/debug.log" "$LOG_DIR/error.log")
    local found_files=()
    
    for file in "${log_files[@]}"; do
        if [ -f "$file" ]; then
            found_files+=("$file")
        fi
    done
    
    if [ ${#found_files[@]} -eq 0 ]; then
        echo -e "${YELLOW}警告: 未找到日志文件，请先启动应用${NC}"
        exit 1
    fi
    
    echo "${found_files[@]}"
}

# 获取日志统计信息
get_log_stats() {
    local debug_log="$LOG_DIR/debug.log"
    local error_log="$LOG_DIR/error.log"
    local stats_file="$TMP_DIR/stats.txt"
    
    {
        echo "=== 日志统计信息 ==="
        echo "时间: $(date '+%Y-%m-%d %H:%M:%S')"
        echo ""
        
        if [ -f "$debug_log" ]; then
            local total_lines=$(wc -l < "$debug_log" 2>/dev/null || echo "0")
            local today_lines=$(grep "$(date '+%Y-%m-%d')" "$debug_log" 2>/dev/null | wc -l || echo "0")
            echo "调试日志: 总计 $total_lines 行，今日 $today_lines 行"
        fi
        
        if [ -f "$error_log" ]; then
            local error_lines=$(wc -l < "$error_log" 2>/dev/null || echo "0")
            local today_errors=$(grep "$(date '+%Y-%m-%d')" "$error_log" 2>/dev/null | wc -l || echo "0")
            echo "错误日志: 总计 $error_lines 行，今日 $today_errors 行"
            
            if [ "$today_errors" -gt "$ERROR_THRESHOLD" ]; then
                echo -e "${RED}⚠️  今日错误数量超过阈值 ($ERROR_THRESHOLD)${NC}"
            fi
        fi
        
        echo ""
        echo "=== 最近错误类型统计 ==="
        if [ -f "$error_log" ]; then
            tail -100 "$error_log" 2>/dev/null | \
            grep -oE '\[error\].*' | \
            sed 's/\[error\][^]]*]//' | \
            sort | uniq -c | sort -nr | head -5 || echo "无错误记录"
        else
            echo "无错误日志文件"
        fi
        
        echo ""
        echo "=== 系统资源使用 ==="
        echo "内存使用: $(ps aux | grep beam | grep -v grep | awk '{sum+=$6} END {printf "%.1f MB\n", sum/1024}' 2>/dev/null || echo "未知")"
        echo "进程数量: $(ps aux | grep beam | grep -v grep | wc -l || echo "0")"
        
    } > "$stats_file"
    
    cat "$stats_file"
}

# 格式化日志行
format_log_line() {
    local line="$1"
    
    # 根据日志级别着色
    if echo "$line" | grep -q "\[error\]"; then
        echo -e "${RED}$line${NC}"
    elif echo "$line" | grep -q "\[warning\]"; then
        echo -e "${YELLOW}$line${NC}"
    elif echo "$line" | grep -q "\[info\]"; then
        echo -e "${GREEN}$line${NC}"
    elif echo "$line" | grep -q "\[debug\]"; then
        echo -e "${BLUE}$line${NC}"
    else
        echo "$line"
    fi
}

# 实时监控模式
real_time_monitor() {
    local log_files
    log_files=($(check_log_files))
    
    echo -e "${GREEN}开始实时监控日志文件...${NC}"
    echo -e "${CYAN}监控文件: ${log_files[*]}${NC}"
    echo -e "${CYAN}刷新间隔: ${REFRESH_INTERVAL}秒，显示行数: ${MAX_LINES}${NC}"
    echo -e "${YELLOW}按 Ctrl+C 停止监控${NC}"
    echo ""
    
    while true; do
        clear
        echo -e "${MAGENTA}=== IMBoy 实时日志监控 ===${NC}"
        echo -e "${CYAN}时间: $(date '+%Y-%m-%d %H:%M:%S')${NC}"
        echo ""
        
        # 显示统计信息
        if [ "$STATS_MODE" = true ]; then
            get_log_stats
            echo ""
        fi
        
        echo -e "${BLUE}=== 最新日志 ===${NC}"
        
        # 合并并排序日志
        local temp_log="$TMP_DIR/merged.log"
        > "$temp_log"
        
        for log_file in "${log_files[@]}"; do
            if [ -f "$log_file" ]; then
                tail -n "$MAX_LINES" "$log_file" >> "$temp_log"
            fi
        done
        
        # 应用过滤器
        if [ -n "$FILTER_PATTERN" ]; then
            grep "$FILTER_PATTERN" "$temp_log" | sort | tail -n "$MAX_LINES" | while IFS= read -r line; do
                format_log_line "$line"
            done
        else
            sort "$temp_log" | tail -n "$MAX_LINES" | while IFS= read -r line; do
                format_log_line "$line"
            done
        fi
        
        sleep "$REFRESH_INTERVAL"
    done
}

# 尾部跟踪模式
tail_mode() {
    local log_files
    log_files=($(check_log_files))
    
    echo -e "${GREEN}尾部跟踪模式 (Ctrl+C 退出)${NC}"
    echo -e "${CYAN}监控文件: ${log_files[*]}${NC}"
    echo ""
    
    if [ -n "$FILTER_PATTERN" ]; then
        tail -f "${log_files[@]}" 2>/dev/null | grep --line-buffered "$FILTER_PATTERN" | while IFS= read -r line; do
            format_log_line "$line"
        done
    else
        tail -f "${log_files[@]}" 2>/dev/null | while IFS= read -r line; do
            format_log_line "$line"
        done
    fi
}

# 统计模式
stats_only_mode() {
    echo -e "${GREEN}日志统计模式${NC}"
    echo ""
    get_log_stats
}

# 主函数
main() {
    parse_args "$@"
    
    if [ "$STATS_MODE" = true ] && [ "$TAIL_MODE" != true ]; then
        stats_only_mode
    elif [ "$TAIL_MODE" = true ]; then
        tail_mode
    else
        real_time_monitor
    fi
}

# 执行主函数
main "$@"
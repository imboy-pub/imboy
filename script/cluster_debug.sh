#!/bin/bash

# IMBoy 集群调试工具
# 用于简化集群环境下的调试和监控操作

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 项目根目录
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RELEASE_DIR="${PROJECT_ROOT}/_rel/imboy"
LOG_DIR="${PROJECT_ROOT}/log"

# 默认节点配置
DEFAULT_NODES=("node1@127.0.0.1" "node2@127.0.0.1")
DEFAULT_COOKIE="imboy_cluster_cookie"

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

log_debug() {
    echo -e "${BLUE}[DEBUG]${NC} $1"
}

# 显示帮助信息
show_help() {
    cat << EOF
IMBoy 集群调试工具

用法: $0 <命令> [选项]

命令:
  status              显示集群状态
  nodes               列出所有节点
  ping <node>         测试节点连通性
  connect <node>      连接到指定节点
  logs <node>         查看节点日志
  monitor             监控集群状态
  sync                同步集群数据
  rebalance           重新平衡集群负载
  health              集群健康检查
  debug <node>        在指定节点启动调试会话
  trace <node> <mod>  在指定节点跟踪模块
  memory <node>       查看节点内存使用
  processes <node>    查看节点进程信息
  restart <node>      重启指定节点
  stop <node>         停止指定节点
  start <node>        启动指定节点
  backup              备份集群数据
  restore <file>      恢复集群数据
  help                显示此帮助信息

选项:
  -c, --cookie <cookie>   指定集群 cookie
  -n, --nodes <nodes>     指定节点列表（逗号分隔）
  -t, --timeout <sec>     设置操作超时时间
  -v, --verbose           详细输出
  -q, --quiet             静默模式

示例:
  $0 status                           # 显示集群状态
  $0 ping node1@127.0.0.1            # 测试节点连通性
  $0 logs node1                       # 查看 node1 日志
  $0 debug node2@127.0.0.1           # 在 node2 启动调试
  $0 trace node1 user_logic          # 跟踪 user_logic 模块
  $0 -n "node1,node2,node3" status   # 指定节点列表

EOF
}

# 解析命令行参数
parse_args() {
    COMMAND=""
    NODES=("${DEFAULT_NODES[@]}")
    COOKIE="$DEFAULT_COOKIE"
    TIMEOUT=30
    VERBOSE=false
    QUIET=false
    
    while [[ $# -gt 0 ]]; do
        case $1 in
            -c|--cookie)
                COOKIE="$2"
                shift 2
                ;;
            -n|--nodes)
                IFS=',' read -ra NODES <<< "$2"
                shift 2
                ;;
            -t|--timeout)
                TIMEOUT="$2"
                shift 2
                ;;
            -v|--verbose)
                VERBOSE=true
                shift
                ;;
            -q|--quiet)
                QUIET=true
                shift
                ;;
            help|--help|-h)
                show_help
                exit 0
                ;;
            *)
                if [[ -z "$COMMAND" ]]; then
                    COMMAND="$1"
                else
                    ARGS+=("$1")
                fi
                shift
                ;;
        esac
    done
    
    if [[ -z "$COMMAND" ]]; then
        log_error "请指定命令"
        show_help
        exit 1
    fi
}

# 检查节点是否在线
check_node_online() {
    local node="$1"
    local result
    
    result=$("$RELEASE_DIR/bin/imboy" rpc net_adm ping "'$node'" 2>/dev/null || echo "pang")
    
    if [[ "$result" == "pong" ]]; then
        return 0
    else
        return 1
    fi
}

# 执行 RPC 调用
exec_rpc() {
    local node="$1"
    local module="$2"
    local function="$3"
    shift 3
    local args="$*"
    
    if [[ -n "$args" ]]; then
        "$RELEASE_DIR/bin/imboy" rpc "$module" "$function" "$args" --name="$node" --cookie="$COOKIE"
    else
        "$RELEASE_DIR/bin/imboy" rpc "$module" "$function" --name="$node" --cookie="$COOKIE"
    fi
}

# 显示集群状态
show_cluster_status() {
    log_info "检查集群状态..."
    
    local online_nodes=0
    local total_nodes=${#NODES[@]}
    
    echo -e "\n${BLUE}节点状态:${NC}"
    printf "%-25s %-10s %-15s %-10s\n" "节点" "状态" "内存使用" "进程数"
    printf "%-25s %-10s %-15s %-10s\n" "----" "----" "--------" "------"
    
    for node in "${NODES[@]}"; do
        if check_node_online "$node"; then
            ((online_nodes++))
            
            # 获取内存使用情况
            local memory=$(exec_rpc "$node" "erlang" "memory" "total" 2>/dev/null | grep -o '[0-9]*' | head -1 || echo "N/A")
            if [[ "$memory" != "N/A" ]]; then
                memory="$((memory / 1024 / 1024))MB"
            fi
            
            # 获取进程数
            local proc_count=$(exec_rpc "$node" "erlang" "system_info" "process_count" 2>/dev/null || echo "N/A")
            
            printf "%-25s ${GREEN}%-10s${NC} %-15s %-10s\n" "$node" "在线" "$memory" "$proc_count"
        else
            printf "%-25s ${RED}%-10s${NC} %-15s %-10s\n" "$node" "离线" "N/A" "N/A"
        fi
    done
    
    echo -e "\n${BLUE}集群概览:${NC}"
    echo "总节点数: $total_nodes"
    echo "在线节点: $online_nodes"
    echo "离线节点: $((total_nodes - online_nodes))"
    
    if [[ $online_nodes -gt 0 ]]; then
        echo -e "\n${BLUE}集群连接:${NC}"
        local first_online_node=""
        for node in "${NODES[@]}"; do
            if check_node_online "$node"; then
                first_online_node="$node"
                break
            fi
        done
        
        if [[ -n "$first_online_node" ]]; then
            local connected_nodes=$(exec_rpc "$first_online_node" "erlang" "nodes" 2>/dev/null || echo "[]")
            echo "已连接节点: $connected_nodes"
        fi
    fi
}

# 列出所有节点
list_nodes() {
    log_info "节点列表:"
    for i in "${!NODES[@]}"; do
        local node="${NODES[$i]}"
        local status="离线"
        local color="$RED"
        
        if check_node_online "$node"; then
            status="在线"
            color="$GREEN"
        fi
        
        echo -e "  $((i+1)). ${color}$node${NC} ($status)"
    done
}

# 测试节点连通性
ping_node() {
    local node="$1"
    
    if [[ -z "$node" ]]; then
        log_error "请指定要测试的节点"
        exit 1
    fi
    
    log_info "测试节点连通性: $node"
    
    if check_node_online "$node"; then
        log_info "节点 $node 在线"
        
        # 获取节点详细信息
        local uptime=$(exec_rpc "$node" "erlang" "statistics" "wall_clock" 2>/dev/null || echo "N/A")
        local version=$(exec_rpc "$node" "erlang" "system_info" "version" 2>/dev/null || echo "N/A")
        
        echo "  运行时间: $uptime"
        echo "  Erlang版本: $version"
    else
        log_error "节点 $node 离线或无法连接"
        exit 1
    fi
}

# 连接到指定节点
connect_node() {
    local node="$1"
    
    if [[ -z "$node" ]]; then
        log_error "请指定要连接的节点"
        exit 1
    fi
    
    if ! check_node_online "$node"; then
        log_error "节点 $node 离线或无法连接"
        exit 1
    fi
    
    log_info "连接到节点: $node"
    "$RELEASE_DIR/bin/imboy" remote_console --name="$node" --cookie="$COOKIE"
}

# 查看节点日志
view_node_logs() {
    local node="$1"
    local lines=${2:-100}
    
    if [[ -z "$node" ]]; then
        log_error "请指定节点名称"
        exit 1
    fi
    
    # 提取节点名称（去掉@后面的部分）
    local node_name=$(echo "$node" | cut -d'@' -f1)
    local log_file="$LOG_DIR/${node_name}.log"
    
    if [[ ! -f "$log_file" ]]; then
        log_file="$LOG_DIR/debug.log"
    fi
    
    if [[ -f "$log_file" ]]; then
        log_info "查看节点 $node 日志 (最近 $lines 行):"
        tail -n "$lines" "$log_file" | while IFS= read -r line; do
            # 简单的日志着色
            if [[ "$line" =~ ERROR ]]; then
                echo -e "${RED}$line${NC}"
            elif [[ "$line" =~ WARNING ]]; then
                echo -e "${YELLOW}$line${NC}"
            elif [[ "$line" =~ INFO ]]; then
                echo -e "${GREEN}$line${NC}"
            else
                echo "$line"
            fi
        done
    else
        log_error "找不到节点 $node 的日志文件"
        exit 1
    fi
}

# 监控集群状态
monitor_cluster() {
    local interval=${1:-5}
    
    log_info "开始监控集群状态 (刷新间隔: ${interval}秒, 按 Ctrl+C 退出)"
    
    while true; do
        clear
        echo "=== IMBoy 集群监控 ==="
        echo "时间: $(date)"
        show_cluster_status
        sleep "$interval"
    done
}

# 集群健康检查
health_check() {
    log_info "执行集群健康检查..."
    
    local issues=0
    
    # 检查节点状态
    echo -e "\n${BLUE}1. 节点状态检查${NC}"
    for node in "${NODES[@]}"; do
        if check_node_online "$node"; then
            echo -e "  ${GREEN}✓${NC} $node 在线"
        else
            echo -e "  ${RED}✗${NC} $node 离线"
            ((issues++))
        fi
    done
    
    # 检查集群连接
    echo -e "\n${BLUE}2. 集群连接检查${NC}"
    local online_nodes=()
    for node in "${NODES[@]}"; do
        if check_node_online "$node"; then
            online_nodes+=("$node")
        fi
    done
    
    if [[ ${#online_nodes[@]} -gt 1 ]]; then
        local first_node="${online_nodes[0]}"
        local connected_nodes=$(exec_rpc "$first_node" "erlang" "nodes" 2>/dev/null || echo "[]")
        local connected_count=$(echo "$connected_nodes" | grep -o "'[^']*'" | wc -l)
        
        if [[ $connected_count -eq $((${#online_nodes[@]} - 1)) ]]; then
            echo -e "  ${GREEN}✓${NC} 所有在线节点已连接"
        else
            echo -e "  ${RED}✗${NC} 节点连接不完整"
            echo "    预期连接: $((${#online_nodes[@]} - 1)), 实际连接: $connected_count"
            ((issues++))
        fi
    else
        echo -e "  ${YELLOW}!${NC} 在线节点少于2个，无法检查集群连接"
    fi
    
    # 检查应用状态
    echo -e "\n${BLUE}3. 应用状态检查${NC}"
    for node in "${online_nodes[@]}"; do
        local apps=$(exec_rpc "$node" "application" "which_applications" 2>/dev/null || echo "error")
        if [[ "$apps" != "error" ]] && echo "$apps" | grep -q "imboy"; then
            echo -e "  ${GREEN}✓${NC} $node 应用运行正常"
        else
            echo -e "  ${RED}✗${NC} $node 应用状态异常"
            ((issues++))
        fi
    done
    
    # 检查内存使用
    echo -e "\n${BLUE}4. 内存使用检查${NC}"
    for node in "${online_nodes[@]}"; do
        local memory=$(exec_rpc "$node" "erlang" "memory" "total" 2>/dev/null | grep -o '[0-9]*' | head -1 || echo "0")
        local memory_mb=$((memory / 1024 / 1024))
        
        if [[ $memory_mb -lt 1000 ]]; then
            echo -e "  ${GREEN}✓${NC} $node 内存使用正常 (${memory_mb}MB)"
        elif [[ $memory_mb -lt 2000 ]]; then
            echo -e "  ${YELLOW}!${NC} $node 内存使用较高 (${memory_mb}MB)"
        else
            echo -e "  ${RED}✗${NC} $node 内存使用过高 (${memory_mb}MB)"
            ((issues++))
        fi
    done
    
    # 总结
    echo -e "\n${BLUE}健康检查结果:${NC}"
    if [[ $issues -eq 0 ]]; then
        echo -e "${GREEN}✓ 集群状态良好，未发现问题${NC}"
    else
        echo -e "${RED}✗ 发现 $issues 个问题，需要关注${NC}"
    fi
    
    return $issues
}

# 在指定节点启动调试会话
debug_node() {
    local node="$1"
    
    if [[ -z "$node" ]]; then
        log_error "请指定要调试的节点"
        exit 1
    fi
    
    if ! check_node_online "$node"; then
        log_error "节点 $node 离线或无法连接"
        exit 1
    fi
    
    log_info "在节点 $node 启动调试会话"
    
    # 启动 recon 和 observer_cli
    cat << EOF | "$RELEASE_DIR/bin/imboy" remote_console --name="$node" --cookie="$COOKIE"
%% 启动调试工具
recon:info().
observer_cli:start().
EOF
}

# 跟踪模块
trace_module() {
    local node="$1"
    local module="$2"
    local time=${3:-10000}
    
    if [[ -z "$node" ]] || [[ -z "$module" ]]; then
        log_error "请指定节点和模块名称"
        exit 1
    fi
    
    if ! check_node_online "$node"; then
        log_error "节点 $node 离线或无法连接"
        exit 1
    fi
    
    log_info "在节点 $node 跟踪模块 $module (时间: ${time}ms)"
    
    exec_rpc "$node" "redbug" "start" "\"$module:*\"" "[{time, $time}]"
}

# 查看节点内存使用
view_memory() {
    local node="$1"
    
    if [[ -z "$node" ]]; then
        log_error "请指定节点名称"
        exit 1
    fi
    
    if ! check_node_online "$node"; then
        log_error "节点 $node 离线或无法连接"
        exit 1
    fi
    
    log_info "查看节点 $node 内存使用情况:"
    
    # 获取内存统计
    local memory_info=$(exec_rpc "$node" "erlang" "memory" 2>/dev/null)
    echo "$memory_info" | sed 's/,/\n/g' | sed 's/[\[\]{}]//g' | while IFS= read -r line; do
        if [[ -n "$line" ]]; then
            echo "  $line"
        fi
    done
    
    echo -e "\n内存使用最多的10个进程:"
    exec_rpc "$node" "recon" "proc_count" "memory" "10"
}

# 查看节点进程信息
view_processes() {
    local node="$1"
    
    if [[ -z "$node" ]]; then
        log_error "请指定节点名称"
        exit 1
    fi
    
    if ! check_node_online "$node"; then
        log_error "节点 $node 离线或无法连接"
        exit 1
    fi
    
    log_info "查看节点 $node 进程信息:"
    
    # 进程统计
    local proc_count=$(exec_rpc "$node" "erlang" "system_info" "process_count")
    local proc_limit=$(exec_rpc "$node" "erlang" "system_info" "process_limit")
    
    echo "进程数量: $proc_count / $proc_limit"
    
    echo -e "\n内存使用最多的10个进程:"
    exec_rpc "$node" "recon" "proc_count" "memory" "10"
    
    echo -e "\n消息队列最长的10个进程:"
    exec_rpc "$node" "recon" "proc_count" "message_queue_len" "10"
}

# 重启节点
restart_node() {
    local node="$1"
    
    if [[ -z "$node" ]]; then
        log_error "请指定要重启的节点"
        exit 1
    fi
    
    log_warn "重启节点: $node"
    read -p "确认重启节点 $node? (y/N): " -n 1 -r
    echo
    
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        if check_node_online "$node"; then
            log_info "停止节点 $node..."
            exec_rpc "$node" "init" "stop"
            sleep 2
        fi
        
        log_info "启动节点 $node..."
        # 这里需要根据实际的节点启动脚本调整
        "$PROJECT_ROOT/script/start_node.sh" "$node" &
        
        # 等待节点启动
        local retries=10
        while [[ $retries -gt 0 ]]; do
            if check_node_online "$node"; then
                log_info "节点 $node 启动成功"
                return 0
            fi
            sleep 2
            ((retries--))
        done
        
        log_error "节点 $node 启动失败"
        exit 1
    else
        log_info "取消重启操作"
    fi
}

# 主函数
main() {
    parse_args "$@"
    
    # 检查项目环境
    if [[ ! -d "$RELEASE_DIR" ]]; then
        log_error "找不到 release 目录: $RELEASE_DIR"
        log_error "请先编译项目: make rel"
        exit 1
    fi
    
    case "$COMMAND" in
        status)
            show_cluster_status
            ;;
        nodes)
            list_nodes
            ;;
        ping)
            ping_node "${ARGS[0]}"
            ;;
        connect)
            connect_node "${ARGS[0]}"
            ;;
        logs)
            view_node_logs "${ARGS[0]}" "${ARGS[1]}"
            ;;
        monitor)
            monitor_cluster "${ARGS[0]}"
            ;;
        health)
            health_check
            ;;
        debug)
            debug_node "${ARGS[0]}"
            ;;
        trace)
            trace_module "${ARGS[0]}" "${ARGS[1]}" "${ARGS[2]}"
            ;;
        memory)
            view_memory "${ARGS[0]}"
            ;;
        processes)
            view_processes "${ARGS[0]}"
            ;;
        restart)
            restart_node "${ARGS[0]}"
            ;;
        *)
            log_error "未知命令: $COMMAND"
            show_help
            exit 1
            ;;
    esac
}

# 脚本入口
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
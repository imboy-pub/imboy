#!/usr/bin/env bash
# IMBoy WebSocket 负载基准测试脚本
# 使用 Erlang 原生客户端，无需额外依赖
#
# 用法:
#   bash scripts/benchmark_ws.sh [选项]
#
# 选项:
#   --host HOST         WebSocket 服务器地址 (默认: 127.0.0.1)
#   --port PORT         WebSocket 服务器端口 (默认: 9800)
#   --clients N         并发客户端数 (默认: 100)
#   --duration SEC      测试持续时间（秒）(默认: 60)
#   --msg-rate N        每客户端每秒消息数 (默认: 1)
#   --token TOKEN       JWT 认证 Token
#   --report-dir DIR    报告输出目录 (默认: docs/benchmark/)
#
# 示例:
#   # 100 客户端，每秒 1 条消息，持续 60 秒
#   bash scripts/benchmark_ws.sh --clients 100 --duration 60
#
#   # 高负载测试
#   bash scripts/benchmark_ws.sh --clients 500 --msg-rate 5 --duration 120

set -euo pipefail

# ─── 默认参数 ─────────────────────────────────
HOST="${BENCH_HOST:-127.0.0.1}"
PORT="${BENCH_PORT:-9800}"
CLIENTS=100
DURATION=60
MSG_RATE=1
TOKEN=""
REPORT_DIR="docs/benchmark"

# ─── 参数解析 ─────────────────────────────────
while [[ $# -gt 0 ]]; do
    case $1 in
        --host)      HOST="$2";       shift 2 ;;
        --port)      PORT="$2";       shift 2 ;;
        --clients)   CLIENTS="$2";    shift 2 ;;
        --duration)  DURATION="$2";   shift 2 ;;
        --msg-rate)  MSG_RATE="$2";   shift 2 ;;
        --token)     TOKEN="$2";      shift 2 ;;
        --report-dir) REPORT_DIR="$2"; shift 2 ;;
        --help|-h)
            head -25 "$0" | tail -22
            exit 0
            ;;
        *)
            echo "未知参数: $1"
            exit 1
            ;;
    esac
done

# ─── 环境检查 ─────────────────────────────────
cd "$(dirname "$0")/.."

if ! command -v erl &> /dev/null; then
    echo "错误: 未找到 erl 命令，请安装 Erlang/OTP"
    exit 1
fi

mkdir -p "$REPORT_DIR"

TIMESTAMP=$(date +%Y%m%d_%H%M%S)
REPORT_FILE="$REPORT_DIR/bench_${TIMESTAMP}.txt"

# ─── 输出配置 ─────────────────────────────────
echo "=================================================="
echo "  IMBoy WebSocket 负载基准测试"
echo "=================================================="
echo "  服务器:    ws://${HOST}:${PORT}/ws"
echo "  并发客户端: ${CLIENTS}"
echo "  持续时间:  ${DURATION}s"
echo "  消息速率:  ${MSG_RATE} msg/s/client"
echo "  预计 QPS:  $((CLIENTS * MSG_RATE)) msg/s"
echo "  报告文件:  ${REPORT_FILE}"
echo "=================================================="
echo ""

# ─── 预检：服务器是否可达 ─────────────────────
echo "[1/4] 检查服务器连通性..."
if curl -s -o /dev/null -w "%{http_code}" "http://${HOST}:${PORT}/v1/init" | grep -q "200"; then
    echo "  ✓ 服务器可达"
else
    echo "  ✗ 无法连接到 http://${HOST}:${PORT}/v1/init"
    echo "  请确认服务器已启动"
    exit 1
fi

# ─── 基线指标 ─────────────────────────────────
echo ""
echo "[2/4] 采集基线指标..."

# 系统基线
echo "--- 系统基线 ---" > "$REPORT_FILE"
echo "时间: $(date -u +%Y-%m-%dT%H:%M:%SZ)" >> "$REPORT_FILE"
echo "配置: clients=${CLIENTS} duration=${DURATION}s msg_rate=${MSG_RATE}" >> "$REPORT_FILE"
echo "" >> "$REPORT_FILE"

# CPU 和内存
echo "--- CPU/Memory ---" >> "$REPORT_FILE"
if [[ "$(uname)" == "Darwin" ]]; then
    sysctl -n hw.ncpu >> "$REPORT_FILE" 2>/dev/null || true
    vm_stat | head -5 >> "$REPORT_FILE" 2>/dev/null || true
else
    nproc >> "$REPORT_FILE" 2>/dev/null || true
    free -h >> "$REPORT_FILE" 2>/dev/null || true
fi
echo "" >> "$REPORT_FILE"

echo "  ✓ 基线已记录"

# ─── HTTP API 基准 ────────────────────────────
echo ""
echo "[3/4] HTTP API 延迟基准..."

echo "--- HTTP API 延迟 (curl, 10 次采样) ---" >> "$REPORT_FILE"
LATENCIES=()
for i in $(seq 1 10); do
    LATENCY=$(curl -s -o /dev/null -w "%{time_total}" "http://${HOST}:${PORT}/v1/init")
    LATENCIES+=("$LATENCY")
    printf "  请求 %2d: %s s\n" "$i" "$LATENCY"
done

# 计算统计
echo "" >> "$REPORT_FILE"
printf '%s\n' "${LATENCIES[@]}" | sort -n | awk '
    { a[NR] = $1; sum += $1 }
    END {
        printf "  样本数: %d\n", NR
        printf "  平均值: %.4f s\n", sum/NR
        printf "  P50:    %.4f s\n", a[int(NR*0.5)+1]
        printf "  P90:    %.4f s\n", a[int(NR*0.9)+1]
        printf "  P99:    %.4f s\n", a[int(NR*0.99)+1]
        printf "  最小值: %.4f s\n", a[1]
        printf "  最大值: %.4f s\n", a[NR]
    }
' | tee -a "$REPORT_FILE"

# ─── WebSocket 并发测试说明 ───────────────────
echo ""
echo "[4/4] WebSocket 并发测试..."
echo ""
echo "--- WebSocket 并发测试 ---" >> "$REPORT_FILE"

if [ -z "$TOKEN" ]; then
    echo "  ⚠ 未提供 --token 参数"
    echo "  WebSocket 连接需要有效的 JWT Token"
    echo ""
    echo "  获取 Token 方式:"
    echo "    1. 在 Erlang shell 中: token_ds:encrypt_token(Uid)"
    echo "    2. 通过 API 登录获取"
    echo ""
    echo "  然后运行:"
    echo "    bash scripts/benchmark_ws.sh --token YOUR_TOKEN --clients $CLIENTS"
    echo ""
    echo "  跳过 WebSocket 压测，仅生成 HTTP 基准报告"
    echo "" >> "$REPORT_FILE"
    echo "WebSocket 测试跳过: 未提供 Token" >> "$REPORT_FILE"
else
    # 使用 Erlang 脚本执行 WebSocket 并发测试
    echo "  启动 ${CLIENTS} 个 WebSocket 客户端..."
    echo "  每客户端发送速率: ${MSG_RATE} msg/s"
    echo "  持续时间: ${DURATION}s"
    echo ""

    erl -noshell -eval "
        Host = \"${HOST}\",
        Port = ${PORT},
        Clients = ${CLIENTS},
        Duration = ${DURATION},
        MsgRate = ${MSG_RATE},
        Token = <<\"${TOKEN}\">>,

        %% 启动必要的应用
        application:ensure_all_started(gun),

        %% 记录连接成功/失败/消息延迟
        ets:new(bench_stats, [named_table, public, {write_concurrency, true}]),
        ets:insert(bench_stats, {connected, 0}),
        ets:insert(bench_stats, {failed, 0}),
        ets:insert(bench_stats, {msg_sent, 0}),
        ets:insert(bench_stats, {msg_recv, 0}),

        io:format(\"开始连接 ~p 个客户端...~n\", [Clients]),

        %% 启动客户端进程
        lists:foreach(fun(I) ->
            spawn(fun() ->
                timer:sleep(I * 10), %% 分散连接
                try
                    {ok, Pid} = gun:open(Host, Port),
                    {ok, _} = gun:await_up(Pid, 5000),
                    WsPath = \"/ws?token=\" ++ binary_to_list(Token),
                    Ref = gun:ws_upgrade(Pid, WsPath),
                    receive
                        {gun_upgrade, Pid, Ref, _, _} ->
                            ets:update_counter(bench_stats, connected, 1),
                            %% 发送消息循环
                            bench_loop(Pid, Duration * 1000, MsgRate)
                    after 5000 ->
                        ets:update_counter(bench_stats, failed, 1)
                    end
                catch _:_ ->
                    ets:update_counter(bench_stats, failed, 1)
                end
            end)
        end, lists:seq(1, Clients)),

        %% 等待测试完成
        timer:sleep((Duration + 10) * 1000),

        [{_, Connected}] = ets:lookup(bench_stats, connected),
        [{_, Failed}] = ets:lookup(bench_stats, failed),
        [{_, Sent}] = ets:lookup(bench_stats, msg_sent),
        [{_, Recv}] = ets:lookup(bench_stats, msg_recv),

        io:format(\"~n=== 测试结果 ===~n\"),
        io:format(\"连接成功: ~p~n\", [Connected]),
        io:format(\"连接失败: ~p~n\", [Failed]),
        io:format(\"消息发送: ~p~n\", [Sent]),
        io:format(\"消息接收: ~p~n\", [Recv]),
        io:format(\"吞吐量:   ~.1f msg/s~n\", [Sent / max(1, Duration)]),
        init:stop()
    " -pa _build/default/lib/*/ebin 2>&1 | tee -a "$REPORT_FILE"
fi

# ─── 总结 ────────────────────────────────────
echo ""
echo "=================================================="
echo "  测试完成"
echo "  报告: ${REPORT_FILE}"
echo "=================================================="
echo "" >> "$REPORT_FILE"
echo "=== 测试完成 ===" >> "$REPORT_FILE"

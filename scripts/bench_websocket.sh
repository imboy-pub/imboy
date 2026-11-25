#!/usr/bin/env bash
# WebSocket 容量压测入口。完整参数说明见 docs/guides/testing/websocket-testing.md。
# 仅限隔离的本地/压测环境；严禁对生产环境运行。
#
# Legacy（did-less）token：
#   TOKEN="$(escript scripts/imboy_ctl user token <uid>)" \
#     CONNS=100 HOLD=30 bash scripts/bench_websocket.sh
#
# 设备绑定 token：TOKENS_FILE 每行一个 JWT，且每个 (uid,did) 必须是不同的
# 服务端有效设备；文件至少包含 CONNS 个 token。
#   TOKENS_FILE=/secure/bench-tokens.txt CONNS=500 HOLD=30 \
#     bash scripts/bench_websocket.sh
#
# 默认 WS_URL=ws://127.0.0.1:9800/api/v1/ws；默认要求 /metrics 服务端对账。
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"
exec python3 "$ROOT/scripts/bench_websocket.py" "$@"

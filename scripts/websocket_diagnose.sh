#!/usr/bin/env bash
# WebSocket 连接诊断 / WebSocket connectivity diagnosis
# 逐层定位 WS 连不上的原因：端口 → HTTP → WS 握手 → 节点视角在线数。
# Locates WS failures layer by layer: port → HTTP → WS handshake → node view.
#
# 用法 / Usage:
#   bash scripts/websocket_diagnose.sh                    # 默认 127.0.0.1:9800
#   HOST=x PORT=y TOKEN=<jwt> bash scripts/websocket_diagnose.sh
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."

HOST="${HOST:-127.0.0.1}"
PORT="${PORT:-9800}"
WS_PATH="${WS_PATH:-/ws}"

step() { printf '\n==> %s\n' "$1"; }

# ============ 1. TCP 端口 / TCP port ============
step "1. TCP 端口 ${HOST}:${PORT}"
if nc -z -w 3 "$HOST" "$PORT" 2>/dev/null; then
  echo "✓ 端口可达"
else
  echo "✗ 端口不可达。后端是否启动？IMBOYENV=local make run" >&2
  echo "  本机监听情况："; lsof -nP -iTCP:"$PORT" -sTCP:LISTEN 2>/dev/null || echo "  （无进程监听 ${PORT}）"
  exit 1
fi

# ============ 2. HTTP 层 / HTTP layer ============
step "2. HTTP 基础响应"
HTTP_CODE=$(curl -s -o /dev/null -w '%{http_code}' --max-time 5 "http://${HOST}:${PORT}/" || echo "000")
echo "GET / → HTTP ${HTTP_CODE}（任意响应均说明 Cowboy 在工作）"

# ============ 3. WS 握手 / WS handshake ============
step "3. WebSocket 升级握手 ${WS_PATH}"
HDRS=(-H "Connection: Upgrade" -H "Upgrade: websocket"
      -H "Sec-WebSocket-Version: 13" -H "Sec-WebSocket-Key: $(openssl rand -base64 16)")
[ -n "${TOKEN:-}" ] && HDRS+=(-H "Authorization: Bearer ${TOKEN}")
WS_CODE=$(curl -s -o /dev/null -w '%{http_code}' --max-time 5 "${HDRS[@]}" \
  "http://${HOST}:${PORT}${WS_PATH}" || echo "000")
case "$WS_CODE" in
  101) echo "✓ HTTP 101 — 握手成功（token 有效）" ;;
  401|403) echo "△ HTTP ${WS_CODE} — 路由正常，鉴权拒绝（未传或无效 TOKEN；属预期，传 TOKEN=<jwt> 重试）" ;;
  400) echo "△ HTTP 400 — 到达 WS handler 但请求被拒（检查 token/did 头要求）" ;;
  404) echo "✗ HTTP 404 — 路由不存在，检查 imboy_router.erl 与 WS_PATH=${WS_PATH}" ;;
  000) echo "✗ 无响应/超时" ;;
  *)   echo "△ HTTP ${WS_CODE} — 非典型响应，建议抓包细查" ;;
esac

# ============ 4. 节点视角 / Node view ============
step "4. 节点视角（RPC）"
if escript scripts/imboy_ctl node status 2>/dev/null; then
  echo "--- 在线 WS 连接数 ---"
  escript scripts/imboy_ctl node connections 2>/dev/null || echo "（connections 查询失败）"
else
  echo "△ RPC 连不上节点（不影响 1-3 的结论；检查节点名/cookie）"
fi

printf '\n诊断完成 / Done。逐层结论见上。\n'

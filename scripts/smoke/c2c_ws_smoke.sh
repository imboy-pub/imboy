#!/usr/bin/env bash
#
# c2c_ws_smoke.sh — Tier-0 WebSocket round-trip 冒烟包装器
#
# 用法：
#   ./c2c_ws_smoke.sh [FROM_UID] [BOB_UID]
#   默认 FROM=1000000051 (Alice), BOB=1000000056 (Bob)
#
# 流程：
#   1. 用 mint_token.escript mint Bob 的 JWT
#   2. 调 python3 c2c_ws_smoke.py 连 Bob WS + 触发 Alice 发 C2C
#   3. 校验 Bob 在超时窗口内收到匹配 MSG_ID 的 WS 帧
#
# 退出码：
#   0    PASS
#   非 0 FAIL（详见 stderr）

set -eu

FROM_UID="${1:-1000000051}"
BOB_UID="${2:-1000000056}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MINT="${SCRIPT_DIR}/mint_token.escript"
C2C_ESCRIPT="${SCRIPT_DIR}/c2c_smoke.escript"
PY="${SCRIPT_DIR}/c2c_ws_smoke.py"

[ -x "$MINT" ]        || { echo "ERROR: missing/not exec: $MINT" >&2; exit 10; }
[ -x "$C2C_ESCRIPT" ] || { echo "ERROR: missing/not exec: $C2C_ESCRIPT" >&2; exit 11; }
[ -f "$PY" ]          || { echo "ERROR: missing: $PY" >&2; exit 12; }

command -v python3 >/dev/null 2>&1 || { echo "ERROR: python3 not found" >&2; exit 13; }

echo "[smoke-ws] mint Bob token (uid=${BOB_UID})..."
BOB_TOKEN="$("$MINT" "$BOB_UID")" || { echo "ERROR: mint Bob token failed" >&2; exit 14; }
[ -n "$BOB_TOKEN" ]   || { echo "ERROR: empty Bob token" >&2; exit 15; }

echo "[smoke-ws] connecting Bob WS + trigger Alice C2C..."
export BOB_TOKEN BOB_UID FROM_UID
export ESCRIPT_PATH="$C2C_ESCRIPT"

if python3 "$PY"; then
    echo "=== smoke-ws PASS (from=${FROM_UID} to=${BOB_UID}) ==="
    exit 0
else
    rc=$?
    echo "=== smoke-ws FAIL (rc=${rc}) ===" >&2
    exit "$rc"
fi

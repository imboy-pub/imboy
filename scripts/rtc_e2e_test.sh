#!/usr/bin/env bash
# RTC (LiveKit SFU) 端到端测试 / RTC (LiveKit SFU) E2E test
#
# 分两级门 / Two staged gates:
#   Stage 1（仅需后端）: POST /api/v1/rtc/room/join → 断言 ws_url/token/room_name
#   Stage 2（需 LiveKit + lk CLI）:
#     lk 发测试流入房 → 断言 SFU 真的建出该房间（媒体面通）
#
# ⚠️ 原 Stage 2b/2c（RoomComposite 录制 → 轮询 Garage 桶）已删除：
#    录制依赖 livekit/egress，而 egress 与 livekit-server 之间**只有 Redis 一条
#    总线**。项目级约束「全栈不引入 Redis」下 egress 无法部署，且该功能三端从未
#    接线（后端零 egress 调用）。要恢复录制 = 先解除 Redis 约束。
#
# 用法 / Usage:
#   API_BASE=http://127.0.0.1:9800 TOKEN=<jwt> GROUP_ID=<gid> \
#   bash scripts/rtc_e2e_test.sh                    # 只跑 Stage 1
#
#   追加 LIVEKIT_URL=ws://127.0.0.1:7880 \
#         LIVEKIT_API_KEY=... LIVEKIT_API_SECRET=...  # 跑 Stage 2
#
# TOKEN 获取: 登录取 access_token；GROUP_ID 须是该用户所在群。
set -euo pipefail

API_BASE="${API_BASE:-http://127.0.0.1:9800}"
TOKEN="${TOKEN:?需要设置 TOKEN（JWT access_token）}"
GROUP_ID="${GROUP_ID:?需要设置 GROUP_ID（当前用户所在群 id）}"

auth=(-H "Authorization: Bearer $TOKEN")
pass() { echo "✓ $1"; }
fail() { echo "✗ $1" >&2; exit 1; }

echo "==> Stage 1: join 群房间"
RESP=$(curl -sf "${auth[@]}" -H "Content-Type: application/json" -X POST \
  "$API_BASE/api/v1/rtc/room/join" \
  -d "{\"kind\":\"group\",\"target_id\":$GROUP_ID,\"did\":\"e2e\"}") || fail "join 请求失败"
WS_URL=$(echo "$RESP" | jq -r '.payload.ws_url // .data.ws_url')
LK_TOKEN=$(echo "$RESP" | jq -r '.payload.token // .data.token')
ROOM=$(echo "$RESP" | jq -r '.payload.room_name // .data.room_name')
[ -n "$WS_URL" ] && [ "$WS_URL" != "null" ] || fail "join 未返回 ws_url: $RESP"
[ -n "$LK_TOKEN" ] && [ "$LK_TOKEN" != "null" ] || fail "join 未返回 token: $RESP"
[ "$ROOM" = "rtc_group_$GROUP_ID" ] || fail "房间名不符预期: $ROOM"
pass "join ok, room=$ROOM"

echo "==> Stage 1b: 非法 target_id 拒绝"
CODE=$(curl -s "${auth[@]}" -H "Content-Type: application/json" -X POST \
  "$API_BASE/api/v1/rtc/room/join" -d '{"kind":"group","target_id":0}' | jq -r '.code')
[ "$CODE" != "0" ] && [ "$CODE" != "200" ] || fail "target_id=0 未被拒绝"
pass "非法参数拒绝 ok"

if [ -z "${LIVEKIT_URL:-}" ]; then
    echo "⚠ 未设置 LIVEKIT_URL，跳过 Stage 2（录制 E2E 需在部署 LiveKit 的主机上跑）"
    echo "STAGE1_OK"
    exit 0
fi

command -v lk >/dev/null || fail "Stage 2 需要 lk CLI（brew install livekit-cli）"
: "${LIVEKIT_API_KEY:?}" "${LIVEKIT_API_SECRET:?}"

export LIVEKIT_URL LIVEKIT_API_KEY LIVEKIT_API_SECRET

echo "==> Stage 2: 发布测试流入房"
lk room join --identity e2e_publisher --publish-demo "$ROOM" >/dev/null 2>&1 &
PUB_PID=$!
trap 'kill $PUB_PID 2>/dev/null || true' EXIT

# 断言 SFU 真的建出了房间。房间是**参与者连上才被创建**的，因此
# "room list 里出现 $ROOM" 等价于「token 有效 + 信令握手成功 + 媒体面接纳发布者」——
# 这是单节点 SFU 能在无 egress 前提下拿到的最强证据。
echo "==> Stage 2b: 轮询 SFU 房间就绪"
for _ in $(seq 1 15); do
    if lk room list 2>/dev/null | grep -q "$ROOM"; then
        pass "SFU 已建房 $ROOM（发布者已接入）"
        echo "SFU_OK"
        exit 0
    fi
    sleep 2
done
fail "30s 内 SFU 未出现房间 $ROOM（检查 LIVEKIT_URL / API key / 媒体端口放行）"

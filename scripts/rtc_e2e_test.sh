#!/usr/bin/env bash
# RTC (LiveKit SFU) 端到端测试 / RTC (LiveKit SFU) E2E test
#
# 分两级门 / Two staged gates:
#   Stage 1（仅需后端）: POST /api/v1/rtc/room/join → 断言 ws_url/token/room_name
#   Stage 2（需 LiveKit + lk CLI + Garage）:
#     lk 发测试流入房 → 启动 RoomComposite 录制 → 轮询 Garage 桶出现录制对象
#
# 用法 / Usage:
#   API_BASE=http://127.0.0.1:9800 TOKEN=<jwt> GROUP_ID=<gid> \
#   bash scripts/rtc_e2e_test.sh                    # 只跑 Stage 1
#
#   追加 LIVEKIT_URL=ws://127.0.0.1:7880 LIVEKIT_API_KEY=... LIVEKIT_API_SECRET=... \
#   GARAGE_ENDPOINT=... GARAGE_BUCKET=imboy-recordings \
#   AWS_ACCESS_KEY_ID=... AWS_SECRET_ACCESS_KEY=...  # 跑 Stage 2
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
: "${GARAGE_ENDPOINT:?}" "${GARAGE_BUCKET:?}"
: "${AWS_ACCESS_KEY_ID:?}" "${AWS_SECRET_ACCESS_KEY:?}"

export LIVEKIT_URL LIVEKIT_API_KEY LIVEKIT_API_SECRET

echo "==> Stage 2: 发布测试流入房"
lk room join --identity e2e_publisher --publish-demo "$ROOM" >/dev/null 2>&1 &
PUB_PID=$!
trap 'kill $PUB_PID 2>/dev/null || true' EXIT
sleep 3

echo "==> Stage 2b: 启动 RoomComposite 录制"
OUT_KEY="rtc-e2e/$(date +%s).mp4"
EGRESS_JSON=$(mktemp)
cat > "$EGRESS_JSON" <<EOF
{
  "room_name": "$ROOM",
  "file_outputs": [{
    "filepath": "$OUT_KEY",
    "s3": {
      "access_key": "$AWS_ACCESS_KEY_ID",
      "secret": "$AWS_SECRET_ACCESS_KEY",
      "region": "garage",
      "endpoint": "$GARAGE_ENDPOINT",
      "bucket": "$GARAGE_BUCKET",
      "force_path_style": true
    }
  }]
}
EOF
EGRESS_ID=$(lk egress start --type room-composite "$EGRESS_JSON" | grep -oE 'EG_[A-Za-z0-9]+' | head -1)
[ -n "$EGRESS_ID" ] || fail "egress 启动失败"
pass "egress 启动 $EGRESS_ID"

sleep 10
lk egress stop --id "$EGRESS_ID" >/dev/null
echo "==> Stage 2c: 轮询 Garage 出现录制对象"
for i in $(seq 1 30); do
    HTTP=$(curl -s -o /dev/null -w '%{http_code}' \
        --aws-sigv4 "aws:amz:garage:s3" \
        --user "$AWS_ACCESS_KEY_ID:$AWS_SECRET_ACCESS_KEY" \
        "$GARAGE_ENDPOINT/$GARAGE_BUCKET/$OUT_KEY")
    [ "$HTTP" = "200" ] && { pass "录制对象已落桶 $OUT_KEY"; echo "RECORDING_OK"; exit 0; }
    sleep 2
done
fail "录制对象 60s 内未出现在 Garage：$OUT_KEY"

#!/usr/bin/env bash
# Garage S3 附件直传端到端测试 / Garage S3 attachment upload E2E test
#
# 串起完整链路 / Exercises the full chain:
#   1. GET  /v1/attachment/presign   → 取上传 presigned PUT URL
#   2. PUT  <put_url>                 → 直传对象到 Garage（不经 Erlang）
#   3. POST /v1/attachment/confirm    → 回调落库 attachment 表
#   4. GET  /v1/attachment/view_url   → 签发短时 presigned GET URL
#   5. GET  <view_url>                → 读回对象，校验内容一致
#   6. GET  <endpoint>/<bucket>/<key> → 无签名直链应 403（确认未开公开读）
#
# 用法 / Usage:
#   API_BASE=http://127.0.0.1:9800 TOKEN=<jwt> \
#   GARAGE_ENDPOINT=http://127.0.0.1:3900 GARAGE_BUCKET=imboy \
#   bash scripts/garage_e2e_test.sh
#
# TOKEN 获取 / Obtain TOKEN: 登录后取 access_token，或 token_ds:encrypt_token(Uid).
set -euo pipefail

API_BASE="${API_BASE:-http://127.0.0.1:9800}"
GARAGE_ENDPOINT="${GARAGE_ENDPOINT:-http://127.0.0.1:3900}"
GARAGE_BUCKET="${GARAGE_BUCKET:-imboy}"
TOKEN="${TOKEN:?需要设置 TOKEN（JWT access_token）}"

MIME="image/png"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
SRC="$TMP/e2e.png"
# 构造 1x1 PNG 测试内容
printf '\x89PNG\r\n\x1a\nE2E-GARAGE-TEST' > "$SRC"

auth=(-H "Authorization: Bearer $TOKEN")
pass() { echo "✓ $1"; }
fail() { echo "✗ $1" >&2; exit 1; }

echo "==> 1. presign"
PRE=$(curl -sf "${auth[@]}" \
  "$API_BASE/v1/attachment/presign?filename=e2e.png&mime_type=$MIME") || fail "presign 请求失败"
PUT_URL=$(echo "$PRE" | jq -r '.payload.put_url // .data.put_url')
KEY=$(echo "$PRE" | jq -r '.payload.object_key // .data.object_key')
[ -n "$PUT_URL" ] && [ "$PUT_URL" != "null" ] || fail "presign 未返回 put_url: $PRE"
echo "$KEY" | grep -qE '^u[0-9]+/' || fail "object_key 未绑定 uid 前缀: $KEY"
pass "presign ok, key=$KEY"

echo "==> 2. PUT 直传 Garage"
PUT_CODE=$(curl -s -o /dev/null -w '%{http_code}' -X PUT \
  -H "Content-Type: $MIME" --data-binary @"$SRC" "$PUT_URL")
[ "$PUT_CODE" = "200" ] || [ "$PUT_CODE" = "204" ] || fail "PUT 直传失败 HTTP $PUT_CODE"
pass "PUT ok ($PUT_CODE)"

echo "==> 3. confirm 落库"
MD5=$(openssl md5 -r "$SRC" 2>/dev/null | awk '{print $1}' || md5 -q "$SRC")
SIZE=$(wc -c < "$SRC" | tr -d ' ')
CONFIRM=$(curl -sf "${auth[@]}" -H "Content-Type: application/json" -X POST \
  "$API_BASE/v1/attachment/confirm" \
  -d "{\"object_key\":\"$KEY\",\"md5\":\"$MD5\",\"mime_type\":\"$MIME\",\"size\":$SIZE}") \
  || fail "confirm 请求失败"
echo "$CONFIRM" | jq -e '.code == 0 or .code == 200' >/dev/null || fail "confirm 落库失败: $CONFIRM"
pass "confirm ok"

echo "==> 4+5. view_url 签发 + 读回"
VIEW=$(curl -sf "${auth[@]}" \
  "$API_BASE/v1/attachment/view_url?object_key=$KEY") || fail "view_url 请求失败"
VURL=$(echo "$VIEW" | jq -r '.payload.url // .data.url')
[ -n "$VURL" ] && [ "$VURL" != "null" ] || fail "view_url 未返回 url: $VIEW"
curl -sf "$VURL" -o "$TMP/got.png" || fail "presigned GET 读回失败"
cmp -s "$SRC" "$TMP/got.png" || fail "读回内容与上传不一致"
pass "view_url 签发 + 读回内容一致"

echo "==> 6. 反向验证：无签名直链应 403"
RAW_CODE=$(curl -s -o /dev/null -w '%{http_code}' "$GARAGE_ENDPOINT/$GARAGE_BUCKET/$KEY")
if [ "$RAW_CODE" = "403" ]; then
  pass "无签名直链已拒绝（公开读已关闭）"
else
  fail "无签名直链返回 $RAW_CODE（期望 403）→ bucket 可能仍开放公开读，存在私有附件泄露风险"
fi

echo ""
echo "✅ Garage S3 端到端链路全部通过 / All E2E steps passed"

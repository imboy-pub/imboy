#!/usr/bin/env bash
# SSO OIDC 登录流端到端测试（dex 版）/ SSO OIDC login flow E2E test against dex
#
# 串起完整链路 / Exercises the full chain:
#   1. docker 启动 dex（scripts/sso/dex-config.yaml：staticClient=imboy + 静态密码用户）
#   2. 节点 eval sso_config_ds:upsert 写 oauth2 provider 配置（覆盖 enc:v1: 密文落库路径）
#   3. GET  /api/v1/auth/oidc/authorize?client=test → 302 到 dex /auth
#   4. curl 走 dex 密码登录表单（POST /auth/local?req=...，login+password）
#   5. 跟随 approval 重定向 → dex 302 回 imboy callback（带 code&state）
#   6. GET  callback → 断言 JSON payload 含非空 token/refreshtoken/uid
#   7. 用该 token 调需鉴权端点（/api/v1/user/show）断言可用
#   8. 重放同一 callback URL → 断言失败（state 一次性消费）
#   9. trap 清理 dex 容器
#
# 前置 / Prerequisites:
#   - docker 可用；imboy 节点已启动（IMBOYENV=local make run，HTTP 端口 9800）
#   - 节点 CLI 可用：_rel/imboy/bin/imboy（用于 eval 写 SSO 配置）
#
# 用法 / Usage:
#   API_BASE=http://127.0.0.1:9800 \
#   DEX_PORT=5556 DEX_IMAGE=dexidp/dex:v2.41.1 \
#   IMBOY_BIN=_rel/imboy/bin/imboy \
#   bash scripts/sso_oidc_e2e.sh
#
# 成功输出末行 / Success marker: SSO_OIDC_E2E_OK
set -euo pipefail

API_BASE="${API_BASE:-http://127.0.0.1:9800}"
DEX_PORT="${DEX_PORT:-5556}"
DEX_IMAGE="${DEX_IMAGE:-dexidp/dex:v2.41.1}"
DEX_ISSUER="http://127.0.0.1:${DEX_PORT}/dex"
IMBOY_BIN="${IMBOY_BIN:-_rel/imboy/bin/imboy}"
DEX_CONTAINER="imboy-sso-e2e-dex"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

TMP="$(mktemp -d)"
COOKIES="$TMP/cookies.txt"

pass() { echo "✓ $1"; }
fail() { echo "✗ $1" >&2; exit 1; }

cleanup() {
  docker rm -f "$DEX_CONTAINER" >/dev/null 2>&1 || true
  rm -rf "$TMP"
}
trap cleanup EXIT

echo "==> 1. 启动 dex ($DEX_IMAGE)"
docker rm -f "$DEX_CONTAINER" >/dev/null 2>&1 || true
docker run -d --name "$DEX_CONTAINER" \
  -p "${DEX_PORT}:5556" \
  -v "${SCRIPT_DIR}/sso/dex-config.yaml:/etc/dex/config.yaml:ro" \
  "$DEX_IMAGE" dex serve /etc/dex/config.yaml >/dev/null || fail "dex 启动失败"

# 等 dex 就绪（discovery 端点可达）
for i in $(seq 1 30); do
  if curl -sf "${DEX_ISSUER}/.well-known/openid-configuration" >/dev/null 2>&1; then
    break
  fi
  [ "$i" = 30 ] && fail "dex 30s 内未就绪"
  sleep 1
done
pass "dex 就绪"

echo "==> 2. 写入 oauth2 provider 配置（经 sso_config_ds 加密落库路径）"
# 用节点 eval 而非直插 SQL：保证 client_secret 走 enc:v1: 加密路径也被 E2E 覆盖
"$IMBOY_BIN" eval "sso_config_ds:upsert(<<\"oauth2\">>, #{
    <<\"provider\">> => <<\"oauth2\">>,
    <<\"enabled\">> => true,
    <<\"client_id\">> => <<\"imboy\">>,
    <<\"client_secret\">> => <<\"imboy-e2e-secret\">>,
    <<\"auth_url\">> => <<\"${DEX_ISSUER}/auth\">>,
    <<\"token_url\">> => <<\"${DEX_ISSUER}/token\">>,
    <<\"userinfo_url\">> => <<\"${DEX_ISSUER}/userinfo\">>,
    <<\"issuer\">> => <<\"${DEX_ISSUER}\">>,
    <<\"redirect_uri\">> => <<\"${API_BASE}/api/v1/auth/oidc/callback\">>,
    <<\"scopes\">> => <<\"openid profile email\">>
})." >/dev/null || fail "sso_config upsert 失败（节点未启动？）"
pass "oauth2 provider 配置已写入"

echo "==> 3. authorize：取 302 Location"
AUTH_LOC=$(curl -s -o /dev/null -w '%{redirect_url}' \
  "$API_BASE/api/v1/auth/oidc/authorize?client=test")
[ -n "$AUTH_LOC" ] || fail "authorize 未返回 302 Location"
echo "$AUTH_LOC" | grep -q "code_challenge_method=S256" || fail "authorize URL 缺 PKCE S256"
echo "$AUTH_LOC" | grep -q "state=" || fail "authorize URL 缺 state"
pass "authorize 302 ok"

echo "==> 4. dex 密码登录表单流"
# dex /auth 会 302 到 /auth/local?req=<reqid>（单 connector 自动跳转）
LOGIN_URL=$(curl -s -o /dev/null -w '%{redirect_url}' -c "$COOKIES" "$AUTH_LOC")
if [ -z "$LOGIN_URL" ]; then
  # 某些版本直接 200 返回表单页，从 HTML 提取 req
  PAGE=$(curl -s -c "$COOKIES" "$AUTH_LOC")
  REQ=$(echo "$PAGE" | grep -o 'req=[^"&]*' | head -1 | cut -d= -f2)
  [ -n "$REQ" ] || fail "无法从 dex 授权页提取 req"
  LOGIN_URL="${DEX_ISSUER}/auth/local?req=${REQ}"
fi
pass "dex 登录页: $LOGIN_URL"

# 提交静态密码用户（见 dex-config.yaml）
APPROVAL_LOC=$(curl -s -o /dev/null -w '%{redirect_url}' -b "$COOKIES" -c "$COOKIES" \
  -d "login=test@example.com" -d "password=password" "$LOGIN_URL")
[ -n "$APPROVAL_LOC" ] || fail "dex 登录提交未重定向（用户名/密码错？）"

# 跟随 approval（skipApprovalScreen=true 时直接 302 回 imboy callback），逐跳跟随
CALLBACK_URL="$APPROVAL_LOC"
for i in $(seq 1 5); do
  case "$CALLBACK_URL" in
    *"/api/v1/auth/oidc/callback"*) break ;;
  esac
  CALLBACK_URL=$(curl -s -o /dev/null -w '%{redirect_url}' -b "$COOKIES" -c "$COOKIES" "$CALLBACK_URL")
  [ -n "$CALLBACK_URL" ] || fail "dex 授权链中断（第 $i 跳）"
done
echo "$CALLBACK_URL" | grep -q "code=" || fail "回调 URL 缺 code: $CALLBACK_URL"
pass "拿到回调 URL（含 code&state）"

echo "==> 5. callback：换 token 签发 imboy JWT"
RESP=$(curl -s "$CALLBACK_URL")
TOKEN=$(echo "$RESP" | jq -r '.payload.token // empty')
REFRESH=$(echo "$RESP" | jq -r '.payload.refreshtoken // empty')
UID_=$(echo "$RESP" | jq -r '.payload.uid // empty')
[ -n "$TOKEN" ] || fail "callback 未返回 token: $RESP"
[ -n "$REFRESH" ] || fail "callback 未返回 refreshtoken: $RESP"
[ -n "$UID_" ] || fail "callback 未返回 uid: $RESP"
pass "签发 ok (uid=$UID_)"

echo "==> 6. token 可用性：调需鉴权端点"
SHOW_CODE=$(curl -s -o "$TMP/show.json" -w '%{http_code}' \
  -H "Authorization: $TOKEN" \
  "$API_BASE/api/v1/user/show?id=$UID_")
[ "$SHOW_CODE" = "200" ] || fail "带 token 调用鉴权端点失败 HTTP $SHOW_CODE"
pass "token 可用"

echo "==> 7. callback 重放：state 一次性消费必须拒绝"
REPLAY=$(curl -s "$CALLBACK_URL")
REPLAY_TOKEN=$(echo "$REPLAY" | jq -r '.payload.token // empty')
[ -z "$REPLAY_TOKEN" ] || fail "callback 重放竟然签发了 token（state 未一次性消费）: $REPLAY"
echo "$REPLAY" | grep -q "SSO" || fail "重放响应非统一错误: $REPLAY"
pass "重放被拒绝"

echo "SSO_OIDC_E2E_OK"

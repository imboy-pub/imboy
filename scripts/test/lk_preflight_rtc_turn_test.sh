#!/usr/bin/env bash
# LK-DEP-01 preflight 两域正负例 + Compose 渲染合同 + secret 扫描（离线测试）。
#
# 覆盖验收 A01（两域必填/格式/唯一性/DNS fail-closed、TURN 证书门）、
# A02（渲染只有一份 LiveKit、无 coturn/eturnal/Redis/Egress、证书 :ro）、
# A05 的 secret 占位符扫描部分。DNS/端口探测全部走 mock，不触网不占端口。
set -uo pipefail

cd "$(dirname "$0")/../.." || exit 1

REPO="$(pwd)"
TMP_ROOT="$(mktemp -d /tmp/imboy_lk_preflight.XXXXXX)"
cleanup() { rm -rf -- "$TMP_ROOT"; }
trap cleanup EXIT

PASS=0
FAIL=0
ok()  { PASS=$((PASS + 1)); echo "  PASS $1"; }
bad() { FAIL=$((FAIL + 1)); echo "  FAIL $1: ${2:-<无详情>}"; }
assert_rc() {
  local want="$1" got="$2" desc="$3" log="${4:-$TMP_ROOT/out.log}"
  if [ "$got" = "$want" ]; then ok "$desc"; else bad "$desc" "exit=$got want=$want tail=$(LC_ALL=C tail -3 "$log" 2>/dev/null | LC_ALL=C tr '\n' '_')"; fi
}
assert_grep() {
  local desc="$1" pattern="$2" file="$3"
  if grep -qE "$pattern" "$file" 2>/dev/null; then ok "$desc"; else bad "$desc" "pattern=$pattern 未命中"; fi
}
assert_not_grep() {
  local desc="$1" pattern="$2" file="$3"
  if grep -qE "$pattern" "$file" 2>/dev/null; then bad "$desc" "不应命中 pattern=$pattern"; else ok "$desc"; fi
}

# ── fixture：把 preflight.sh 复制进临时目录（它从自己所在目录读 .env）─────────
FIXTURE="$TMP_ROOT/deploy"
mkdir -p "$FIXTURE"
cp deploy/preflight.sh "$FIXTURE/preflight.sh"

# preflight 正向基线 .env（五个域名齐全；.test.invalid 是 RFC 保留域，永不触网）
cat > "$FIXTURE/.env" <<'ENV'
API_DOMAIN=api.test.invalid
ADMIN_DOMAIN=admin.test.invalid
CS_WIDGET_DOMAIN=cs.test.invalid
RTC_DOMAIN=rtc.test.invalid
TURN_DOMAIN=turn.test.invalid
CERTBOT_EMAIL=ops@test.invalid
POSTGRES_USER=imboy_user
POSTGRES_PASSWORD=postgres_password_40_chars_ok_pad_pad
POSTGRES_DB=imboy_pro
JWT_KEY=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
POSTGRE_AES_KEY=bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
ADM_COOKIE_SECRET=cccccccccccccccccccccccccccccccccccccccc
# 低熵假值（单字符重复）只测长度/互异规则，避免 gitleaks generic-api-key 误报
IMBOY_SOLIDIFIED_KEY=dddddddddddddddddddddddddddddddddddd
IMBOY_SOLIDIFIED_KEY_IV=eeeeeeeeeeeeeeee
IMBOY_PASSWORD_SALT=ffffffffffffffffffffffffffffffffffffffff
GRAFANA_ADMIN_PASSWORD=grafana_password_40_ok_pad_pad_pad_123
LIVEKIT_API_KEY=livekit_api_key_40_ok_pad_pad_pad_1234
LIVEKIT_API_SECRET=gggggggggggggggggggggggggggggggggggggggg
IMBOY_API_AUTH_SWITCH=on
IMBOY_PRODUCT_PROFILE=community
IMBOY_E2EE_MODE=required
IMBOY_FEATURE_E2EE=true
IMBOY_FEATURE_CHANNEL=true
IMBOY_FEATURE_CHANNEL_ORDER=true
IMBOY_GARAGE_ENDPOINT=http://garage:3900
IMBOY_GARAGE_ACCESS_KEY=GKtestaccesskey0000000000000
IMBOY_GARAGE_SECRET_KEY=hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
GARAGE_RPC_SECRET=iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
LIVEKIT_TURN_ENABLED=false
DATA_DIR=./data
IMBOY_LOGIN_RSA_PUB_KEY_FILE=/opt/imboy/priv_runtime/keys/login_rsa_pub.pem
IMBOY_LOGIN_RSA_PRIV_KEY_FILE=/opt/imboy/priv_runtime/keys/login_rsa_priv.pem
ENV

# RSA 密钥文件（preflight 把容器路径翻译回 ${DATA_DIR}/backend_priv 再查存在性）
KEYS_DIR="$FIXTURE/data/backend_priv/keys"
mkdir -p "$KEYS_DIR"
printf 'fake-pem\n' > "$KEYS_DIR/login_rsa_priv.pem"
printf 'fake-pem\n' > "$KEYS_DIR/login_rsa_pub.pem"

# .env 行级编辑：set_env_line KEY VALUE / del_env_line KEY
set_env_line() {
  local key="$1" val="$2" tmp="$FIXTURE/.env.tmp"
  awk -v k="$key" -v v="$val" 'BEGIN{FS=OFS="="} $1==k{print k"="v; found=1; next} {print} END{if(!found) print k"="v}' \
    "$FIXTURE/.env" >"$tmp" && mv "$tmp" "$FIXTURE/.env"
}
del_env_line() {
  local key="$1" tmp="$FIXTURE/.env.tmp"
  grep -v "^${key}=" "$FIXTURE/.env" >"$tmp" && mv "$tmp" "$FIXTURE/.env"
}
run_preflight() {
  # 真实调用形态：在 deploy 目录内执行（DATA_DIR 的相对路径以此为基准）
  ( cd "$FIXTURE" && env PATH="${MOCK_BIN:+$MOCK_BIN:}$PATH" ./preflight.sh --edition community "$@" ) \
    >"$TMP_ROOT/out.log" 2>&1
  echo $?
}

echo "== A01 preflight 两域正负例 =="

RC="$(run_preflight)"
assert_rc 0 "$RC" "正向：五域齐全 + TURN 关闭 → 通过（exit 0）"

del_env_line RTC_DOMAIN
RC="$(run_preflight)"
assert_rc 1 "$RC" "负向：缺 RTC_DOMAIN → 拒绝（exit 1）"
assert_grep "缺 RTC_DOMAIN 有明确报错" 'RTC_DOMAIN 未设置' "$TMP_ROOT/out.log"

set_env_line RTC_DOMAIN "rtc.test.invalid"
set_env_line TURN_DOMAIN "turn.test.invalid"
set_env_line TURN_DOMAIN "rtc.test.invalid"
RC="$(run_preflight)"
assert_rc 1 "$RC" "负向：RTC_DOMAIN == TURN_DOMAIN → 拒绝"
assert_grep "两域相同有明确报错" 'RTC_DOMAIN 与 TURN_DOMAIN 不能相同' "$TMP_ROOT/out.log"

set_env_line TURN_DOMAIN "api.test.invalid"
RC="$(run_preflight)"
assert_rc 1 "$RC" "负向：TURN_DOMAIN 撞 API_DOMAIN → 拒绝"

set_env_line TURN_DOMAIN "turn.test.invalid"
set_env_line RTC_DOMAIN "https://rtc.test.invalid"
RC="$(run_preflight)"
assert_rc 1 "$RC" "负向：RTC_DOMAIN 带 https:// 前缀 → 拒绝"
assert_grep "非法域名格式有明确报错" 'RTC_DOMAIN 不是有效的纯域名' "$TMP_ROOT/out.log"

set_env_line RTC_DOMAIN "rtc.example.com"
RC="$(run_preflight)"
assert_rc 1 "$RC" "负向：RTC_DOMAIN 留占位符 example.com → 拒绝"

set_env_line RTC_DOMAIN "rtc.test.invalid"

# ── DNS 开关：mock 解析器（不触网）────────────────────────────────────────────
MOCK_BIN="$TMP_ROOT/bin"
mkdir -p "$MOCK_BIN"
cat > "$MOCK_BIN/getent" <<'MOCK'
#!/bin/sh
# MOCK_DNS_OK=1 时一切解析成功；否则解析失败（模拟无 A 记录）
[ "${MOCK_DNS_OK:-0}" = 1 ] || exit 2
echo "10.0.0.1 stream 10.0.0.1:1"
MOCK
cat > "$MOCK_BIN/ss" <<'MOCK'
#!/bin/sh
# MOCK_SS_3478=1 时报告 3478 已被占用（模拟旧 TURN eturnal 未退场）
[ "${MOCK_SS_3478:-0}" = 1 ] && printf 'udp   UNCONN 0 0 0.0.0.0:3478 0.0.0.0:*\n'
exit 0
MOCK
chmod +x "$MOCK_BIN/getent" "$MOCK_BIN/ss"

RC="$(run_preflight --dns)"
assert_rc 1 "$RC" "DNS 开关开启 + 无 A 记录 → 拒绝（fail-closed）"
assert_grep "DNS 失败有明确报错" 'RTC_DOMAIN.*无 A 记录解析' "$TMP_ROOT/out.log"

RC="$(MOCK_DNS_OK=1 run_preflight --dns)"
assert_rc 0 "$RC" "DNS 开关开启 + 解析成功 → 通过"

RC="$(MOCK_DNS_OK=1 run_preflight)"
assert_grep "DNS 开关默认关闭只提示" 'DNS 检查未开启' "$TMP_ROOT/out.log"

# ── TURN 开关门：开关值 / 端口双活 / 证书就绪 ────────────────────────────────
set_env_line LIVEKIT_TURN_ENABLED "maybe"
RC="$(run_preflight)"
assert_rc 1 "$RC" "负向：LIVEKIT_TURN_ENABLED=maybe → 拒绝"
assert_grep "开关非法值有明确报错" 'LIVEKIT_TURN_ENABLED 仅支持' "$TMP_ROOT/out.log"

set_env_line LIVEKIT_TURN_ENABLED "true"
RC="$(run_preflight)"
assert_rc 1 "$RC" "负向：TURN 开启但证书未就绪 → 拒绝（两段式首装提示）"
assert_grep "证书未就绪有指引" 'TURN TLS 证书未就绪' "$TMP_ROOT/out.log"

TURN_CERT_DIR="$FIXTURE/data/certbot/conf/live/turn.test.invalid"
mkdir -p "$TURN_CERT_DIR"
RC="$(MOCK_SS_3478=1 run_preflight)"
assert_rc 1 "$RC" "负向：TURN 开启 + 3478 被旧 TURN 占用 → 拒绝（不得双活）"
assert_grep "双活有明确报错" '端口 3478.*已被占用' "$TMP_ROOT/out.log"

printf 'cert\n' > "$TURN_CERT_DIR/fullchain.pem"
printf 'key\n'  > "$TURN_CERT_DIR/privkey.pem"
RC="$(run_preflight)"
assert_rc 0 "$RC" "正向：TURN 开启 + 证书就绪 + 端口空闲 → 通过"
assert_grep "证书就绪路径被确认" 'TURN TLS 证书就绪' "$TMP_ROOT/out.log"

del_env_line LIVEKIT_API_SECRET
RC="$(run_preflight)"
assert_rc 1 "$RC" "回归：LIVEKIT_API_SECRET 缺失仍拒绝（现有检查不回归）"
set_env_line LIVEKIT_API_SECRET "livekit_api_secret_at_least_32_chars"

echo "== A02 Compose 渲染合同（干跑，不启动任何容器） =="
command -v docker >/dev/null 2>&1 || { echo "  SKIP 本机无 docker，跳过渲染断言"; docker() { return 127; }; }

ENV_FILE="$TMP_ROOT/compose.env"
cat > "$ENV_FILE" <<'ENV'
POSTGRES_USER=u
POSTGRES_PASSWORD=p
POSTGRES_DB=d
JWT_KEY=k32chars________________________________!
ADM_COOKIE_SECRET=a32chars________________________________!
POSTGRE_AES_KEY=e32chars________________________________!
IMBOY_SOLIDIFIED_KEY=s32chars________________________________!
IMBOY_SOLIDIFIED_KEY_IV=iv16chars_________!
IMBOY_PASSWORD_SALT=ps32chars________________________________!
GRAFANA_ADMIN_PASSWORD=g32chars________________________________!
API_DOMAIN=api.test.invalid
ADMIN_DOMAIN=adm.test.invalid
CS_WIDGET_DOMAIN=cs.test.invalid
RTC_DOMAIN=rtc.test.invalid
TURN_DOMAIN=turn.test.invalid
LIVEKIT_API_KEY=lkkey0000000000000000000000000
LIVEKIT_API_SECRET=lksecret_at_least_32_chars______!
IMBOY_GARAGE_ACCESS_KEY=GKaccesskey0000000000000000
IMBOY_GARAGE_SECRET_KEY=garagesecret0000000000000000
GARAGE_RPC_SECRET=rpcsecret00000000000000000000000!
LIVEKIT_TURN_CERT_DIR=/tmp/imboy-fake-turn-certs
ENV

BASE_YML="deploy/docker-compose.community.yml"
TURN_YML="deploy/docker-compose.livekit-turn.yml"
RENDER="$TMP_ROOT/render-full.yml"

if COMPOSE_PROJECT_NAME=imboy_lk_test docker compose --env-file "$ENV_FILE" \
    -f "$BASE_YML" -f "$TURN_YML" config >"$RENDER" 2>"$TMP_ROOT/render.err"; then
  ok "base+TURN overlay 渲染成功"
else
  bad "base+TURN overlay 渲染成功" "$(tail -5 "$TMP_ROOT/render.err")"
fi

LK_DIGEST="sha256:5d3dcc475d064536d9948ebe4eeab8e3b24d6f07a46f6d71a3415a2901bbdc52"
LK_IMAGE_COUNT="$(grep -c "image: livekit/livekit-server@${LK_DIGEST}\$" "$RENDER" || true)"
[ "$LK_IMAGE_COUNT" = 1 ] && ok "有且只有一份 LiveKit（image@digest 锁定 v1.13.7 amd64）" \
  || bad "有且只有一份 LiveKit（image@digest 锁定 v1.13.7 amd64）" "count=$LK_IMAGE_COUNT"
# digest 锁定强断言：渲染结果不得残留裸 tag 引用（防回退浮动 tag）
if grep -q 'image: livekit/livekit-server:v' "$RENDER"; then
  bad "无裸 tag LiveKit 引用" "$(grep 'image: livekit/livekit-server:v' "$RENDER" | head -2)"
else
  ok "无裸 tag LiveKit 引用"
fi

assert_not_grep "渲染无 redis"     '(^|[^a-z])redis'  "$RENDER"
assert_not_grep "渲染无 coturn"    'coturn'           "$RENDER"
assert_not_grep "渲染无 eturnal"   'eturnal'          "$RENDER"
assert_not_grep "渲染无 egress"    'egress'           "$RENDER"
# TURN 证书只读挂载（compose config 结构化输出：target + read_only 相邻）
if grep -A2 'target: /etc/livekit/certs' "$RENDER" | grep -q 'read_only: true'; then
  ok "TURN 证书只读挂载（:ro）"
else
  bad "TURN 证书只读挂载（:ro）" "rendered=$(grep -A2 'target: /etc/livekit/certs' "$RENDER" | tr '\n' ' ')"
fi
assert_grep     "ws_url 默认收敛 rtc 域" 'wss://rtc\.test\.invalid' "$RENDER"
assert_grep     "LIVEKIT_CONFIG turn 段启用" 'enabled: true' "$RENDER"
assert_grep     "TURN TLS 固定 5349" 'tls_port: 5349' "$RENDER"
assert_grep     "TURN UDP 3478"      'udp_port: 3478' "$RENDER"
assert_grep     "TURN relay 段起点 50201" 'relay_range_start: 50201' "$RENDER"
assert_grep     "TURN relay 段终点 50500" 'relay_range_end: 50500' "$RENDER"
assert_grep     "媒体段维持 50000-50200" 'port_range_start: 50000' "$RENDER"
assert_grep     "TURN 域来自 TURN_DOMAIN" 'domain: turn\.test\.invalid' "$RENDER"
assert_grep     "容器内证书路径 fullchain" 'cert_file: /etc/livekit/certs/fullchain\.pem' "$RENDER"
# 端口发布（结构化输出：target 与 protocol 相邻）；端口段展开为逐端口条目
if grep -A3 'target: 3478' "$RENDER" | grep -q 'protocol: udp'; then
  ok "3478 以 UDP 发布"
else
  bad "3478 以 UDP 发布" "未找到 target 3478 + udp"
fi
if grep -A3 'target: 5349' "$RENDER" | grep -q 'protocol: tcp'; then
  ok "5349 以 TCP 发布"
else
  bad "5349 以 TCP 发布" "未找到 target 5349 + tcp"
fi
if grep -q 'target: 50201' "$RENDER" && grep -q 'target: 50500' "$RENDER"; then
  ok "relay 段 50201-50500 完整展开发布"
else
  bad "relay 段 50201-50500 完整展开发布" "边界端口缺失"
fi

RENDER_BASE="$TMP_ROOT/render-base.yml"
if COMPOSE_PROJECT_NAME=imboy_lk_test docker compose --env-file "$ENV_FILE" \
    -f "$BASE_YML" config >"$RENDER_BASE" 2>"$TMP_ROOT/render-base.err"; then
  ok "base（TURN 关闭）渲染成功"
else
  bad "base（TURN 关闭）渲染成功" "$(tail -5 "$TMP_ROOT/render-base.err")"
fi
assert_not_grep "base 渲染无 turn 段（W4 共存形态）" 'turn:' "$RENDER_BASE"
assert_not_grep "base 渲染不发布 3478/5349（共存期归 eturnal）" 'target: (3478|5349)' "$RENDER_BASE"
assert_grep     "base 渲染 ws_url 仍收敛 rtc 域" 'wss://rtc\.test\.invalid' "$RENDER_BASE"

echo "== A05 secret 占位符扫描（.env.example 不含真实密钥） =="
SECRET_LEAKS="$(awk -F= '
/^[A-Z0-9_]+=/ {
  name=$1
  if (name ~ /(_PASSWORD|_SECRET|_TOKEN|_SALT|_KEY|_KEY_IV)$/ && name !~ /_FILE$/ && name !~ /(MEM_LIMIT|SWITCH|MODE|ENABLED)$/) {
    val=$0; sub(/^[^=]*=/, "", val)
    if (val != "" && val !~ /CHANGE_ME/ && val !~ /example/) print FILENAME": "$0
  }
}' deploy/.env.example)"
if [ -z "$SECRET_LEAKS" ]; then
  ok ".env.example 密钥类字段全为空或占位符"
else
  bad ".env.example 密钥类字段全为空或占位符" "$SECRET_LEAKS"
fi
# 私钥块检查跳过注释（.env.example 的微信私钥格式示例是文档，不是泄露）
if grep -v '^[[:space:]]*#' deploy/.env.example | grep -qE 'BEGIN (RSA )?PRIVATE KEY'; then
  bad "改动文件无未注释私钥块" "命中"
else
  ok "改动文件无未注释私钥块"
fi

echo "== 结果：PASS=${PASS} FAIL=${FAIL} =="
[ "$FAIL" -eq 0 ]

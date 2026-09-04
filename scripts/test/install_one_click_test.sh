#!/usr/bin/env bash
# 一键安装控制流离线测试：所有 Docker、TLS、网络与 sanity 调用均使用本地桩。
set -uo pipefail

cd "$(dirname "$0")/../.." || exit 1

TMP_ROOT="$(mktemp -d /tmp/imboy_install_one_click.XXXXXX)"
FIXTURE="$TMP_ROOT/deploy"
MOCK_BIN="$TMP_ROOT/bin"
MOCK_LOG="$TMP_ROOT/events.log"

cleanup() {
  rm -rf -- "$TMP_ROOT"
}
trap cleanup EXIT

mkdir -p "$FIXTURE/nginx" "$TMP_ROOT/scripts" "$MOCK_BIN"
cp deploy/install.sh deploy/.env.example deploy/docker-compose.community.yml \
  deploy/docker-compose.uptrace.yml "$FIXTURE/"
cp deploy/nginx/init-letsencrypt.sh "$FIXTURE/nginx/"
touch "$TMP_ROOT/scripts/sanity_check.sh"

set_env() {
  local key="$1" value="$2" tmp="$FIXTURE/.env.tmp"
  awk -v k="$key" -v v="$value" 'BEGIN{FS=OFS="="} $1==k{print k"="v; found=1; next} {print} END{if(!found) print k"="v}' \
    "$FIXTURE/.env" >"$tmp"
  mv "$tmp" "$FIXTURE/.env"
}

cp "$FIXTURE/.env.example" "$FIXTURE/.env"
set_env API_DOMAIN api.test.invalid
set_env ADMIN_DOMAIN admin.test.invalid
set_env CERTBOT_EMAIL certbot@test.invalid
set_env UPTRACE_ENABLED true
set_env UPTRACE_DOMAIN uptrace.test.invalid
set_env UPTRACE_ADMIN_EMAIL admin@test.invalid
set_env IMBOY_PAYMENT_GATEWAY_ENABLED true
set_env JWT_KEY keep_existing_jwt_key_1234567890

cat >"$MOCK_BIN/docker" <<'MOCK'
#!/bin/sh
set -u
printf 'docker %s\n' "$*" >>"$MOCK_LOG"
case " $* " in
  *" compose version "*) exit 0 ;;
  *" compose "*" pull --policy missing "*) [ "${MOCK_PULL_FAIL:-0}" != 1 ]; exit ;;
  *" compose "*" images -q imboy_backend "*) echo test-backend-image-id; exit 0 ;;
  *" inspect --format "*) echo 'ghcr.io/imboy-pub/imboy-backend@sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'; exit 0 ;;
  *" compose "*" run "*" imboy_certbot "*)
    previous=""
    domain=""
    for arg in "$@"; do
      [ "$previous" = -d ] && domain="$arg"
      previous="$arg"
    done
    [ -n "$domain" ] || exit 2
    cert_dir="${DATA_DIR:-./data}/certbot/conf/live/$domain"
    mkdir -p "$cert_dir"
    printf 'issued fullchain\n' >"$cert_dir/fullchain.pem"
    printf 'issued private key\n' >"$cert_dir/privkey.pem"
    exit 0
    ;;
esac
exit 0
MOCK

cat >"$MOCK_BIN/curl" <<'MOCK'
#!/bin/sh
printf '{"status":"ok"}'
MOCK

cat >"$MOCK_BIN/sleep" <<'MOCK'
#!/bin/sh
exit 0
MOCK

cat >"$MOCK_BIN/bash" <<'MOCK'
#!/bin/sh
set -u
printf 'bash COMPOSE_FILES=%s args=%s\n' "${COMPOSE_FILES:-}" "$*" >>"$MOCK_LOG"
case "${1:-}" in
  *sanity_check.sh) [ "${MOCK_SANITY_FAIL:-0}" != 1 ] ;;
  *) exit 0 ;;
esac
MOCK
chmod +x "$MOCK_BIN/docker" "$MOCK_BIN/curl" "$MOCK_BIN/sleep" "$MOCK_BIN/bash"

PASS=0
FAIL=0
ok() { PASS=$((PASS + 1)); echo "  PASS $1"; }
bad() { FAIL=$((FAIL + 1)); echo "  FAIL $1: ${2:-<无详情>}"; }
assert_match() {
  local description="$1" pattern="$2" file="$3"
  if grep -Eq "$pattern" "$file"; then ok "$description"; else bad "$description" "pattern=$pattern"; fi
}

echo "== 一键安装控制流（全离线 mock） =="
: >"$MOCK_LOG"
if env PATH="$MOCK_BIN:$PATH" MOCK_LOG="$MOCK_LOG" \
    /bin/bash "$FIXTURE/install.sh" --edition community --yes \
    >"$TMP_ROOT/success.log" 2>&1; then
  ok "已有客户配置可一次执行完成"
else
  bad "已有客户配置可一次执行完成" "$(tail -20 "$TMP_ROOT/success.log")"
fi

assert_match "已有内部密钥不被覆盖" '^JWT_KEY=keep_existing_jwt_key_1234567890$' "$FIXTURE/.env"
assert_match "占位内部密钥被自动补齐" '^POSTGRES_PASSWORD=[0-9a-f]{32}$' "$FIXTURE/.env"
assert_match "Uptrace 密钥被自动补齐" '^UPTRACE_SERVICE_SECRET=[0-9a-f]{48}$' "$FIXTURE/.env"
assert_match "支付宝回调按 API_DOMAIN 派生" '^IMBOY_ALIPAY_NOTIFY_URL=https://api\.test\.invalid/api/v1/payment/callback/alipay$' "$FIXTURE/.env"
assert_match "微信回调按 API_DOMAIN 派生" '^IMBOY_WECHAT_NOTIFY_URL=https://api\.test\.invalid/api/v1/payment/callback/wechat$' "$FIXTURE/.env"
assert_match "启动加载 Uptrace overlay" 'docker compose -f docker-compose\.community\.yml -f docker-compose\.uptrace\.yml up -d' "$MOCK_LOG"
pull_line="$(grep -n ' pull --policy missing$' "$MOCK_LOG" | head -1 | cut -d: -f1)"
up_line="$(grep -n ' up -d$' "$MOCK_LOG" | head -1 | cut -d: -f1)"
if [ -n "$pull_line" ] && [ -n "$up_line" ] && [ "$pull_line" -lt "$up_line" ]; then
  ok "缺失镜像在启动服务前拉齐"
else
  bad "缺失镜像在启动服务前拉齐" "pull=${pull_line:-无} up=${up_line:-无}"
fi
assert_match "TLS 使用同一 Compose 文件集合" 'bash COMPOSE_FILES=docker-compose\.community\.yml docker-compose\.uptrace\.yml args=nginx/init-letsencrypt\.sh' "$MOCK_LOG"
assert_match "sanity 使用同一 Compose 文件集合" 'args=\.\./scripts/sanity_check\.sh --compose-file docker-compose\.community\.yml --compose-file docker-compose\.uptrace\.yml' "$MOCK_LOG"

: >"$MOCK_LOG"
if env PATH="$MOCK_BIN:$PATH" MOCK_LOG="$MOCK_LOG" \
    COMPOSE_FILES="docker-compose.community.yml docker-compose.uptrace.yml" \
    /bin/bash "$FIXTURE/nginx/init-letsencrypt.sh" >"$TMP_ROOT/tls.log" 2>&1; then
  ok "三个域名可完成首次 TLS 签发控制流"
else
  bad "三个域名可完成首次 TLS 签发控制流" "$(tail -20 "$TMP_ROOT/tls.log")"
fi
assert_match "API 域名证书存在" '^issued fullchain$' "$FIXTURE/data/certbot/conf/live/api.test.invalid/fullchain.pem"
assert_match "Admin 域名证书存在" '^issued fullchain$' "$FIXTURE/data/certbot/conf/live/admin.test.invalid/fullchain.pem"
assert_match "Uptrace 域名证书存在" '^issued fullchain$' "$FIXTURE/data/certbot/conf/live/uptrace.test.invalid/fullchain.pem"
if grep -q 'docker .*openssl req' "$MOCK_LOG"; then
  bad "OpenSSL 不应被误当成 Certbot 子命令" "$(grep 'docker .*openssl req' "$MOCK_LOG")"
else
  ok "临时证书由宿主机 OpenSSL 生成"
fi

: >"$MOCK_LOG"
if env PATH="$MOCK_BIN:$PATH" MOCK_LOG="$MOCK_LOG" MOCK_PULL_FAIL=1 \
    /bin/bash "$FIXTURE/install.sh" --edition community --yes \
    >"$TMP_ROOT/pull_failure.log" 2>&1; then
  bad "镜像拉取失败必须中止" "安装器错误返回成功"
else
  assert_match "镜像拉取失败必须中止" '镜像拉取失败，尚未启动任何服务' "$TMP_ROOT/pull_failure.log"
  if grep -q ' up -d$' "$MOCK_LOG"; then
    bad "镜像拉取失败不得启动服务" "$(grep ' up -d$' "$MOCK_LOG")"
  else
    ok "镜像拉取失败不得启动服务"
  fi
fi

rm "$FIXTURE/data/backend_priv/keys/login_rsa_priv.pem"
if env PATH="$MOCK_BIN:$PATH" MOCK_LOG="$MOCK_LOG" MOCK_SANITY_FAIL=1 \
    /bin/bash "$FIXTURE/install.sh" --edition community --yes \
    >"$TMP_ROOT/failure.log" 2>&1; then
  bad "sanity 失败必须向上传播" "安装器错误返回成功"
else
  assert_match "sanity 失败必须向上传播" 'sanity_check 未通过' "$TMP_ROOT/failure.log"
fi
openssl rsa -in "$FIXTURE/data/backend_priv/keys/login_rsa_priv.pem" -pubout \
  -out "$TMP_ROOT/derived_public.pem" 2>/dev/null
if cmp -s "$TMP_ROOT/derived_public.pem" "$FIXTURE/data/backend_priv/keys/login_rsa_pub.pem"; then
  ok "私钥重建时同步更新公钥"
else
  bad "私钥重建时同步更新公钥" "RSA 密钥对不匹配"
fi

echo "== 结果：PASS=${PASS} FAIL=${FAIL} =="
[ "$FAIL" -eq 0 ]

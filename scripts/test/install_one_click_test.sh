#!/usr/bin/env bash
# 一键安装控制流离线测试：Docker、网络、preflight/cleanup/sanity 均使用本地桩。
# Offline regression: no Docker daemon, network, deployment or Python.
set -uo pipefail

cd "$(dirname "$0")/../.." || exit 1

TMP_ROOT="$(mktemp -d /tmp/imboy_install_one_click.XXXXXX)"
FIXTURE="$TMP_ROOT/deploy"
MOCK_BIN="$TMP_ROOT/bin"
TOOLS_BIN="$TMP_ROOT/tools"
MOCK_LOG="$TMP_ROOT/events.log"
OUTPUT="$TMP_ROOT/install.log"

cleanup() {
  rm -rf -- "$TMP_ROOT"
}
trap cleanup EXIT

mkdir -p "$FIXTURE/nginx" "$FIXTURE/cleanup-db" "$TMP_ROOT/scripts" "$MOCK_BIN" "$TOOLS_BIN"
# 仅开放这些工具；宿主机即使有 Python，子进程 PATH 也找不到它。
for bin in awk cat chmod cmp cp cut date dirname env grep head jq mkdir mktemp mv \
    openssl rm sed seq sh stat tail touch tr wc; do
  tool_path="$(command -v "$bin")" || { echo "缺少测试依赖: $bin" >&2; exit 1; }
  ln -s "$tool_path" "$TOOLS_BIN/$bin"
done
TEST_PATH="$MOCK_BIN:$TOOLS_BIN"
cp deploy/install.sh deploy/init-cleanup-db.sh deploy/.env.example deploy/docker-compose.community.yml \
  deploy/docker-compose.uptrace.yml "$FIXTURE/"
cp deploy/cleanup-db/bootstrap.sql deploy/cleanup-db/verify.sql "$FIXTURE/cleanup-db/"
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
# 三域 + LiveKit 两域（CSD-DEP-01 / LK-DEP-01）都是 MANUAL_VARS 必填项
set_env CS_WIDGET_DOMAIN cs.test.invalid
set_env RTC_DOMAIN rtc.test.invalid
set_env TURN_DOMAIN turn.test.invalid
set_env CERTBOT_EMAIL certbot@test.invalid
set_env UPTRACE_ENABLED true
set_env UPTRACE_DOMAIN uptrace.test.invalid
set_env UPTRACE_ADMIN_EMAIL admin@test.invalid
set_env IMBOY_PAYMENT_GATEWAY_ENABLED true
set_env JWT_KEY keep_existing_jwt_key_1234567890123456
set_env IMBOY_DIRECT_CLEANUP_PG_USER imboy_cleanup

cat >"$MOCK_BIN/docker" <<'MOCK'
#!/bin/bash
set -euo pipefail
printf 'docker %s\n' "$*" >>"$MOCK_LOG"
# Fixtures contain simple assignments only; read them as data, not source/eval.
resolved_env() {
  jq -Rn '[inputs | select(test("^[A-Za-z_][A-Za-z0-9_]*=")) |
    capture("^(?<key>[^=]+)=(?<value>.*)$")] | from_entries | . + env' < .env
}
config_json() {
  resolved_env | jq '
    . as $e | ($e.BACKEND_IMAGE // "registry.test/backend:z") as $image |
    {services: {
      imboy_backend: {image: $image, environment: {
        IMBOY_BASE_URL: ("https://" + $e.API_DOMAIN),
        IMBOY_WIDGET_BASE_URL: ("https://" + $e.CS_WIDGET_DOMAIN),
        IMBOY_WIDGET_DOMAIN: $e.CS_WIDGET_DOMAIN,
        IMBOY_EDGE_INTERNAL_TOKEN: $e.IMBOY_EDGE_INTERNAL_TOKEN,
        IMBOY_JWT_KEY: $e.JWT_KEY, IMBOY_POSTGRE_AES_KEY: $e.POSTGRE_AES_KEY,
        IMBOY_ADM_COOKIE_SECRET: $e.ADM_COOKIE_SECRET,
        IMBOY_PG_USER: $e.POSTGRES_USER, IMBOY_PG_PASSWORD: $e.POSTGRES_PASSWORD,
        IMBOY_PG_HOST: "imboy_pg18", IMBOY_PG_PORT: "5432", IMBOY_PG_DB: $e.POSTGRES_DB,
        IMBOY_SOLIDIFIED_KEY: $e.IMBOY_SOLIDIFIED_KEY,
        IMBOY_SOLIDIFIED_KEY_IV: $e.IMBOY_SOLIDIFIED_KEY_IV,
        IMBOY_PASSWORD_SALT: $e.IMBOY_PASSWORD_SALT,
        IMBOY_LIVEKIT_API_KEY: $e.LIVEKIT_API_KEY,
        IMBOY_LIVEKIT_API_SECRET: $e.LIVEKIT_API_SECRET,
        IMBOY_DIRECT_CLEANUP_PG_USER: $e.IMBOY_DIRECT_CLEANUP_PG_USER,
        IMBOY_DIRECT_CLEANUP_PG_PASSWORD: $e.IMBOY_DIRECT_CLEANUP_PG_PASSWORD}},
      imboy_edge: {image: ($e.MOCK_EDGE_IMAGE // $image),
        environment: {EDGE_INTERNAL_TOKEN: $e.IMBOY_EDGE_INTERNAL_TOKEN}},
      imboy_nginx: {environment: {API_DOMAIN: $e.API_DOMAIN,
        CS_WIDGET_DOMAIN: $e.CS_WIDGET_DOMAIN}},
      imboy_pg18: {environment: {POSTGRES_USER: $e.POSTGRES_USER,
        POSTGRES_PASSWORD: $e.POSTGRES_PASSWORD, POSTGRES_DB: $e.POSTGRES_DB}},
      imboy_grafana: {environment: {GF_SECURITY_ADMIN_PASSWORD: $e.GRAFANA_ADMIN_PASSWORD}}
    }}' | jq "${MOCK_JSON_FILTER:-.}"
}
case " $* " in
  *" compose version "*) exit 0 ;;
  *" compose "*" config --environment "*)
    if [ -n "${MOCK_ENV_FILE:-}" ]; then cat "$MOCK_ENV_FILE"; else
      resolved_env | jq -r 'to_entries[] | select(.key | test("^(API|ADMIN|CS_WIDGET|RTC|TURN)_DOMAIN$")) | "\(.key)=\(.value)"'
    fi
    exit "${MOCK_ENV_RC:-0}" ;;
  *" compose "*" config --format json "*)
    count=0; [ ! -f "$MOCK_LOG.count" ] || read -r count < "$MOCK_LOG.count"
    count=$((count + 1)); printf '%s\n' "$count" > "$MOCK_LOG.count"
    mode="${MOCK_JSON_MODE:-valid}"
    [ "$count" -le 1 ] || mode="${MOCK_IMAGE_JSON_MODE:-$mode}"
    case "$mode" in
      empty) : ;;
      malformed) printf '{"services":' ;;
      multiple) config_json; printf '\n{}\n' ;;
      *) config_json ;;
    esac
    if [ "$count" -gt 1 ]; then exit "${MOCK_IMAGE_JSON_RC:-0}"; fi
    exit "${MOCK_JSON_RC:-0}" ;;
  *" compose "*" pull --policy missing "*) [ "${MOCK_PULL_FAIL:-0}" != 1 ]; exit ;;
  *" create --network none --entrypoint /bin/sh "*)
    printf '%s\n' "$6" >>"$MOCK_LOG.probes"
    [ "${MOCK_CREATE_FAIL:-0}" != 1 ] || exit 1
    echo test-edge-probe; exit 0 ;;
  *" start -a test-edge-probe "*) [ "${MOCK_PROBE_FAIL:-0}" != 1 ]; exit ;;
  *" compose "*" images -q imboy_backend "*) echo test-backend-image-id; exit 0 ;;
  *" inspect --format "*) echo 'ghcr.io/imboy-pub/imboy-backend@sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'; exit 0 ;;
  *" compose "*" exec "*"imboy_ctl adm create "*)
    [ "${MOCK_ADMIN_FAIL:-0}" != 1 ] || { echo 'mock admin setup failed'; exit 1; }
    printf 'ADMIN_ID=1000000001\nCREATED=true\n'; exit 0 ;;
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
case " $* " in *'/healthz '*) printf 'curl healthz\n' >>"$MOCK_LOG" ;; esac
[ "${MOCK_HEALTH_FAIL:-0}" != 1 ] || exit 1
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
  *preflight.sh)
    jq -n '{api: env.API_DOMAIN, admin: env.ADMIN_DOMAIN, widget: env.CS_WIDGET_DOMAIN,
      rtc: env.RTC_DOMAIN, turn: env.TURN_DOMAIN, base: env.IMBOY_BASE_URL,
      resolved: env.IMBOY_PREFLIGHT_RESOLVED_DOMAINS,
      unexpected: env.IMBOY_TEST_UNEXPECTED}' >"$MOCK_LOG.domains"
    [ "${MOCK_PREFLIGHT_FAIL:-0}" != 1 ] ;;
  *init-letsencrypt.sh) [ "${MOCK_TLS_FAIL:-0}" != 1 ] ;;
  *init-cleanup-db.sh) [ "${MOCK_CLEANUP_FAIL:-0}" != 1 ] ;;
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
INSTALL_EXTRA_ARGS=()
INSTALL_BASH_ARGS=()
run_install() {
  : >"$MOCK_LOG"; : >"$MOCK_LOG.probes"
  rm -f "$MOCK_LOG.count" "$MOCK_LOG.domains"
  env -i PATH="$TEST_PATH" MOCK_LOG="$MOCK_LOG" \
    IMBOY_PASSWORD_SALT=TEST_SECRET_MUST_NOT_LEAK_000000000000 "$@" \
    /bin/bash "${INSTALL_BASH_ARGS[@]}" "$FIXTURE/install.sh" --edition community --yes "${INSTALL_EXTRA_ARGS[@]}" >"$OUTPUT" 2>&1
}
reject_before_start() {
  local description="$1" reason="$2"; shift 2
  if run_install "$@"; then
    bad "$description" "安装器错误返回成功"
  elif grep -q ' up -d$' "$MOCK_LOG"; then
    bad "$description" "失败前启动了服务"
  elif ! grep -q -- "$reason" "$OUTPUT"; then
    bad "$description" "未命中预期失败阶段: $reason"
  else
    ok "$description"
  fi
  if grep -Eq 'TEST_SECRET_MUST_NOT_LEAK|keep_existing_jwt_key_1234567890123456' "$OUTPUT"; then
    bad "$description 密钥脱敏" "输出包含配置值"
  fi
}

echo "== 一键安装控制流（全离线 mock） =="
if env -i PATH="$TEST_PATH" /bin/sh -c 'for bin in python python2 python3 pip pip3; do command -v "$bin" && exit 0; done; exit 1' >/dev/null 2>&1; then
  bad "无 Python 测试 PATH" "意外发现 Python"
else
  ok "测试 PATH 不含 python/python2/python3/pip/pip3"
fi
if run_install; then
  ok "无 Python 环境下已有客户配置可一次执行完成"
else
  bad "已有客户配置可一次执行完成" "$(tail -20 "$OUTPUT")"
fi

assert_match "已有内部密钥不被覆盖" '^JWT_KEY=keep_existing_jwt_key_1234567890123456$' "$FIXTURE/.env"
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
assert_match "默认只验证清理账号，沿用完整 Compose 文件集合" 'bash COMPOSE_FILES=docker-compose\.community\.yml docker-compose\.uptrace\.yml args=init-cleanup-db\.sh$' "$MOCK_LOG"
health_line="$(grep -n '^curl healthz$' "$MOCK_LOG" | head -1 | cut -d: -f1)"
cleanup_line="$(grep -n 'args=init-cleanup-db\.sh$' "$MOCK_LOG" | head -1 | cut -d: -f1)"
sanity_line="$(grep -n 'args=\.\./scripts/sanity_check\.sh ' "$MOCK_LOG" | head -1 | cut -d: -f1)"
if [ -n "$health_line" ] && [ -n "$cleanup_line" ] && [ -n "$sanity_line" ] \
    && [ "$health_line" -lt "$cleanup_line" ] && [ "$cleanup_line" -lt "$sanity_line" ]; then
  ok "清理账号门禁在后端健康之后、正式验收之前执行"
else
  bad "清理账号门禁顺序" "未满足 healthz → cleanup → sanity"
fi

: >"$MOCK_LOG"
if env -i PATH="$TEST_PATH" MOCK_LOG="$MOCK_LOG" \
    COMPOSE_FILES="docker-compose.community.yml docker-compose.uptrace.yml" \
    /bin/bash "$FIXTURE/nginx/init-letsencrypt.sh" >"$TMP_ROOT/tls.log" 2>&1; then
  ok "五个必选域名和 Uptrace 可完成首次 TLS 签发控制流"
else
  bad "六个域名可完成首次 TLS 签发控制流" "$(tail -20 "$TMP_ROOT/tls.log")"
fi
assert_match "API 域名证书存在" '^issued fullchain$' "$FIXTURE/data/certbot/conf/live/api.test.invalid/fullchain.pem"
assert_match "Admin 域名证书存在" '^issued fullchain$' "$FIXTURE/data/certbot/conf/live/admin.test.invalid/fullchain.pem"
for domain in cs rtc turn; do
  assert_match "$domain 域名证书存在" '^issued fullchain$' "$FIXTURE/data/certbot/conf/live/$domain.test.invalid/fullchain.pem"
done
assert_match "Uptrace 域名证书存在" '^issued fullchain$' "$FIXTURE/data/certbot/conf/live/uptrace.test.invalid/fullchain.pem"
if grep -q 'docker .*openssl req' "$MOCK_LOG"; then
  bad "OpenSSL 不应被误当成 Certbot 子命令" "$(grep 'docker .*openssl req' "$MOCK_LOG")"
else
  ok "临时证书由宿主机 OpenSSL 生成"
fi

: >"$MOCK_LOG"
if run_install MOCK_PULL_FAIL=1; then
  bad "镜像拉取失败必须中止" "安装器错误返回成功"
else
  assert_match "镜像拉取失败必须中止" '镜像拉取失败，尚未启动任何服务' "$OUTPUT"
  if grep -q ' up -d$' "$MOCK_LOG"; then
    bad "镜像拉取失败不得启动服务" "$(grep ' up -d$' "$MOCK_LOG")"
  else
    ok "镜像拉取失败不得启动服务"
  fi
fi

rm "$FIXTURE/data/backend_priv/keys/login_rsa_priv.pem"
if run_install MOCK_SANITY_FAIL=1; then
  bad "sanity 失败必须向上传播" "安装器错误返回成功"
else
  assert_match "sanity 失败必须向上传播" 'sanity_check 未通过' "$OUTPUT"
fi
openssl rsa -in "$FIXTURE/data/backend_priv/keys/login_rsa_priv.pem" -pubout \
  -out "$TMP_ROOT/derived_public.pem" 2>/dev/null
if cmp -s "$TMP_ROOT/derived_public.pem" "$FIXTURE/data/backend_priv/keys/login_rsa_pub.pem"; then
  ok "私钥重建时同步更新公钥"
else
  bad "私钥重建时同步更新公钥" "RSA 密钥对不匹配"
fi

# 重跑不得更换已有密钥、RSA 或业务数据。
cp "$FIXTURE/.env" "$TMP_ROOT/env.before"
cp "$FIXTURE/data/backend_priv/keys/login_rsa_priv.pem" "$TMP_ROOT/rsa.before"
printf 'existing customer data\n' >"$FIXTURE/data/keep-data"
if run_install && cmp -s "$FIXTURE/.env" "$TMP_ROOT/env.before" \
    && cmp -s "$FIXTURE/data/backend_priv/keys/login_rsa_priv.pem" "$TMP_ROOT/rsa.before" \
    && grep -qx 'existing customer data' "$FIXTURE/data/keep-data"; then
  ok "重复安装保留全部现有密钥、RSA 私钥与数据"
else
  bad "重复安装数据保护" "重跑失败或既有内容变化"
fi
if [ "$(stat -c '%a' "$FIXTURE/.env")" = 600 ] \
    && [ "$(stat -c '%a' "$FIXTURE/data/backend_priv/keys/login_rsa_priv.pem")" = 600 ]; then
  ok ".env 和 RSA 私钥保持 0600"
else
  bad "敏感文件权限" "不是 0600"
fi

# --init-cleanup-db 只授权 helper 的 create 模式，不改变凭据生成的幂等约定。
# The helper itself is mocked here; real LOGIN/password/role checks need PostgreSQL.
INSTALL_EXTRA_ARGS=(--init-cleanup-db)
if run_install; then
  assert_match "显式初始化传入 --create 并沿用 Compose 文件集合" 'bash COMPOSE_FILES=docker-compose\.community\.yml docker-compose\.uptrace\.yml args=init-cleanup-db\.sh --create$' "$MOCK_LOG"
else
  bad "显式清理账号初始化控制流" "安装器未成功调用 helper"
fi
if run_install && cmp -s "$FIXTURE/.env" "$TMP_ROOT/env.before"; then
  ok "重复显式初始化保留清理账号及所有已有凭据"
else
  bad "重复显式初始化凭据保护" "重跑失败或既有配置变化"
fi
INSTALL_EXTRA_ARGS=()

echo "== 解析与失败关闭（无服务启动） =="
INSTALL_EXTRA_ARGS=(--edition business --init-cleanup-db)
reject_before_start "商务版拒绝隐式使用社区库管理流程" '--init-cleanup-db'
[ ! -s "$MOCK_LOG" ] && ok "商务版初始化参数拒绝发生在 Docker 操作前" \
  || bad "商务版初始化参数拒绝过晚" "拒绝前已访问 Docker"
INSTALL_EXTRA_ARGS=()
mv "$TOOLS_BIN/jq" "$TMP_ROOT/jq.saved"
reject_before_start "缺 jq 必须中止" 'jq'
[ ! -s "$MOCK_LOG" ] && ok "缺 jq 时尚未访问 Docker" || bad "缺 jq 时已访问 Docker"
mv "$TMP_ROOT/jq.saved" "$TOOLS_BIN/jq"
mv "$TMP_ROOT/scripts/sanity_check.sh" "$TMP_ROOT/sanity.saved"
reject_before_start "缺正式验收脚本必须中止" 'sanity_check'
mv "$TMP_ROOT/sanity.saved" "$TMP_ROOT/scripts/sanity_check.sh"
for dependency in init-cleanup-db.sh cleanup-db/bootstrap.sql cleanup-db/verify.sql; do
  mv "$FIXTURE/$dependency" "$TMP_ROOT/cleanup-dependency.saved"
  reject_before_start "缺清理账号依赖 $dependency 必须中止" "$dependency"
  mv "$TMP_ROOT/cleanup-dependency.saved" "$FIXTURE/$dependency"
done
reject_before_start "preflight 失败不得启动服务" 'preflight 未通过' MOCK_PREFLIGHT_FAIL=1
reject_before_start "环境输出有效但 Compose 失败必须中止" 'Compose 环境解析失败' MOCK_ENV_RC=23
: >"$TMP_ROOT/env.empty"
reject_before_start "空域名输出且无继承值必须中止" 'preflight 未通过' "MOCK_ENV_FILE=$TMP_ROOT/env.empty"
for mode in empty malformed multiple; do
  reject_before_start "Compose JSON $mode 必须中止" 'Compose 密钥校验失败' "MOCK_JSON_MODE=$mode"
done
reject_before_start "密钥 JSON 有效但生产者失败必须中止" 'Compose 密钥校验失败' MOCK_JSON_RC=24

# 每次只破坏一个原有安全属性；同时捕获 jq 的 null 隐式转换差异。
while IFS='|' read -r description filter; do
  reject_before_start "$description" 'Compose 密钥校验失败' "MOCK_JSON_FILTER=$filter"
done <<'CASES'
缺 services|del(.services)
services 类型错误|.services = []
缺 backend environment|del(.services.imboy_backend.environment)
JWT 缺失|del(.services.imboy_backend.environment.IMBOY_JWT_KEY)
JWT 为 null|.services.imboy_backend.environment.IMBOY_JWT_KEY = null
JWT 为数字|.services.imboy_backend.environment.IMBOY_JWT_KEY = 123
JWT 太短|.services.imboy_backend.environment.IMBOY_JWT_KEY = "short"
JWT CHANGE_ME 占位符|.services.imboy_backend.environment.IMBOY_JWT_KEY = "CHANGE_ME_00000000000000000000000000000000"
JWT example 占位符不区分大小写|.services.imboy_backend.environment.IMBOY_JWT_KEY = "ExAmPlE_00000000000000000000000000000000"
核心密钥复用|.services.imboy_backend.environment.IMBOY_POSTGRE_AES_KEY = .services.imboy_backend.environment.IMBOY_JWT_KEY
edge token 不一致|.services.imboy_edge.environment.EDGE_INTERNAL_TOKEN = "different"
PG 用户不一致|.services.imboy_pg18.environment.POSTGRES_USER = "different"
PG 密码不一致|.services.imboy_pg18.environment.POSTGRES_PASSWORD = "different"
Grafana 密码为空|.services.imboy_grafana.environment.GF_SECURITY_ADMIN_PASSWORD = ""
清理用户缺失|del(.services.imboy_backend.environment.IMBOY_DIRECT_CLEANUP_PG_USER)
清理密码缺失|del(.services.imboy_backend.environment.IMBOY_DIRECT_CLEANUP_PG_PASSWORD)
清理密码过短|.services.imboy_backend.environment.IMBOY_DIRECT_CLEANUP_PG_PASSWORD = "short"
清理用户为 null|.services.imboy_backend.environment.IMBOY_DIRECT_CLEANUP_PG_USER = null
清理用户复用应用用户|.services.imboy_backend.environment.IMBOY_DIRECT_CLEANUP_PG_USER = .services.imboy_backend.environment.IMBOY_PG_USER
清理密码复用应用密码|.services.imboy_backend.environment.IMBOY_DIRECT_CLEANUP_PG_PASSWORD = .services.imboy_backend.environment.IMBOY_PG_PASSWORD
API origin 不一致|.services.imboy_backend.environment.IMBOY_BASE_URL = "https://wrong.test.invalid"
Widget origin 不一致|.services.imboy_backend.environment.IMBOY_WIDGET_BASE_URL = "https://wrong.test.invalid"
Widget domain 不一致|.services.imboy_backend.environment.IMBOY_WIDGET_DOMAIN = "wrong.test.invalid"
API domain null 不得拼接为空串|.services.imboy_nginx.environment.API_DOMAIN = null | .services.imboy_backend.environment.IMBOY_BASE_URL = "https://"
Widget domain null 不得拼接为空串|.services.imboy_nginx.environment.CS_WIDGET_DOMAIN = null | .services.imboy_backend.environment.IMBOY_WIDGET_BASE_URL = "https://" | .services.imboy_backend.environment.IMBOY_WIDGET_DOMAIN = null
CASES
for key in IMBOY_PG_USER IMBOY_PG_PASSWORD IMBOY_SOLIDIFIED_KEY IMBOY_SOLIDIFIED_KEY_IV \
    IMBOY_PASSWORD_SALT IMBOY_LIVEKIT_API_KEY IMBOY_LIVEKIT_API_SECRET; do
  reject_before_start "必需字段 $key 缺失" 'Compose 密钥校验失败' \
    "MOCK_JSON_FILTER=del(.services.imboy_backend.environment.$key)"
done

# 监控仍为可选；清理凭据不得同时缺失后绕过安装器校验。
reject_before_start "清理用户与密码同时缺失必须中止" 'Compose 密钥校验失败' \
  'MOCK_JSON_FILTER=del(.services.imboy_backend.environment.IMBOY_DIRECT_CLEANUP_PG_USER, .services.imboy_backend.environment.IMBOY_DIRECT_CLEANUP_PG_PASSWORD)'
if run_install 'MOCK_JSON_FILTER=del(.services.imboy_grafana)'; then
  ok "未声明的可选监控保持原语义"
else
  bad "可选监控兼容语义" "有效配置被拒绝"
fi

echo "== 镜像识别与 probe =="
if run_install BACKEND_IMAGE=registry.test/z-backend:override MOCK_EDGE_IMAGE=registry.test/a-edge:override; then
  printf '%s\n' registry.test/z-backend:override registry.test/a-edge:override >"$TMP_ROOT/images.expected"
  if cmp -s "$TMP_ROOT/images.expected" "$MOCK_LOG.probes"; then
    ok "实际镜像覆盖值按 backend、edge 顺序分别检查"
  else bad "镜像顺序或覆盖值" "probe 不符合实际 Compose 镜像"; fi
else bad "独立镜像覆盖" "有效配置失败"; fi
if run_install && [ "$(wc -l <"$MOCK_LOG.probes")" -eq 1 ]; then
  ok "相同 backend/edge 镜像只检查一次"
else bad "镜像稳定去重" "probe 次数异常"; fi
for filter in 'del(.services.imboy_backend.image)' \
    '.services.imboy_backend.image = null' '.services.imboy_edge.image = 123' \
    '.services.imboy_edge.image = ""' '.services.imboy_edge.image = "first\nsecond"'; do
  reject_before_start "无效镜像: $filter" 'Compose 镜像解析失败' "MOCK_JSON_FILTER=$filter"
done
for mode in empty malformed multiple; do
  reject_before_start "镜像阶段 JSON $mode 必须中止" 'Compose 镜像解析失败' "MOCK_IMAGE_JSON_MODE=$mode"
done
reject_before_start "镜像 JSON 有效但生产者失败必须中止" 'Compose 镜像解析失败' MOCK_IMAGE_JSON_RC=25
reject_before_start "probe 创建失败不得启动服务" '无法创建 edge' MOCK_CREATE_FAIL=1
reject_before_start "probe 文件检查失败不得启动服务" '缺少可运行的 edge' MOCK_PROBE_FAIL=1

echo "== Compose 环境覆盖与字面值解析 =="
if run_install API_DOMAIN=override.test.invalid; then
  if jq -e '.api == "override.test.invalid" and .base == "https://override.test.invalid" and .resolved == "1"' "$MOCK_LOG.domains" >/dev/null; then
    ok "Compose 环境覆盖正确传入 preflight"
  else bad "域名覆盖" "preflight 环境不符"; fi
else bad "域名覆盖" "有效覆盖配置失败"; fi
if run_install "MOCK_ENV_FILE=$TMP_ROOT/env.empty" API_DOMAIN=api.test.invalid \
    ADMIN_DOMAIN=admin.test.invalid CS_WIDGET_DOMAIN=cs.test.invalid \
    RTC_DOMAIN=rtc.test.invalid TURN_DOMAIN=turn.test.invalid; then
  ok "保留五个域名的已继承环境回退语义"
else bad "域名环境回退" "完整继承环境被拒绝"; fi
# 仅验证 parser 数据边界；真实域名合法性仍由真实 preflight 负责。
literal='literal=$(printf should-not-run)=with spaces'
printf '%s\n' "API_DOMAIN=$literal" ADMIN_DOMAIN=admin.test.invalid \
  CS_WIDGET_DOMAIN=cs.test.invalid RTC_DOMAIN=rtc.test.invalid TURN_DOMAIN=turn.test.invalid \
  'PATH=/invalid-path' 'IMBOY_TEST_UNEXPECTED=not-exported' 'unrelated multiline data' >"$TMP_ROOT/env.literal"
if run_install "MOCK_ENV_FILE=$TMP_ROOT/env.literal" \
    'MOCK_JSON_FILTER=.services.imboy_backend.environment.IMBOY_JWT_KEY = "TEST_SECRET_MUST_NOT_LEAK=first\n$(printf data-only)"'; then
  if jq -e --arg literal "$literal" '.api == $literal and .unexpected == null' "$MOCK_LOG.domains" >/dev/null \
      && ! grep -q TEST_SECRET_MUST_NOT_LEAK "$OUTPUT"; then
    ok "等号、空格、换行和命令语法保持数据，未知环境键不导入"
  else bad "字面值解析或密钥脱敏" "值变化、未知键泄漏或日志包含密钥"; fi
else bad "字面值解析" "预检桩未收到字面值"; fi

echo "== 初始化与安装后失败传播 =="
INSTALL_EXTRA_ARGS=(--admin-phone 13800000001 --admin-password DummyAdminPass123)
if run_install IMBOY_CTL_COOKIE=mock_cookie_only \
    && grep -q 'ADMIN_ID=1000000001' "$OUTPUT"; then
  ok "无 Python 环境经既有容器内 CLI 初始化管理员"
else bad "管理员初始化" "未取得成功结果"; fi
# Even an explicit bash -x -v must stop tracing before loading credentials.
INSTALL_BASH_ARGS=(-x -v)
INSTALL_EXTRA_ARGS=(--init-cleanup-db --admin-phone 13800000001 --admin-password DummyAdminPass123)
if run_install IMBOY_CTL_COOKIE=mock_cookie_only; then
  leaked=0
  while IFS= read -r secret; do
    [ -n "$secret" ] || continue
    if grep -Fq -- "$secret" "$OUTPUT" "$MOCK_LOG"; then leaked=1; fi
  done < <(
    awk -F= '$1 == "POSTGRES_PASSWORD" || $1 == "IMBOY_DIRECT_CLEANUP_PG_PASSWORD" \
      {sub(/^[^=]*=/, ""); print}' "$FIXTURE/.env"
    printf '%s\n' TEST_SECRET_MUST_NOT_LEAK_000000000000 \
      keep_existing_jwt_key_1234567890123456 DummyAdminPass123 mock_cookie_only
  )
  if [ "$leaked" -eq 0 ]; then
    ok "bash -x -v 下清理/数据库/管理员凭据不进入输出或命令日志"
  else
    bad "Shell 跟踪脱敏" "输出或命令日志包含测试凭据（不回显）"
  fi
else
  bad "Shell 跟踪模式" "安装控制流未完成"
fi
INSTALL_BASH_ARGS=()
INSTALL_EXTRA_ARGS=(--admin-phone 13800000001 --admin-password DummyAdminPass123)
if run_install; then
  bad "初始化缺 cookie 必须失败" "安装器错误返回成功"
else
  assert_match "初始化缺 cookie 必须失败" 'IMBOY_CTL_COOKIE' "$OUTPUT"
fi
if run_install IMBOY_CTL_COOKIE=mock_cookie_only MOCK_ADMIN_FAIL=1; then
  bad "管理员初始化失败必须传播" "安装器错误返回成功"
else
  assert_match "管理员初始化失败必须传播" '超管创建失败' "$OUTPUT"
fi
INSTALL_EXTRA_ARGS=()
for mode in verify create; do
  [ "$mode" != create ] || INSTALL_EXTRA_ARGS=(--init-cleanup-db)
  if run_install MOCK_CLEANUP_FAIL=1; then
    bad "清理账号 $mode 失败必须中止" "安装器错误返回成功"
  elif ! grep -q 'args=init-cleanup-db\.sh' "$MOCK_LOG"; then
    bad "清理账号 $mode 失败阶段" "未执行清理账号 helper"
  elif grep -q 'args=\.\./scripts/sanity_check\.sh' "$MOCK_LOG" \
      || grep -q '部署完成' "$OUTPUT"; then
    bad "清理账号 $mode 失败不得继续验收或宣称完成" "失败后仍继续交付"
  else
    ok "清理账号 $mode 失败传播，不执行 sanity 或宣称完成"
  fi
done
if run_install && cmp -s "$FIXTURE/.env" "$TMP_ROOT/env.before"; then
  ok "清理账号步骤失败后可重跑，配置和凭据保持不变"
else
  bad "清理账号失败恢复控制流" "重跑失败或凭据被更换"
fi
INSTALL_EXTRA_ARGS=()
for stage in TLS HEALTH; do
  if run_install "MOCK_${stage}_FAIL=1"; then
    bad "$stage 失败必须中止" "安装器错误返回成功"
  elif grep -q '部署完成' "$OUTPUT"; then
    bad "$stage 失败不得宣称完成" "出现部署完成文案"
  else ok "$stage 失败传播且不宣称完成"; fi
  if grep -q 'args=init-cleanup-db\.sh' "$MOCK_LOG" \
      || grep -q 'args=\.\./scripts/sanity_check\.sh' "$MOCK_LOG"; then
    bad "$stage 失败不得执行清理账号步骤或 sanity" "前置失败后仍继续"
  else
    ok "$stage 失败时不执行清理账号步骤或 sanity"
  fi
done

echo "== 结果：PASS=${PASS} FAIL=${FAIL} =="
[ "$FAIL" -eq 0 ]

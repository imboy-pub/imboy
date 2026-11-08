#!/usr/bin/env bash
# Offline package-entry regression; real Bash/flock, mock original installer.
# All mutations stay inside a temporary fixture. No Docker, network or Python.
set -uo pipefail
TEST_SOURCE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TEST_ENTRY="$TEST_SOURCE_ROOT/deploy/standalone/install.sh"
if [ ! -f "$TEST_ENTRY" ]; then
  TEST_ENTRY="$TEST_SOURCE_ROOT/install.sh"
fi
TEST_TOOL_PATH="$PATH"
TEST_BASH="$(command -v bash)"
TEST_TMP="$(mktemp -d /tmp/imboy_delivery_guard.XXXXXX)"
trap 'rm -rf -- "$TEST_TMP"' EXIT
PASS=0
FAIL=0
CASE_NO=0
EXTRA_ENV=()
ok() { PASS=$((PASS + 1)); printf 'PASS %s\n' "$1"; }
bad() { FAIL=$((FAIL + 1)); printf 'FAIL %s\n' "$1"; }

fixture() {
  CASE_NO=$((CASE_NO + 1))
  CASE_ROOT="$TEST_TMP/case $CASE_NO"
  CASE_LOG="$TEST_TMP/events.$CASE_NO"
  CASE_OUT="$TEST_TMP/output.$CASE_NO"
  EXTRA_ENV=()
  mkdir -p "$CASE_ROOT/scripts" "$CASE_ROOT/deploy"
  cp "$TEST_ENTRY" "$CASE_ROOT/install.sh"
  cp "$TEST_SOURCE_ROOT/scripts/install_guard.sh" "$CASE_ROOT/scripts/install_guard.sh"
  cp "$TEST_SOURCE_ROOT/deploy/.env.example" "$CASE_ROOT/deploy/.env.example"
  cp "$CASE_ROOT/deploy/.env.example" "$CASE_ROOT/deploy/.env"
  cat >"$CASE_ROOT/scripts/check_delivery.sh" <<'MOCK'
#!/usr/bin/env bash
exit "${MOCK_CHECK_RC:-0}"
MOCK
  cat >"$CASE_ROOT/deploy/install.sh" <<'MOCK'
#!/usr/bin/env bash
set -eu
printf '%s\n' "$*" >>"$MOCK_EVENTS"
exit "${MOCK_INSTALL_RC:-0}"
MOCK
}
set_key() {
  awk -v k="$1" -v v="$2" 'BEGIN{FS=OFS="="} $1==k {print k"="v; found=1; next} {print} END{if(!found) print k"="v}' \
    "$CASE_ROOT/deploy/.env" >"$CASE_ROOT/deploy/.env.tmp"
  mv "$CASE_ROOT/deploy/.env.tmp" "$CASE_ROOT/deploy/.env"
}
existing_state() {
  local key
  for key in POSTGRES_PASSWORD JWT_KEY POSTGRE_AES_KEY ADM_COOKIE_SECRET \
    IMBOY_EDGE_INTERNAL_TOKEN GRAFANA_ADMIN_PASSWORD IMBOY_SOLIDIFIED_KEY \
    IMBOY_SOLIDIFIED_KEY_IV IMBOY_PASSWORD_SALT LIVEKIT_API_KEY LIVEKIT_API_SECRET \
    GARAGE_RPC_SECRET IMBOY_GARAGE_ACCESS_KEY IMBOY_GARAGE_SECRET_KEY \
    IMBOY_DIRECT_CLEANUP_PG_PASSWORD; do
    set_key "$key" "OFFLINE_DUMMY_${key}_0123456789abcdef"
  done
  mkdir -p "$CASE_ROOT/deploy/data/pg18" "$CASE_ROOT/deploy/data/garage" \
    "$CASE_ROOT/deploy/data/backend_priv/keys" "$CASE_ROOT/deploy/data/certbot/conf"
  printf '18\n' >"$CASE_ROOT/deploy/data/pg18/PG_VERSION"
  printf 'object data sentinel\n' >"$CASE_ROOT/deploy/data/garage/sentinel"
  printf 'offline dummy private key sentinel; not a PEM\n' >"$CASE_ROOT/deploy/data/backend_priv/keys/login_rsa_priv.pem"
  printf 'offline dummy certificate sentinel; not a PEM\n' >"$CASE_ROOT/deploy/data/certbot/conf/sentinel"
}
run_entry() {
  (cd / && env -i PATH="$TEST_TOOL_PATH" MOCK_EVENTS="$CASE_LOG" "${EXTRA_ENV[@]}" \
    "$TEST_BASH" "$CASE_ROOT/install.sh" "$@") >"$CASE_OUT" 2>&1
}
reject() {
  local description="$1" marker="$2"; shift 2
  if run_entry "$@"; then
    bad "$description (unexpected success)"
  elif [ -e "$CASE_LOG" ]; then
    bad "$description (original installer was called)"
  elif ! grep -qF -- "$marker" "$CASE_OUT"; then
    bad "$description (expected error not found)"
  else
    ok "$description"
  fi
}

fixture
if run_entry && grep -qx -- '--edition community' "$CASE_LOG"; then
  ok 'fresh template invokes community installer without implicit --yes (outside cwd, spaced path)'
else bad 'fresh template delegation'; fi

fixture
rm "$CASE_ROOT/deploy/.env"
if run_entry --check && [ ! -e "$CASE_ROOT/.delivery-install.lock" ] \
  && [ ! -e "$CASE_ROOT/deploy/.env" ] && [ ! -e "$CASE_LOG" ]; then
  ok '--check with no state or .env is read-only and creates no lock'
else bad 'read-only first check'; fi

fixture
rm "$CASE_ROOT/deploy/.env"
EXTRA_ENV=(TZ=Asia/Shanghai)
if run_entry --check && [ ! -e "$CASE_ROOT/.delivery-install.lock" ] \
  && [ ! -e "$CASE_ROOT/deploy/.env" ] && [ ! -e "$CASE_LOG" ]; then
  ok 'unconfigured package can be checked with an inherited host timezone'
else bad 'first check with host timezone'; fi

fixture
existing_state
cp "$CASE_ROOT/deploy/.env" "$TEST_TMP/env.before"
cp "$CASE_ROOT/deploy/data/backend_priv/keys/login_rsa_priv.pem" "$TEST_TMP/private.before"
if run_entry && cmp -s "$CASE_ROOT/deploy/.env" "$TEST_TMP/env.before" \
  && cmp -s "$CASE_ROOT/deploy/data/backend_priv/keys/login_rsa_priv.pem" "$TEST_TMP/private.before" \
  && grep -qx '18' "$CASE_ROOT/deploy/data/pg18/PG_VERSION" \
  && grep -qx 'object data sentinel' "$CASE_ROOT/deploy/data/garage/sentinel" \
  && grep -qx 'offline dummy certificate sentinel; not a PEM' "$CASE_ROOT/deploy/data/certbot/conf/sentinel"; then
  ok 'existing .env, private key, PG/Garage/certificate sentinels remain unchanged'
else bad 'existing state preservation'; fi
if [ ! -e "$CASE_ROOT/deploy/data/backend_priv/keys/login_rsa_pub.pem" ] && run_entry; then
  ok 'missing public key permits original installer to derive it from existing private key'
else bad 'public key repair delegation'; fi

fixture
existing_state
rm "$CASE_ROOT/deploy/.env"
reject 'existing data without .env is blocked' '已有数据但 deploy/.env 缺失'

fixture
existing_state
set_key JWT_KEY ''
reject 'existing data with empty key is blocked' '已有数据但 JWT_KEY'

fixture
existing_state
set_key IMBOY_GARAGE_SECRET_KEY CHANGE_ME_KEEP_OLD_STORAGE_KEY
reject 'existing data with placeholder storage key is blocked' '已有数据但 IMBOY_GARAGE_SECRET_KEY'

fixture
existing_state
rm "$CASE_ROOT/deploy/data/backend_priv/keys/login_rsa_priv.pem"
reject 'existing data without private key is blocked' '已有数据但 RSA 私钥'

for spec in 'DATA_DIR=/opt/imboy-data' 'BACKEND_PORT=19800' 'UPTRACE_ENABLED=true' 'IMBOY_EB_DIRECT_ENABLED=true'; do
  fixture
  set_key "${spec%%=*}" "${spec#*=}"
  reject "unsupported ${spec%%=*} is blocked" '此交付包要求'
done

fixture
EXTRA_ENV=(API_DOMAIN=override.test.invalid)
reject 'inherited domain override is blocked' '继承环境中的 API_DOMAIN'

fixture
EXTRA_ENV=(BACKEND_IMAGE=registry.test.invalid/backend:offline)
reject 'inherited commented-template image override is blocked' '继承环境中的 BACKEND_IMAGE'

fixture
EXTRA_ENV=(POSTGRES_USER=imboy_user)
if run_entry; then ok 'inherited value identical to .env is accepted'; else bad 'matching inherited value'; fi

for key in COMPOSE_FILE COMPOSE_FILES COMPOSE_PROFILES COMPOSE_PROJECT_NAME; do
  fixture
  EXTRA_ENV=("$key=")
  reject "inherited $key (even empty) is blocked" "继承环境包含 $key"
done

fixture
set_key COMPOSE_PROFILES enterprise-direct
reject '.env cannot activate an unsupported Compose profile' '.env 内禁止配置 COMPOSE_PROFILES'

fixture
printf '\nJWT_KEY=OFFLINE_DUPLICATE\n' >>"$CASE_ROOT/deploy/.env"
reject 'duplicate .env key is blocked' '.env 重复配置键：JWT_KEY'

fixture
set_key BACKEND_PORT '"9800"'
reject 'quoted .env values are rejected without evaluation' '须为无引号字面值'

fixture
set_key JWT_KEY '$(printf not-executed)'
reject 'shell substitution remains data and is rejected' '须为无引号字面值'
if grep -qF '$(printf not-executed)' "$CASE_OUT"; then
  bad 'error messages disclose a configuration value'
else ok 'guard errors show field names without secret values'; fi

fixture
reject 'business edition cannot be selected' '此包仅支持 --edition community' --edition business

fixture
if run_entry --yes && grep -qx -- '--edition community --yes' "$CASE_LOG" \
  && grep -qF 'Docker 自动安装' "$CASE_OUT"; then
  ok 'explicit --yes is forwarded with Docker installation notice'
else bad 'explicit --yes delegation'; fi

fixture
if run_entry --init-cleanup-db && grep -qx -- '--edition community --init-cleanup-db' "$CASE_LOG"; then
  ok 'explicit cleanup initialization is forwarded after delivery guards'
else bad 'cleanup initialization delegation'; fi

fixture
reject '--check cannot create a cleanup LOGIN' '--check 不接受部署参数' --check --init-cleanup-db

fixture
exec 9>"$CASE_ROOT/.delivery-install.lock"
flock -n 9 || { bad 'test could not acquire fixture lock'; exit 1; }
reject 'concurrent installation is rejected before original installer' '本目录已有安装进程'
if run_entry --check && [ ! -e "$CASE_LOG" ]; then
  ok '--check stays read-only while installation lock is held'
else bad 'check during active lock'; fi
flock -u 9
exec 9>&-

fixture
EXTRA_ENV=(MOCK_CHECK_RC=29)
run_entry
rc=$?
if [ "$rc" -eq 29 ] && [ ! -e "$CASE_ROOT/.delivery-install.lock" ] && [ ! -e "$CASE_LOG" ]; then
  ok 'integrity-check failure code propagates before lock or installer'
else bad 'integrity-check failure propagation'; fi

fixture
EXTRA_ENV=(MOCK_INSTALL_RC=37)
run_entry
rc=$?
EXTRA_ENV=()
if [ "$rc" -eq 37 ] && run_entry; then
  ok 'original installer failure code propagates and releases lock'
else bad 'original failure propagation or lock release'; fi

printf 'RESULT PASS=%s FAIL=%s\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]

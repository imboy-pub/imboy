#!/usr/bin/env bash
# 独立 migrate 入口离线测试：验证 Gate 发现的真实节点名会传给迁移 RPC。
set -uo pipefail

cd "$(dirname "$0")/../.."

TMP_ROOT="$(mktemp -d /tmp/imboy_migrate_gate.XXXXXX)"
MOCK_BIN="$TMP_ROOT/bin"
MOCK_LOG="$TMP_ROOT/ssh.log"
MOCK_CALLS="$TMP_ROOT/ssh.calls"
mkdir -p "$MOCK_BIN"

cleanup() {
  rm -rf -- "$TMP_ROOT"
}
trap cleanup EXIT

cp scripts/imboy-deploy.sh "$TMP_ROOT/imboy-deploy.sh"

write_env() {
  local nginx_conf="${1:-/etc/nginx/imboy.conf}"
  local admin_remote="${2:-/www/wwwroot/admin}"
  local server_user="${3:-tester}"
  printf '%s\n' \
  'SERVER_HOST=example.invalid' \
  'SERVER_PORT=2222' \
  'DEPLOY_VSN=1.0.0' \
  'DEPLOY_PROJECT_DIR=/srv/imboy' \
  'DEPLOY_BRANCH=main' \
  'DEPLOY_BLUE_PORT=9800' \
  'DEPLOY_GREEN_PORT=9801' \
  'DEPLOY_COOKIE=testcookie' \
  'ADMIN_BUILD_DIR=../imboyadmin' \
  'DB_CONTAINER=postgres' \
  'DB_NAME=imboy' \
  'DB_USER=postgres' \
  'DEPLOY_EXPAND_MIGRATIONS=00000064_msg_store_sender_did.up.sql' \
  >"$TMP_ROOT/.env.deploy"
  printf 'SERVER_USER=%q\nNGINX_CONF=%q\nADMIN_REMOTE_DIR=%q\n' \
    "$server_user" "$nginx_conf" "$admin_remote" >>"$TMP_ROOT/.env.deploy"
}

write_env

cat >"$MOCK_BIN/ssh" <<'MOCK'
#!/usr/bin/env bash
set -u

for last_arg in "$@"; do :; done
cmd="${last_arg:-}"
printf '%s\n' SSH_CALL >>"$MOCK_CALLS"

case "$cmd" in
  *"BLUE_STATE="*"ACTIVE_PID="*)
    [ "${MOCK_GATE_STATE:-ok}" = ok ] || exit 2
    printf '%s\n' "${MOCK_CTL_NODE:-08171234@127.0.0.1}"
    ;;
  *"make ctl ARGS='db migrate'"*)
    printf '%s\n' "$cmd" >>"$MOCK_LOG"
    ;;
esac
exit 0
MOCK
chmod +x "$MOCK_BIN/ssh"

PASS=0
FAIL=0

ok() {
  PASS=$((PASS + 1))
  echo "  PASS $1"
}

bad() {
  FAIL=$((FAIL + 1))
  echo "  FAIL $1: ${2:-<无详情>}"
}

run_migrate() {
  : >"$MOCK_LOG"
  : >"$MOCK_CALLS"
  env PATH="$MOCK_BIN:$PATH" \
    MOCK_LOG="$MOCK_LOG" \
    MOCK_CALLS="$MOCK_CALLS" \
    MOCK_GATE_STATE="${1:-ok}" \
    MOCK_CTL_NODE="${2:-08171234@127.0.0.1}" \
    bash "$TMP_ROOT/imboy-deploy.sh" migrate \
    >"$TMP_ROOT/output.log" 2>&1
}

echo "== 独立 migrate Gate（全离线 mock） =="

if run_migrate ok '08171234@127.0.0.1' \
   && grep -q "CTL_NODE='08171234@127.0.0.1'" "$MOCK_LOG"; then
  ok "迁移 RPC 使用 Gate 从活动监听进程发现的节点名"
else
  bad "迁移 RPC 未使用活动节点名" "$(tr '\n' ',' <"$MOCK_LOG")"
fi

NGINX_PWN="$TMP_ROOT/nginx_pwn"
write_env "/tmp/x';touch $NGINX_PWN;#" /www/wwwroot/admin tester
: >"$MOCK_CALLS"
if env PATH="$MOCK_BIN:$PATH" MOCK_CALLS="$MOCK_CALLS" MOCK_LOG="$MOCK_LOG" \
   bash "$TMP_ROOT/imboy-deploy.sh" rollback >"$TMP_ROOT/output.log" 2>&1; then
  bad "恶意 NGINX_CONF 应在 SSH 前被拒绝" ""
elif grep -q 'NGINX_CONF 必须是' "$TMP_ROOT/output.log" \
     && [ ! -s "$MOCK_CALLS" ] && [ ! -e "$NGINX_PWN" ]; then
  ok "恶意 NGINX_CONF 未触发 SSH"
else
  bad "恶意 NGINX_CONF 未命中预期 allowlist" "$(<"$TMP_ROOT/output.log")"
fi

ADMIN_PWN="$TMP_ROOT/admin_pwn"
write_env /etc/nginx/imboy.conf "/www/wwwroot/admin';touch $ADMIN_PWN;#" tester
: >"$MOCK_CALLS"
if env PATH="$MOCK_BIN:$PATH" MOCK_CALLS="$MOCK_CALLS" MOCK_LOG="$MOCK_LOG" \
   bash "$TMP_ROOT/imboy-deploy.sh" admin >"$TMP_ROOT/output.log" 2>&1; then
  bad "恶意 ADMIN_REMOTE_DIR 应在 SSH 前被拒绝" ""
elif grep -q 'ADMIN_REMOTE_DIR 必须位于' "$TMP_ROOT/output.log" \
     && [ ! -s "$MOCK_CALLS" ] && [ ! -e "$ADMIN_PWN" ]; then
  ok "恶意 ADMIN_REMOTE_DIR 未触发 SSH"
else
  bad "恶意 ADMIN_REMOTE_DIR 未命中预期 allowlist" "$(<"$TMP_ROOT/output.log")"
fi

USER_PWN="$TMP_ROOT/user_pwn"
write_env /etc/nginx/imboy.conf /www/wwwroot/admin "root;touch $USER_PWN"
: >"$MOCK_CALLS"
if env PATH="$MOCK_BIN:$PATH" MOCK_CALLS="$MOCK_CALLS" MOCK_LOG="$MOCK_LOG" \
   bash "$TMP_ROOT/imboy-deploy.sh" rollback >"$TMP_ROOT/output.log" 2>&1; then
  bad "恶意 SERVER_USER 应在 SSH 前被拒绝" ""
elif grep -q 'SERVER_USER 非法' "$TMP_ROOT/output.log" \
     && [ ! -s "$MOCK_CALLS" ] && [ ! -e "$USER_PWN" ]; then
  ok "恶意 SERVER_USER 未触发 SSH"
else
  bad "恶意 SERVER_USER 未命中预期 allowlist" "$(<"$TMP_ROOT/output.log")"
fi

write_env
if run_migrate fail '08171234@127.0.0.1'; then
  bad "监听/drain Gate 失败时应拒绝迁移" ""
elif [ ! -s "$MOCK_LOG" ]; then
  ok "监听/drain Gate 失败后未执行迁移"
else
  bad "监听/drain Gate 失败后仍执行迁移" "$(tr '\n' ',' <"$MOCK_LOG")"
fi

if run_migrate ok "bad';touch"; then
  bad "不安全节点名应被拒绝" ""
elif [ ! -s "$MOCK_LOG" ]; then
  ok "不安全节点名未进入远端迁移命令"
else
  bad "不安全节点名进入迁移命令" "$(tr '\n' ',' <"$MOCK_LOG")"
fi

echo
echo "总计: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]

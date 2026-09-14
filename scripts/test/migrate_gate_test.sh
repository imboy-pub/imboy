#!/usr/bin/env bash
# 独立 migrate 入口离线测试：验证 Gate 发现的真实节点名会传给迁移 RPC。
set -uo pipefail

cd "$(dirname "$0")/../.." || exit 1

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
mkdir -p "$TMP_ROOT/lib"
cat >"$TMP_ROOT/lib/blue_green_deploy.sh" <<'MOCK'
#!/usr/bin/env bash
printf '%s\n' "$*" >"$BLUE_GREEN_LOG"
printf 'internal=%s\n' "${IMBOY_DEPLOY_INTERNAL:-}" >>"$BLUE_GREEN_LOG"
MOCK

write_env() {
  local nginx_conf="${1:-/etc/nginx/imboy.conf}"
  local admin_remote="${2:-/www/wwwroot/admin}"
  local server_user="${3:-tester}"
  local prodadm_conf="${4:-/etc/nginx/imboy-admin.conf}"
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
  'DEPLOY_EXPAND_MIGRATIONS="00000064_msg_store_sender_did.up.sql 00000108_group_attachment_anchor.up.sql 00000109_c2g_timeline_generation_boundary.up.sql 00000111_c2g_request_recipient_boundary.up.sql 00000112_e2ee_group_session_attestation.up.sql"' \
  >"$TMP_ROOT/.env.deploy"
  printf 'SERVER_USER=%q\nNGINX_CONF=%q\nPRODADM_CONF=%q\nADMIN_REMOTE_DIR=%q\n' \
    "$server_user" "$nginx_conf" "$prodadm_conf" "$admin_remote" >>"$TMP_ROOT/.env.deploy"
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
printf '#!/usr/bin/env bash\nexit 0\n' >"$MOCK_BIN/rsync"
chmod +x "$MOCK_BIN/rsync" "$TMP_ROOT/lib/blue_green_deploy.sh"

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
  local gate_state="${1:-ok}"
  local ctl_node="${2:-08171234@127.0.0.1}"
  shift 2
  : >"$MOCK_LOG"
  : >"$MOCK_CALLS"
  env PATH="$MOCK_BIN:$PATH" \
    MOCK_LOG="$MOCK_LOG" \
    MOCK_CALLS="$MOCK_CALLS" \
    MOCK_GATE_STATE="$gate_state" \
    MOCK_CTL_NODE="$ctl_node" \
    bash "$TMP_ROOT/imboy-deploy.sh" migrate "$@" \
    >"$TMP_ROOT/output.log" 2>&1
}

echo "== 独立 migrate Gate（全离线 mock） =="

if bash -c '
  set -u
  source scripts/.env.deploy.example
  read -r -a migrations <<< "$DEPLOY_EXPAND_MIGRATIONS"
  [ "${#migrations[@]}" -eq 5 ]
  [ "$DEPLOY_SALES_RELEASE" = true ]
  [ "$DEPLOY_E2EE_MODE" = required ]
'; then
  ok ".env.deploy.example 可加载完整 expand 清单"
else
  bad ".env.deploy.example 无法加载完整 expand 清单" ""
fi

CUSTOM_ENV="$TMP_ROOT/customers/acme.env"
mkdir -p "$(dirname "$CUSTOM_ENV")"
cp "$TMP_ROOT/.env.deploy" "$CUSTOM_ENV"
CUSTOM_ENV_REAL="$(cd "$(dirname "$CUSTOM_ENV")" && pwd -P)/$(basename "$CUSTOM_ENV")"
if run_migrate ok '08171234@127.0.0.1' --env-file "$CUSTOM_ENV" \
   && grep -q "配置源: $CUSTOM_ENV_REAL" "$TMP_ROOT/output.log"; then
  ok "--env-file 使用指定客户配置"
else
  bad "--env-file 未使用指定客户配置" "$(<"$TMP_ROOT/output.log")"
fi

BLUE_GREEN_LOG="$TMP_ROOT/blue-green.log"
: >"$MOCK_CALLS"
if env PATH="$MOCK_BIN:$PATH" MOCK_CALLS="$MOCK_CALLS" MOCK_LOG="$MOCK_LOG" \
   BLUE_GREEN_LOG="$BLUE_GREEN_LOG" \
   bash "$TMP_ROOT/imboy-deploy.sh" api -v -l --env-file "$CUSTOM_ENV" \
   >"$TMP_ROOT/output.log" 2>&1 \
   && grep -qE '^-v -l example\.invalid 1\.0\.0 [0-9]{8}$' "$BLUE_GREEN_LOG" \
   && grep -qx 'internal=1' "$BLUE_GREEN_LOG" \
   && grep -q 'source=local-rsync' "$TMP_ROOT/output.log"; then
  ok "api -v -l 将本地上传模式透传给私有蓝绿实现"
else
  bad "api -v -l 未正确透传" "$(tr '\n' ',' <"$TMP_ROOT/output.log")"
fi

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

PRODADM_PWN="$TMP_ROOT/prodadm_pwn"
write_env /etc/nginx/imboy.conf /www/wwwroot/admin tester "/tmp/x';touch $PRODADM_PWN;#"
: >"$MOCK_CALLS"
if env PATH="$MOCK_BIN:$PATH" MOCK_CALLS="$MOCK_CALLS" MOCK_LOG="$MOCK_LOG" \
   bash "$TMP_ROOT/imboy-deploy.sh" rollback >"$TMP_ROOT/output.log" 2>&1; then
  bad "恶意 PRODADM_CONF 应在 SSH 前被拒绝" ""
elif grep -q 'PRODADM_CONF 必须是' "$TMP_ROOT/output.log" \
     && [ ! -s "$MOCK_CALLS" ] && [ ! -e "$PRODADM_PWN" ]; then
  ok "恶意 PRODADM_CONF 未触发 SSH"
else
  bad "恶意 PRODADM_CONF 未命中预期 allowlist" "$(<"$TMP_ROOT/output.log")"
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

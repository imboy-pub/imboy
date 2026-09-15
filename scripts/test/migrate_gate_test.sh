#!/usr/bin/env bash
# 独立 migrate 入口离线测试：验证 Gate 发现的真实节点名会传给迁移 RPC。
set -uo pipefail

cd "$(dirname "$0")/../.." || exit 1

TMP_ROOT="$(mktemp -d /tmp/imboy_migrate_gate.XXXXXX)"
MOCK_BIN="$TMP_ROOT/bin"
MOCK_LOG="$TMP_ROOT/ssh.log"
MOCK_CALLS="$TMP_ROOT/ssh.calls"
TEST_SCRIPT_DIR="$TMP_ROOT/scripts"
TEST_DEPLOY="$TEST_SCRIPT_DIR/imboy-deploy.sh"
TEST_ENV="$TEST_SCRIPT_DIR/.env.deploy"
TEST_PLUGIN_KEY="$TMP_ROOT/plugin-signing-public.raw"
mkdir -p "$MOCK_BIN" "$TEST_SCRIPT_DIR/lib"

cleanup() {
  rm -rf -- "$TMP_ROOT"
}
trap cleanup EXIT

cp scripts/imboy-deploy.sh "$TEST_DEPLOY"
printf '%s\n' 0.0.0 >"$TMP_ROOT/VERSION"
printf '%s\n' '## [1.0.0] - 2026-09-14' >"$TMP_ROOT/CHANGELOG.md"
printf '%s\n' \
  '{release, {imboy, "0.0.0"}, [' \
  '    imboy' \
  ']}.' >"$TMP_ROOT/relx.config"
cp "$TMP_ROOT/relx.config" "$TMP_ROOT/relxpro.config"
printf '0123456789abcdef0123456789abcdef' >"$TEST_PLUGIN_KEY"
cat >"$TEST_SCRIPT_DIR/lib/blue_green_deploy.sh" <<'MOCK'
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
  'DEPLOY_RELX_CONFIG=relxpro.config' \
  'DEPLOY_NODE_NAME=prod-test123' \
  "DEPLOY_PLUGIN_TRUSTED_PUBLIC_KEY_FILE=$TEST_PLUGIN_KEY" \
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
  >"$TEST_ENV"
  printf 'SERVER_USER=%q\nNGINX_CONF=%q\nPRODADM_CONF=%q\nADMIN_REMOTE_DIR=%q\n' \
    "$server_user" "$nginx_conf" "$prodadm_conf" "$admin_remote" >>"$TEST_ENV"
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
chmod +x "$MOCK_BIN/rsync" "$TEST_SCRIPT_DIR/lib/blue_green_deploy.sh"

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
    bash "$TEST_DEPLOY" migrate "$@" \
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
cp "$TEST_ENV" "$CUSTOM_ENV"
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
   bash "$TEST_DEPLOY" api -v -l --env-file "$CUSTOM_ENV" \
   >"$TMP_ROOT/output.log" 2>&1 \
   && grep -Fxq -- '-v -l example.invalid 1.0.0 prod-test123' "$BLUE_GREEN_LOG" \
   && grep -qx 'internal=1' "$BLUE_GREEN_LOG" \
   && grep -Fxq '1.0.0' "$TMP_ROOT/VERSION" \
   && grep -Fq '{release, {imboy, "1.0.0"}, [' "$TMP_ROOT/relx.config" \
   && grep -Fq '{release, {imboy, "1.0.0"}, [' "$TMP_ROOT/relxpro.config" \
   && grep -q 'source=local-rsync' "$TMP_ROOT/output.log"; then
  ok "api -v -l 自动同步版本并透传本地模式与显式节点名"
else
  bad "api -v -l 未正确透传" "$(tr '\n' ',' <"$TMP_ROOT/output.log")"
fi

printf '%s\n' 0.0.0 >"$TMP_ROOT/VERSION"
printf '%s\n' '## [0.9.0] - 2026-09-01' >"$TMP_ROOT/CHANGELOG.md"
: >"$MOCK_CALLS"
if env PATH="$MOCK_BIN:$PATH" MOCK_CALLS="$MOCK_CALLS" MOCK_LOG="$MOCK_LOG" \
   bash "$TEST_DEPLOY" api --env-file "$CUSTOM_ENV" \
   >"$TMP_ROOT/output.log" 2>&1; then
  bad "非本地发布缺少 CHANGELOG 目标版本时应拒绝" ""
elif grep -q 'CHANGELOG.md 缺少发布版本标题' "$TMP_ROOT/output.log" \
     && grep -Fxq '0.0.0' "$TMP_ROOT/VERSION" && [ ! -s "$MOCK_CALLS" ]; then
  ok "非本地发布的 CHANGELOG 门禁早于 SSH"
else
  bad "非本地发布的 CHANGELOG 门禁未在 SSH 前失败" "$(tr '\n' ',' <"$TMP_ROOT/output.log")"
fi

: >"$MOCK_CALLS"
if env PATH="$MOCK_BIN:$PATH" MOCK_CALLS="$MOCK_CALLS" MOCK_LOG="$MOCK_LOG" \
   BLUE_GREEN_LOG="$BLUE_GREEN_LOG" \
   bash "$TEST_DEPLOY" api -l --env-file "$CUSTOM_ENV" \
   >"$TMP_ROOT/output.log" 2>&1 \
   && grep -q 'CHANGELOG.md 缺少发布版本标题.*-l 本地发布继续' "$TMP_ROOT/output.log" \
   && grep -Fxq '1.0.0' "$TMP_ROOT/VERSION" \
   && grep -Fxq -- '-l example.invalid 1.0.0 prod-test123' "$BLUE_GREEN_LOG"; then
  ok "-l 本地发布缺少 CHANGELOG 目标版本时警告并继续"
else
  bad "-l 本地发布未按警告模式继续" "$(tr '\n' ',' <"$TMP_ROOT/output.log")"
fi
printf '%s\n' '## [1.0.0] - 2026-09-14' >"$TMP_ROOT/CHANGELOG.md"

write_env
printf '%s\n' 'DEPLOY_PLUGIN_TRUSTED_PUBLIC_KEY_FILE=/definitely/missing/plugin.raw' >>"$TEST_ENV"
: >"$MOCK_CALLS"
if env PATH="$MOCK_BIN:$PATH" MOCK_CALLS="$MOCK_CALLS" MOCK_LOG="$MOCK_LOG" \
   bash "$TEST_DEPLOY" api -l >"$TMP_ROOT/output.log" 2>&1; then
  bad "销售版缺失插件公钥应在 SSH 前拒绝" ""
elif grep -q '公钥文件不存在或不可读' "$TMP_ROOT/output.log" && [ ! -s "$MOCK_CALLS" ]; then
  ok "销售版插件公钥门禁早于 SSH"
else
  bad "插件公钥门禁未命中预期" "$(<"$TMP_ROOT/output.log")"
fi
write_env

write_env
printf '%s\n' "DEPLOY_NODE_NAME='bad;touch_node'" >>"$TEST_ENV"
: >"$MOCK_CALLS"
if env PATH="$MOCK_BIN:$PATH" MOCK_CALLS="$MOCK_CALLS" MOCK_LOG="$MOCK_LOG" \
   bash "$TEST_DEPLOY" api >"$TMP_ROOT/output.log" 2>&1; then
  bad "恶意 DEPLOY_NODE_NAME 应在 SSH 前被拒绝" ""
elif grep -q 'DEPLOY_NODE_NAME 非法' "$TMP_ROOT/output.log" && [ ! -s "$MOCK_CALLS" ]; then
  ok "恶意 DEPLOY_NODE_NAME 未触发 SSH"
else
  bad "恶意 DEPLOY_NODE_NAME 未命中预期 allowlist" "$(<"$TMP_ROOT/output.log")"
fi
write_env

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
   bash "$TEST_DEPLOY" rollback >"$TMP_ROOT/output.log" 2>&1; then
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
   bash "$TEST_DEPLOY" admin >"$TMP_ROOT/output.log" 2>&1; then
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
   bash "$TEST_DEPLOY" rollback >"$TMP_ROOT/output.log" 2>&1; then
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
   bash "$TEST_DEPLOY" rollback >"$TMP_ROOT/output.log" 2>&1; then
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

#!/usr/bin/env bash
# 蓝绿部署控制流测试：用本地 ssh 桩记录事件，不连接任何服务器。
set -uo pipefail

cd "$(dirname "$0")/../.." || exit 1

DEPLOY="scripts/lib/blue_green_deploy.sh"
TEST_VSN="$(tr -d '[:space:]' < VERSION)"
TEST_SOURCE_HEAD=0123456789abcdef0123456789abcdef01234567
TMP_ROOT="$(mktemp -d /tmp/imboy_deploy_sequence.XXXXXX)"
MOCK_BIN="$TMP_ROOT/bin"
MOCK_LOG="$TMP_ROOT/events.log"
TEST_PLUGIN_KEY="$TMP_ROOT/plugin-signing-public.raw"
mkdir -p "$MOCK_BIN"
printf '0123456789abcdef0123456789abcdef' >"$TEST_PLUGIN_KEY"

cleanup() {
  rm -rf -- "$TMP_ROOT"
}
trap cleanup EXIT

cat >"$MOCK_BIN/ssh" <<'MOCK'
#!/usr/bin/env bash
set -u

for last_arg in "$@"; do :; done
cmd="${last_arg:-}"

case "$cmd" in
  *"cd '/srv/imboy' && git rev-parse HEAD"*)
    printf '%s\n' "${MOCK_REMOTE_SOURCE_HEAD:-$TEST_SOURCE_HEAD}"
    exit 0
    ;;
  *"head -n1 '/srv/imboy/VERSION'"*)
    printf '%s\n' "$TEST_VSN"
    exit 0
    ;;
  *"/etc/source-head"*)
    [ "${MOCK_SOURCE_HEAD_MATCH:-1}" = 1 ]
    exit
    ;;
  *"BLUE_UPSTREAM="*"GREEN_UPSTREAM="*)
    [ "${MOCK_FAIL_AT:-}" != "rollback_unknown" ] || exit 2
    printf '%s\n' "${MOCK_NGINX_COLOR:-green}"
    exit 0
    ;;
  *"PORTS="*"sed -nE"*"server[[:space:]]"*)
    [ "${MOCK_FAIL_AT:-}" != "rollback_unknown" ] || exit 2
    [ "${MOCK_FAIL_AT:-}" != "discovery_tool" ] || exit 2
    if [ -n "${MOCK_NGINX_PORT:-}" ]; then
      printf '%s\n' "$MOCK_NGINX_PORT"
    elif [ -n "${MOCK_NGINX_COLOR:-}" ]; then
      case "$MOCK_NGINX_COLOR" in blue) echo 9800 ;; green) echo 9801 ;; *) echo "$MOCK_NGINX_COLOR" ;; esac
    else
      case "${MOCK_CURRENT_COLOR:-blue}" in blue) echo 9800 ;; green) echo 9801 ;; none) echo 9800 ;; legacy) echo 9802 ;; esac
    fi
    exit 0
    ;;
  *"! ss -tlnH"*"grep -q ."*)
    exit 0
    ;;
  *"ss -tlnH"*"grep -q ."*)
    [ "${MOCK_CURRENT_COLOR:-blue}" != none ]
    exit
    ;;
  *"BLUE_STATE="*"GREEN_STATE="*)
    [ "${MOCK_FAIL_AT:-}" != "discovery_tool" ] || exit 2
    printf '%s\n' "${MOCK_CURRENT_COLOR:-blue}"
    exit 0
    ;;
  *"for DIR in "*"/usr/local/imboy-"*)
    printf '%s\n' "${MOCK_ACTIVE_RELEASE_DIR:-/usr/local/imboy-0.9.0-oldnode}"
    exit 0
    ;;
  *"[ -d '/usr/local/imboy-"*)
    [ "${MOCK_RELEASE_EXISTS:-0}" = 1 ]
    exit
    ;;
  *"rm -rf -- '/usr/local/imboy-"*)
    printf '%s\n' CLEAN_FAILED_RELEASE >>"$MOCK_LOG"
    exit 0
    ;;
  *"OLD_PID="*)
    printf '%s\n' "/usr/local/imboy-0.9.0-oldnode"
    exit 0
    ;;
  *"PID="*"lsof -ti:"*"/proc/"*)
    printf '%s\n' "${MOCK_ACTIVE_RELEASE_DIR:-/usr/local/imboy-0.9.0-oldnode}"
    exit 0
    ;;
  *"date +%s%3N"*)
    printf '%s\n' 1790791200000
    exit 0
    ;;
  *"version IN (108,109,110,111,112) AND dirty = true"*)
    printf '%s\n' "${MOCK_BOUNDARY_DIRTY:-0}"
    exit 0
    ;;
  *"FROM public.msg_store_staging s"*)
    printf '%s\n' BOUNDARY_BACKLOG >>"$MOCK_LOG"
    printf '%s\n' "${MOCK_BOUNDARY_BACKLOG_READY:-1}"
    exit 0
    ;;
  *"to_regclass('public.msg_c2g_recipient_snapshot')"*)
    probe_count="$(grep -c -x BOUNDARY_STRUCTURE "$MOCK_LOG" 2>/dev/null || true)"
    printf '%s\n' BOUNDARY_STRUCTURE >>"$MOCK_LOG"
    if [ "$probe_count" -gt 0 ] && [ -n "${MOCK_BOUNDARY_FINAL_READY:-}" ]; then
      printf '%s\n' "$MOCK_BOUNDARY_FINAL_READY"
    elif [ "${MOCK_ATTESTATION_SCHEMA_READY:-1}" != 1 ]; then
      printf '%s\n' 0
    elif [ "${MOCK_BOUNDARY_READY:-1}" = 0 ] && grep -q -x AUTO_TRUE "$MOCK_LOG"; then
      printf '%s\n' 1
    else
      printf '%s\n' "${MOCK_BOUNDARY_READY:-1}"
    fi
    exit 0
    ;;
  *"e2ee_group_session_attestation_pkey"*)
    printf '%s\n' "${MOCK_ATTESTATION_SCHEMA_READY:-1}"
    exit 0
    ;;
  *"test -f '/srv/imboy/.deploy-c2g-boundary-v109-ready'"*)
    [ "${MOCK_MARKER_READY:-1}" = 1 ]
    exit
    ;;
  *"test -f '/srv/imboy/priv/migrations/"*)
    [ "${MOCK_FAIL_AT:-}" != "required_probe" ] || exit 255
    exit 0
    ;;
  *": > '/srv/imboy/.deploy-c2g-boundary-v109-ready'"*)
    printf '%s\n' CUTOVER_MARKER >>"$MOCK_LOG"
    exit 0
    ;;
  *"docker exec -i"*"00000064_msg_store_sender_did.up.sql"*)
    printf '%s\n' EXPAND >>"$MOCK_LOG"
    [ "${MOCK_FAIL_AT:-}" != "expand" ]
    exit
    ;;
  *"cd '/usr/local/imboy-0.9.0-oldnode'"*"bin/imboy daemon"*)
    printf '%s\n' RECOVER_OLD >>"$MOCK_LOG"
    exit 0
    ;;
  *"IMBOY_AUTO_MIGRATE="*"bin/imboy daemon"*)
    printf '%s\n' DAEMON >>"$MOCK_LOG"
    case "$cmd" in
      *"IMBOY_AUTO_MIGRATE='true'"*) printf '%s\n' AUTO_TRUE >>"$MOCK_LOG" ;;
      *) printf '%s\n' AUTO_FALSE >>"$MOCK_LOG" ;;
    esac
    case "$cmd" in
      *"IMBOY_E2EE_MODE='required'"*) printf '%s\n' E2EE_REQUIRED >>"$MOCK_LOG" ;;
      *"IMBOY_E2EE_MODE='disabled'"*) printf '%s\n' E2EE_DISABLED >>"$MOCK_LOG" ;;
    esac
    case "$cmd" in
      *"IMBOY_PLUGIN_TRUSTED_PUBLIC_KEY_FILES="*) printf '%s\n' PLUGIN_KEY_CONFIGURED >>"$MOCK_LOG" ;;
    esac
    case "$cmd" in
      *"IMBOY_TSID_STATE_DIR="*"IMBOY_TSID_NODE_ID='1'"*) printf '%s\n' TSID_BLUE_CONFIGURED >>"$MOCK_LOG" ;;
      *"IMBOY_TSID_STATE_DIR="*"IMBOY_TSID_NODE_ID='2'"*) printf '%s\n' TSID_GREEN_CONFIGURED >>"$MOCK_LOG" ;;
    esac
    case "$cmd" in
      *"IMBOY_TSID_BOOTSTRAP_FLOOR_UNIX_MS='1790791200000'"*) printf '%s\n' TSID_FLOOR_RESOLVED >>"$MOCK_LOG" ;;
    esac
    exit 0
    ;;
  *"curl -fsS"*"/healthz"*)
    if [[ "$cmd" == *"RECOVERY_HEALTH=1"* ]]; then
      printf '%s\n' RECOVERY_HEALTH >>"$MOCK_LOG"
      [ "${MOCK_RECOVERY_FAIL:-0}" != 1 ]
      exit
    fi
    [ "${MOCK_FAIL_AT:-}" != "health" ] \
      && [ "${MOCK_FAIL_AT:-}" != "rollback_health" ]
    exit
    ;;
  *": imboy-nginx-rollback"*)
    case "$cmd" in
      *"/etc/nginx/imboy.conf"*"/etc/nginx/prodadm.conf"*"/etc/nginx/cs.conf"*) ;;
      *) exit 9 ;;
    esac
    printf '%s\n' ROLLBACK >>"$MOCK_LOG"
    [ "${MOCK_FAIL_AT:-}" != "rollback_sed" ] || exit 1
    if [ "${MOCK_FAIL_AT:-}" = "rollback_reload" ]; then
      exit 1
    fi
    case "${MOCK_NGINX_COLOR:-green}" in
      green) case "$cmd" in *"9801;|server 127.0.0.1:9800;"*) exit 0 ;; *) exit 1 ;; esac ;;
      blue)  case "$cmd" in *"9800;|server 127.0.0.1:9801;"*) exit 0 ;; *) exit 1 ;; esac ;;
    esac
    ;;
  *": imboy-nginx-cutover"*)
    case "$cmd" in
      *"/etc/nginx/imboy.conf"*"/etc/nginx/prodadm.conf"*"/etc/nginx/cs.conf"*) ;;
      *) exit 9 ;;
    esac
    printf '%s\n' SWITCH >>"$MOCK_LOG"
    exit 0
    ;;
  *"/bin/imboy"*" stop"*)
    printf '%s\n' STOP >>"$MOCK_LOG"
    case "${MOCK_FAIL_AT:-}" in
      stop) exit 1 ;;
      stop_timeout)
        case "$cmd" in *"timeout 20s"*) exit 124 ;; *) exit 0 ;; esac
        ;;
      *) exit 0 ;;
    esac
    ;;
  *"for round in TERM KILL"*"pgrep -x heart"*)
    printf '%s\n' CLEAN_RELEASE_PROCESSES >>"$MOCK_LOG"
    exit 0
    ;;
  *"LISTENERS="*"ss -tlnH"*)
    case "${MOCK_FAIL_AT:-}" in
      ss_error) exit 2 ;;
      port_open) exit 1 ;;
      *) exit 0 ;;
    esac
    ;;
  *"make ctl ARGS='db migrate'"*)
    printf '%s\n' MIGRATE >>"$MOCK_LOG"
    [ "${MOCK_FAIL_AT:-}" != "migrate" ]
    exit
    ;;
  *)
    exit 0
    ;;
esac
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

run_deploy() {
  local fail_at="$1" current_color="$2"
  local -a expand_env=(
    "IMBOY_DEPLOY_EXPAND_MIGRATIONS=${TEST_EXPAND_MIGRATIONS-00000064_msg_store_sender_did.up.sql 00000108_group_attachment_anchor.up.sql 00000109_c2g_timeline_generation_boundary.up.sql 00000111_c2g_request_recipient_boundary.up.sql 00000112_e2ee_group_session_attestation.up.sql}"
  )
  shift 2
  : >"$MOCK_LOG"
  if [ "${TEST_UNSET_EXPAND_MIGRATIONS:-0}" -eq 1 ]; then
    expand_env=(-u IMBOY_DEPLOY_EXPAND_MIGRATIONS)
  fi
  env \
    "${expand_env[@]}" \
    PATH="$MOCK_BIN:$PATH" \
    MOCK_LOG="$MOCK_LOG" \
    MOCK_FAIL_AT="$fail_at" \
    MOCK_CURRENT_COLOR="$current_color" \
    MOCK_NGINX_PORT="${MOCK_NGINX_PORT:-}" \
    MOCK_BOUNDARY_READY="${MOCK_BOUNDARY_READY:-1}" \
    MOCK_BOUNDARY_FINAL_READY="${MOCK_BOUNDARY_FINAL_READY:-}" \
    MOCK_BOUNDARY_DIRTY="${MOCK_BOUNDARY_DIRTY:-0}" \
    MOCK_ATTESTATION_SCHEMA_READY="${MOCK_ATTESTATION_SCHEMA_READY:-1}" \
    MOCK_MARKER_READY="${MOCK_MARKER_READY:-1}" \
    MOCK_RELEASE_EXISTS="${MOCK_RELEASE_EXISTS:-0}" \
    MOCK_ACTIVE_RELEASE_DIR="${MOCK_ACTIVE_RELEASE_DIR:-/usr/local/imboy-0.9.0-oldnode}" \
    MOCK_SOURCE_HEAD_MATCH="${MOCK_SOURCE_HEAD_MATCH:-1}" \
    MOCK_REMOTE_SOURCE_HEAD="${MOCK_REMOTE_SOURCE_HEAD:-}" \
    TEST_SOURCE_HEAD="$TEST_SOURCE_HEAD" \
    TEST_VSN="$TEST_VSN" \
    IMBOY_DEPLOY_USER=tester \
    IMBOY_DEPLOY_PORT=2222 \
    IMBOY_DEPLOY_PROJECT_DIR=/srv/imboy \
    IMBOY_DEPLOY_NGINX_CONF=/etc/nginx/imboy.conf \
    IMBOY_DEPLOY_PRODADM_CONF=/etc/nginx/prodadm.conf \
    IMBOY_DEPLOY_CS_NGINX_CONF=/etc/nginx/cs.conf \
    IMBOY_DEPLOY_BLUE_PORT=9800 \
    IMBOY_DEPLOY_GREEN_PORT=9801 \
    IMBOY_DEPLOY_LEGACY_PORT="${TEST_LEGACY_PORT:-}" \
    IMBOY_DEPLOY_NODE_HOST=127.0.0.1 \
    IMBOY_DEPLOY_COOKIE=testcookie \
    IMBOY_DEPLOY_BRANCH=main \
    IMBOY_DEPLOY_SOURCE_HEAD="$TEST_SOURCE_HEAD" \
    IMBOY_DEPLOY_STOP_OLD=true \
    IMBOY_DEPLOY_DB_CONTAINER=postgres \
    IMBOY_DEPLOY_DB_NAME=imboy_test \
    IMBOY_DEPLOY_DB_USER=postgres \
    IMBOY_DEPLOY_SALES_RELEASE="${TEST_SALES_RELEASE:-true}" \
    IMBOY_DEPLOY_E2EE_MODE="${TEST_E2EE_MODE:-}" \
    IMBOY_DEPLOY_PLUGIN_TRUSTED_PUBLIC_KEY_FILE="$TEST_PLUGIN_KEY" \
    IMBOY_DEPLOY_TSID_STATE_DIR="${TEST_TSID_STATE_DIR:-}" \
    IMBOY_DEPLOY_TSID_BLUE_NODE_ID="${TEST_TSID_BLUE_NODE_ID:-}" \
    IMBOY_DEPLOY_TSID_GREEN_NODE_ID="${TEST_TSID_GREEN_NODE_ID:-}" \
    IMBOY_DEPLOY_TSID_BOOTSTRAP_MODE="${TEST_TSID_BOOTSTRAP_MODE:-}" \
    IMBOY_DEPLOY_TSID_BOOTSTRAP_FLOOR_UNIX_MS="${TEST_TSID_BOOTSTRAP_FLOOR_UNIX_MS:-}" \
    IMBOY_DEPLOY_TSID_BOOTSTRAP_LEGACY_ACK="${TEST_TSID_BOOTSTRAP_LEGACY_ACK:-}" \
    IMBOY_DEPLOY_INTERNAL=1 \
    bash "$DEPLOY" "$@" example.invalid "$TEST_VSN" testnode \
    >"$TMP_ROOT/output.log" 2>&1
}

run_rollback() {
  local fail_at="$1" nginx_color="$2"
  : >"$MOCK_LOG"
  env \
    PATH="$MOCK_BIN:$PATH" \
    MOCK_LOG="$MOCK_LOG" \
    MOCK_FAIL_AT="$fail_at" \
    MOCK_NGINX_COLOR="$nginx_color" \
    IMBOY_DEPLOY_USER=tester \
    IMBOY_DEPLOY_PORT=2222 \
    IMBOY_DEPLOY_PROJECT_DIR=/srv/imboy \
    IMBOY_DEPLOY_NGINX_CONF=/etc/nginx/imboy.conf \
    IMBOY_DEPLOY_PRODADM_CONF=/etc/nginx/prodadm.conf \
    IMBOY_DEPLOY_CS_NGINX_CONF=/etc/nginx/cs.conf \
    IMBOY_DEPLOY_BLUE_PORT=9800 \
    IMBOY_DEPLOY_GREEN_PORT=9801 \
    IMBOY_DEPLOY_COOKIE=testcookie \
    IMBOY_DEPLOY_INTERNAL=1 \
    bash "$DEPLOY" --rollback example.invalid "$TEST_VSN" testnode \
    >"$TMP_ROOT/output.log" 2>&1
}

event_line() {
  local event="$1"
  grep -n -x "$event" "$MOCK_LOG" | head -1 | cut -d: -f1
}

assert_absent() {
  local description="$1" event="$2"
  if grep -q -x "$event" "$MOCK_LOG"; then
    bad "$description" "意外事件=$event; events=$(tr '\n' ',' <"$MOCK_LOG")"
  else
    ok "$description"
  fi
}

assert_success_order() {
  local expand daemon switch stop migrate
  expand="$(event_line EXPAND)"
  daemon="$(event_line DAEMON)"
  switch="$(event_line SWITCH)"
  stop="$(event_line STOP)"
  migrate="$(event_line MIGRATE)"
  if [ -n "$expand" ] && [ -n "$daemon" ] && [ -n "$switch" ] \
     && [ -n "$stop" ] && [ -n "$migrate" ] \
     && [ "$expand" -lt "$daemon" ] && [ "$daemon" -lt "$switch" ] \
     && [ "$switch" -lt "$stop" ] && [ "$stop" -lt "$migrate" ]; then
    ok "成功路径事件顺序为 expand → daemon → switch → stop → migrate"
  else
    bad "成功路径事件顺序错误" "$(tr '\n' ',' <"$MOCK_LOG")"
  fi
}

echo "== 蓝绿部署控制流（全离线 mock） =="

if run_deploy "" blue; then
  assert_success_order
  if grep -q -x E2EE_REQUIRED "$MOCK_LOG"; then
    ok "销售版默认以 required E2EE 启动"
  else
    bad "销售版门禁通过后未以 required E2EE 启动" "$(tr '\n' ',' <"$MOCK_LOG")"
  fi
  if grep -q -x PLUGIN_KEY_CONFIGURED "$MOCK_LOG"; then
    ok "销售版新节点显式加载 release 内可信插件公钥"
  else
    bad "销售版新节点未加载可信插件公钥" "$(tr '\n' ',' <"$MOCK_LOG")"
  fi
else
  bad "成功路径应退出 0" "$(<"$TMP_ROOT/output.log")"
fi

if MOCK_NGINX_PORT=9802 \
   TEST_LEGACY_PORT=9802 \
   TEST_TSID_STATE_DIR=/var/lib/imboy/tsid-v2 \
   TEST_TSID_BLUE_NODE_ID=1 \
   TEST_TSID_GREEN_NODE_ID=2 \
   TEST_TSID_BOOTSTRAP_MODE=manual_floor \
   TEST_TSID_BOOTSTRAP_FLOOR_UNIX_MS=now \
   TEST_TSID_BOOTSTRAP_LEGACY_ACK=I-CONFIRM-OLD-WRITER-STOPPED \
   run_deploy "" legacy; then
  stop="$(event_line STOP)"
  daemon="$(event_line DAEMON)"
  switch="$(event_line SWITCH)"
  if [ -n "$stop" ] && [ -n "$daemon" ] && [ -n "$switch" ] \
     && [ "$stop" -lt "$daemon" ] && [ "$daemon" -lt "$switch" ] \
     && grep -q -x TSID_BLUE_CONFIGURED "$MOCK_LOG" \
     && grep -q -x TSID_FLOOR_RESOLVED "$MOCK_LOG"; then
    ok "legacy 端口迁移先停旧 writer，再以 .env TSID 参数启动蓝槽并切流"
  else
    bad "legacy → 蓝绿初始化时序或 TSID 参数错误" "$(tr '\n' ',' <"$MOCK_LOG")"
  fi
else
  bad "legacy → 蓝绿初始化应由单一部署命令完成" "$(<"$TMP_ROOT/output.log")"
fi

if MOCK_NGINX_PORT=9802 \
   TEST_LEGACY_PORT=9802 \
   TEST_TSID_STATE_DIR=/var/lib/imboy/tsid-v2 \
   TEST_TSID_BLUE_NODE_ID=1 \
   TEST_TSID_GREEN_NODE_ID=2 \
   TEST_TSID_BOOTSTRAP_MODE=manual_floor \
   TEST_TSID_BOOTSTRAP_FLOOR_UNIX_MS=now \
   TEST_TSID_BOOTSTRAP_LEGACY_ACK=I-CONFIRM-OLD-WRITER-STOPPED \
   run_deploy health legacy; then
  bad "legacy 候选健康失败时部署应退出非零" ""
elif [ -n "$(event_line CLEAN_RELEASE_PROCESSES)" ] \
     && [ -n "$(event_line RECOVER_OLD)" ] \
     && [ -n "$(event_line RECOVERY_HEALTH)" ]; then
  ok "legacy 候选健康失败时回收候选并自动恢复旧节点"
else
  bad "legacy 候选健康失败后未完整恢复" "$(tr '\n' ',' <"$MOCK_LOG")"
fi

if MOCK_RELEASE_EXISTS=1 \
   MOCK_ACTIVE_RELEASE_DIR="/usr/local/imboy-${TEST_VSN}-testnode" \
   run_deploy "" blue; then
  if grep -q '重复部署直接成功' "$TMP_ROOT/output.log" \
     && ! grep -qE '^(EXPAND|DAEMON|SWITCH|STOP|MIGRATE)$' "$MOCK_LOG"; then
    ok "相同版本和节点已健康时重复部署幂等成功"
  else
    bad "幂等成功路径仍执行了发布副作用" "$(tr '\n' ',' <"$MOCK_LOG")"
  fi
else
  bad "相同健康 release 重复部署应退出 0" "$(<"$TMP_ROOT/output.log")"
fi

if MOCK_RELEASE_EXISTS=1 MOCK_SOURCE_HEAD_MATCH=0 \
   MOCK_ACTIVE_RELEASE_DIR="/usr/local/imboy-${TEST_VSN}-testnode" \
   run_deploy "" blue; then
  bad "相同版本但 source HEAD 不同不得幂等成功" ""
elif grep -q 'source HEAD 与本次候选不一致' "$TMP_ROOT/output.log"; then
  ok "相同版本 release 仍以 source HEAD 区分候选"
else
  bad "source HEAD 幂等门禁错误文案异常" "$(<"$TMP_ROOT/output.log")"
fi

if MOCK_REMOTE_SOURCE_HEAD=ffffffffffffffffffffffffffffffffffffffff run_deploy "" blue; then
  bad "远端 Git HEAD 与本地候选不一致时不得部署" ""
elif grep -q '与本地候选.*不一致' "$TMP_ROOT/output.log" \
     && ! grep -qE '^(DAEMON|SWITCH|STOP|MIGRATE)$' "$MOCK_LOG"; then
  ok "远端 Git HEAD 不匹配时在构建和切流前失败"
else
  bad "远端 Git HEAD 门禁未在副作用前失败" "$(<"$TMP_ROOT/output.log")"
fi

if MOCK_RELEASE_EXISTS=1 run_deploy "" blue; then
  if grep -q -x CLEAN_FAILED_RELEASE "$MOCK_LOG"; then
    ok "上次失败的非活动 release 自动清理后可重试"
  else
    bad "失败残留未自动清理" "$(tr '\n' ',' <"$MOCK_LOG")"
  fi
else
  bad "失败残留重试路径应继续部署" "$(<"$TMP_ROOT/output.log")"
fi

if TEST_SALES_RELEASE=false run_deploy "" blue; then
  if grep -q -x E2EE_DISABLED "$MOCK_LOG"; then
    ok "非销售版仍默认以 disabled E2EE 启动"
  else
    bad "非销售版 E2EE 默认值异常" "$(tr '\n' ',' <"$MOCK_LOG")"
  fi
else
  bad "非销售版默认 E2EE 控制流应退出 0" "$(<"$TMP_ROOT/output.log")"
fi

if TEST_E2EE_MODE=disabled run_deploy "" blue; then
  bad "销售版不得显式降级到 disabled E2EE" ""
elif grep -q "销售版 IMBOY_DEPLOY_E2EE_MODE 必须为 required/compliance" "$TMP_ROOT/output.log"; then
  ok "销售版显式降级 E2EE 会在连接服务器前失败"
else
  bad "销售版 E2EE 降级错误文案异常" "$(<"$TMP_ROOT/output.log")"
fi

if MOCK_BOUNDARY_READY=0 run_deploy "" blue; then
  stop="$(event_line STOP)"
  daemon="$(event_line DAEMON)"
  backlog="$(event_line BOUNDARY_BACKLOG)"
  marker="$(event_line CUTOVER_MARKER)"
  if [ -n "$stop" ] && [ -n "$daemon" ] && [ -n "$backlog" ] && [ -n "$marker" ] \
     && [ "$stop" -lt "$daemon" ] && [ "$daemon" -lt "$marker" ] \
     && [ "$daemon" -lt "$backlog" ] \
     && grep -q -x AUTO_TRUE "$MOCK_LOG" \
     && ! grep -q -x EXPAND "$MOCK_LOG" \
     && ! grep -q -x MIGRATE "$MOCK_LOG"; then
    ok "首次启用 C2G boundary 先停旧节点，由 boot migration 登记后才写 cutover marker"
  else
    bad "首次 C2G boundary 未消除旧节点混写窗口" "$(tr '\n' ',' <"$MOCK_LOG")"
  fi
else
  bad "首次 C2G boundary 维护式发布应退出 0" "$(<"$TMP_ROOT/output.log")"
fi

if MOCK_BOUNDARY_READY=1 MOCK_MARKER_READY=0 run_deploy "" blue; then
  stop="$(event_line STOP)"
  daemon="$(event_line DAEMON)"
  migrate="$(event_line MIGRATE)"
  marker="$(event_line CUTOVER_MARKER)"
  if [ -n "$stop" ] && [ -n "$daemon" ] && [ -n "$migrate" ] && [ -n "$marker" ] \
     && [ "$stop" -lt "$daemon" ] && [ "$daemon" -lt "$migrate" ] \
     && [ "$migrate" -lt "$marker" ]; then
    ok "迁移已登记但 cutover 未完成时仍走维护恢复，并在完整迁移后写 marker"
  else
    bad "C2G boundary 失败重试过早恢复滚动发布" "$(tr '\n' ',' <"$MOCK_LOG")"
  fi
else
  bad "C2G boundary 失败重试维护路径应退出 0" "$(<"$TMP_ROOT/output.log")"
fi

if MOCK_BOUNDARY_DIRTY=1 run_deploy "" blue; then
  bad "migration 108 dirty 时应在维护切换前退出非零" ""
else
  assert_absent "migration 108 dirty 时不停旧节点" STOP
  assert_absent "migration 108 dirty 时不启动新节点" DAEMON
  assert_absent "migration 108 dirty 时不执行迁移" MIGRATE
  assert_absent "migration 108 dirty 时不切流" SWITCH
  assert_absent "migration 108 dirty 时不写 cutover marker" CUTOVER_MARKER
  grep -q "dirty=true" "$TMP_ROOT/output.log" \
    && ok "migration 108 dirty 提示明确要求人工受控恢复" \
    || bad "migration 108 dirty 缺少明确恢复提示" "$(<"$TMP_ROOT/output.log")"
fi

if TEST_UNSET_EXPAND_MIGRATIONS=1 run_deploy "" blue; then
  bad "release 含必需迁移时未设置 expand 清单应退出非零" ""
else
  assert_absent "expand 清单未设置时不停旧节点" STOP
  assert_absent "expand 清单未设置时不启动新节点" DAEMON
  assert_absent "expand 清单未设置时不切流" SWITCH
  assert_absent "expand 清单未设置时不执行迁移" MIGRATE
  grep -q "release 包含 boundary 迁移.*请显式配置 DEPLOY_EXPAND_MIGRATIONS" "$TMP_ROOT/output.log" \
    && ok "expand 清单未设置时返回明确配置错误" \
    || bad "expand 清单未设置时错误文案异常" "$(<"$TMP_ROOT/output.log")"
fi

if TEST_EXPAND_MIGRATIONS='' run_deploy "" blue; then
  bad "release 含必需迁移时 expand 清单为空应退出非零" ""
else
  assert_absent "expand 清单为空时不停旧节点" STOP
  assert_absent "expand 清单为空时不启动新节点" DAEMON
  assert_absent "expand 清单为空时不切流" SWITCH
  assert_absent "expand 清单为空时不执行迁移" MIGRATE
  grep -q "release 包含 boundary 迁移.*请显式配置 DEPLOY_EXPAND_MIGRATIONS" "$TMP_ROOT/output.log" \
    && ok "expand 清单为空时返回明确配置错误" \
    || bad "expand 清单为空时错误文案异常" "$(<"$TMP_ROOT/output.log")"
fi

if run_deploy required_probe blue; then
  bad "必需 migration 文件探测失败时应退出非零" ""
else
  assert_absent "必需 migration 探测失败时不停旧节点" STOP
  assert_absent "必需 migration 探测失败时不启动新节点" DAEMON
  assert_absent "必需 migration 探测失败时不切流" SWITCH
  assert_absent "必需 migration 探测失败时不执行迁移" MIGRATE
  grep -q "无法探测必需的 expand 迁移文件" "$TMP_ROOT/output.log" \
    && ok "必需 migration 探测失败时返回明确错误" \
    || bad "必需 migration 探测失败时错误文案异常" "$(<"$TMP_ROOT/output.log")"
fi

if run_deploy expand blue; then
  bad "expand 失败应退出非零" ""
else
  assert_absent "expand 失败后不启动新节点" DAEMON
  assert_absent "expand 失败后不切流" SWITCH
fi

if MOCK_ATTESTATION_SCHEMA_READY=0 run_deploy "" blue; then
  bad "migration 112 表存在但 schema 残缺时应退出非零" ""
else
  assert_absent "migration 112 schema 残缺时不切流" SWITCH
  assert_absent "migration 112 schema 残缺时不写 cutover marker" CUTOVER_MARKER
  grep -q "最终 schema/backlog 校验失败，拒绝切流" "$TMP_ROOT/output.log" \
    && ok "migration 112 schema 残缺时在切流前返回明确错误" \
    || bad "migration 112 schema 残缺时错误文案异常" "$(<"$TMP_ROOT/output.log")"
fi

if MOCK_BOUNDARY_FINAL_READY=0 run_deploy "" blue; then
  bad "已有 marker 的常规发布最终 schema 漂移时应退出非零" ""
else
  assert_absent "已有 marker 的常规发布最终 schema 漂移时不切流" SWITCH
  if [ "$(grep -c -x BOUNDARY_STRUCTURE "$MOCK_LOG")" -eq 2 ] \
     && [ "$(grep -c -x BOUNDARY_BACKLOG "$MOCK_LOG")" -eq 1 ]; then
    ok "最终结构漂移时短路 backlog，并在切流前拒绝发布"
  else
    bad "boundary 结构/backlog 探测未按结构就绪状态短路" "$(tr '\n' ',' <"$MOCK_LOG")"
  fi
fi

if run_deploy health blue; then
  bad "health 失败应退出非零" ""
else
  assert_absent "health 失败后不切流" SWITCH
  assert_absent "health 失败后不迁移" MIGRATE
  [ -n "$(event_line CLEAN_RELEASE_PROCESSES)" ] \
    && ok "health 失败后自动回收候选节点" \
    || bad "health 失败后遗留候选节点" "$(tr '\n' ',' <"$MOCK_LOG")"
fi

if MOCK_BOUNDARY_READY=0 run_deploy health blue; then
  bad "boundary bootstrap 新节点失败时部署应退出非零" ""
elif [ -n "$(event_line RECOVER_OLD)" ] && [ -n "$(event_line RECOVERY_HEALTH)" ]; then
  ok "boundary bootstrap 切流前失败会自动恢复旧节点并检查健康"
else
  bad "boundary bootstrap 切流前失败未恢复旧节点" "$(tr '\n' ',' <"$MOCK_LOG")"
fi

if run_deploy stop blue; then
  [ -n "$(event_line CLEAN_RELEASE_PROCESSES)" ] && [ -n "$(event_line MIGRATE)" ] \
    && ok "旧节点优雅停止失败后精确回收进程并继续迁移" \
    || bad "旧节点优雅停止失败后未完成定向回收" "$(tr '\n' ',' <"$MOCK_LOG")"
else
  bad "旧节点优雅停止失败不应阻断可恢复发布" "$(<"$TMP_ROOT/output.log")"
fi

if MOCK_NGINX_PORT=9802 \
   TEST_LEGACY_PORT=9802 \
   TEST_TSID_STATE_DIR=/var/lib/imboy/tsid-v2 \
   TEST_TSID_BLUE_NODE_ID=1 \
   TEST_TSID_GREEN_NODE_ID=2 \
   TEST_TSID_BOOTSTRAP_MODE=manual_floor \
   TEST_TSID_BOOTSTRAP_FLOOR_UNIX_MS=now \
   TEST_TSID_BOOTSTRAP_LEGACY_ACK=I-CONFIRM-OLD-WRITER-STOPPED \
   run_deploy stop_timeout legacy; then
  [ -n "$(event_line CLEAN_RELEASE_PROCESSES)" ] \
    && [ -n "$(event_line DAEMON)" ] \
    && [ -n "$(event_line SWITCH)" ] \
    && ok "legacy 9802 优雅停止超时后精确回收并继续发布" \
    || bad "legacy 9802 超时回收后未完成发布" "$(tr '\n' ',' <"$MOCK_LOG")"
else
  bad "legacy 9802 优雅停止超时不应阻断可恢复发布" "$(<"$TMP_ROOT/output.log")"
fi

if run_deploy port_open blue; then
  bad "停止返回成功但旧端口仍开时应退出非零" ""
else
  assert_absent "旧端口仍开放时不迁移" MIGRATE
fi

if run_deploy ss_error blue; then
  bad "旧端口状态查询失败时应退出非零" ""
else
  assert_absent "端口状态查询失败时不迁移" MIGRATE
fi

if run_deploy discovery_tool blue; then
  bad "运行色探测工具失败时应退出非零" ""
else
  assert_absent "运行色探测失败时不启动新节点" DAEMON
  assert_absent "运行色探测失败时不迁移" MIGRATE
fi

if run_deploy migrate blue; then
  bad "migrate 失败应退出非零" ""
else
  [ -n "$(event_line MIGRATE)" ] \
    && ok "migrate 失败被真实执行并向上传播" \
    || bad "migrate 失败用例未触达迁移" "$(tr '\n' ',' <"$MOCK_LOG")"
fi

if run_deploy "" blue --no-migrate; then
  assert_absent "--no-migrate 不停止旧节点" STOP
  assert_absent "--no-migrate 不执行迁移" MIGRATE
else
  bad "--no-migrate 外层编排路径应成功返回" "$(<"$TMP_ROOT/output.log")"
fi

if MOCK_NGINX_COLOR=none run_deploy "" none; then
  bad "首次安装不得在未切流时谎报成功" ""
elif grep -q '缺少可验证的蓝绿 upstream' "$TMP_ROOT/output.log" \
     && ! grep -qE '^(EXPAND|DAEMON|SWITCH|STOP|MIGRATE)$' "$MOCK_LOG"; then
  ok "首次安装缺少预置 upstream 时在构建、启动和迁移前 fail-closed"
else
  bad "首次安装缺少 upstream 的错误文案异常" "$(<"$TMP_ROOT/output.log")"
fi

if MOCK_NGINX_COLOR=none run_deploy "" none --no-migrate; then
  bad "首次安装不得接受 --no-migrate" ""
else
  assert_absent "首次安装拒绝 --no-migrate 后不启动节点" DAEMON
  assert_absent "首次安装拒绝 --no-migrate 后不迁移" MIGRATE
fi

if MOCK_NGINX_COLOR=green run_deploy "" none; then
  recover="$(event_line RECOVER_OLD)"
  daemon="$(event_line DAEMON)"
  if [ -n "$recover" ] && [ -n "$daemon" ] && [ "$recover" -lt "$daemon" ]; then
    ok "双端口停机但 Nginx 指向 green 时先恢复原节点再继续蓝绿发布"
  else
    bad "已有部署停机时未优先恢复 Nginx 当前节点" "$(tr '\n' ',' <"$MOCK_LOG")"
  fi
else
  bad "已有部署停机恢复后的蓝绿发布应成功" "$(<"$TMP_ROOT/output.log")"
fi

if MOCK_NGINX_COLOR=green MOCK_RECOVERY_FAIL=1 run_deploy "" none; then
  bad "Nginx 当前节点恢复不健康时应退出非零" ""
else
  assert_absent "原节点恢复不健康时不启动目标节点" DAEMON
  assert_absent "原节点恢复不健康时不执行迁移" MIGRATE
  assert_absent "原节点恢复不健康时不切流" SWITCH
fi

if MOCK_NGINX_COLOR=green MOCK_BOUNDARY_READY=0 run_deploy health none; then
  bad "停机恢复后再次发生切流前失败应退出非零" ""
elif [ "$(grep -c -x RECOVER_OLD "$MOCK_LOG")" -eq 2 ] \
     && [ "$(grep -c -x RECOVERY_HEALTH "$MOCK_LOG")" -eq 2 ]; then
  ok "停机预恢复不会耗尽后续切流前失败的自动恢复机会"
else
  bad "停机预恢复后再次失败未二次恢复原节点" "$(tr '\n' ',' <"$MOCK_LOG")"
fi

if run_rollback "" green && grep -q -x ROLLBACK "$MOCK_LOG"; then
  ok "两色同时存活时按 Nginx 当前 green 精确回滚到 blue"
else
  bad "green → blue 回滚路径失败" "$(tr '\n' ',' <"$MOCK_LOG")"
fi

if run_rollback rollback_unknown green; then
  bad "未知或非唯一 upstream 应拒绝回滚" ""
else
  assert_absent "未知 upstream 未修改 Nginx" ROLLBACK
fi

if run_rollback rollback_health green; then
  bad "回滚目标健康失败应拒绝切流" ""
else
  assert_absent "不健康回滚目标未修改 Nginx" ROLLBACK
fi

if run_rollback rollback_sed green; then
  bad "upstream 替换未生效应返回失败" ""
else
  ok "upstream 替换未生效不会谎报回滚成功"
fi

if run_rollback rollback_reload green; then
  bad "Nginx reload 失败应恢复磁盘配置并返回失败" ""
else
  ok "Nginx reload 失败会恢复备份配置"
fi

echo
echo "总计: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]

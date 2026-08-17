#!/usr/bin/env bash
# 蓝绿部署控制流测试：用本地 ssh 桩记录事件，不连接任何服务器。
set -uo pipefail

cd "$(dirname "$0")/../.."

DEPLOY="scripts/deploy.sh"
TMP_ROOT="$(mktemp -d /tmp/imboy_deploy_sequence.XXXXXX)"
MOCK_BIN="$TMP_ROOT/bin"
MOCK_LOG="$TMP_ROOT/events.log"
mkdir -p "$MOCK_BIN"

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
  *"BLUE_UPSTREAM="*"GREEN_UPSTREAM="*)
    [ "${MOCK_FAIL_AT:-}" != "rollback_unknown" ] || exit 2
    printf '%s\n' "${MOCK_NGINX_COLOR:-green}"
    exit 0
    ;;
  *"BLUE_STATE="*"GREEN_STATE="*)
    [ "${MOCK_FAIL_AT:-}" != "discovery_tool" ] || exit 2
    printf '%s\n' "${MOCK_CURRENT_COLOR:-blue}"
    exit 0
    ;;
  *"[ -d '/usr/local/imboy-"*)
    exit 1
    ;;
  *"OLD_PID="*)
    printf '%s\n' "/usr/local/imboy-0.9.0-oldnode"
    exit 0
    ;;
  *"docker exec -i"*"00000064_msg_store_sender_did.up.sql"*)
    printf '%s\n' EXPAND >>"$MOCK_LOG"
    [ "${MOCK_FAIL_AT:-}" != "expand" ]
    exit
    ;;
  *"IMBOY_AUTO_MIGRATE="*"bin/imboy daemon"*)
    printf '%s\n' DAEMON >>"$MOCK_LOG"
    case "$cmd" in
      *"IMBOY_AUTO_MIGRATE='true'"*) printf '%s\n' AUTO_TRUE >>"$MOCK_LOG" ;;
      *) printf '%s\n' AUTO_FALSE >>"$MOCK_LOG" ;;
    esac
    exit 0
    ;;
  *"curl -fsS"*"/healthz"*)
    [ "${MOCK_FAIL_AT:-}" != "health" ] \
      && [ "${MOCK_FAIL_AT:-}" != "rollback_health" ]
    exit
    ;;
  *"ROLLBACK_BLUE_AFTER="*"ROLLBACK_GREEN_AFTER="*)
    printf '%s\n' ROLLBACK >>"$MOCK_LOG"
    [ "${MOCK_FAIL_AT:-}" != "rollback_sed" ] || exit 1
    if [ "${MOCK_FAIL_AT:-}" = "rollback_reload" ]; then
      case "$cmd" in
        *"nginx -s reload || {"*"cp '/etc/nginx/imboy.conf'.bak '/etc/nginx/imboy.conf'"*) exit 1 ;;
        *) exit 0 ;;
      esac
    fi
    case "${MOCK_NGINX_COLOR:-green}" in
      green) case "$cmd" in *"9801;|server 127.0.0.1:9800;"*) exit 0 ;; *) exit 1 ;; esac ;;
      blue)  case "$cmd" in *"9800;|server 127.0.0.1:9801;"*) exit 0 ;; *) exit 1 ;; esac ;;
    esac
    ;;
  *"nginx -t && nginx -s reload"*)
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
  shift 2
  : >"$MOCK_LOG"
  env \
    PATH="$MOCK_BIN:$PATH" \
    MOCK_LOG="$MOCK_LOG" \
    MOCK_FAIL_AT="$fail_at" \
    MOCK_CURRENT_COLOR="$current_color" \
    IMBOY_DEPLOY_USER=tester \
    IMBOY_DEPLOY_PORT=2222 \
    IMBOY_DEPLOY_PROJECT_DIR=/srv/imboy \
    IMBOY_DEPLOY_NGINX_CONF=/etc/nginx/imboy.conf \
    IMBOY_DEPLOY_BLUE_PORT=9800 \
    IMBOY_DEPLOY_GREEN_PORT=9801 \
    IMBOY_DEPLOY_NODE_HOST=127.0.0.1 \
    IMBOY_DEPLOY_COOKIE=testcookie \
    IMBOY_DEPLOY_BRANCH=main \
    IMBOY_DEPLOY_STOP_OLD=true \
    IMBOY_DEPLOY_DB_CONTAINER=postgres \
    IMBOY_DEPLOY_DB_NAME=imboy_test \
    IMBOY_DEPLOY_DB_USER=postgres \
    IMBOY_DEPLOY_EXPAND_MIGRATIONS=00000064_msg_store_sender_did.up.sql \
    IMBOY_DEPLOY_SALES_RELEASE=true \
    IMBOY_DEPLOY_E2EE_MODE=disabled \
    bash "$DEPLOY" "$@" example.invalid 1.0.0 testnode \
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
    IMBOY_DEPLOY_BLUE_PORT=9800 \
    IMBOY_DEPLOY_GREEN_PORT=9801 \
    IMBOY_DEPLOY_COOKIE=testcookie \
    bash "$DEPLOY" --rollback example.invalid 1.0.0 testnode \
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
else
  bad "成功路径应退出 0" "$(<"$TMP_ROOT/output.log")"
fi

if run_deploy expand blue; then
  bad "expand 失败应退出非零" ""
else
  assert_absent "expand 失败后不启动新节点" DAEMON
  assert_absent "expand 失败后不切流" SWITCH
fi

if run_deploy health blue; then
  bad "health 失败应退出非零" ""
else
  assert_absent "health 失败后不切流" SWITCH
  assert_absent "health 失败后不迁移" MIGRATE
fi

if run_deploy stop blue; then
  bad "旧节点停止失败应退出非零" ""
else
  assert_absent "旧节点停止失败后不迁移" MIGRATE
fi

if run_deploy stop_timeout blue; then
  bad "旧节点停止超时时应退出非零" ""
else
  assert_absent "旧节点停止超时后不迁移" MIGRATE
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

if run_deploy "" none; then
  assert_absent "首次空库安装不预跑单条 expand" EXPAND
  [ -n "$(event_line AUTO_TRUE)" ] \
    && ok "首次空库安装通过 boot 执行完整迁移" \
    || bad "首次安装未开启 bootstrap 迁移" "$(tr '\n' ',' <"$MOCK_LOG")"
else
  bad "首次空库安装控制流应成功" "$(<"$TMP_ROOT/output.log")"
fi

if run_deploy "" none --no-migrate; then
  bad "首次安装不得接受 --no-migrate" ""
else
  assert_absent "首次安装拒绝 --no-migrate 后不启动节点" DAEMON
  assert_absent "首次安装拒绝 --no-migrate 后不迁移" MIGRATE
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

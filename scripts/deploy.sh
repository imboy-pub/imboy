#!/usr/bin/env bash
set -Eeuo pipefail

# =============================================================================
# 脚本做的事 / What this script does
#
# 蓝绿零停机部署 Imboy 后端到生产服务器。
# Zero-downtime blue-green deployment of the Imboy backend to production.
#
# 执行流程 / Steps:
#   1. 检测当前运行色（蓝/绿）  Detect active color (blue/green)
#   2. 选择对立色为部署目标     Pick opposite color as deploy target
#   3. 服务器拉代码 + 编译 release（内嵌 ERTS）
#      Pull code + build self-contained release (ERTS embedded, no symlinks)
#   4. 解包到独立目录，生成 vm.args
#      Extract to isolated dir and write vm.args
#   5. 以目标端口启动新节点     Start new node on target port
#   6. 轮询端口确认启动成功     Poll until node is ready (40s timeout)
#   7. 执行数据库迁移           Run DB migrations
#   8. 切换 Nginx upstream 并 reload  Switch Nginx and reload
#   9. 停止旧节点（可选）       Stop old node (optional, STOP_OLD=false 跳过)
#
# 用法 / Usage:
#   bash ./script/deploy.sh [-v|--verbose] <SERVER_HOST> <VSN> <NODE_NAME>
#
# 示例 / Examples:
#   bash ./script/deploy.sh 10.0.0.10 1.0.0-rc.1 001
#   bash ./script/deploy.sh -v 10.0.0.10 1.0.0-rc.1 002
#
# 环境变量（均可选）/ Environment variables (all optional):
#   IMBOY_DEPLOY_USER        SSH 用户       SSH user            (default: root)
#   IMBOY_DEPLOY_PORT        SSH 端口       SSH port            (default: 32)
#   IMBOY_DEPLOY_PROJECT_DIR 远端项目目录   Remote project dir  (default: /www/wwwroot/imboy-api)
#   IMBOY_DEPLOY_NGINX_CONF  Nginx 配置路径 Nginx conf path
#   IMBOY_DEPLOY_BLUE_PORT   蓝端口         Blue port           (default: 9800)
#   IMBOY_DEPLOY_GREEN_PORT  绿端口         Green port          (default: 9801)
#   IMBOY_DEPLOY_NODE_HOST   节点 host      Node host           (default: 127.0.0.1)
#   IMBOY_DEPLOY_COOKIE      节点 cookie    Node cookie         (default: imboy)
#   IMBOY_DEPLOY_BRANCH      部署分支       Deploy branch       (default: main)
#   IMBOY_DEPLOY_STOP_OLD    完成后停旧节点 Stop old node after deploy (default: true)
# =============================================================================

# ---------- 静默控制 / Verbosity control ----------
# 默认静默；-v/--verbose 透传远端命令输出
# Quiet by default; -v/--verbose passes SSH output through
SILENT=1
while [[ $# -gt 0 ]]; do
  case "$1" in
    -v|--verbose) SILENT=0; shift ;;
    -s|--silent)  SILENT=1; shift ;;
    --) shift; break ;;
    -*) echo "未知参数 / Unknown flag: $1" >&2; exit 1 ;;
    *)  break ;;
  esac
done

if [ $# -ne 3 ]; then
  grep '^# 用法' "$0" -A 3 | sed 's/^# //'
  exit 1
fi

# ---------- 参数 / Parameters ----------
SERVER_HOST="$1"
VSN="$2"
NODE_NAME="$3"

SERVER_USER="${IMBOY_DEPLOY_USER:-root}"
SERVER_PORT="${IMBOY_DEPLOY_PORT:-32}"
PROJECT_DIR="${IMBOY_DEPLOY_PROJECT_DIR:-/www/wwwroot/imboy-api}"
NGINX_CONF="${IMBOY_DEPLOY_NGINX_CONF:-/www/server/panel/vhost/nginx/pro.imboy.pub.conf}"
BLUE_PORT="${IMBOY_DEPLOY_BLUE_PORT:-9800}"
GREEN_PORT="${IMBOY_DEPLOY_GREEN_PORT:-9801}"
NODE_HOST="${IMBOY_DEPLOY_NODE_HOST:-127.0.0.1}"
COOKIE="${IMBOY_DEPLOY_COOKIE:-imboy}"
BRANCH="${IMBOY_DEPLOY_BRANCH:-main}"
STOP_OLD="${IMBOY_DEPLOY_STOP_OLD:-true}"

RELEASE_DIR="/usr/local/imboy-${VSN}-${NODE_NAME}"
RELEASE_TARBALL="${PROJECT_DIR}/_rel/imboy/imboy-${VSN}.tar.gz"

# 校验参数格式，防止注入 vm.args 或 rm -rf 路径
# Validate inputs to prevent vm.args injection and path anomalies
[[ "$SERVER_HOST" =~ ^[a-zA-Z0-9._-]+$  ]] || { echo "无效 SERVER_HOST / invalid SERVER_HOST: $SERVER_HOST" >&2; exit 1; }
[[ "$BRANCH"      =~ ^[a-zA-Z0-9._/-]+$ ]] || { echo "无效 BRANCH / invalid BRANCH: $BRANCH" >&2; exit 1; }
[[ "$VSN"       =~ ^[a-zA-Z0-9._-]+$ ]] || { echo "无效 VSN / invalid VSN: $VSN" >&2; exit 1; }
[[ "$NODE_NAME" =~ ^[a-zA-Z0-9_-]+$  ]] || { echo "NODE_NAME 含非法字符 / invalid NODE_NAME: $NODE_NAME" >&2; exit 1; }
[[ "$COOKIE"    =~ ^[a-zA-Z0-9_-]+$  ]] || { echo "COOKIE 含非法字符 / invalid COOKIE: $COOKIE" >&2; exit 1; }
[[ "$RELEASE_DIR" == /usr/local/imboy-?* ]] || { echo "RELEASE_DIR 路径异常 / anomalous RELEASE_DIR: $RELEASE_DIR" >&2; exit 1; }

# 规范化 STOP_OLD：接受 true/1/yes（大小写不敏感）
# Normalize STOP_OLD: accept true/1/yes case-insensitively
case "$(echo "$STOP_OLD" | tr '[:upper:]' '[:lower:]')" in true|1|yes) STOP_OLD=true ;; *) STOP_OLD=false ;; esac

# ---------- 日志函数 / Log helpers ----------
log()  { echo -e "\033[36m[$(date '+%H:%M:%S')] $*\033[0m"; }
ok()   { echo -e "\033[32m✓ $*\033[0m"; }
fail() { echo -e "\033[31m✗ $*\033[0m" >&2; exit 1; }

trap 'fail "脚本意外终止，使用 -v 查看详情 / Aborted — rerun with -v for details"' ERR

# =============================================================================
# SSH ControlMaster：建立一条持久主连接，后续命令复用，避免重复握手
# SSH ControlMaster: one persistent TCP connection reused by all ssh_exec calls
# =============================================================================
SSH_CTRL="/tmp/imboy-deploy-$$"
SSH_OPTS=(-p "$SERVER_PORT" -o ControlPath="$SSH_CTRL" -o StrictHostKeyChecking=accept-new)

_cleanup() {
  ssh "${SSH_OPTS[@]}" -O exit "$SERVER_USER@$SERVER_HOST" 2>/dev/null || true
  rm -f "$SSH_CTRL"
}
trap '_cleanup' EXIT

log "连接服务器 $SERVER_USER@$SERVER_HOST:$SERVER_PORT / Connecting to server..."
ssh -fNM -o ControlMaster=yes "${SSH_OPTS[@]}" "$SERVER_USER@$SERVER_HOST"
ok "SSH 连接就绪 / SSH connection ready"

# 执行远端命令；静默模式丢弃输出，详细模式透传
# Run remote command; discard output in silent, pass through in verbose
ssh_exec() {
  if [ "$SILENT" -eq 1 ]; then
    ssh "${SSH_OPTS[@]}" "$SERVER_USER@$SERVER_HOST" "$1" >/dev/null 2>&1
  else
    ssh "${SSH_OPTS[@]}" "$SERVER_USER@$SERVER_HOST" "$1"
  fi
}

# 捕获远端 stdout（不走 ssh_exec，避免静默模式将输出丢入 /dev/null）
# Capture remote stdout — bypass ssh_exec to avoid silent-mode discard
ssh_capture() {
  ssh "${SSH_OPTS[@]}" "$SERVER_USER@$SERVER_HOST" "$1" | tr -d '\r'
}

# 轮询端口，最多等 40s（每 2s 一次，共 20 次）
# Poll until port is bound, timeout 40 s (2 s × 20 attempts)
# 单次 SSH 调用在远端执行整个等待循环，避免 20 次往返
# Single SSH call runs the entire wait loop remotely, avoiding 20 round trips
wait_for_port() {
  local port=$1
  ssh_exec "
    for i in \$(seq 1 20); do
      ss -tlnH "sport = :$port" 2>/dev/null | grep -q . && exit 0
      sleep 2
    done
    exit 1
  "
}

# =============================================================================
# 1️⃣ 检测当前运行色 / Detect active color
# =============================================================================
log "检测蓝绿运行状态... / Detecting active blue-green slot..."

CURRENT_COLOR="$(ssh_capture "
  if   lsof -i:$BLUE_PORT  >/dev/null 2>&1; then echo blue
  elif lsof -i:$GREEN_PORT >/dev/null 2>&1; then echo green
  else echo none
  fi
")"

# 选择对立色；首次部署（none）默认蓝
# Pick opposite color; first deploy defaults to blue
case "$CURRENT_COLOR" in
  blue)  TARGET_COLOR=green; APP_PORT=$GREEN_PORT; OLD_PORT=$BLUE_PORT  ;;
  green) TARGET_COLOR=blue;  APP_PORT=$BLUE_PORT;  OLD_PORT=$GREEN_PORT ;;
  *)     TARGET_COLOR=blue;  APP_PORT=$BLUE_PORT;  OLD_PORT=""          ;;
esac

ok "当前: $CURRENT_COLOR → 目标: $TARGET_COLOR (port=$APP_PORT) / Current: $CURRENT_COLOR → Target: $TARGET_COLOR"

# =============================================================================
# 2️⃣ 安全确认目标目录 / Confirm target dir is safe to overwrite
# =============================================================================
if ssh_exec "[ -d '$RELEASE_DIR' ]"; then
  echo "⚠️  远端目录已存在: $RELEASE_DIR / Remote dir exists: $RELEASE_DIR"
  if [[ ! -t 0 ]]; then
    fail "非交互环境，请先手动删除目标目录 / Non-interactive: delete $RELEASE_DIR manually first"
  fi
  if ! read -r -t 30 -p "删除并继续部署？(y/N) / Delete and continue? (y/N): " answer; then
    fail "确认超时（30s），已中止 / Confirmation timed out after 30s"
  fi
  [[ "${answer:-N}" =~ ^[yY]$ ]] || fail "用户取消 / Cancelled"
  ssh_exec "rm -rf '$RELEASE_DIR'"
fi

# =============================================================================
# 3️⃣ 拉代码 + 编译 release
# RELX_DEV_MODE=false 确保产物不含符号链接，RELX_INCLUDE_ERTS=true 内嵌运行时
# RELX_DEV_MODE=false: no symlinks in tarball; RELX_INCLUDE_ERTS=true: embed ERTS
# =============================================================================
log "拉取代码并编译 release... / Pulling code and building release..."
ssh_exec "
  set -e
  cd '$PROJECT_DIR'
  git fetch origin
  git checkout '$BRANCH'
  git reset --hard origin/'$BRANCH'
  IMBOYENV=pro \
    RELX_REL_VSN='$VSN' \
    RELX_DEV_MODE=false \
    RELX_INCLUDE_ERTS=true \
    make rel
  # 用服务器实际 sys.pro.config（含生产凭证）覆盖编译产物里的占位符 sys.config
  # Overlay generated sys.config with server's authoritative pro config (real credentials)
  REL_SYS_CONFIG='$PROJECT_DIR/_rel/imboy/releases/$VSN/sys.config'
  if [ -f '$PROJECT_DIR/config/sys.pro.config' ] && [ -f \"\$REL_SYS_CONFIG\" ]; then
    cp '$PROJECT_DIR/config/sys.pro.config' \"\$REL_SYS_CONFIG\"
    # 将 sys.config 中的 http_port 动态改为本次目标端口（蓝绿切换时端口不同）
    sed -i \"s/{http_port,[ ]*[0-9]\\+}/{http_port, $APP_PORT}/\" \"\$REL_SYS_CONFIG\"
  fi
"
ok "release 编译完成 / Release built"

# =============================================================================
# 4️⃣ 解包 + 生成 vm.args（节点身份 + BEAM 调优参数）
# Extract tarball + write vm.args (node identity + BEAM tuning flags)
# =============================================================================
log "解包 release + 写入 vm.args... / Extracting release and writing vm.args..."
# <<'VMARGS' 防止远端 shell 对内容二次展开；本地变量在外层双引号中已展开
# <<'VMARGS' prevents remote re-expansion; local vars are expanded by outer double-quotes
ssh_exec "
  set -e
  mkdir -p '$RELEASE_DIR'
  cd '$RELEASE_DIR' && tar -xzf '$RELEASE_TARBALL'
  REL_VSN_DIR=\$(find '$RELEASE_DIR/releases' -maxdepth 1 -mindepth 1 -type d | sort -V | tail -1)
  cat > \"\$REL_VSN_DIR/vm.args\" <<'VMARGS'
-name ${NODE_NAME}@${NODE_HOST}
-setcookie ${COOKIE}
-heart
-kernel inet_dist_use_interface '{127,0,0,1}'
-env ERL_EPMD_ADDRESS 127.0.0.1
+K true
+A 256
+S 4
+MSe true
+P 1048576
+Q 1048576
+sbwt none
+sbwtdcpu none
+sbwtdio none
+swt very_low
+stbt db
+zdbbl 81920
VMARGS
"
ok "release 已解包，vm.args 已写入 / Release extracted, vm.args written"

# =============================================================================
# 5️⃣ 启动新节点 + 轮询确认就绪 / Start new node + poll for readiness
# =============================================================================
log "启动新节点 (port=$APP_PORT)... / Starting new node..."
ssh_exec "cd '$RELEASE_DIR' && IMBOYENV=pro IMBOY_HTTP_PORT='$APP_PORT' ./bin/imboy daemon"

# 轮询取代原来的固定 sleep 5，在慢服务器上不会误报失败
# Polling replaces fixed sleep 5; won't false-fail on slow servers
wait_for_port "$APP_PORT" || fail "新节点 40s 内未就绪 / Node not ready within 40s (port=$APP_PORT)"
ok "新节点已就绪 (port=$APP_PORT) / New node ready"

# =============================================================================
# 6️⃣ 数据库迁移 / Run DB migrations
# =============================================================================
log "执行数据库迁移... / Running DB migrations..."
ssh_exec "cd '$PROJECT_DIR' && make ctl ARGS='db migrate'"
ok "数据库迁移完成 / DB migrations applied"

# =============================================================================
# 7️⃣ 切换 Nginx upstream / Switch Nginx upstream
# 首次部署（OLD_PORT 为空）跳过自动切换，提示人工配置
# Skip auto-switch on first deploy (OLD_PORT empty); prompt for manual config
# =============================================================================
if [ -n "$OLD_PORT" ]; then
  log "切换 Nginx: $OLD_PORT → $APP_PORT..."
  ssh_exec "
    cp '$NGINX_CONF' '$NGINX_CONF'.bak
    sed -i 's|server 127.0.0.1:$OLD_PORT;|server 127.0.0.1:$APP_PORT;|g' '$NGINX_CONF'
    grep -q 'server 127.0.0.1:$APP_PORT;' '$NGINX_CONF' \
      || { cp '$NGINX_CONF'.bak '$NGINX_CONF'; echo 'Nginx upstream 替换失败，已回滚 / replacement failed, rolled back' >&2; exit 1; }
    nginx -t && nginx -s reload
  "
  ok "Nginx 已切换至 $TARGET_COLOR / Nginx switched to $TARGET_COLOR"
else
  echo "ℹ️  首次部署：请手动将 Nginx upstream 设为 127.0.0.1:$APP_PORT，然后执行 nginx -s reload"
  echo "ℹ️  First deploy: set Nginx upstream to 127.0.0.1:$APP_PORT, then run nginx -s reload"
fi

# =============================================================================
# 8️⃣ 停止旧节点（可选）/ Stop old node (optional)
# IMBOY_DEPLOY_STOP_OLD=false 保留旧节点，便于快速回滚
# Set IMBOY_DEPLOY_STOP_OLD=false to keep old node alive for rollback
# =============================================================================
if [ "$STOP_OLD" = "true" ] && [ -n "$OLD_PORT" ]; then
  log "停止旧节点 (port=$OLD_PORT)... / Stopping old node..."
  OLD_DIR="$(ssh_capture \
    "find /usr/local -maxdepth 1 -name 'imboy-*-*' -type d ! -path '$RELEASE_DIR' | sort -V | head -1")" || OLD_DIR=""
  if [ -n "$OLD_DIR" ]; then
    ssh_exec "$OLD_DIR/bin/imboy stop || true"
    ok "旧节点已停止: $OLD_DIR / Old node stopped"
  fi
fi

# =============================================================================
# 完成 / Done
# =============================================================================
echo
ok "蓝绿部署完成 / Blue-green deploy complete"
printf -- "----------------------------------------------\n"
printf "%-22s %s\n" "版本 / Version:"  "$VSN"
printf "%-22s %s\n" "节点 / Node:"     "${NODE_NAME}@${NODE_HOST}"
printf "%-22s %s\n" "环境 / Slot:"     "$TARGET_COLOR"
printf "%-22s %s\n" "端口 / Port:"     "$APP_PORT"
printf "%-22s %s\n" "目录 / Dir:"      "$RELEASE_DIR"
printf -- "----------------------------------------------\n"

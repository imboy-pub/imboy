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
#   bash ./scripts/deploy.sh [-v|--verbose] [-l|--local] [-M|--no-migrate] <SERVER_HOST> <VSN> <NODE_NAME>
#
# 示例 / Examples:
#   bash ./scripts/deploy.sh 10.0.0.10 1.0.0-rc.1 001              # git pull + 编译
#   bash ./scripts/deploy.sh -v 10.0.0.10 1.0.0-rc.1 002           # 详细输出
#   bash ./scripts/deploy.sh -l 10.0.0.10 1.0.0-rc.1 dbg           # 本地源码 rsync（无需推 tag）
#   bash ./scripts/deploy.sh -v -l 10.0.0.10 1.0.0-rc.1 dbg        # 本地 rsync + 详细输出
#   bash ./scripts/deploy.sh -M 10.0.0.10 1.0.0-rc.1 001           # 跳过迁移（由上层调用方负责）
#   bash ./scripts/deploy.sh --rollback 10.0.0.10 1.0.0-rc.1 001   # 切回另一色（不回滚迁移）
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
#   IMBOY_DEPLOY_DB_CONTAINER PostgreSQL 容器名  PostgreSQL container
#   IMBOY_DEPLOY_DB_NAME      PostgreSQL 数据库名 Database name
#   IMBOY_DEPLOY_DB_USER      PostgreSQL 用户名  Database user
#   IMBOY_DEPLOY_EXPAND_MIGRATIONS 切流前执行的可加性迁移文件（空格分隔）
# =============================================================================

# ---------- 静默控制 / Verbosity control ----------
# 默认静默；-v/--verbose 透传远端命令输出
# Quiet by default; -v/--verbose passes SSH output through
SILENT=1
LOCAL_MODE=0
SKIP_MIGRATE=0
ROLLBACK=0
while [[ $# -gt 0 ]]; do
  case "$1" in
    -v|--verbose)    SILENT=0; shift ;;
    -s|--silent)     SILENT=1; shift ;;
    -l|--local)      LOCAL_MODE=1; shift ;;
    -M|--no-migrate) SKIP_MIGRATE=1; shift ;;
    --rollback)      ROLLBACK=1; shift ;;
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
DB_CONTAINER="${IMBOY_DEPLOY_DB_CONTAINER:-}"
DB_NAME="${IMBOY_DEPLOY_DB_NAME:-}"
DB_USER="${IMBOY_DEPLOY_DB_USER:-}"
EXPAND_MIGRATIONS="${IMBOY_DEPLOY_EXPAND_MIGRATIONS:-}"
# --local 模式：从本地 rsync 源码到远端，跳过 git pull
# --local mode: rsync local source to remote, skip git pull
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOCAL_SRC_DIR="${IMBOY_LOCAL_SRC_DIR:-$(cd "$SCRIPT_DIR/.." && pwd)}"

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
if [[ -n "$DB_CONTAINER" && ! "$DB_CONTAINER" =~ ^[a-zA-Z0-9_.-]+$ ]]; then
  echo "IMBOY_DEPLOY_DB_CONTAINER 含非法字符 / invalid DB container" >&2
  exit 1
fi
if [[ -n "$DB_NAME" && ! "$DB_NAME" =~ ^[a-zA-Z0-9_.-]+$ ]]; then
  echo "IMBOY_DEPLOY_DB_NAME 含非法字符 / invalid DB name" >&2
  exit 1
fi
if [[ -n "$DB_USER" && ! "$DB_USER" =~ ^[a-zA-Z0-9_.-]+$ ]]; then
  echo "IMBOY_DEPLOY_DB_USER 含非法字符 / invalid DB user" >&2
  exit 1
fi

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
# C-51：只探端口是**不够**的。
#   目标色端口上残留着上一次部署的进程时，`ss` 立刻就能看到端口被监听，
#   于是部署判定"就绪"→ 切流 → 流量被打到**旧二进制**上，而部署日志全绿。
#   本次改为探 /healthz 并核对它自报的版本号：
#     - HTTP 200  ⇒ 进程真的能服务（且 PG 可达，见 C-49 的 503 语义）
#     - version 匹配 ⇒ 端口后面站着的确实是这次要发的那个版本
# 缺任何一条都判失败，宁可部署中止也不要把流量切到错误的进程上。
wait_for_health() {
  local port=$1 expect_vsn=$2
  ssh_exec "
    for i in \$(seq 1 20); do
      BODY=\$(curl -fsS --max-time 3 \"http://127.0.0.1:$port/healthz\" 2>/dev/null || true)
      case \"\$BODY\" in
        *'\"status\":\"ok\"'*)
          case \"\$BODY\" in
            *'\"version\":\"$expect_vsn\"'*) exit 0 ;;
            *) echo \"就绪但版本不符 / ready but version mismatch: \$BODY\" >&2 ;;
          esac
          ;;
      esac
      sleep 2
    done
    exit 1
  "
}

# 保留端口探测供"旧节点是否还活着"这类不关心版本的判断使用。
# ⚠️ 不要再拿它当**部署就绪**判据 —— 那正是 C-51 修掉的坑。
wait_for_port() {
  local port=$1
  ssh_exec "
    for i in \$(seq 1 20); do
      ss -tlnH \"sport = :$port\" 2>/dev/null | grep -q . && exit 0
      sleep 2
    done
    exit 1
  "
}

# =============================================================================
# Expand 迁移：切流前先补齐新代码必需的可空列
#
# 本轮 00000064 为纯 expand：只给 msg_store 增加可空 sender_did 列，旧节点
# 不会受影响，新节点却会在归档读写 SQL 中直接引用它。不能把它留到切流之后
# 的全量迁移阶段，否则新节点会先接流量再因 schema 不完整报错。
#
# 这里只执行显式列出的、经过发布评审确认的 expand SQL；完整迁移仍在切流后
# 由 db migrate 执行并登记版本。这样不会把未知的 contract 迁移整体提前。
# =============================================================================
run_expand_migrations() {
  local migration
  local remote_file
  local -a migrations=()

  # 源码同步/编译已完成，若本次 release 带有 00000064，就强制要求配置
  # expand 清单，避免调用方无意间绕过 schema 兼容门。
  if ssh_exec "test -f '$PROJECT_DIR/priv/migrations/00000064_msg_store_sender_did.up.sql'"; then
    [ -n "$EXPAND_MIGRATIONS" ] || fail "检测到 00000064，但未配置 IMBOY_DEPLOY_EXPAND_MIGRATIONS，拒绝切流"
  fi
  [ -n "$EXPAND_MIGRATIONS" ] || {
    log "无显式 expand 迁移，跳过切流前 schema 扩展"
    return 0
  }
  [ -n "$DB_CONTAINER" ] || fail "执行 expand 迁移需要 IMBOY_DEPLOY_DB_CONTAINER"
  [ -n "$DB_NAME" ] || fail "执行 expand 迁移需要 IMBOY_DEPLOY_DB_NAME"
  [ -n "$DB_USER" ] || fail "执行 expand 迁移需要 IMBOY_DEPLOY_DB_USER"

  read -r -a migrations <<< "$EXPAND_MIGRATIONS"
  [ "${#migrations[@]}" -gt 0 ] || fail "IMBOY_DEPLOY_EXPAND_MIGRATIONS 为空"
  for migration in "${migrations[@]}"; do
    [[ "$migration" =~ ^[a-zA-Z0-9._-]+\.up\.sql$ ]] \
      || fail "expand 迁移文件名非法: $migration"
    remote_file="$PROJECT_DIR/priv/migrations/$migration"
    ssh_exec "test -s '$remote_file'" \
      || fail "远端缺少 expand 迁移文件: $remote_file"
    log "执行切流前 expand 迁移: $migration"
    ssh_exec "docker exec -i '$DB_CONTAINER' psql -v ON_ERROR_STOP=1 -U '$DB_USER' -d '$DB_NAME' -f - < '$remote_file'" \
      || fail "expand 迁移失败: $migration"
  done

  # 对本轮关键列做机器验证；不把 psql 输出带回日志，避免泄漏环境细节。
  if printf '%s\n' "${migrations[@]}" | grep -qx '00000064_msg_store_sender_did.up.sql'; then
    ssh_exec "docker exec '$DB_CONTAINER' psql -Atq -U '$DB_USER' -d '$DB_NAME' -c \"SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='msg_store' AND column_name='sender_did'\" | grep -qx 1" \
      || fail "schema 验证失败：public.msg_store.sender_did 不存在"
    ok "schema 已确认：public.msg_store.sender_did"
  fi
}

# =============================================================================
# 0️⃣ 回滚模式 / Rollback mode（C-52）
#
# 蓝绿部署的价值一半在"能快速切回去"，而此前脚本**没有任何回滚入口** ——
# 出事时只能手工 sed nginx 配置，正是最不该手工操作的时刻。
#
# 回滚只做一件事：把 nginx 切回另一色，并在切之前**确认那一色真的健康**。
# ⚠️ 它**不回滚数据库迁移**。迁移已应用的 schema 变更无法靠切流撤销，
#   这也是 expand/contract 纪律不可省的原因（见 7️⃣ 的说明）。
#   同理，只有当初部署时 IMBOY_DEPLOY_STOP_OLD=false 保留了旧节点，
#   回滚才有目标可切 —— 否则脚本会直接告诉你旧节点已不在。
# =============================================================================
if [ "$ROLLBACK" -eq 1 ]; then
  log "回滚模式 / Rollback mode"
  CUR="$(ssh_capture "
    if   lsof -i:$BLUE_PORT  >/dev/null 2>&1; then echo blue
    elif lsof -i:$GREEN_PORT >/dev/null 2>&1; then echo green
    else echo none
    fi")"
  case "$CUR" in
    blue)  RB_PORT="$GREEN_PORT"; RB_COLOR=green; CUR_PORT="$BLUE_PORT" ;;
    green) RB_PORT="$BLUE_PORT";  RB_COLOR=blue;  CUR_PORT="$GREEN_PORT" ;;
    *)     fail "未检测到运行中的节点，无法回滚 / No running node detected" ;;
  esac

  # 切之前必须确认目标色真的能服务 —— 切到一个死节点上比不回滚更糟。
  # 这里不校验版本：回滚的目标本来就是**旧**版本。
  ssh_exec "curl -fsS --max-time 3 \"http://127.0.0.1:$RB_PORT/healthz\" | grep -q '\"status\":\"ok\"'" \
    || fail "目标色 $RB_COLOR (port=$RB_PORT) 不健康或未运行，拒绝回滚 / rollback target not healthy。
  若当初部署时设了 IMBOY_DEPLOY_STOP_OLD=true，旧节点已被停止，需先手工启动它。"

  ssh_exec "
    cp '$NGINX_CONF' '$NGINX_CONF'.bak
    sed -i 's|server 127.0.0.1:$CUR_PORT;|server 127.0.0.1:$RB_PORT;|g' '$NGINX_CONF'
    grep -q 'server 127.0.0.1:$RB_PORT;' '$NGINX_CONF' \
      || { cp '$NGINX_CONF'.bak '$NGINX_CONF'; echo 'Nginx upstream 替换失败，已回滚 / replacement failed' >&2; exit 1; }
    nginx -t && nginx -s reload
  " || fail "Nginx 回滚失败 / Nginx rollback failed"

  ok "已回滚至 $RB_COLOR (port=$RB_PORT) / Rolled back。⚠️ 数据库迁移未回滚，请人工确认 schema 与该版本兼容"
  exit 0
fi

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
if [ "$LOCAL_MODE" -eq 1 ]; then
  log "[--local] 同步本地源码到远端 $PROJECT_DIR ... / Syncing local source to remote..."
  rsync -az --delete \
    --exclude='.git/' \
    --exclude='_build/' \
    --exclude='_rel/' \
    --exclude='deps/' \
    --exclude='log/' \
    --exclude='*.beam' \
    --exclude='*.d' \
    --exclude='config/sys.pro.config' \
    --exclude='config/sys.runtime.config' \
    --exclude='scripts/.env.deploy' \
    -e "ssh -p $SERVER_PORT -o ControlPath=$SSH_CTRL -o StrictHostKeyChecking=accept-new" \
    "$LOCAL_SRC_DIR/" \
    "$SERVER_USER@$SERVER_HOST:$PROJECT_DIR/"
  ok "本地源码已同步 / Local source synced"
else
  log "拉取代码... / Pulling code from git..."
  ssh_exec "
    set -e
    cd '$PROJECT_DIR'
    git fetch origin
    git checkout '$BRANCH'
    git reset --hard origin/'$BRANCH'
  "
  ok "代码已拉取 / Code pulled"
fi

log "编译 release... / Building release..."
ssh_exec "
  set -e
  cd '$PROJECT_DIR'
  # 销售版必须显式开启严格 E2EE、频道和付费频道入口；sys.pro.config
  # 是部署环境提供的忽略文件，校验器只输出策略布尔值，不输出任何密钥。
  test -f config/sys.pro.config || {
    echo '缺少 config/sys.pro.config：拒绝生成销售版 release' >&2
    exit 1
  }
  escript scripts/validate_sales_release_config.escript config/sys.pro.config
  # 全量清理后重编：-l 模式 rsync 会同步本地自动生成的 ebin/imboy.app（已列新模块），
  # 但 --exclude='*.beam' 排除了对应 beam，致 erlang.mk 因 .app mtime 较新而跳过重建，
  # release 组装时报 module_not_found。make clean 强制从源码全量重编，规避此陷阱。
  make clean
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
  # 解包后覆盖 http_port（tarball 内含旧值，必须在这里改）
  sed -i \"s/{http_port,[ ]*[0-9]\\+}/{http_port, $APP_PORT}/\" \"\$REL_VSN_DIR/sys.config\"
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
ssh_exec "cd '$RELEASE_DIR' && IMBOYENV=pro HTTP_PORT='$APP_PORT' IMBOY_HTTP_PORT='$APP_PORT' ./bin/imboy daemon"

# 轮询取代原来的固定 sleep 5，在慢服务器上不会误报失败
# Polling replaces fixed sleep 5; won't false-fail on slow servers
# C-51：探 /healthz + 校验版本，而不是只看端口有没有被监听。
# 失败信息刻意点名"可能是残留进程"——这是实际最常见的原因，
# 直接写出来能省掉一轮排查。
wait_for_health "$APP_PORT" "$VSN" \
  || fail "新节点 40s 内未就绪或版本不符 / Node not ready or version mismatch within 40s (port=$APP_PORT, expect=$VSN)。
  常见原因：目标色端口上有上一次部署的残留进程。
  排查：ssh $SERVER_HOST \"ss -tlnp 'sport = :$APP_PORT'\" 并确认进程的 -root 目录"
ok "新节点已就绪且版本匹配 (port=$APP_PORT, vsn=$VSN) / New node ready, version verified"

# =============================================================================
# 5.5️⃣ 切流前 Expand schema / Apply additive schema changes before traffic switch
#
# 新节点尚未接收业务流量，此时给数据库增加可空列对旧节点兼容；完成后再切流。
# =============================================================================
run_expand_migrations

# =============================================================================
# 6️⃣ 切换 Nginx upstream / Switch Nginx upstream
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
# 7️⃣ 数据库迁移 / Run remaining DB migrations（C-52：完整迁移仍在切流之后）
#
# 为什么完整迁移在切流之后：此前迁移跑在「新节点已起、nginx 仍指向旧节点」的窗口里，
# 也就是**旧节点正在服务流量时执行破坏性迁移** —— 一个 DROP COLUMN 会当场
# 打断正在被使用的旧代码。切流之后旧节点已不接流量，破坏性变更才安全。
#
# 切流前的 run_expand_migrations 只执行显式批准的、旧代码兼容的增量 DDL，
# 解决新代码在切流瞬间依赖新列的问题；其余迁移仍须遵循 expand/contract 纪律：
#     - 新增列/表（expand）必须先于依赖它的新代码发布，或新代码能容忍其缺失
#     - 删除列/表（contract）只能在旧代码彻底下线后的**下一次**发布里做
#   本次调整解决的是"expand 迟于切流"与"contract 撞上旧节点"两个相反时序，
#   不是免除迁移评审。
#
# 迁移失败时**不自动切回** nginx：此刻新节点已在服务，贸然切回旧节点可能撞上
# 已部分应用的 schema。改为显式提示 --rollback，由人判断。
# =============================================================================
if [ "$SKIP_MIGRATE" -eq 1 ]; then
  log "跳过数据库迁移（--no-migrate）/ Skipping DB migrations"
else
  log "执行数据库迁移... / Running DB migrations..."
  # CTL_NODE 必须显式指定为本次刚启动的节点名，Makefile 默认值 imboy@127.0.0.1
  # 与 vm.args 里实际写入的 ${NODE_NAME}@${NODE_HOST} 不一致，不传会报
  # "cannot reach 'imboy@127.0.0.1'" 并中止部署（实测复现）。同理 cookie
  # 也必须显式传 IMBOY_CTL_COOKIE，否则 imboy_ctl 默认 cookie=imboy，
  # 当 IMBOY_DEPLOY_COOKIE（如 .env.deploy 的 imboycookie）不是默认值时连不上。
  ssh_exec "cd '$PROJECT_DIR' && CTL_NODE='${NODE_NAME}@${NODE_HOST}' IMBOY_CTL_COOKIE='${COOKIE}' make ctl ARGS='db migrate'" \
    || fail "数据库迁移失败 / DB migration failed。流量已切到新节点且 schema 可能部分应用。
  回滚请显式执行：bash ./scripts/deploy.sh --rollback $SERVER_HOST $VSN $NODE_NAME
  （回滚只切回 nginx，**不回滚已应用的迁移**）"
  ok "数据库迁移完成 / DB migrations applied"
fi

# =============================================================================
# 8️⃣ 停止旧节点（可选）/ Stop old node (optional)
# IMBOY_DEPLOY_STOP_OLD=false 保留旧节点，便于快速回滚
# Set IMBOY_DEPLOY_STOP_OLD=false to keep old node alive for rollback
# =============================================================================
if [ "$STOP_OLD" = "true" ] && [ -n "$OLD_PORT" ]; then
  log "停止旧节点 (port=$OLD_PORT)... / Stopping old node..."
  # 按目录名版本号排序取最小值会挑到无关的历史陈旧目录（实测复现：
  # 停了一个早已不跑的 rc.10 目录，真正占着 OLD_PORT 的进程被晾在原地）。
  # 必须反查实际监听 OLD_PORT 的进程，从其命令行 -root 参数取真实目录。
  OLD_DIR="$(ssh_capture \
    "OLD_PID=\$(lsof -ti:$OLD_PORT -sTCP:LISTEN 2>/dev/null | head -1); \
     [ -n \"\$OLD_PID\" ] && ps -o cmd= -p \"\$OLD_PID\" | grep -oE -- '-root [^ ]+' | awk '{print \$2}' | head -1")" || OLD_DIR=""
  if [ -n "$OLD_DIR" ]; then
    ssh_exec "$OLD_DIR/bin/imboy stop || true"
    ok "旧节点已停止: $OLD_DIR / Old node stopped"
  else
    echo "⚠️  未能定位监听 port=$OLD_PORT 的进程目录，请手动确认并停止 / Could not resolve dir for port=$OLD_PORT, stop manually"
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

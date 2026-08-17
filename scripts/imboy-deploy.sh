#!/usr/bin/env bash
# =============================================================================
# imboy 统一部署入口 / Unified deploy entry point
#
# 用法 / Usage:
#   bash scripts/imboy-deploy.sh <component> [options]
#
# Component:
#   all       全量部署：api（内含 migrate）→ admin
#   api       部署 Erlang 后端（HTTP 无停机，WebSocket 短暂重连）
#   admin     仅部署 React 管理后台（本地构建 + 上传）
#   migrate   仅在另一蓝绿节点已停止后执行数据库迁移
#   rollback  回滚：将 Nginx 切回旧节点端口
#
# 前置条件 / Prerequisites:
#   1. cp scripts/.env.deploy.example scripts/.env.deploy
#   2. 编辑 scripts/.env.deploy 填写服务器地址和各项 key
#   3. 确保本机已配置 SSH 免密登录（ssh-copy-id）
#
# 示例 / Examples:
#
#   bash scripts/imboy-deploy.sh all
#   bash scripts/imboy-deploy.sh api
#   bash scripts/imboy-deploy.sh admin
#   bash scripts/imboy-deploy.sh migrate
#   bash scripts/imboy-deploy.sh rollback
# =============================================================================
set -Eeuo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ENV_FILE="$SCRIPT_DIR/.env.deploy"

# ---------- 加载配置 / Load config ----------
if [[ ! -f "$ENV_FILE" ]]; then
  echo "❌ 未找到 $ENV_FILE"
  echo "   请先执行: cp scripts/.env.deploy.example scripts/.env.deploy"
  echo "   并填写服务器 IP、Cookie 等配置"
  exit 1
fi
# shellcheck source=/dev/null
source "$ENV_FILE"

# 必填项校验
for var in SERVER_HOST SERVER_PORT SERVER_USER \
           DEPLOY_VSN DEPLOY_PROJECT_DIR DEPLOY_BRANCH \
           DEPLOY_BLUE_PORT DEPLOY_GREEN_PORT DEPLOY_COOKIE NGINX_CONF \
           ADMIN_BUILD_DIR ADMIN_REMOTE_DIR \
           DB_CONTAINER DB_NAME DB_USER DEPLOY_EXPAND_MIGRATIONS; do
  [[ -n "${!var:-}" ]] || { echo "❌ .env.deploy 缺少必填项: $var"; exit 1; }
done

COMPONENT="${1:-all}"

# ---------- 颜色日志 ----------
log()  { echo -e "\033[36m[$(date '+%H:%M:%S')] $*\033[0m"; }
ok()   { echo -e "\033[32m✓ $*\033[0m"; }
warn() { echo -e "\033[33m⚠ $*\033[0m"; }
fail() { echo -e "\033[31m✗ $*\033[0m" >&2; exit 1; }

# 所有 component 都在建立 SSH 前共享同一组 allowlist，避免 admin/rollback
# 绕过 deploy.sh 内的校验后把 .env.deploy 内容拼进远端 shell。
[[ "$SERVER_HOST" =~ ^[a-zA-Z0-9._-]+$ ]] \
  || fail "SERVER_HOST 非法，拒绝建立 SSH"
[[ "$SERVER_USER" =~ ^[a-zA-Z_][a-zA-Z0-9_-]*$ ]] \
  || fail "SERVER_USER 非法，拒绝建立 SSH"
[[ "$SERVER_PORT" =~ ^[0-9]+$ ]] && [ "$SERVER_PORT" -ge 1 ] && [ "$SERVER_PORT" -le 65535 ] \
  || fail "SERVER_PORT 非法，拒绝建立 SSH"
[[ "$DEPLOY_BLUE_PORT" =~ ^[0-9]+$ ]] && [ "$DEPLOY_BLUE_PORT" -ge 1024 ] \
  && [ "$DEPLOY_BLUE_PORT" -le 65535 ] \
  || fail "DEPLOY_BLUE_PORT 非法，拒绝建立 SSH"
[[ "$DEPLOY_GREEN_PORT" =~ ^[0-9]+$ ]] && [ "$DEPLOY_GREEN_PORT" -ge 1024 ] \
  && [ "$DEPLOY_GREEN_PORT" -le 65535 ] \
  || fail "DEPLOY_GREEN_PORT 非法，拒绝建立 SSH"
[[ "$DEPLOY_BLUE_PORT" != "$DEPLOY_GREEN_PORT" ]] \
  || fail "蓝绿端口不得相同"
[[ "$DEPLOY_PROJECT_DIR" =~ ^/[a-zA-Z0-9._/-]+$ && "$DEPLOY_PROJECT_DIR" != "/" \
   && "$DEPLOY_PROJECT_DIR" != *..* ]] \
  || fail "DEPLOY_PROJECT_DIR 必须是无 .. 的安全绝对路径"
[[ "$NGINX_CONF" =~ ^/[a-zA-Z0-9._/-]+$ && "$NGINX_CONF" != "/" \
   && "$NGINX_CONF" != *..* ]] \
  || fail "NGINX_CONF 必须是无 .. 的安全绝对路径"
[[ "$ADMIN_REMOTE_DIR" =~ ^/www/wwwroot/[a-zA-Z0-9._/-]+$ \
   && "$ADMIN_REMOTE_DIR" != *..* ]] \
  || fail "ADMIN_REMOTE_DIR 必须位于 /www/wwwroot/<站点>"
[[ "$DEPLOY_COOKIE" =~ ^[a-zA-Z0-9_-]+$ ]] \
  || fail "DEPLOY_COOKIE 非法，拒绝建立 SSH"

case "$COMPONENT" in
  all|api|admin|migrate|rollback) ;;
  *) echo "用法: bash scripts/imboy-deploy.sh <all|api|admin|migrate|rollback>"; exit 1 ;;
esac

if [[ "$COMPONENT" == all || "$COMPONENT" == admin ]]; then
  ADMIN_BUILD_PATH="$(cd "$SCRIPT_DIR/$ADMIN_BUILD_DIR" 2>/dev/null && pwd -P)" \
    || fail "ADMIN_BUILD_DIR 不存在或不可访问"
  [[ "$ADMIN_BUILD_PATH" != "/" && -f "$ADMIN_BUILD_PATH/package.json" ]] \
    || fail "ADMIN_BUILD_DIR 必须指向含 package.json 的具体项目目录"
fi

# ---------- SSH 复用连接 ----------
SSH_CTRL="/tmp/imboy-deploy-$$"
SSH_OPTS=(-p "$SERVER_PORT" -o ControlPath="$SSH_CTRL" -o StrictHostKeyChecking=accept-new -o ConnectTimeout=10)

cleanup() { ssh "${SSH_OPTS[@]}" -O exit "$SERVER_USER@$SERVER_HOST" 2>/dev/null || true; rm -f "$SSH_CTRL"; }
trap cleanup EXIT

_ssh_connect() {
  log "连接 $SERVER_USER@$SERVER_HOST:$SERVER_PORT ..."
  ssh -fNM -o ControlMaster=yes "${SSH_OPTS[@]}" "$SERVER_USER@$SERVER_HOST" \
    || fail "SSH 连接失败，请检查 SERVER_HOST / SERVER_PORT / SERVER_USER"
  ok "SSH 连接就绪"
}

ssh_exec() { ssh "${SSH_OPTS[@]}" "$SERVER_USER@$SERVER_HOST" "$1"; }
ssh_cap()  { ssh "${SSH_OPTS[@]}" "$SERVER_USER@$SERVER_HOST" "$1" | tr -d '\r'; }

# =============================================================================
# deploy_api — 蓝绿部署 Erlang 后端（HTTP 连续可用，WebSocket 短暂重连）
# =============================================================================
deploy_api() {
  log "▶ 部署 Erlang 后端 (蓝绿) — 委托 deploy.sh ..."

  # deploy.sh 负责完整的「expand → 启动 → 切流 → 停旧节点 → migrate」时序。
  # 不在本层拆开 migrate，否则两个入口可能在旧 WebSocket 尚存活时误跑完整迁移。
  NODE_ID="$(date '+%m%d%H%M')"

  # 将 .env.deploy 变量映射为 deploy.sh 的 IMBOY_DEPLOY_* 环境变量。
  IMBOY_DEPLOY_PORT="$SERVER_PORT" \
  IMBOY_DEPLOY_USER="$SERVER_USER" \
  IMBOY_DEPLOY_PROJECT_DIR="$DEPLOY_PROJECT_DIR" \
  IMBOY_DEPLOY_NGINX_CONF="$NGINX_CONF" \
  IMBOY_DEPLOY_BLUE_PORT="$DEPLOY_BLUE_PORT" \
  IMBOY_DEPLOY_GREEN_PORT="$DEPLOY_GREEN_PORT" \
  IMBOY_DEPLOY_COOKIE="$DEPLOY_COOKIE" \
  IMBOY_DEPLOY_BRANCH="$DEPLOY_BRANCH" \
  IMBOY_DEPLOY_STOP_OLD="${DEPLOY_STOP_OLD:-true}" \
  IMBOY_DEPLOY_DB_CONTAINER="$DB_CONTAINER" \
  IMBOY_DEPLOY_DB_NAME="$DB_NAME" \
  IMBOY_DEPLOY_DB_USER="$DB_USER" \
  IMBOY_DEPLOY_EXPAND_MIGRATIONS="$DEPLOY_EXPAND_MIGRATIONS" \
  IMBOY_DEPLOY_SALES_RELEASE="${DEPLOY_SALES_RELEASE:-true}" \
  IMBOY_DEPLOY_E2EE_MODE="${DEPLOY_E2EE_MODE:-disabled}" \
    bash "$SCRIPT_DIR/deploy.sh" \
      "$SERVER_HOST" "$DEPLOY_VSN" "$NODE_ID"

  ok "▶ Erlang 后端部署完成"
}

# =============================================================================
# deploy_admin — 本地构建 React SPA 后上传至服务器
# =============================================================================
deploy_admin() {
  log "▶ 部署管理后台 Admin Frontend ..."

  local BUILD_DIR="$ADMIN_BUILD_PATH"
  local ADMIN_REMOTE_REAL

  # --delete/rm 只能作用于明确标记的站点目录。使用 realpath 后的固定目标，防止
  # ADMIN_REMOTE_DIR 被替换为指向系统目录的符号链接。
  if ! ADMIN_REMOTE_REAL="$(ssh_cap "
    REAL=\$(realpath -e '$ADMIN_REMOTE_DIR') || exit 2
    case \"\$REAL\" in /www/wwwroot/?*) ;; *) exit 3 ;; esac
    [ -d \"\$REAL\" ] && [ -f \"\$REAL/.imboy-admin-root\" ] || exit 4
    printf '%s\\n' \"\$REAL\"
  ")"; then
    fail "后台远端目录必须存在、位于 /www/wwwroot/ 且含 .imboy-admin-root 标记"
  fi
  [[ "$ADMIN_REMOTE_REAL" =~ ^/www/wwwroot/[a-zA-Z0-9._/-]+$ \
     && "$ADMIN_REMOTE_REAL" != *..* ]] \
    || fail "后台远端 realpath 不在批准目录内"

  # 本地构建（需要 bun）
  if ! command -v bun &>/dev/null; then
    fail "本机未安装 bun，请先安装: https://bun.sh"
  fi

  log "本地构建 Admin Frontend ..."
  (cd "$BUILD_DIR" && bun install --frozen-lockfile && bun run build)
  ok "Admin Frontend 构建完成: $BUILD_DIR/dist"

  # 上传（rsync 增量，比 scp 快）
  log "上传至 $SERVER_USER@$SERVER_HOST:$ADMIN_REMOTE_REAL ..."
  if command -v rsync &>/dev/null; then
    rsync -az --delete --exclude='.user.ini' --exclude='.imboy-admin-root' \
      -e "ssh -p $SERVER_PORT -o ControlPath=$SSH_CTRL" \
      "$BUILD_DIR/dist/" \
      "$SERVER_USER@$SERVER_HOST:$ADMIN_REMOTE_REAL/"
  else
    # rsync 不可用时回退 scp
    ssh_exec "rm -rf '${ADMIN_REMOTE_REAL:?}/'*"
    scp -P "$SERVER_PORT" -o "ControlPath=$SSH_CTRL" -r \
      "$BUILD_DIR/dist/." \
      "$SERVER_USER@$SERVER_HOST:$ADMIN_REMOTE_REAL/"
  fi
  ok "▶ Admin Frontend 部署完成 → $ADMIN_REMOTE_REAL"
}

# =============================================================================
# deploy_migrate — 执行数据库迁移
# =============================================================================
deploy_migrate() {
  log "▶ 执行数据库迁移 ..."
  [[ "$DEPLOY_BLUE_PORT" =~ ^[0-9]+$ && "$DEPLOY_GREEN_PORT" =~ ^[0-9]+$ \
     && "$DEPLOY_BLUE_PORT" != "$DEPLOY_GREEN_PORT" ]] \
    || fail "蓝绿端口配置非法，拒绝独立迁移"
  [[ "$DEPLOY_VSN" =~ ^[a-zA-Z0-9._-]+$ ]] \
    || fail "DEPLOY_VSN 非法，拒绝独立迁移"
  [[ "$DEPLOY_PROJECT_DIR" =~ ^/[a-zA-Z0-9._/-]+$ && "$DEPLOY_PROJECT_DIR" != *..* ]] \
    || fail "DEPLOY_PROJECT_DIR 非安全绝对路径，拒绝独立迁移"
  [[ "$NGINX_CONF" =~ ^/[a-zA-Z0-9._/-]+$ && "$NGINX_CONF" != *..* ]] \
    || fail "NGINX_CONF 非安全绝对路径，拒绝独立迁移"
  [[ "$DEPLOY_COOKIE" =~ ^[a-zA-Z0-9_-]+$ ]] \
    || fail "DEPLOY_COOKIE 非法，拒绝独立迁移"
  [[ "${IMBOY_DEPLOY_NODE_HOST:-127.0.0.1}" =~ ^[a-zA-Z0-9._-]+$ ]] \
    || fail "IMBOY_DEPLOY_NODE_HOST 非法，拒绝独立迁移"

  # 独立补跑迁移前，必须证明 Nginx 只指向一个健康的目标版本，且另一色端口
  # 已可靠关闭。ss/grep/curl 任一失败都拒绝迁移，不能把空输出当作已 drain。
  local ActiveCtlNode
  if ! ActiveCtlNode="$(ssh_cap "
    set -eu
    command -v ss >/dev/null 2>&1
    command -v lsof >/dev/null 2>&1
    BLUE_STATE=\$(ss -tlnH 'sport = :${DEPLOY_BLUE_PORT}')
    GREEN_STATE=\$(ss -tlnH 'sport = :${DEPLOY_GREEN_PORT}')
    BLUE_UPSTREAM=\$(awk '/^[[:space:]]*server[[:space:]]+127\\.0\\.0\\.1:${DEPLOY_BLUE_PORT};/{n++} END{print n+0}' '${NGINX_CONF}')
    GREEN_UPSTREAM=\$(awk '/^[[:space:]]*server[[:space:]]+127\\.0\\.0\\.1:${DEPLOY_GREEN_PORT};/{n++} END{print n+0}' '${NGINX_CONF}')
    if [ \"\$BLUE_UPSTREAM\" -eq 1 ] && [ \"\$GREEN_UPSTREAM\" -eq 0 ]; then
      [ -n \"\$BLUE_STATE\" ] && [ -z \"\$GREEN_STATE\" ]
      ACTIVE_PORT='${DEPLOY_BLUE_PORT}'
    elif [ \"\$GREEN_UPSTREAM\" -eq 1 ] && [ \"\$BLUE_UPSTREAM\" -eq 0 ]; then
      [ -n \"\$GREEN_STATE\" ] && [ -z \"\$BLUE_STATE\" ]
      ACTIVE_PORT='${DEPLOY_GREEN_PORT}'
    else
      exit 2
    fi
    BODY=\$(curl -fsS --max-time 3 \"http://127.0.0.1:\$ACTIVE_PORT/healthz\")
    case \"\$BODY\" in *'\"status\":\"ok\"'*) ;; *) exit 3 ;; esac
    case \"\$BODY\" in *'\"version\":\"${DEPLOY_VSN}\"'*) ;; *) exit 4 ;; esac
    PID_LIST=\$(lsof -ti:\"\$ACTIVE_PORT\" -sTCP:LISTEN) || exit 5
    ACTIVE_PID=\$(printf '%s\\n' \"\$PID_LIST\" | head -1)
    [ -n \"\$ACTIVE_PID\" ] || exit 6
    ps -o cmd= -p \"\$ACTIVE_PID\" \
      | grep -oE -- '-name [^ ]+' \
      | awk '{print \$2}' \
      | head -1
  ")"; then
    fail "独立迁移前置检查失败：须由 Nginx 指向目标版本，且另一蓝绿节点已停止"
  fi
  [[ "$ActiveCtlNode" =~ ^[a-zA-Z0-9_-]+@[a-zA-Z0-9._-]+$ ]] \
    || fail "无法从活动监听进程取得安全的 Erlang 节点名，拒绝独立迁移"

  # 必须使用 Gate 从实际监听进程发现的节点名；统一入口每次发布都会生成动态
  # NODE_ID，Makefile 默认的 imboy@127.0.0.1 无法代表当前蓝绿节点。
  ssh_exec "cd '${DEPLOY_PROJECT_DIR}' && CTL_NODE='${ActiveCtlNode}' IMBOY_CTL_COOKIE='${DEPLOY_COOKIE}' make ctl ARGS='db migrate'"
  ok "▶ 数据库迁移完成"
}

# =============================================================================
# rollback — 将 Nginx 切回旧节点端口
# =============================================================================
rollback() {
  log "▶ 回滚委托 deploy.sh 的唯一 upstream + health 门禁 ..."
  IMBOY_DEPLOY_PORT="$SERVER_PORT" \
  IMBOY_DEPLOY_USER="$SERVER_USER" \
  IMBOY_DEPLOY_PROJECT_DIR="$DEPLOY_PROJECT_DIR" \
  IMBOY_DEPLOY_NGINX_CONF="$NGINX_CONF" \
  IMBOY_DEPLOY_BLUE_PORT="$DEPLOY_BLUE_PORT" \
  IMBOY_DEPLOY_GREEN_PORT="$DEPLOY_GREEN_PORT" \
  IMBOY_DEPLOY_COOKIE="$DEPLOY_COOKIE" \
  IMBOY_DEPLOY_BRANCH="$DEPLOY_BRANCH" \
    bash "$SCRIPT_DIR/deploy.sh" --rollback "$SERVER_HOST" "$DEPLOY_VSN" rollback
}

# =============================================================================
# 主入口 / Main
# =============================================================================
_ssh_connect

case "$COMPONENT" in
  all)
    deploy_api
    deploy_admin
    echo
    ok "════ 全量部署完成 / Full deploy complete ════"
    ;;
  api)
    deploy_api
    ;;
  admin)
    deploy_admin
    ;;
  migrate)
    deploy_migrate
    ;;
  rollback)
    rollback
    ;;
  *) exit 1 ;;
esac

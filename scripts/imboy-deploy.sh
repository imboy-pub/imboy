#!/usr/bin/env bash
# =============================================================================
# imboy 统一部署入口 / Unified deploy entry point
#
# 用法 / Usage:
#   bash scripts/imboy-deploy.sh <component> [options]
#
# Component:
#   all       全量部署：api → migrate → admin
#   api       仅部署 Erlang 后端（蓝绿零停机）
#   admin     仅部署 React 管理后台（本地构建 + 上传）
#   migrate   仅执行数据库迁移
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
           ADMIN_BUILD_DIR ADMIN_REMOTE_DIR; do
  [[ -n "${!var:-}" ]] || { echo "❌ .env.deploy 缺少必填项: $var"; exit 1; }
done

COMPONENT="${1:-all}"

# ---------- 颜色日志 ----------
log()  { echo -e "\033[36m[$(date '+%H:%M:%S')] $*\033[0m"; }
ok()   { echo -e "\033[32m✓ $*\033[0m"; }
warn() { echo -e "\033[33m⚠ $*\033[0m"; }
fail() { echo -e "\033[31m✗ $*\033[0m" >&2; exit 1; }

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
# deploy_api — 蓝绿零停机部署 Erlang 后端
# =============================================================================
deploy_api() {
  log "▶ 部署 Erlang 后端 (蓝绿) — 委托 deploy.sh ..."

  local NODE_ID; NODE_ID="$(date '+%m%d%H%M')"

  # 将 .env.deploy 变量映射为 deploy.sh 的 IMBOY_DEPLOY_* 环境变量，
  # 传 --no-migrate 让迁移由 deploy_migrate() 统一负责（避免 all 模式重复执行）
  IMBOY_DEPLOY_PORT="$SERVER_PORT" \
  IMBOY_DEPLOY_USER="$SERVER_USER" \
  IMBOY_DEPLOY_PROJECT_DIR="$DEPLOY_PROJECT_DIR" \
  IMBOY_DEPLOY_NGINX_CONF="$NGINX_CONF" \
  IMBOY_DEPLOY_BLUE_PORT="$DEPLOY_BLUE_PORT" \
  IMBOY_DEPLOY_GREEN_PORT="$DEPLOY_GREEN_PORT" \
  IMBOY_DEPLOY_COOKIE="$DEPLOY_COOKIE" \
  IMBOY_DEPLOY_BRANCH="$DEPLOY_BRANCH" \
  IMBOY_DEPLOY_STOP_OLD="${DEPLOY_STOP_OLD:-true}" \
    bash "$SCRIPT_DIR/deploy.sh" --no-migrate \
      "$SERVER_HOST" "$DEPLOY_VSN" "$NODE_ID"

  ok "▶ Erlang 后端部署完成"
}

# =============================================================================
# deploy_admin — 本地构建 React SPA 后上传至服务器
# =============================================================================
deploy_admin() {
  log "▶ 部署管理后台 Admin Frontend ..."

  local BUILD_DIR="$SCRIPT_DIR/$ADMIN_BUILD_DIR"

  # 本地构建（需要 bun）
  if ! command -v bun &>/dev/null; then
    fail "本机未安装 bun，请先安装: https://bun.sh"
  fi

  log "本地构建 Admin Frontend ..."
  (cd "$BUILD_DIR" && bun install --frozen-lockfile && bun run build)
  ok "Admin Frontend 构建完成: $BUILD_DIR/dist"

  # 上传（rsync 增量，比 scp 快）
  log "上传至 $SERVER_USER@$SERVER_HOST:$ADMIN_REMOTE_DIR ..."
  if command -v rsync &>/dev/null; then
    rsync -az --delete --exclude='.user.ini' -e "ssh -p $SERVER_PORT -o ControlPath=$SSH_CTRL" \
      "$BUILD_DIR/dist/" \
      "$SERVER_USER@$SERVER_HOST:$ADMIN_REMOTE_DIR/"
  else
    # rsync 不可用时回退 scp
    ssh_exec "rm -rf '${ADMIN_REMOTE_DIR:?}/'*"
    scp -P "$SERVER_PORT" -o "ControlPath=$SSH_CTRL" -r \
      "$BUILD_DIR/dist/." \
      "$SERVER_USER@$SERVER_HOST:$ADMIN_REMOTE_DIR/"
  fi
  ok "▶ Admin Frontend 部署完成 → $ADMIN_REMOTE_DIR"
}

# =============================================================================
# deploy_migrate — 执行数据库迁移
# =============================================================================
deploy_migrate() {
  log "▶ 执行数据库迁移 ..."
  ssh_exec "cd '${DEPLOY_PROJECT_DIR}' && make ctl ARGS='db migrate'"
  ok "▶ 数据库迁移完成"
}

# =============================================================================
# rollback — 将 Nginx 切回旧节点端口
# =============================================================================
rollback() {
  log "▶ 回滚：检测蓝绿状态 ..."

  local CURRENT_PORT OLD_PORT
  CURRENT_PORT="$(ssh_cap "grep 'server 127.0.0.1:' '${NGINX_CONF}' | grep -o '[0-9]\+;' | tr -d ';' | head -1")"
  if [[ "$CURRENT_PORT" == "$DEPLOY_BLUE_PORT" ]]; then
    OLD_PORT=$DEPLOY_GREEN_PORT
  else
    OLD_PORT=$DEPLOY_BLUE_PORT
  fi

  # 检查旧端口是否还有节点在跑
  ssh_exec "ss -tlnH 'sport = :${OLD_PORT}' 2>/dev/null | grep -q ." \
    || fail "旧节点 (port=$OLD_PORT) 未在运行，无法回滚"

  log "将 Nginx 切回 port=$OLD_PORT ..."
  ssh_exec "
    cp '${NGINX_CONF}' '${NGINX_CONF}.rollback.bak'
    sed -i 's|server 127.0.0.1:${CURRENT_PORT};|server 127.0.0.1:${OLD_PORT};|g' '${NGINX_CONF}'
    nginx -t && nginx -s reload
  "
  ok "▶ 回滚完成 → Nginx 现在指向 port=$OLD_PORT"
}

# =============================================================================
# 主入口 / Main
# =============================================================================
_ssh_connect

case "$COMPONENT" in
  all)
    deploy_api
    deploy_migrate
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
  *)
    echo "用法: bash scripts/imboy-deploy.sh <all|api|admin|migrate|rollback>"
    exit 1
    ;;
esac

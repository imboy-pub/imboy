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
  log "▶ 部署 Erlang 后端 (蓝绿) ..."

  # 自动生成节点序号（yyyymmddHHMM）
  local NODE_ID; NODE_ID="$(date '+%m%d%H%M')"
  local RELEASE_DIR="/usr/local/imboy-${DEPLOY_VSN}-${NODE_ID}"
  local TARBALL="${DEPLOY_PROJECT_DIR}/_rel/imboy/imboy-${DEPLOY_VSN}.tar.gz"

  # 1. 检测当前活跃色
  local CURRENT_COLOR
  CURRENT_COLOR="$(ssh_cap "
    if   ss -tlnH 'sport = :${DEPLOY_BLUE_PORT}'  2>/dev/null | grep -q .; then echo blue
    elif ss -tlnH 'sport = :${DEPLOY_GREEN_PORT}' 2>/dev/null | grep -q .; then echo green
    else echo none
    fi
  ")"
  local TARGET_COLOR APP_PORT OLD_PORT
  case "$CURRENT_COLOR" in
    blue)  TARGET_COLOR=green; APP_PORT=$DEPLOY_GREEN_PORT; OLD_PORT=$DEPLOY_BLUE_PORT  ;;
    green) TARGET_COLOR=blue;  APP_PORT=$DEPLOY_BLUE_PORT;  OLD_PORT=$DEPLOY_GREEN_PORT ;;
    *)     TARGET_COLOR=blue;  APP_PORT=$DEPLOY_BLUE_PORT;  OLD_PORT=""                 ;;
  esac
  ok "当前: $CURRENT_COLOR → 目标: $TARGET_COLOR (port=$APP_PORT)"

  # 2. 服务器拉代码 + 编译
  log "拉取代码并编译 release ..."
  ssh_exec "
    set -e
    cd '${DEPLOY_PROJECT_DIR}'
    git fetch origin
    git checkout '${DEPLOY_BRANCH}'
    git reset --hard origin/'${DEPLOY_BRANCH}'
    IMBOYENV=pro RELX_REL_VSN='${DEPLOY_VSN}' \
      RELX_DEV_MODE=false RELX_INCLUDE_ERTS=true \
      make rel
  "
  ok "release 编译完成"

  # 3. 解包 + vm.args
  log "解包 release + 写入 vm.args ..."
  ssh_exec "
    set -e
    mkdir -p '${RELEASE_DIR}'
    cd '${RELEASE_DIR}' && tar -xzf '${TARBALL}'
    VSN_DIR=\$(find '${RELEASE_DIR}/releases' -maxdepth 1 -mindepth 1 -type d | sort -V | tail -1)
    cat > \"\$VSN_DIR/vm.args\" <<'VMARGS'
-name ${NODE_ID}@127.0.0.1
-setcookie ${DEPLOY_COOKIE}
-heart
-kernel inet_dist_use_interface '{127,0,0,1}'
+K true
+A 256
+P 1048576
+Q 1048576
VMARGS
  "
  ok "vm.args 写入完成，节点名: ${NODE_ID}@127.0.0.1"

  # 4. 启动新节点
  log "启动新节点 (port=$APP_PORT) ..."
  ssh_exec "
    cd '${RELEASE_DIR}'
    IMBOYENV=pro IMBOY_HTTP_PORT='${APP_PORT}' ./bin/imboy daemon
  "

  # 5. 轮询等待就绪（最多 40s）
  log "等待新节点就绪 ..."
  ssh_exec "
    for i in \$(seq 1 20); do
      ss -tlnH 'sport = :${APP_PORT}' 2>/dev/null | grep -q . && exit 0
      sleep 2
    done
    exit 1
  " || fail "新节点 40s 内未就绪 (port=$APP_PORT)"
  ok "新节点已就绪"

  # 6. 切换 Nginx
  if [[ -n "$OLD_PORT" ]]; then
    log "切换 Nginx upstream: $OLD_PORT → $APP_PORT ..."
    ssh_exec "
      cp '${NGINX_CONF}' '${NGINX_CONF}.bak'
      sed -i 's|server 127.0.0.1:${OLD_PORT};|server 127.0.0.1:${APP_PORT};|g' '${NGINX_CONF}'
      grep -q 'server 127.0.0.1:${APP_PORT};' '${NGINX_CONF}' \
        || { cp '${NGINX_CONF}.bak' '${NGINX_CONF}'; echo 'upstream 替换失败已回滚' >&2; exit 1; }
      nginx -t && nginx -s reload
    "
    ok "Nginx 已切换至 $TARGET_COLOR (port=$APP_PORT)"
  else
    warn "首次部署：请手动将 Nginx upstream 设为 127.0.0.1:$APP_PORT 后执行 nginx -s reload"
  fi

  # 7. 停旧节点（可选）
  if [[ "${DEPLOY_STOP_OLD:-true}" == "true" ]] && [[ -n "$OLD_PORT" ]]; then
    log "停止旧节点 (port=$OLD_PORT) ..."
    local OLD_DIR
    OLD_DIR="$(ssh_cap \
      "find /usr/local -maxdepth 1 -name 'imboy-*' -type d ! -path '${RELEASE_DIR}' | sort -V | tail -1")" || OLD_DIR=""
    if [[ -n "$OLD_DIR" ]]; then
      ssh_exec "$OLD_DIR/bin/imboy stop || true"
      ok "旧节点已停止: $OLD_DIR"
    fi
  fi

  ok "▶ Erlang 后端部署完成 → port=$APP_PORT, dir=$RELEASE_DIR"
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
    rsync -az --delete -e "ssh -p $SERVER_PORT -o ControlPath=$SSH_CTRL" \
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

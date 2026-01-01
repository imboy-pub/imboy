#!/bin/bash
set -Eeuo pipefail

# =====================================================
# Imboy 生产环境 全自动蓝绿部署脚本（单应用架构）
# =====================================================

# ---------------- 参数 ----------------
if [ $# -ne 3 ]; then
  echo "用法: $0 <SERVER_HOST> <VSN> <NODE_NAME>"
  echo "示例: $0 192.168.1.100 1.0.0 imboy1"
  exit 1
fi

SERVER_HOST="$1"
VSN="$2"
NODE_NAME="$3"

SERVER_USER=root
SERVER_PORT=32

PROJECT_DIR="/www/wwwroot/imboy-api"
NGINX_CONF="/www/server/panel/vhost/nginx/pro.imboy.pub.conf"

# ---------------- 端口 ----------------
# 单应用架构：每个环境只有一个 HTTP 端口
BLUE_PORT=9800
GREEN_PORT=9801

# ---------------- 日志 ----------------
log()  { echo -e "\033[36m[$(date '+%H:%M:%S')] $1\033[0m"; }
ok()   { echo -e "\033[32m✓ $1\033[0m"; }
fail() { echo -e "\033[31m✗ $1\033[0m"; exit 1; }

ssh_exec() {
  ssh -p "$SERVER_PORT" "$SERVER_USER@$SERVER_HOST" "$1"
}

# =====================================================
# 1️⃣ 当前运行环境检测（端口）
# =====================================================
detect_color() {
  ssh_exec "
    if lsof -i:$BLUE_PORT >/dev/null 2>&1; then
      echo blue
    elif lsof -i:$GREEN_PORT >/dev/null 2>&1; then
      echo green
    else
      echo none
    fi
  "
}

detect_current() {
  ssh_exec "
    if lsof -i:9800 >/dev/null 2>&1; then
      echo blue:9800
    elif lsof -i:9801 >/dev/null 2>&1; then
      echo green:9801
    else
      echo none:0
    fi
  "
}

confirm() {
  echo
  echo "⚠️  远程部署目录已存在：$1"
  echo -n "是否删除并继续部署？(y/N): "
  read answer
  case "$answer" in
    y|Y) return 0 ;;
    *)   echo "✗ 用户终止部署"; exit 1 ;;
  esac
}

CURRENT_COLOR="$(detect_color)"
CURRENT_PORT_INFO="$(detect_current)"

# =====================================================
# 2️⃣ 目标部署环境计算
# =====================================================

if [ "$CURRENT_COLOR" = "blue" ]; then
  TARGET_COLOR=green
  APP_PORT=$GREEN_PORT
elif [ "$CURRENT_COLOR" = "green" ]; then
  TARGET_COLOR=blue
  APP_PORT=$BLUE_PORT
else
  TARGET_COLOR=blue
  APP_PORT=$BLUE_PORT
fi

ok "目标部署环境：$TARGET_COLOR (port=$APP_PORT)"

RELEASE_DIR="/usr/local/imboy-$VSN-$NODE_NAME"
RELEASE_TARBALL="$PROJECT_DIR/_rel/imboy/imboy-$VSN.tar.gz"

VM_ARGS="$PROJECT_DIR/config/vm.pro.args"
SYS_CONFIG="$PROJECT_DIR/config/sys.pro.config"
RELX_CONFIG="$PROJECT_DIR/relxpro.config"

# =====================================================
# 检查部署目录是否存在
# =====================================================

log "当前运行环境：$CURRENT_COLOR (port $CURRENT_PORT_INFO)"
log "检查远程部署目录是否存在"
if ssh_exec "[ -d $RELEASE_DIR ]"; then
  confirm "$RELEASE_DIR"
  log "删除旧部署目录 $RELEASE_DIR"
  ssh_exec "rm -rf $RELEASE_DIR"
fi

ssh_exec "mkdir -p $RELEASE_DIR"
ok "部署目录准备完成"

# =====================================================
# 3️⃣ 更新配置
# =====================================================

log "更新 relx 版本号"
ssh_exec "sed -i 's/{release, {[[:space:]]*imboy,[[:space:]]*\"[^\"]*\"}/{release, {imboy, \"${VSN}\"}/' $RELX_CONFIG"

log "设置节点名"
ssh_exec "sed -i 's/^-name .*/-name ${NODE_NAME}@127.0.0.1/' $VM_ARGS"

log "设置监听端口（单应用架构：移除 http_port_adm）"
ssh_exec "
  sed -i '
    s/{http_port, *[0-9]*}/{http_port, ${APP_PORT}}/;
  ' $SYS_CONFIG
"

# 如果存在 http_port_adm 配置，需要删除或注释
log "移除旧的 ADM 端口配置（如果存在）"
ssh_exec "
  if grep -q 'http_port_adm' $SYS_CONFIG; then
    sed -i '/http_port_adm/d' $SYS_CONFIG
  fi
"

ok "配置文件已更新"

# =====================================================
# 4️⃣ 编译 release
# =====================================================

log "编译 release（静默模式）"

ssh_exec "
  set -e
  cd $PROJECT_DIR &&
  git pull origin dev --rebase >/dev/null 2>&1 &&
  make clean            >/dev/null 2>&1 &&
  make                  >/dev/null 2>&1 &&
  make rel IMBOYENV=pro  >/dev/null 2>&1
"

ok "release 编译完成"

# =====================================================
# 5️⃣ 部署 release
# =====================================================

log "准备部署目录 $RELEASE_DIR"
ssh_exec "rm -rf $RELEASE_DIR && mkdir -p $RELEASE_DIR"

log "解压 release"
ssh_exec "cd $RELEASE_DIR && tar -xzf $RELEASE_TARBALL"

ok "release 已部署到 $RELEASE_DIR"

# =====================================================
# 6️⃣ 启动新节点
# =====================================================

log "启动新节点 ($TARGET_COLOR)"
ssh_exec "$RELEASE_DIR/bin/imboy daemon"

sleep 5

if ! ssh_exec "lsof -i:$APP_PORT >/dev/null 2>&1"; then
  fail "新节点启动失败，端口 $APP_PORT 未监听"
fi

ok "新节点启动成功（端口 $APP_PORT）"

# =====================================================
# 7️⃣ 切换 Nginx
# =====================================================

log "切换 Nginx upstream -> $TARGET_COLOR"
ssh_exec "
  sed -i '
    /upstream pro_imboy_api {/,/}/ {
      s/server 127.0.0.1:[0-9]\\+/server 127.0.0.1:${APP_PORT}/
    }
  ' $NGINX_CONF &&
  nginx -t
"

# 如果存在 ADM upstream 配置，也需要更新
ssh_exec "
  if grep -q 'upstream pro_imboy_adm' $NGINX_CONF; then
    sed -i '
      /upstream pro_imboy_adm {/,/}/ {
        s/server 127.0.0.1:[0-9]\\+/server 127.0.0.1:${APP_PORT}/
      }
    ' $NGINX_CONF &&
    nginx -t
  fi
"

ok "Nginx 配置已切换"

log "重载 Nginx"
ssh_exec "nginx -s reload"

ok "Nginx 已重载"

# =====================================================
# 8️⃣ 停止旧节点
# =====================================================

if [ "$CURRENT_COLOR" != "none" ]; then
  # 查找旧版本目录
  OLD_DIR="\$(ssh_exec 'ls -d /usr/local/imboy-*-* 2>/dev/null | grep -v $RELEASE_DIR | head -1')"
  if [ -n "\$OLD_DIR" ]; then
    log "停止旧节点: \$OLD_DIR"
    ssh_exec "\$OLD_DIR/bin/imboy stop || true"
    ok "旧节点已停止"
  fi
fi

# =====================================================
# 完成
# =====================================================

echo
ok "🎉 蓝绿部署完成"
echo "----------------------------------"
echo "版本       : $VSN"
echo "节点名     : $NODE_NAME"
echo "运行环境   : $TARGET_COLOR"
echo "应用端口   : $APP_PORT"
echo "部署目录   : $RELEASE_DIR"
echo "----------------------------------"
echo ""
echo "管理后台访问："
echo "  API 路由: http://$SERVER_HOST:$APP_PORT/api/*"
echo "  ADM 路由: http://$SERVER_HOST:$APP_PORT/adm/*"
echo "  WebSocket: ws://$SERVER_HOST:$APP_PORT/ws"
echo "----------------------------------"

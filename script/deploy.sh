#!/bin/bash
set -Eeuo pipefail

# =====================================================
# Imboy 生产环境 全自动蓝绿部署脚本
# =====================================================

# ---------------- 参数 ----------------
if [ $# -ne 3 ]; then
  echo "用法: $0 <SERVER_HOST> <VSN> <NODE_NAME>"
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
BLUE_API=9800
BLUE_ADM=9806
GREEN_API=9801
GREEN_ADM=9807

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
    if lsof -i:$BLUE_API >/dev/null 2>&1; then
      echo blue
    elif lsof -i:$GREEN_API >/dev/null 2>&1; then
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
CURRENT_API_PORT="$(detect_current)"


# =====================================================
# 2️⃣ 目标部署环境计算
# =====================================================

if [ "$CURRENT_COLOR" = "blue" ]; then
  TARGET_COLOR=green
  API_PORT=$GREEN_API
  ADM_PORT=$GREEN_ADM
elif [ "$CURRENT_COLOR" = "green" ]; then
  TARGET_COLOR=blue
  API_PORT=$BLUE_API
  ADM_PORT=$BLUE_ADM
else
  TARGET_COLOR=blue
  API_PORT=$BLUE_API
  ADM_PORT=$BLUE_ADM
fi

ok "目标部署环境：$TARGET_COLOR (api port=$API_PORT)"

RELEASE_DIR="/usr/local/imboy-$VSN-$NODE_NAME"
RELEASE_TARBALL="$PROJECT_DIR/_rel/imboy/imboy-$VSN.tar.gz"

VM_ARGS="$PROJECT_DIR/config/vm.pro.args"
SYS_CONFIG="$PROJECT_DIR/config/sys.pro.config"
RELX_CONFIG="$PROJECT_DIR/relxpro.config"


# =====================================================
# 检查部署目录是否存在
# =====================================================

log "当前运行环境：$CURRENT_COLOR (api port $CURRENT_API_PORT)"
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

log "设置监听端口"
ssh_exec "
  sed -i '
    s/{http_port, *[0-9]*}/{http_port, ${API_PORT}}/;
    s/{http_port_adm, *[0-9]*}/{http_port_adm, ${ADM_PORT}}/
  ' $SYS_CONFIG
"

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

# =====================================================
# 6️⃣ 启动新节点
# =====================================================

# log "启动新节点 ($TARGET_COLOR)"
# ssh_exec "$RELEASE_DIR/bin/imboy daemon"

# sleep 5

# if ! ssh_exec "lsof -i:$API_PORT >/dev/null 2>&1"; then
#   fail "新节点启动失败，端口未监听"
# fi

# ok "新节点启动成功"

# =====================================================
# 7️⃣ 切换 Nginx
# =====================================================

log "切换 Nginx upstream -> $TARGET_COLOR"
ssh_exec "
  sed -i '
    /upstream pro_imboy_api {/,/}/ {
      s/server 127.0.0.1:[0-9]\\+/server 127.0.0.1:${API_PORT}/
    }
    /upstream pro_imboy_adm {/,/}/ {
      s/server 127.0.0.1:[0-9]\\+/server 127.0.0.1:${ADM_PORT}/
    }
  ' $NGINX_CONF &&
  nginx -t
"

ok "Nginx 已切换（待手动reload）"

# =====================================================
# 8️⃣ 停止旧节点
# =====================================================

if [ "$CURRENT_COLOR" != "none" ]; then
  OLD_DIR="/usr/local/imboy-*-$CURRENT_COLOR"
  log "可手动停止旧节点 $OLD_DIR/bin/imboy stop"
  # log "停止旧节点 ($CURRENT_COLOR)"
  # ssh_exec "$OLD_DIR/bin/imboy stop || true"
fi

# ok "旧节点已停止"

# =====================================================
# 完成
# =====================================================

echo
ok "🎉 蓝绿部署完成"
echo "----------------------------------"
echo "版本       : $VSN"
echo "运行环境   : $TARGET_COLOR"
echo "API 端口   : $API_PORT"
echo "ADM 端口   : $ADM_PORT"
echo "部署目录   : $RELEASE_DIR"
echo  "可执行命令: $RELEASE_DIR/bin/imboy daemon && nginx -s reload"
echo "----------------------------------"

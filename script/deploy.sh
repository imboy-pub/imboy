#!/bin/bash
set -Eeuo pipefail

# =====================================================
# Imboy 生产环境 蓝绿部署脚本（macOS / Linux 通用）
#
# 用法:
#   ./deploy.sh <SERVER_HOST> <VSN> <blue|green> <NODE_NAME>
# =====================================================

# ---------------- 参数 ----------------
if [ $# -ne 4 ]; then
  echo "用法: $0 <SERVER_HOST> <VSN> <blue|green> <NODE_NAME>"
  exit 1
fi

SERVER_HOST="$1"
VSN="$2"
COLOR="$3"
NODE_NAME="$4"

SERVER_USER=root
SERVER_PORT=32

# =====================================================
# 端口映射（Bash 3.2 安全写法）
# =====================================================

case "$COLOR" in
  blue)
    API_PORT=9800
    ADM_PORT=9806
    ;;
  green)
    API_PORT=9801
    ADM_PORT=9807
    ;;
  *)
    echo "✗ COLOR 只能是 blue 或 green"
    exit 1
    ;;
esac

# ---------------- 项目路径 ----------------
PROJECT_DIR="/www/wwwroot/imboy-api"

VM_ARGS_FILE="$PROJECT_DIR/config/vm.pro.args"
SYS_CONFIG_FILE="$PROJECT_DIR/config/sys.pro.config"

NGINX_CONF="/www/server/panel/vhost/nginx/pro.imboy.pub.conf"

RELEASE_TARBALL="$PROJECT_DIR/_rel/imboy/imboy-$VSN.tar.gz"
RELEASE_DIR="/usr/local/imboy-$VSN-$NODE_NAME"

# ---------------- 日志工具 ----------------
log()  { echo -e "\033[36m[$(date '+%H:%M:%S')] $1\033[0m"; }
ok()   { echo -e "\033[32m✓ $1\033[0m"; }
fail() { echo -e "\033[31m✗ $1\033[0m"; exit 1; }

ssh_exec() {
  ssh -p "$SERVER_PORT" "$SERVER_USER@$SERVER_HOST" "$1"
}

# =====================================================
# 1️⃣ Erlang 节点名
# =====================================================

log "设置 Erlang 节点名 -> -name ${NODE_NAME}@127.0.0.1"
ssh_exec "
  sed -i 's/^-name .*/-name ${NODE_NAME}@127.0.0.1/' $VM_ARGS_FILE
"
ok "vm.pro.args 已更新"

# =====================================================
# 2️⃣ 监听端口
# =====================================================

log "设置监听端口 api=$API_PORT adm=$ADM_PORT"
ssh_exec "
  sed -i '
    s/{http_port, *[0-9]*}/{http_port, ${API_PORT}}/;
    s/{http_port_adm, *[0-9]*}/{http_port_adm, ${ADM_PORT}}/
  ' $SYS_CONFIG_FILE
"
ok "sys.pro.config 已更新"

# =====================================================
# 3️⃣ 编译 release
# =====================================================

log "开始编译 release (VSN=$VSN)"
ssh_exec "
  cd $PROJECT_DIR &&
  make clean &&
  make &&
  make rel IMBOYENV=pro
"
ok "release 编译完成"

# =====================================================
# 4️⃣ 解压
# =====================================================

log "准备部署目录 $RELEASE_DIR"
ssh_exec "rm -rf $RELEASE_DIR && mkdir -p $RELEASE_DIR"

log "解压 release 包"
ssh_exec "
  cd $RELEASE_DIR &&
  tar -xzf $RELEASE_TARBALL
"
ok "release 解压完成"

# =====================================================
# 5️⃣ Nginx 校验（不 reload）
# =====================================================
# =====================================================
# 5️⃣.5️⃣ 切换 Nginx upstream（blue / green）
# =====================================================

log "切换 Nginx upstream -> $COLOR (api=$API_PORT adm=$ADM_PORT)"

ssh_exec "
  sed -i '
    /upstream pro_imboy_api {/,/}/ {
      s/server 127.0.0.1:[0-9]\\+/server 127.0.0.1:${API_PORT}/
    }
    /upstream pro_imboy_adm {/,/}/ {
      s/server 127.0.0.1:[0-9]\\+/server 127.0.0.1:${ADM_PORT}/
    }
  ' $NGINX_CONF
"

log "校验 Nginx upstream 是否包含端口"
ssh_exec "
  grep -E '${API_PORT}|${ADM_PORT}' $NGINX_CONF &&
  nginx -t
"
ok "Nginx 配置校验通过"

# =====================================================
# 完成
# =====================================================

echo
ok "🎉 部署完成"
echo "----------------------------------"
echo "版本       : $VSN"
echo "环境       : $COLOR"
echo "节点       : $NODE_NAME"
echo "API 端口   : $API_PORT"
echo "ADM 端口   : $ADM_PORT"
echo "部署目录   : $RELEASE_DIR"
echo  "可执行命令: $RELEASE_DIR/bin/imboy daemon"
echo "----------------------------------"

#!/bin/bash
set -Eeuo pipefail

# =====================================================
# Imboy 生产环境 全自动蓝绿部署脚本（单应用架构）
# =====================================================

# ---------------- 静默控制 ----------------
SILENT=1   # 默认静默

while [[ $# -gt 0 ]]; do
  case "$1" in
    -v|--verbose)
      SILENT=0
      shift
      ;;
    -s|--silent)
      SILENT=1
      shift
      ;;
    --)
      shift
      break
      ;;
    -*)
      echo "未知参数: $1"
      exit 1
      ;;
    *)
      break
      ;;
  esac
done

# ---------------- 参数 ----------------
if [ $# -ne 3 ]; then
  echo "用法: $0 [-v|--verbose|-s|--silent] <SERVER_HOST> <VSN> <NODE_NAME>"
  echo "示例: $0 -v 192.168.1.100 0.7.3 001"
  echo "bash ./script/deploy.sh -v 192.168.1.110 0.7.3 001"
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
BLUE_PORT=9800
GREEN_PORT=9801

# ---------------- 日志 ----------------
log()  { echo -e "\033[36m[$(date '+%H:%M:%S')] $1\033[0m"; }
ok()   { echo -e "\033[32m✓ $1\033[0m"; }
fail() { echo -e "\033[31m✗ $1\033[0m"; exit 1; }

trap 'fail "脚本执行失败，请使用 -v 或 --verbose 查看详细输出"' ERR

# ---------------- SSH ----------------
ssh_exec() {
  if [ "$SILENT" -eq 1 ]; then
    ssh -tt -p "$SERVER_PORT" "$SERVER_USER@$SERVER_HOST" "$1" >/dev/null 2>&1
  else
    ssh -tt -p "$SERVER_PORT" "$SERVER_USER@$SERVER_HOST" "$1"
  fi
}

# =====================================================
# 1️⃣ 当前运行环境检测
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

CURRENT_COLOR="$(detect_color)"

# =====================================================
# 2️⃣ 目标部署环境
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
# 3️⃣ 检查部署目录
# =====================================================
if ssh_exec "[ -d $RELEASE_DIR ]"; then
  echo
  echo "⚠️  远程部署目录已存在：$RELEASE_DIR"
  echo -n "是否删除并继续部署？(y/N): "
  read answer
  [[ "$answer" =~ ^[yY]$ ]] || exit 1
  ssh_exec "rm -rf $RELEASE_DIR"
fi

ssh_exec "mkdir -p $RELEASE_DIR"
ok "部署目录准备完成"

# =====================================================
# 4️⃣ 更新配置
# =====================================================
ssh_exec "sed -i 's/{release, {[[:space:]]*imboy,[[:space:]]*\"[^\"]*\"}/{release, {imboy, \"${VSN}\"}/' $RELX_CONFIG"
ssh_exec "sed -i 's/^-name .*/-name ${NODE_NAME}@127.0.0.1/' $VM_ARGS"
ssh_exec "sed -i 's/{http_port, *[0-9]*}/{http_port, ${APP_PORT}}/' $SYS_CONFIG"
ssh_exec "sed -i '/http_port_adm/d' $SYS_CONFIG || true"

ok "配置文件已更新"

# =====================================================
# 5️⃣ 编译 release（⚠️ 已移除 make clean）
# =====================================================
log "编译 release（$( [ "$SILENT" -eq 1 ] && echo "静默" || echo "详细" )模式）"

ssh_exec "
  set -e
  cd $PROJECT_DIR
  git pull origin dev --rebase
  make
  make rel IMBOYENV=pro
"

ok "release 编译完成"

# =====================================================
# 6️⃣ 部署 release
# =====================================================
ssh_exec "rm -rf $RELEASE_DIR && mkdir -p $RELEASE_DIR"
ssh_exec "cd $RELEASE_DIR && tar -xzf $RELEASE_TARBALL"

ok "release 已部署"

# =====================================================
# 7️⃣ 启动新节点
# =====================================================
ssh_exec "$RELEASE_DIR/bin/imboy daemon"
sleep 5

ssh_exec "lsof -i:$APP_PORT >/dev/null 2>&1" || fail "新节点启动失败"

ok "新节点启动成功（端口 ${APP_PORT}）"

# =====================================================
# 8️⃣ 切换 Nginx
# =====================================================
ssh_exec "
  sed -i '/upstream pro_imboy_api {/,/}/ s/server 127.0.0.1:[0-9]\\+/server 127.0.0.1:${APP_PORT}/' $NGINX_CONF
"

# nginx -t
# nginx -s reload

ok "Nginx 请手动 nginx -t nginx -s reload"

# =====================================================
# 9️⃣ 停止旧节点
# =====================================================
# OLD_DIR=\"\$(ls -d /usr/local/imboy-*-* 2>/dev/null | grep -v '$RELEASE_DIR' | head -1)\"
# if [ -n \"$OLD_DIR\" ]; then
#   ssh_exec \"$OLD_DIR/bin/imboy stop || true\"
#   ok \"旧节点已停止\"
# fi

# =====================================================
# 完成
# =====================================================
echo
ok "🎉 蓝绿部署完成"
echo "----------------------------------"
echo "版本       : ${VSN}"
echo "节点名     : ${NODE_NAME}"
echo "运行环境   : ${TARGET_COLOR}"
echo "应用端口   : ${APP_PORT}"
echo "部署目录   : ${RELEASE_DIR}"
echo "可执行命令: ${RELEASE_DIR}/bin/imboy daemon && nginx -s reload"
echo "----------------------------------"

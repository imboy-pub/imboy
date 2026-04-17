#!/usr/bin/env bash
set -Eeuo pipefail

# =====================================================
# Imboy 生产环境 全自动蓝绿部署脚本（单应用架构）
#
# 与历史版本的差异：
#   * 不再 sed 修改 relxpro.config / sys.pro.config / vm.pro.args（这些
#     文件早已 broken / 不存在），统一用单一 relx.config + sys.config。
#   * 节点身份（NODE_NAME）、分发监听、cookie 在远端 release 目录里
#     就地生成 vm.args（与 script/start_node.sh 一致）。
#   * 蓝绿端口（HTTP_PORT）走环境变量 IMBOY_HTTP_PORT 注入，再由
#     imboy_env.erl 在 application 启动期覆盖 sys.config 中的 http_port。
#   * 生产强校验由 imboy_app:validate_runtime_config/0 在 IMBOYENV=pro
#     启动时执行（密钥 / TURN / api_auth_switch / pg_password 等）。
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
  echo "示例: $0 -v <server_host> <version> <node_name>"
  echo "示例: bash ./script/deploy.sh -v 10.0.0.10 1.0.0 001"
  echo "可选环境变量:"
  echo "  IMBOY_DEPLOY_USER          SSH 用户，默认 root"
  echo "  IMBOY_DEPLOY_PORT          SSH 端口，默认 32"
  echo "  IMBOY_DEPLOY_PROJECT_DIR   远端项目目录，默认 /www/wwwroot/imboy-api"
  echo "  IMBOY_DEPLOY_NGINX_CONF    远端 nginx 配置路径"
  echo "  IMBOY_DEPLOY_BLUE_PORT     蓝环境端口，默认 9800"
  echo "  IMBOY_DEPLOY_GREEN_PORT    绿环境端口，默认 9801"
  echo "  IMBOY_DEPLOY_NODE_HOST     节点 host，默认 127.0.0.1"
  echo "  IMBOY_DEPLOY_COOKIE        节点 cookie，默认 imboy"
  exit 1
fi

SERVER_HOST="$1"
VSN="$2"
NODE_NAME="$3"

SERVER_USER="${IMBOY_DEPLOY_USER:-root}"
SERVER_PORT="${IMBOY_DEPLOY_PORT:-32}"

PROJECT_DIR="${IMBOY_DEPLOY_PROJECT_DIR:-/www/wwwroot/imboy-api}"
NGINX_CONF="${IMBOY_DEPLOY_NGINX_CONF:-/www/server/panel/vhost/nginx/pro.imboy.pub.conf}"

NODE_HOST="${IMBOY_DEPLOY_NODE_HOST:-127.0.0.1}"
COOKIE="${IMBOY_DEPLOY_COOKIE:-imboy}"

# ---------------- 端口 ----------------
BLUE_PORT="${IMBOY_DEPLOY_BLUE_PORT:-9800}"
GREEN_PORT="${IMBOY_DEPLOY_GREEN_PORT:-9801}"

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
# 4️⃣ 编译 release（统一 relx.config，VSN 由 PROJECT_VERSION 控制）
# =====================================================
log "编译 release（$( [ "$SILENT" -eq 1 ] && echo "静默" || echo "详细" )模式）"

# RELX_REL_VSN 让 erlang.mk / relx 把 release 版本对齐到 $VSN，
# 不必再 sed 修改 relx.config 的版本号字段。
ssh_exec "
  set -e
  cd $PROJECT_DIR
  git pull origin dev --rebase
  make
  IMBOYENV=pro RELX_REL_VSN=$VSN make rel
"

ok "release 编译完成"

# =====================================================
# 5️⃣ 部署 release 并就地生成 vm.args（注入 NODE_NAME / COOKIE）
# =====================================================
ssh_exec "rm -rf $RELEASE_DIR && mkdir -p $RELEASE_DIR"
ssh_exec "cd $RELEASE_DIR && tar -xzf $RELEASE_TARBALL"

# 找到 release 内 vm.args 的实际路径（dev_mode=true 用 releases/X/vm.args）
ssh_exec "
  set -e
  REL_VSN_DIR=\$(find $RELEASE_DIR/releases -maxdepth 1 -mindepth 1 -type d | sort -V | tail -1)
  cat > \$REL_VSN_DIR/vm.args <<EOF
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
EOF
"

ok "release 已部署，vm.args 已就地生成"

# =====================================================
# 6️⃣ 启动新节点（端口 / 环境通过环境变量注入，不再 sed sys.config）
# =====================================================
ssh_exec "
  cd $RELEASE_DIR && \
  IMBOYENV=pro \
  IMBOY_HTTP_PORT=$APP_PORT \
  ./bin/imboy daemon
"
sleep 5

ssh_exec "lsof -i:$APP_PORT >/dev/null 2>&1" || fail "新节点启动失败"

ok "新节点启动成功（端口 ${APP_PORT}）"

# =====================================================
# 7️⃣ 切换 Nginx
# =====================================================
ssh_exec "
  sed -i '/upstream pro_imboy_api {/,/}/ s/server 127.0.0.1:[0-9]\\+/server 127.0.0.1:${APP_PORT}/' $NGINX_CONF
"

# nginx -t
# nginx -s reload

ok "Nginx 请手动 nginx -t nginx -s reload"

# =====================================================
# 8️⃣ 停止旧节点（手动操作，注释保留以便维护）
# =====================================================
# OLD_DIR="$(ls -d /usr/local/imboy-*-* 2>/dev/null | grep -v '$RELEASE_DIR' | head -1)"
# if [ -n "$OLD_DIR" ]; then
#   ssh_exec "$OLD_DIR/bin/imboy stop || true"
#   ok "旧节点已停止"
# fi

# =====================================================
# 完成
# =====================================================
echo
ok "🎉 蓝绿部署完成"
echo "----------------------------------"
echo "版本       : ${VSN}"
echo "节点名     : ${NODE_NAME}@${NODE_HOST}"
echo "运行环境   : ${TARGET_COLOR}"
echo "应用端口   : ${APP_PORT}"
echo "部署目录   : ${RELEASE_DIR}"
echo "下一步     : ${RELEASE_DIR}/bin/imboy console（验证）后 nginx -s reload"
echo "----------------------------------"

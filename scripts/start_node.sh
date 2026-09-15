#!/usr/bin/env bash
set -Eeuo pipefail
# 用法: ./script/start_node.sh <nodename> [cookie] [port] [exclude_apps] [daemon]
# 例如: ./script/start_node.sh node1 imboycookie 9801
# 例如: ./script/start_node.sh node2 imboycookie 9802 "imadm,imcron"
# 例如: ./script/start_node.sh node3 imboycookie 9803 "imadm" daemon

NODE="${1:-}"
COOKIE="${2:-imboycookie}"
PORT="${3:-9800}"
EXCLUDE_APPS="${4:-}"
DAEMON="${5:-}"
NODE_HOST="${IMBOY_NODE_HOST:-127.0.0.1}"
DIST_INTERFACE="${IMBOY_DIST_INTERFACE:-{127,0,0,1}}"

[ -z "$NODE" ] && {
  echo "Usage: $0 <nodename> [cookie] [port] [exclude_apps] [daemon]"
  echo "Environment:"
  echo "  IMBOY_NODE_HOST       节点 host，默认 127.0.0.1"
  echo "  IMBOY_DIST_INTERFACE  分布式监听地址，默认 {127,0,0,1}"
  echo "  .env                  仓根基础变量文件（gitignored，可选）：启动前自动加载"
  echo "  .env.local            仓根本地变量文件（gitignored，可选）：启动前自动加载，"
  echo "                        同名变量**覆盖** .env（用于注入 BIGMODEL_API_KEY 等"
  echo "                        以 {env, Var} 在运行时解析的密钥）"
  exit 1
}

cd "$(dirname "$0")/.." || exit 1

# 本地变量注入（可选）：仓根 .env / .env.local 不入仓（.gitignore），放本地开发所需密钥，
# 例如 BIGMODEL_API_KEY —— config/sys.local.config 以 {env, <<"BIGMODEL_API_KEY">>}
# 在**运行时**从 OS 环境变量解析（imboy_llm_registry:resolve_env/1 → os:getenv/1），
# 故密钥必须在本进程环境里（不写进 config 文件）。
#
# 两个文件都加载，顺序：.env 在前、.env.local 在后 —— 后者同名覆盖前者，
# 与 dotenv 系（Vite/Next 等）的「.env.local 优先级更高」约定一致。
# 为什么要收 .env：这是本仓 docker/生产模板的对应文件，也是使用者最自然会去填的文件；
# 只认 .env.local 会让「明明填了密钥却 provider_unavailable」变成一个静默陷阱
# （实测踩过：密钥在 .env 里、节点却读不到，全程无任何报错）。
# 安全性：sys.local.config 只以 {env, ...} 引用 ARK_API_KEY / BAILIAN_API_KEY /
# BIGMODEL_API_KEY 三个变量，因此 .env 里其余 IMBOY_* 项不会改变本地库连接或密钥体系。
# IMBOYENV / HTTP_PORT 由脚本参数与调用方环境决定，两个文件都不得覆盖：
# 先留存现场值，加载后原样恢复。
_SavedIMBOYENV="${IMBOYENV-}"
_SavedHTTPPORT="${HTTP_PORT-}"
for _envfile in .env .env.local; do
  if [ -f "$_envfile" ]; then
    set -a
    # shellcheck disable=SC1091
    . "./$_envfile"
    set +a
    echo "已加载 $_envfile"
  fi
done
unset _envfile
export IMBOYENV="$_SavedIMBOYENV"
export HTTP_PORT="$_SavedHTTPPORT"

export IMBOYENV="${IMBOYENV:-local}"
export HTTP_PORT="$PORT"

REL_BIN="_rel/imboy/bin/imboy"
[ -x "$REL_BIN" ] || { echo "未找到 release 脚本: $REL_BIN，请先执行 make rel"; exit 2; }
REL_RELEASE_DIR=$(find _rel/imboy/releases -maxdepth 1 -mindepth 1 -type d | sort -V | tail -1)
[ -n "$REL_RELEASE_DIR" ] || { echo "未找到 release 版本目录，请先执行 make rel"; exit 2; }
VM_ARGS_FILE="$REL_RELEASE_DIR/vm.args"
REL_VSN="$(basename "$REL_RELEASE_DIR")"
REL_APP_EBIN="_rel/imboy/lib/imboy-${REL_VSN}/ebin"
[ -d "$REL_APP_EBIN" ] || { echo "release 应用目录缺失: $REL_APP_EBIN，请执行 make rel"; exit 2; }

echo "编译并校验 release beam 新鲜度..."
make compile >/dev/null
for beam in ebin/*.beam; do
  rel_beam="$REL_APP_EBIN/$(basename "$beam")"
  if [ ! -f "$rel_beam" ] || ! cmp -s "$beam" "$rel_beam"; then
    echo "release beam 陈旧或缺失: $(basename "$beam")，请执行 make rel" >&2
    exit 2
  fi
done

# 准备vm.args
cat > "$VM_ARGS_FILE" <<EOF
-name ${NODE}@${NODE_HOST}
-setcookie ${COOKIE}
-heart
-kernel inet_dist_use_interface ${DIST_INTERFACE}
+K true
+A 1024
+P 20480
+Q 20480
+S 2
+MSe true
EOF

# 生成排除应用的eval命令
gen_exclude_cmd() {
  [ -z "$EXCLUDE_APPS" ] && return
  echo "-eval '"
  IFS=',' read -ra APPS <<< "$EXCLUDE_APPS"
  for app in "${APPS[@]}"; do
    echo "  case application:stop('$app') of"
    echo "    ok -> io:format(\"成功停止应用: $app~n\");"
    echo "    {error, {not_started, '$app'}} -> ok;"
    echo "    Err_$app -> io:format(\"停止应用 $app 错误: ~p~n\", [Err_$app])"
    echo "  end,"
  done
  echo "  ok.'"
}

# 启动节点
if [ "$DAEMON" = "daemon" ]; then
  echo "启动节点(daemon模式): $NODE"
  "$REL_BIN" daemon
  sleep 1  # 等待节点启动
  if [ -n "$EXCLUDE_APPS" ]; then
      IFS=',' read -ra APPS <<< "$EXCLUDE_APPS"
      for app in "${APPS[@]}"; do
          echo "尝试停止应用: $app"
          # 调用并获取结果
          result=$("$REL_BIN" rpc application stop "['$app']")
          case "$result" in
              "ok")
                  echo "成功停止应用: $app"
                  ;;
              "{error,{not_started,"*)
                  echo "应用 $app 未运行，无需停止"
                  ;;
              *)
                  echo "停止应用 $app 失败: $result"
                  ;;
          esac
      done
  fi

else
  echo "启动节点(console模式): $NODE"
  if [ -n "$EXCLUDE_APPS" ]; then
    # 带排除应用的console启动
    EXCLUDE_CMD=$(gen_exclude_cmd)
    echo "执行命令: $REL_BIN console $EXCLUDE_CMD"
    eval "$REL_BIN" console "$EXCLUDE_CMD"
  else
    echo "执行命令: $REL_BIN console "
    # 普通console启动
    "$REL_BIN" console
  fi
fi

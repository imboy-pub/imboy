#!/bin/bash
# 用法: ./script/start_node.sh <nodename> [cookie] [port] [exclude_apps] [daemon]
# 例如: ./script/start_node.sh node1 imboycookie 9801
# 例如: ./script/start_node.sh node2 imboycookie 9802 "imadm,imcron"
# 例如: ./script/start_node.sh node3 imboycookie 9803 "imadm" daemon

NODE="$1"
COOKIE="${2:-imboycookie}"
PORT="${3:-9800}"
EXCLUDE_APPS="$4"
DAEMON="$5"

[ -z "$NODE" ] && { echo "Usage: $0 <nodename> [cookie] [port] [exclude_apps] [daemon]"; exit 1; }

cd "$(dirname "$0")/.." || exit 1

export IMBOYENV="${IMBOYENV:-local}"
export HTTP_PORT="$PORT"

REL_BIN="_rel/imboy/bin/imboy"
REL_RELEASE_DIR=$(find _rel/imboy/releases -maxdepth 1 -mindepth 1 -type d | sort -V | tail -1)
VM_ARGS_FILE="$REL_RELEASE_DIR/vm.args"

# 准备vm.args
cat > "$VM_ARGS_FILE" <<EOF
-name ${NODE}@127.0.0.1
-setcookie ${COOKIE}
-heart
-kernel inet_dist_use_interface '{127,0,0,1}'
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

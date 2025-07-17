#!/bin/bash
# 用法: ./script/stop_node.sh <nodename>
# 例如: ./script/stop_node.sh node2

NODE="$1"

if [ -z "$NODE" ]; then
  echo "用法: $0 <nodename>"
  exit 1
fi

cd "$(dirname "$0")/.." || exit 1

REL_BIN="_rel/imboy/bin/imboy"
if [ ! -x "$REL_BIN" ]; then
  echo "未找到 release 脚本: $REL_BIN"
  echo "请先执行: make rel"
  exit 2
fi

echo "尝试停止节点 ${NODE}@127.0.0.1 ..."

"$REL_BIN" rpc init stop

if [ $? -eq 0 ]; then
  echo "✅ 节点 ${NODE}@127.0.0.1 已成功关闭"
else
  echo "❌ 停止失败，请确认节点是否在线、cookie 是否正确"
fi

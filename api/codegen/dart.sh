#!/usr/bin/env bash
# imboy/api/codegen/dart.sh — Flutter 端 protobuf 代码生成入口
#
# 关联：.claude/plans/quality-loop.md v1.3 T3.4
# 实际工作由 imboyapp/scripts/regen_protobuf.sh（已存在）完成
# 本脚本作为 v1.1 §324 入口 wrapper，把 imboyapp 现成脚本暴露在 imboy/api/codegen/ 下

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
IMBOY_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"
IMBOYAPP_DIR="$(cd "${IMBOY_DIR}/.." && pwd)/imboyapp"
TARGET_SCRIPT="${IMBOYAPP_DIR}/scripts/regen_protobuf.sh"

# 邻居仓存在性检查
if [ ! -d "${IMBOYAPP_DIR}" ]; then
  echo "ERROR: imboyapp 邻居仓未找到: ${IMBOYAPP_DIR}" >&2
  echo "       v1.1 polyrepo 拓扑要求 imboy/ 与 imboyapp/ 同级" >&2
  exit 1
fi

if [ ! -x "${TARGET_SCRIPT}" ]; then
  if [ -f "${TARGET_SCRIPT}" ]; then
    echo "WARN: ${TARGET_SCRIPT} 存在但不可执行，自动 chmod +x" >&2
    chmod +x "${TARGET_SCRIPT}"
  else
    echo "ERROR: 目标脚本缺失: ${TARGET_SCRIPT}" >&2
    exit 1
  fi
fi

# protoc-gen-dart 检查（避免到 imboyapp 脚本里再失败）
export PATH="${PATH}:${HOME}/.pub-cache/bin"
if ! command -v protoc-gen-dart >/dev/null 2>&1; then
  echo "ERROR: protoc-gen-dart 未在 PATH 上" >&2
  echo "       安装：dart pub global activate protoc_plugin" >&2
  echo "       并确保 ~/.pub-cache/bin 在 PATH" >&2
  exit 1
fi

echo "[dart.sh] Delegating to ${TARGET_SCRIPT}..."
"${TARGET_SCRIPT}"

echo "[dart.sh] Done. Generated under imboyapp/lib/service/protocol/"

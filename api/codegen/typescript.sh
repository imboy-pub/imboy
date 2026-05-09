#!/usr/bin/env bash
# imboy/api/codegen/typescript.sh — React (TypeScript) 端 protobuf 代码生成
#
# 关联：.claude/plans/quality-loop.md v1.3 T3.4
# admin 仓无现成 codegen 脚本，由本脚本独立实现（v1.1 §324）

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
IMBOY_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"
ADMIN_DIR="$(cd "${IMBOY_DIR}/.." && pwd)/imboy-admin-frontend"
PROTO_FILE="${IMBOY_DIR}/api/proto/imboy.proto"
OUT_DIR="${ADMIN_DIR}/src/api/_gen/proto"

# 邻居仓存在性检查
if [ ! -d "${ADMIN_DIR}" ]; then
  echo "ERROR: imboy-admin-frontend 邻居仓未找到: ${ADMIN_DIR}" >&2
  echo "       v1.1 polyrepo 拓扑要求 imboy/ 与 imboy-admin-frontend/ 同级" >&2
  exit 1
fi

if [ ! -e "${PROTO_FILE}" ]; then
  echo "ERROR: ${PROTO_FILE} 缺失（应为 symlink → ../../src/imboy.proto，T3.3 落地）" >&2
  exit 1
fi

# protoc 检查
if ! command -v protoc >/dev/null 2>&1; then
  echo "ERROR: protoc 未在 PATH 上" >&2
  echo "       安装：brew install protobuf" >&2
  exit 1
fi

# protoc-gen-ts_proto 检查（找 PATH + admin 仓 node_modules）
if ! command -v protoc-gen-ts_proto >/dev/null 2>&1; then
  TS_PROTO_BIN="${ADMIN_DIR}/node_modules/.bin/protoc-gen-ts_proto"
  if [ -x "${TS_PROTO_BIN}" ]; then
    export PATH="${ADMIN_DIR}/node_modules/.bin:${PATH}"
  else
    echo "ERROR: protoc-gen-ts_proto 未在 PATH 上，且 admin/node_modules 也无此插件" >&2
    echo "       安装方式（任选）：" >&2
    echo "         全局：npm i -g ts-proto    或    bun add -g ts-proto" >&2
    echo "         项目：cd ${ADMIN_DIR} && bun add -d ts-proto" >&2
    exit 1
  fi
fi

mkdir -p "${OUT_DIR}"

echo "[typescript.sh] Generating TypeScript protobuf..."
echo "  proto:  ${PROTO_FILE}"
echo "  output: ${OUT_DIR}"

protoc \
  --proto_path="${IMBOY_DIR}/api/proto" \
  --plugin=protoc-gen-ts_proto="$(command -v protoc-gen-ts_proto)" \
  --ts_proto_out="${OUT_DIR}" \
  --ts_proto_opt=esModuleInterop=true,forceLong=string,outputServices=false,useExactTypes=true,stringEnums=true \
  "${PROTO_FILE}"

echo "[typescript.sh] Done. Generated .ts files in ${OUT_DIR}"
echo "  注：forceLong=string 让 sint64 TSID 在 TS 端为 string（与 admin/CLAUDE.md EntityId 约定一致）"

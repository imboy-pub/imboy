#!/usr/bin/env bash
# imboy/api/codegen/erlang.sh — Erlang 端 protobuf 代码生成入口
#
# 关联：.claude/plans/quality-loop.md v1.3 T3.4
# 实际工作由 imboy/Makefile 的 compile_proto.erl（gpb 内联）完成
# 本脚本仅作为 v1.1 §324 codegen 入口的 wrapper，让三端使用方式一致

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
IMBOY_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"

if [ ! -f "${IMBOY_DIR}/Makefile" ]; then
  echo "ERROR: imboy/Makefile not found at ${IMBOY_DIR}" >&2
  exit 1
fi

if [ ! -f "${IMBOY_DIR}/src/imboy.proto" ]; then
  echo "ERROR: src/imboy.proto missing — proto 真源缺失" >&2
  exit 1
fi

echo "[erlang.sh] Delegating to erlang.mk compile_proto.erl via 'make compile'..."
cd "${IMBOY_DIR}" && make compile

echo "[erlang.sh] Done. Generated:"
echo "  - imboy/src/imboy_pb.erl"
echo "  - imboy/include/imboy_pb.hrl"

#!/usr/bin/env bash
# IMBOY_HIRD_AUTH_REPLAY_POC_V1 — 带 marker DB env 的套件运行入口。
#
# C4 CLOSEOUT（2026-10-13）：连接配置改为**显式 PoC 测试连接**——本机
# 非生产实例（trust 无密码），全部值由本脚本显式声明（env 可覆盖）。
# 自本轮起不再解析日常配置文件 sys.local.config：emit_pg_env.py 仅保留
# 作为上一轮行为的证据，不再被本脚本调用。
# 用法: run_pg_suite.sh <suite...>
set -euo pipefail

WT="$(cd "$(dirname "$0")/../.." && pwd)"

export HIRDPOC_PG_HOST="${HIRDPOC_PG_HOST:-127.0.0.1}"
export HIRDPOC_PG_PORT="${HIRDPOC_PG_PORT:-5432}"
export HIRDPOC_PG_USER="${HIRDPOC_PG_USER:-leeyi}"
export HIRDPOC_PG_DB="${HIRDPOC_PG_DB:-postgres}"
export HIRDPOC_PG_MAINT_DB="${HIRDPOC_PG_MAINT_DB:-postgres}"
export HIRDPOC_PG_PASSWORD="${HIRDPOC_PG_PASSWORD:-}"
# 不再从日常配置注入 -config；marker_db 的 pg_conf Defaults 由套件
# ensure_env 显式 set_env 提供（见 P1 install 部分）。
unset HIRDPOC_EUNIT_CONFIG || true

exec "$WT/scripts/hird_poc_v1/run_suites.sh" "$@"

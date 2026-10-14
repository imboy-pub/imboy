#!/usr/bin/env bash
# IMBOY_HIRD_AUTH_REPLAY_POC_V1 — 任务专用测试 VM（P0）。
# 用法: run_suites.sh <suite1> [suite2 ...]   （suite = 测试模块名）
# 环境：
#   IMBOY_HIRD_BIN       本轮重建编译器绝对路径（必填校验）
#   IMBOY_HIRD_FORCE_REBUILD=1（默认强制）
#   IMBOY_HIRD_OUT_ROOT  相对 worktree 的输出根（默认 _build/hird_poc_v1/spike）
# 日志：_build/hird_poc_v1/logs/<suite>.<ts>.log（退出码随行打印）
set -uo pipefail

WT="$(cd "$(dirname "$0")/../.." && pwd)"
MAIN_TREE="/Users/leeyi/project/imboy.pub/imboy"
VM="$WT/_build/hird_poc_v1/vm"
LOGDIR="$WT/_build/hird_poc_v1/logs"
mkdir -p "$LOGDIR"

HIRD_BIN="${IMBOY_HIRD_BIN:-$WT/_build/hird_poc_v1/toolchain/cargo-target/release/hird}"
[ -x "$HIRD_BIN" ] || { echo "FATAL: hird compiler not executable: $HIRD_BIN"; exit 10; }

export IMBOY_HIRD_BIN="$HIRD_BIN"
export IMBOY_HIRD_FORCE_REBUILD="${IMBOY_HIRD_FORCE_REBUILD:-1}"

cd "$WT"

PA_ARGS=()
for d in "$MAIN_TREE"/deps/*/ebin; do PA_ARGS+=(-pa "$d"); done

TS="$(date -u +%Y%m%dT%H%M%SZ)"
OVERALL=0
# 可选 -config（marker_db 的 pg_conf 默认值来源；env 仍全量覆盖连接目标）
CONFIG_ARG=()
if [ -n "${HIRDPOC_EUNIT_CONFIG:-}" ] && [ -f "${HIRDPOC_EUNIT_CONFIG}" ]; then
  CONFIG_ARG=(-config "${HIRDPOC_EUNIT_CONFIG%.config}")
fi
for suite in "$@"; do
  LOG="$LOGDIR/${suite}.${TS}.log"
  echo "=== RUN $suite (compiler $(basename "$HIRD_BIN")) -> $LOG"
  erl -noshell -boot no_dot_erlang +P 32768 \
    ${CONFIG_ARG[@]+"${CONFIG_ARG[@]}"} \
    -pa "$VM/ebin" -pa "$VM/test_ebin" "${PA_ARGS[@]}" \
    -eval "
      R = (catch eunit:test([$suite], [verbose])),
      io:format(\"~n=== SUITE_RESULT: ~p~n\", [R]),
      case R of ok -> halt(0); _ -> halt(1) end." \
    > "$LOG" 2>&1
  rc=$?
  echo "--- $suite exit=$rc"
  tail -4 "$LOG"
  [ $rc -ne 0 ] && OVERALL=1
done
exit $OVERALL

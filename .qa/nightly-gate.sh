#!/usr/bin/env bash
# =============================================================================
# nightly-gate.sh — nightly.yml 慢门 job 的统一包装（P4-Q1 / Golden Gates §6.1）
#
# 职责（每个慢门 job 调一行，替代在 workflow 里内联重复 retry/classify 逻辑）：
#   1. 执行 gate 命令，输出 tee 落盘（含 ANSI 原样保存，classify 自行清洗）
#   2. 失败 -> step 级 retry 一次（FLAKY 判定的唯一实现形态：
#      re-run 整 job 无法自动判 flaky，必须 step 内重试）
#      retry 过 -> 首跑失败全计 FLAKY（不阻断）；retry 仍败 -> 用 retry 日志分类
#   3. 调 .qa/classify.sh 出五分类（NEW/KNOWN/FIXED/FLAKY/INFRA）
#   4. 机器行 + NEW 明细写入 $GITHUB_OUTPUT（qa-issue job 汇总用；本地运行时忽略）
#
# 退出码（= job 红/绿）：
#   NEW_FAILURE / PARSE_ERROR -> 非零（阻断）；KNOWN 存量 / FLAKY / RATCHET / PASS -> 0
#   （KNOWN 不阻断是计划 §6.1 钉死语义；NEW 阻断即「零 NEW_FAILURE 棘轮」）
#
# 用法: nightly-gate.sh GATE LOGFILE -- <gate 命令...>
#   例: nightly-gate.sh full-eunit eunit-full.log -- make eunit EUNIT_ERL_OPTS=...
# =============================================================================
set -uo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CLASSIFY="${REPO_ROOT}/.qa/classify.sh"

if [ $# -lt 3 ] || [ "$3" != "--" ]; then
  echo "用法: $0 GATE LOGFILE -- <gate 命令...>" >&2
  exit 2
fi
GATE="$1"
LOGFILE="$2"
shift 3

run_gate() {  # run_gate <logfile> <cmd...>；退出码 = gate 命令退出码（pipefail 保护）
  local logfile="$1"; shift
  local rc=0
  "$@" 2>&1 | tee "$logfile" || rc=${PIPESTATUS[0]}
  return "$rc"
}

emit_outputs() {  # emit_outputs <classify 机器行> <NEW 明细块>；本地无 GITHUB_OUTPUT 时静默
  local summary="$1" new_items="$2"
  if [ -n "${GITHUB_OUTPUT:-}" ]; then
    {
      printf 'summary=%s\n' "$summary"
      printf 'new_items<<QA_EOF\n%s\nQA_EOF\n' "$new_items"
    } >> "$GITHUB_OUTPUT"
  fi
}

capture_classify() {  # capture_classify <logfile> [额外 classify 参数...]；透传 classify 退出码
  local log="$1"; shift
  local out rc=0
  out="$("$CLASSIFY" "$GATE" "$log" --gate-rc 0 "$@" 2>&1)" || rc=$?
  printf '%s\n' "$out"
  local summary new_items
  summary="$(printf '%s\n' "$out" | grep '^QA_CLASSIFY ' | head -1)"
  new_items="$(printf '%s\n' "$out" | sed -n '/^-- NEW/,/^-- /p' | sed '1d;/^-- /,$d')"
  emit_outputs "$summary" "$new_items"
  return "$rc"
}

# --- 首跑 ----------------------------------------------------------------------
run_gate "$LOGFILE" "$@"
rc1=$?

if [ "$rc1" -eq 0 ]; then
  echo "== nightly-gate: gate 一次通过 (rc=0)"
  capture_classify "$LOGFILE"
  exit $?
fi

echo "== nightly-gate: 首跑失败 (rc=${rc1})，step 级 retry 一次（FLAKY 判定）..."
RETRY_LOG="${LOGFILE}.retry"
run_gate "$RETRY_LOG" "$@"
rc2=$?

if [ "$rc2" -eq 0 ]; then
  echo "== nightly-gate: retry 通过 -> 首跑失败全部计 FLAKY（不阻断，单独计数）"
  capture_classify "$LOGFILE" --flaky-passed
  exit $?
fi

echo "== nightly-gate: retry 仍失败 (rc=${rc2}) -> 用 retry 日志做 NEW/KNOWN/FIXED 分类"
capture_classify "$RETRY_LOG" --gate-rc "$rc2"
exit $?

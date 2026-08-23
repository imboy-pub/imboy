#!/usr/bin/env bash
# =============================================================================
# classify.sh — nightly gate 失败五分类（P4-Q1 / Golden Gates 计划 §6.1，语义钉死）
#
#   NEW_FAILURE   不在 known 清单 -> 阻断（exit 1，nightly 红 + P4-Q4 建 issue）
#   KNOWN_FAILURE known 清单内    -> 只计数
#   FIXED         上期 known 本期消失 -> 棘轮收紧建议（人工 review 后移除，脚本不自动改 yaml）
#   FLAKY         重跑即过        -> 单独计数不阻断（由 CI step 级 retry + --flaky-passed 表达）
#   INFRA_FAILURE CI/环境问题     -> 不计入产品质量（保守启发式，仅无 gate 失败标识时才判）
#
# 用法:
#   classify.sh GATE LOGFILE [--known FILE] [--flaky-passed] [--gh-output FILE]
#   classify.sh GATE LOGFILE --emit-known        # 首跑盘点：输出 YAML 片段供人工回填
#
#   GATE   ∈ full-eunit | dialyze | eqwalizer-lib | gradualizer | elvis
#   LOGFILE 该 gate 的完整输出（CI 里 tee 落盘；elvis 的 ANSI 色码会先被清洗）
#   --known        known-failures.yaml 路径（默认 <仓根>/.qa/known-failures.yaml）
#   --flaky-passed 标记「本 LOGFILE 中的失败经 step 级 retry 已通过」→ 全计 FLAKY 不阻断
#   --gh-output    把机器行追加到该文件（CI 传 $GITHUB_OUTPUT，供 qa-issue job 汇总）
#
# 输出（stdout）:
#   机器行  QA_CLASSIFY gate=<g> NEW=<n> KNOWN=<k> FIXED=<f> FLAKY=<fl> INFRA=<i> VERDICT=<v>
#   明细块  NEW 逐条 / FIXED 逐条（可从 known 移除建议）
# 退出码: 0=PASS/RATCHET/FLAKY/INFRA  1=NEW_FAILURE  2=用法/输入错误  3=解析漂移（人工介入）
# =============================================================================
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DEFAULT_KNOWN="${REPO_ROOT}/.qa/known-failures.yaml"

GATE=""
LOGFILE=""
KNOWN_FILE="${DEFAULT_KNOWN}"
FLAKY_PASSED=0
GH_OUTPUT=""
EMIT_KNOWN=0
GATE_RC=0

usage() {
  sed -n '2,30p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
  exit 2
}

while [ $# -gt 0 ]; do
  case "$1" in
    full-eunit|dialyze|eqwalizer-lib|gradualizer|elvis) GATE="$1" ;;
    --known)        KNOWN_FILE="${2:?}"; shift ;;
    --flaky-passed) FLAKY_PASSED=1 ;;
    --gh-output)    GH_OUTPUT="${2:?}"; shift ;;
    --gate-rc)      GATE_RC="${2:?}"; shift ;;
    --emit-known)   EMIT_KNOWN=1 ;;
    -h|--help)      usage ;;
    *)  if [ -z "$LOGFILE" ] && [ -f "$1" ]; then LOGFILE="$1"; else
          echo "ERROR: 未知参数或不存在的日志文件: $1" >&2; usage
        fi ;;
  esac
  shift
done
[ -n "$GATE" ] && [ -n "$LOGFILE" ] || usage
[ -f "$LOGFILE" ] || { echo "ERROR: 日志不存在: $LOGFILE" >&2; exit 2; }
[ -f "$KNOWN_FILE" ] || { echo "ERROR: known 清单不存在: $KNOWN_FILE" >&2; exit 2; }

# 去除 ANSI 色码（elvis 默认彩色输出；对其他 gate 无副作用）。
clean_log() {
  local esc
  esc=$'\x1b'
  sed -E "s/${esc}\\[[0-9;]*m//g" "$1"
}

# 从 gate 输出提取当前失败标识（每 gate 一个稳定可解析粒度），输出排序去重清单。
extract_failures() {
  local gate="$1" log="$2"
  case "$gate" in
    # eunit 标准: "mod: test...*failed*" / "mod:42: t...*timed out*" / "*aborted*"
    full-eunit)
      grep -E '\.\.\.\*(failed|timed out|aborted)\*' "$log" \
        | sed -E 's/^([a-zA-Z0-9_@]+):.*/\1/' || true ;;
    # dialyzer 原生: "src/foo.erl:123: Warning: ..."
    dialyze)
      grep -oE '^[^ :]+\.erl:[0-9]+: Warning' "$log" | cut -d: -f1 || true ;;
    # make eqwalize-layer: "❌ <mod>"（crash 形如 "❌ mod (crash, ...)"，$2 同样是 mod）
    eqwalizer-lib)
      grep '❌' "$log" | awk '{print $2}' | grep -v '^(' || true ;;
    # make gradualize-audit: "❌ src/<dir>/<mod>.erl" -> 纯模块名（对齐 baseline 口径）
    gradualizer)
      grep '❌' "$log" | awk '{print $2}' | sed -e 's|^src/||' -e 's|\.erl$||' \
        | awk -F/ '{print $NF}' || true ;;
    # elvis: "# <path> [FAIL]"
    elvis)
      grep -E '^# .* \[FAIL\]' "$log" | awk '{print $2}' || true ;;
  esac | sort -u
}

# 汇总行的失败总数（解析漂移防御用：汇总>0 而逐条提取=0 → PARSE_ERROR，宁可人工介入不静默漏报）。
# 各分支必须 || true 兜底：grep 无匹配时 pipefail 会令 $(...) 非零退出，set -e 误杀主流程。
summary_failed_count() {
  local gate="$1" log="$2"
  case "$gate" in
    full-eunit)     { grep -oE 'Failed: [0-9]+|Aborted: [1-9][0-9]*' "$log" \
                      | grep -oE '[0-9]+' | awk '{s+=$1} END{print s+0}'; } || true ;;
    dialyze)        grep -cE '\.erl:[0-9]+: Warning' "$log" || true ;;
    eqwalizer-lib)  { grep -oE 'failing=[0-9]+' "$log" | grep -oE '[0-9]+' \
                      | awk '{s+=$1} END{print s+0}'; } || true ;;
    gradualizer)    { grep -oE 'failing=[0-9]+' "$log" | grep -oE '[0-9]+' \
                      | awk '{s+=$1} END{print s+0}'; } || true ;;
    elvis)          grep -cE '^# .* \[FAIL\]' "$log" || true ;;
  esac
}

# 从 known-failures.yaml 提取 gates.<gate>.known_failures 清单（awk 状态机，零 yq 依赖；
# 依赖本仓 yaml 的固定缩进：gate 两空格、列表项六空格 "- "；空行/注释行不终结 gates 块）。
load_known() {
  local gate="$1" yaml="$2"
  awk -v want="  ${gate}:" '
    /^gates:/     { in_gates=1; active=0; next }
    in_gates && /^[^[:space:]#]/ { in_gates=0; active=0; next }  # 顶格非注释 = gates 块结束
    in_gates && /^  [A-Za-z0-9_-]+:$/ { active = ($0 == want) ? 1 : 0; next }
    active && /^      - / { sub(/^      - /, ""); print }
  ' "$yaml" | sort -u
}

# INFRA 启发式（保守：命中行数；仅当 gate 失败标识为空时才据此判 INFRA_FAILURE）。
infra_hit_count() {
  grep -cE 'Could not resolve host|Connection reset by peer|No space left on device|has received a shutdown signal|timed out on the runner|Failed to setup toolchain|error: failed to fetch|fatal: unable to access' "$1" || true
}

# ----------------------------------------------------------------------------
# 主流程
# ----------------------------------------------------------------------------
CLEAN_LOG="$(mktemp "${TMPDIR:-/tmp}/qa_classify.XXXXXX")"
trap 'rm -f "$CLEAN_LOG"' EXIT
clean_log "$LOGFILE" > "$CLEAN_LOG"

if [ "$EMIT_KNOWN" -eq 1 ]; then
  echo "  ${GATE}:"
  echo "    known_failures:"
  if extract_failures "$GATE" "$CLEAN_LOG" | grep -q .; then
    extract_failures "$GATE" "$CLEAN_LOG" | awk '{printf "      - %s\n", $1}'
  else
    echo "      []  # 本次运行无失败标识（保留空列表请手工改为 known_failures: [] 单行）"
  fi
  exit 0
fi

current_failures="$(extract_failures "$GATE" "$CLEAN_LOG" || true)"
known_list="$(load_known "$GATE" "$KNOWN_FILE")"

current_n=$(printf '%s\n' "$current_failures" | grep -c . || true)
summary_n="$(summary_failed_count "$GATE" "$CLEAN_LOG")"

# 解析漂移防御：汇总行证明有失败但逐条提取为空 -> 格式变了，退出 3 人工介入。
if [ "${summary_n:-0}" -gt 0 ] && [ "$current_n" -eq 0 ]; then
  echo "QA_CLASSIFY gate=${GATE} NEW=0 KNOWN=0 FIXED=0 FLAKY=0 INFRA=0 VERDICT=PARSE_ERROR" \
    | tee -a "${GH_OUTPUT:-/dev/null}"
  echo "ERROR: 汇总行显示 ${summary_n} 个失败但逐条提取为 0 —— gate 输出格式漂移，" >&2
  echo "       请人工核对 ${LOGFILE} 并更新 .qa/classify.sh 解析器。" >&2
  exit 3
fi

# 假绿防御：gate 命令退出非零（--gate-rc 传入），但日志既无失败标识也无 infra 迹象
# -> 无法归因（解析器缺口/未知崩溃），退出 3 人工介入，绝不让它混成 PASS。
if [ "$GATE_RC" -ne 0 ] && [ "$current_n" -eq 0 ] && [ "$(infra_hit_count "$CLEAN_LOG")" -eq 0 ]; then
  echo "QA_CLASSIFY gate=${GATE} NEW=0 KNOWN=0 FIXED=0 FLAKY=0 INFRA=0 VERDICT=PARSE_ERROR" \
    | tee -a "${GH_OUTPUT:-/dev/null}"
  echo "ERROR: gate 退出码 ${GATE_RC} 非零，但日志无失败标识也无环境异常迹象 —— 无法归因，" >&2
  echo "       请人工核对 ${LOGFILE}（可能是 classify.sh 解析缺口）。" >&2
  exit 3
fi

# FLAKY：CI step 级 retry 已通过 -> 本 log 的失败全部计 FLAKY，不阻断。
if [ "$FLAKY_PASSED" -eq 1 ] && [ "$current_n" -gt 0 ]; then
  echo "QA_CLASSIFY gate=${GATE} NEW=0 KNOWN=0 FIXED=0 FLAKY=${current_n} INFRA=0 VERDICT=FLAKY" \
    | tee -a "${GH_OUTPUT:-/dev/null}"
  echo "-- FLAKY（retry 即过，不阻断；连续 3 次升级 issue 属 P4-Q4 统计口径）:"
  printf '%s\n' "$current_failures" | sed 's/^/   /'
  exit 0
fi

# comm 前的输入：空集合必须产出空流（printf '%s\n' "" 会引入空行污染三路比较）
as_lines() { [ -n "$1" ] && printf '%s\n' "$1" || :; }

comm_new="$(comm -13 <(as_lines "$known_list") <(as_lines "$current_failures") | grep . || true)"
comm_known="$(comm -12 <(as_lines "$known_list") <(as_lines "$current_failures"))"
comm_fixed="$(comm -23 <(as_lines "$known_list") <(as_lines "$current_failures") | grep . || true)"

new_n=$(printf '%s\n' "$comm_new" | grep -c . || true)
known_n=$(printf '%s\n' "$comm_known" | grep -c . || true)
fixed_n=$(printf '%s\n' "$comm_fixed" | grep -c . || true)
infra_n=0
verdict="PASS"

if [ "$new_n" -gt 0 ]; then
  verdict="NEW_FAILURE"
elif [ "$current_n" -eq 0 ] && [ "$(infra_hit_count "$CLEAN_LOG")" -gt 0 ]; then
  # gate 未产出有效结果 + 环境异常迹象 -> INFRA（此场景 FIXED 不可信，置 0）
  verdict="INFRA_FAILURE"
  fixed_n=0
  infra_n="$(infra_hit_count "$CLEAN_LOG")"
elif [ "$fixed_n" -gt 0 ]; then
  verdict="RATCHET"
fi

echo "QA_CLASSIFY gate=${GATE} NEW=${new_n} KNOWN=${known_n} FIXED=${fixed_n} FLAKY=0 INFRA=${infra_n} VERDICT=${verdict}" \
  | tee -a "${GH_OUTPUT:-/dev/null}"

if [ "$new_n" -gt 0 ]; then
  echo "-- NEW（不在 known-failures.yaml，阻断 + P4-Q4 建 issue）:"
  printf '%s\n' "$comm_new" | sed 's/^/   /'
fi
if [ "$fixed_n" -gt 0 ]; then
  echo "-- FIXED（上期 known 本期消失，建议人工 review 后从 known 清单移除，棘轮收紧）:"
  printf '%s\n' "$comm_fixed" | sed 's/^/   /'
fi
if [ "$infra_n" -gt 0 ]; then
  echo "-- INFRA（环境类失败迹象 ${infra_n} 行，不计产品质量；建议人工复核归因）"
elif [ "$new_n" -gt 0 ]; then
  extra_infra="$(infra_hit_count "$CLEAN_LOG")"
  [ "$extra_infra" -gt 0 ] && echo "-- 注意: log 另有 ${extra_infra} 行环境异常迹象（与产品失败并存，仍按产品失败分类）"
fi

[ "$verdict" = "NEW_FAILURE" ] && exit 1
exit 0

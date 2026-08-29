#!/usr/bin/env bash
# =============================================================================
# Dialyzer 递减基线门（CI-00）
#
# 语义：dialyzer.baseline 记录当前全部告警指纹（模块文件 + 归一化告警正文，
#       不含行号列号——代码挪行不误伤）；本轮输出中出现基线外的新指纹即红
#       （exit 1）。存量指纹减少不红（提示可人工收紧基线）。
#
# 用法：
#   bash scripts/check_dialyzer_baseline.sh <dialyzer输出log>
#       解析给定 log（make dialyze 的完整输出，含 PLT 段也能正确跳过），
#       对照 dialyzer.baseline，新增 > 0 则 exit 1。
#   bash scripts/check_dialyzer_baseline.sh --update <log>
#       以给定 log 重新生成基线（人工 review 后使用，收紧/初次盘点）。
#
# 指纹规则：
#   - 告警起始行 `file.erl:line[:col]: 正文` → 指纹 = basename(file) + "|" +
#     归一化正文（起始行正文 + 后续续行 strip/空白折叠后拼接）。
#   - `Unknown functions/types:` 段条目 `  m:f/a (path:line:col)` → 指纹 =
#     basename(path) + "|Unknown function/type m:f/a"（去行号）。
#   - 仅解析分析段：取最后一个 "Proceeding with analysis..."（或 PLT done）
#     之后、最后一个 " done in " 之前的内容，PLT 构建段的噪音不入基线。
# =============================================================================
set -euo pipefail

BASELINE_FILE="${DIALYZER_BASELINE:-dialyzer.baseline}"

# ---- 解析 dialyzer 输出 → 指纹清单（stdout，每行一条） ----
parse_dialyzer_log() {
    local log_file="$1"
    awk '
    BEGIN { in_analysis = 0; done_count = 0; saw_plt = 0; }
    # 记录 PLT 构建段，分析段从 "Proceeding with analysis..." 开始
    /Proceeding with analysis\.\.\./ { in_analysis = 1; next }
    /Creating PLT/ { saw_plt = 1; next }
    # 段终止行：done in / done (…) —— 标记当前段结束
    /^[[:space:]]*done in / || /^done \(/ {
        if (in_analysis) { in_analysis = 0; exit }   # 分析段结束即停
        if (saw_plt) { plt_done = 1 }                # PLT 段结束
        next
    }
    !in_analysis { next }
    { print }
    ' "$log_file" | awk '
    function flush_warn() {
        if (warn_file != "") {
            gsub(/[[:space:]]+/, " ", warn_desc)
            sub(/^ /, "", warn_desc); sub(/ $/, "", warn_desc)
            print warn_file "|" warn_desc
            warn_file = ""
        }
    }
    function norm_pos(p,   n, a, cnt) {
        # "../src/X.erl:133:5" -> basename X.erl（去行号列号）
        n = p; sub(/:[0-9]+(:[0-9]+)?$/, "", n)
        cnt = split(n, a, "/")
        return a[cnt]
    }
    BEGIN { warn_file = ""; in_unknown = 0; }
    # 告警起始行：file.erl:line[:col]: 正文（顶格）
    /^[A-Za-z0-9_.\/-]+\.erl:[0-9]+(:[0-9]+)?:[[:space:]]*/ {
        flush_warn(); in_unknown = 0
        line = $0
        # 拆 file 与正文
        match(line, /^[A-Za-z0-9_.\/-]+\.erl:[0-9]+(:[0-9]+)?:/)
        pos = substr(line, RSTART, RLENGTH)
        desc = substr(line, RSTART + RLENGTH)
        sub(/^[[:space:]]*/, "", desc)
        sub(/^Warning:[[:space:]]*/, "", desc)
        file = pos; sub(/:[0-9]+(:[0-9]+)?:$/, "", file)
        cnt = split(file, parts, "/"); file = parts[cnt]
        warn_file = file; warn_desc = desc
        next
    }
    # Unknown functions/types 段标题
    /^Unknown functions:/ { flush_warn(); in_unknown = "fn"; next }
    /^Unknown types:/     { flush_warn(); in_unknown = "ty"; next }
    # 段条目：  m:f/a (path:line:col)  或裸  m:f/a
    in_unknown && /^[[:space:]]+[^[:space:]]/ {
        line = $0
        # 去掉位置括号（若带）
        if (match(line, /\([^()]*:[0-9]+(:[0-9]+)?\)/)) {
            pos = substr(line, RSTART + 1, RLENGTH - 2)
            line = substr(line, 1, RSTART - 1) substr(line, RSTART + RLENGTH)
            gsub(/^[[:space:]]+|[[:space:]]+$/, "", line)
            print norm_pos(pos) "|" (in_unknown == "fn" ? "Unknown function " : "Unknown type ") line
        } else {
            gsub(/^[[:space:]]+|[[:space:]]+$/, "", line)
            if (line != "") print "global|" (in_unknown == "fn" ? "Unknown function " : "Unknown type ") line
        }
        next
    }
    in_unknown != 0 { in_unknown = 0 }   # 段结束（非缩进行）
    # 告警续行（缩进/无起始特征）→ 追加到当前告警正文
    warn_file != "" {
        line = $0
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", line)
        if (line != "") warn_desc = warn_desc " " line
        next
    }
    END { flush_warn() }
    '
}

main() {
    local mode="check"
    local log_file=""
    for arg in "$@"; do
        case "$arg" in
            --update) mode="update" ;;
            -h|--help) usage; exit 0 ;;
            *) log_file="$arg" ;;
        esac
    done
    if [[ -z "$log_file" ]]; then
        usage; exit 2
    fi
    if [[ ! -f "$log_file" ]]; then
        echo "ERROR: dialyzer log 不存在: $log_file" >&2; exit 2
    fi

    local current
    current="$(mktemp)"
    parse_dialyzer_log "$log_file" | LC_ALL=C sort -u > "$current"
    local n_cur
    n_cur="$(wc -l < "$current" | tr -d ' ')"

    if [[ "$mode" == "update" ]]; then
        {
            echo "# Dialyzer 递减基线（CI-00 ratchet）"
            echo "# 每行一条指纹：<源文件basename>|<归一化告警正文>（无行号列号，代码挪行不误伤）"
            echo "# 生成：bash scripts/check_dialyzer_baseline.sh --update <dialyze输出log>"
            echo "# 门槛：新增（基线外）任一指纹即红；指纹减少后可人工收紧本文件。"
            cat "$current"
        } > "$BASELINE_FILE"
        echo "基线已更新: ${BASELINE_FILE}（${n_cur} 条指纹）"
        rm -f "$current"
        return 0
    fi

    if [[ ! -f "$BASELINE_FILE" ]]; then
        echo "ERROR: 基线文件不存在: $BASELINE_FILE（用 --update 生成）" >&2
        rm -f "$current"; exit 2
    fi
    # 过滤基线中的注释行
    grep -v '^#' "$BASELINE_FILE" | LC_ALL=C sort -u > "${BASELINE_FILE}.current_cmp" 2>/dev/null || true
    local baseline_cmp="${BASELINE_FILE}.current_cmp"

    local new_cnt removed_cnt
    new_cnt="$(comm -23 "$current" "$baseline_cmp" | wc -l | tr -d ' ')"
    removed_cnt="$(comm -13 "$current" "$baseline_cmp" | wc -l | tr -d ' ')"

    local rc=0
    if (( new_cnt > 0 )); then
        echo "DIALYZER BASELINE GATE: RED —— 新增 $new_cnt 条告警（基线外，必须修复或经 review 后收紧基线）:"
        comm -23 "$current" "$baseline_cmp" | head -50 | sed 's/^/  NEW /'
        rc=1
    fi
    if (( removed_cnt > 0 )); then
        echo "DIALYZER BASELINE: $removed_cnt 条指纹已消失（可收紧基线，不影响本轮红绿）:"
        comm -13 "$current" "$baseline_cmp" | head -20 | sed 's/^/  GONE /'
    fi
    if (( rc == 0 )); then
        echo "DIALYZER BASELINE GATE: GREEN —— 告警 $n_cur 条，全部在基线内，无新增。"
    fi
    rm -f "$current" "${BASELINE_FILE}.current_cmp"
    exit "$rc"
}

usage() {
    sed -n '2,25p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
}

main "$@"

#!/usr/bin/env bash
# ============================================================
# TSID V10 节点登记表校验（C-1 交付物）
# ------------------------------------------------------------
# 校验 deploy/tsid-node-registry.tsv：
#   1. 每行 dc_id / node_id 为非负整数且在位布局允许范围内；
#   2. 全集群 (dc_id, node_id) 组合唯一（V10 唯一性硬前提）；
#   3. 当前节点 (dc_id, node_id) 恰在册一次。
#
# 用法：
#   bash deploy/check_tsid_node_registry.sh <registry.tsv> <dc_id> <node_id> [dc_bits]
#   bash deploy/check_tsid_node_registry.sh --self-test
#
# 退出码：0 = 通过；1 = 有错误。
# 本脚本不修改任何文件，只读校验。
# ============================================================
set -uo pipefail

ERRORS=0
err()  { echo "  ✗ $*" >&2; ERRORS=$((ERRORS + 1)); }
ok()   { echo "  ✓ $*"; }
info() { echo "  · $*"; }

# check_registry <registry.tsv> <cur_dc> <cur_node> <dc_bits>
check_registry() {
    local file="$1" cur_dc="$2" cur_node="$3" dc_bits="${4:-3}"
    ERRORS=0

    if [[ ! -f "$file" ]]; then
        err "登记表不存在：$file"
        return 1
    fi
    if ! [[ "$dc_bits" =~ ^[0-9]+$ ]] || (( dc_bits < 0 || dc_bits > 10 )); then
        err "dc_bits 必须在 [0,10]（当前: ${dc_bits}）"
        return 1
    fi
    local max_dc=$(( (1 << dc_bits) - 1 ))
    local max_node=$(( (1 << (10 - dc_bits)) - 1 ))

    # 解析有效行（跳过 # 注释与空行），字段以 TAB 分隔
    local combos="" hosts="" lineno=0 rows=0
    while IFS=$'\t' read -r host dc node extra || [[ -n "${host:-}" ]]; do
        lineno=$((lineno + 1))
        # 去掉行尾 CR 与首尾空白
        host="$(printf '%s' "${host:-}" | tr -d '\r' | sed 's/^[[:space:]]*//; s/[[:space:]]*$//')"
        dc="$(printf '%s' "${dc:-}" | tr -d '\r' | sed 's/^[[:space:]]*//; s/[[:space:]]*$//')"
        node="$(printf '%s' "${node:-}" | tr -d '\r' | sed 's/^[[:space:]]*//; s/[[:space:]]*$//')"
        [[ -z "$host" || "$host" == \#* ]] && continue
        rows=$((rows + 1))
        if ! [[ "$dc" =~ ^[0-9]+$ ]]; then
            err "行 ${lineno}（${host}）dc_id 非负整数：'${dc}'"
            continue
        fi
        if ! [[ "$node" =~ ^[0-9]+$ ]]; then
            err "行 ${lineno}（${host}）node_id 非负整数：'${node}'"
            continue
        fi
        if (( dc < 0 || dc > max_dc )); then
            err "行 ${lineno}（${host}）dc_id 越界：允许 [0,${max_dc}]（当前 ${dc}，dc_bits=${dc_bits}）"
        fi
        if (( node < 0 || node > max_node )); then
            err "行 ${lineno}（${host}）node_id 越界：允许 [0,${max_node}]（当前 ${node}，dc_bits=${dc_bits}）"
        fi
        combos="${combos}${dc}:${node}"$'\n'
        hosts="${hosts}${host}"$'\n'
    done < "$file"

    if (( rows == 0 )); then
        err "登记表无有效数据行：$file"
        return 1
    fi

    # 组合唯一性
    local dup_combo
    dup_combo="$(printf '%s' "$combos" | sed '/^$/d' | sort | uniq -d)"
    if [[ -n "$dup_combo" ]]; then
        while read -r c; do
            [[ -z "$c" ]] && continue
            err "(dc_id,node_id) 重复：${c} —— 同 CombinedNode 双写 = ID 碰撞，必须修正"
        done <<< "$dup_combo"
    fi

    # host 唯一性
    local dup_host
    dup_host="$(printf '%s' "$hosts" | sed '/^$/d' | sort | uniq -d)"
    if [[ -n "$dup_host" ]]; then
        while read -r h; do
            [[ -z "$h" ]] && continue
            err "host 重复：${h}"
        done <<< "$dup_host"
    fi

    # 当前节点在册恰一次
    local cnt
    cnt="$(printf '%s' "$combos" | sed '/^$/d' | grep -cx "${cur_dc}:${cur_node}" || true)"
    if (( cnt == 0 )); then
        err "当前节点 (dc_id=${cur_dc}, node_id=${cur_node}) 不在登记表中 —— 未登记的实例不得发号"
    elif (( cnt > 1 )); then
        err "当前节点 (dc_id=${cur_dc}, node_id=${cur_node}) 在表中出现 ${cnt} 次"
    else
        ok "当前节点 (dc_id=${cur_dc}, node_id=${cur_node}) 在册唯一"
    fi

    if (( ERRORS == 0 )); then
        ok "登记表通过：${rows} 个节点，(dc_id,node_id) 全唯一，范围合法"
    fi
    return $(( ERRORS > 0 ? 1 : 0 ))
}

# ------------------------------------------------------------
# --self-test：负向 fixtures 必须被抓到
# ------------------------------------------------------------
run_self_test() {
    echo "▶ TSID 节点登记表校验自测（负向 fixtures 必须全部被抓到）"
    local dir passed=0 failed=0
    dir="$(mktemp -d)"

    _t() {
        local name="$1" expect_err="$2" file="$3" dc="$4" node="$5" bits="${6:-3}"
        local rc=0
        check_registry "$file" "$dc" "$node" "$bits" >/dev/null 2>&1 || rc=1
        if { [[ "$expect_err" == "err" ]] && (( rc != 0 )); } ||
           { [[ "$expect_err" == "ok" ]] && (( rc == 0 )); }; then
            ok "自测 ${name}"; passed=$((passed + 1))
        else
            err "自测 ${name} 失败（期望 ${expect_err}，实际 rc=${rc}）"; failed=$((failed + 1))
        fi
    }

    printf 'a\t1\t1\nb\t1\t2\n' > "$dir/good.tsv"
    _t "合法表放行" ok "$dir/good.tsv" 1 1

    printf 'a\t1\t1\nb\t1\t1\n' > "$dir/dupcombo.tsv"
    _t "组合重复被拒" err "$dir/dupcombo.tsv" 1 1

    printf 'a\t1\t1\nb\t1\t200\n' > "$dir/range.tsv"
    _t "node_id 越界被拒" err "$dir/range.tsv" 1 1

    printf 'a\t1\t1\nb\t9\t2\n' > "$dir/dc.tsv"
    _t "dc_id 越界被拒" err "$dir/dc.tsv" 1 1

    printf 'a\t1\t1\nb\tx\t2\n' > "$dir/nonint.tsv"
    _t "非整数被拒" err "$dir/nonint.tsv" 1 1

    printf 'a\t1\t1\nb\t1\t2\n' > "$dir/missing.tsv"
    _t "当前节点不在册被拒" err "$dir/missing.tsv" 1 3

    printf '# comment\n\na\t1\t1\n' > "$dir/comments.tsv"
    _t "注释/空行被忽略" ok "$dir/comments.tsv" 1 1

    printf 'a\t1\t1\nb\ta\t1\n' > "$dir/duphost.tsv"
    _t "host 重复被拒" err "$dir/duphost.tsv" 1 1

    echo "  ── 自测: ${passed} passed / ${failed} failed"
    rm -rf "$dir"
    [[ $failed -eq 0 ]]
}

main() {
    if [[ "${1:-}" == "--self-test" ]]; then
        run_self_test
        exit $?
    fi
    if (( $# < 3 )); then
        echo "用法: bash deploy/check_tsid_node_registry.sh <registry.tsv> <dc_id> <node_id> [dc_bits]" >&2
        echo "      bash deploy/check_tsid_node_registry.sh --self-test" >&2
        exit 2
    fi
    echo "▶ TSID 节点登记表校验：$1（当前节点 dc_id=$2 node_id=$3）"
    check_registry "$1" "$2" "$3" "${4:-3}"
    rc=$?
    (( rc == 0 )) && echo "  结果：PASS" || echo "  结果：FAIL（${ERRORS} 处错误）" >&2
    exit $rc
}

main "$@"

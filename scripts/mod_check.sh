#!/usr/bin/env bash
# mod_check.sh — include/deps.mk 依赖版本审计（check）与定点升级（up）
#
# 用法：
#   scripts/mod_check.sh check              # 审计 deps.mk 全部 dep_* 定义
#   scripts/mod_check.sh check NAME...      # 只审计指定依赖（如: check cowboy jsone）
#   scripts/mod_check.sh up NAME...         # 升级到上游最新稳定版
#   scripts/mod_check.sh up NAME=VER ...    # 升级到指定版本（tag 需真实存在）
#   scripts/mod_check.sh up all             # 构建图内全部依赖升到最新稳定版
#                                           # （已最新/pin分支 自动跳过；图外定义不动）
#
# 设计要点：
#   * git 依赖：git ls-remote --tags --refs 查上游 tag，仅认可 semver 形状
#     （v?X[.Y[.Z]]，排除 -rc/-beta 等预发布），与当前 pin 做分段数字比较。
#   * hex 依赖：hex.pm API 的 latest_stable_version 不可靠（如 chatterbox
#     会返回旧版 0.8.0 而非 0.16.0），因此取 releases 全列表自行取最大稳定版。
#   * 构建图判定：解析 Makefile 的 DEPS/BUILD_DEPS/TEST_DEPS/DOC_DEPS，
#     叠加 EXTRA_GRAPH（Makefile 未直接声明的传递依赖 pin）。图外定义只
#     审计标注，不参与默认升级建议。
#   * macOS bash 3.2 兼容：不用关联数组、不用 gawk 特有函数；并发查询走
#     xargs -P + 每任务独立结果文件。
#   * up 只改 include/deps.mk（git 跟踪，可 diff/回滚），不动 deps/ 缓存，
#     升级后按提示 rm -rf deps/<name> 再 make 重新拉取。

set -u

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
DEPS_MK="$ROOT/include/deps.mk"
MAKEFILE="$ROOT/Makefile"

# Makefile 未直接列出、但实际会进构建图的依赖：
#   fs        — 被 sync 依赖（sync 仅 IMBOYENV=local 进 DEPS）；
#   hex_core  — erlang.mk 在存在 hex 依赖且项目未显式声明时自动注入
#               （erlang.mk:1242 "automatically depend on hex_core"）。
EXTRA_GRAPH="fs hex_core"

PARALLEL="${MOD_CHECK_JOBS:-8}"
HEX_TIMEOUT=20
# 注意不能置空：_worker 子进程靠它继承结果目录
MOD_CHECK_RES="${MOD_CHECK_RES:-}"

die() { echo "❌ $*" >&2; exit 1; }

# ---------- 基础工具 ----------

# 分段数字比较: 输出 gt|lt|eq
ver_cmp() {
    awk -F. -v a="$1" -v b="$2" 'BEGIN{
        n = split(a, A, "."); m = split(b, B, ".");
        for (i = 1; i <= 4; i++) {
            x = (i <= n ? A[i] + 0 : 0); y = (i <= m ? B[i] + 0 : 0);
            if (x > y) { print "gt"; exit }
            if (x < y) { print "lt"; exit }
        }
        print "eq"
    }'
}

is_stable_ver() { printf '%s' "$1" | grep -Eq '^[0-9]+(\.[0-9]+)+$'; }

# 上游全部稳定 tag（去 v 前缀、升序、去重）；失败输出空
git_tags() {
    git ls-remote --tags --refs "$1" 2>/dev/null \
        | awk '{print $2}' | sed -e 's|^refs/tags/||' -e 's|^v||' \
        | grep -E '^[0-9]+(\.[0-9]+)+$' \
        | sort -t. -k1,1n -k2,2n -k3,3n -k4,4n -u
}

# 上游是否存在分支（区分"仓库不可达"与"无 tag"）
git_has_repo() {
    [ -n "$(git ls-remote --heads "$1" 2>/dev/null | head -1)" ]
}

# hex 包全部稳定 release（升序）；失败输出空
hex_versions() {
    curl -sf --max-time "$HEX_TIMEOUT" "https://hex.pm/api/packages/$1" \
        | python3 -c 'import sys,json
try:
    for r in json.load(sys.stdin)["releases"]:
        print(r["version"])
except Exception:
    pass' 2>/dev/null \
        | grep -E '^[0-9]+(\.[0-9]+)+$' \
        | sort -t. -k1,1n -k2,2n -k3,3n -k4,4n -u
}

# ---------- deps.mk 解析 ----------
# 输出行: name|type|source|cur_ref|v_style|line_no
#   git: source=url；hex: source=hex 包名（erlang.mk 语义：第 3 段是包名，
#        如 dep_chatterbox = hex 0.16.0 ts_chatterbox → 查 ts_chatterbox；
#        缺省包名=变量名）；ln: 跳过
# 字段布局（"dep_x = git URL REF" 等号独立成词）: $1=名 $2="=" $3=类型 ...
parse_deps_mk() {
    awk '
        /^dep_[a-zA-Z0-9_-]+[[:space:]]*=/ {
            line = NR
            name = substr($1, 5)
            type = $3
            if (type == "git") { src = $4; ref = $5 }
            else if (type == "hex") {
                src = ($5 != "" ? $5 : name)
                ref = $4
            }
            else { next }
            v = (ref ~ /^v[0-9]/) ? "v" : ""
            sub(/^v/, "", ref)
            printf "%s|%s|%s|%s|%s|%d\n", name, type, src, ref, v, line
        }' "$DEPS_MK"
}

# ---------- 构建图解析 ----------
parse_graph() {
    {
        grep -E '^(DEPS|BUILD_DEPS|TEST_DEPS|DOC_DEPS|DEP_PLUGINS)([[:space:]]*\+?=)' "$MAKEFILE" 2>/dev/null \
            | sed -e 's/#.*$//' -e 's/[+?]=//g' -e 's/^[A-Z_]*//' | tr ' \t' '\n'
        printf '%s\n' $EXTRA_GRAPH
    } | grep -v '^$' | sort -u
}

in_graph() {  # $1=name $2=graph(多行)
    case "
$2
" in *"
$1
"*) return 0 ;; *) return 1 ;; esac
}

# ---------- 单依赖查询 ----------
# 入参: name|type|source|cur_ref|v_style  → 输出: name|latest|status
# status: latest(已是最新) | up(可升级) | major(大版本) | branch(pin分支且有tag)
#         | notag(pin分支且无tag) | fail(查询失败)
query_one() {
    IFS='|' read -r name type src cur vstyle <<<"$1"
    case "$type" in
        git)
            tags="$(git_tags "$src")"
            if [ -z "$tags" ] && ! git_has_repo "$src"; then
                echo "$name||fail"; return
            fi
            latest="$(printf '%s\n' "$tags" | tail -1)"
            latest="${latest:-}"
            if ! is_stable_ver "$cur"; then
                if [ -n "$latest" ]; then
                    echo "$name|$latest|branch"
                else
                    echo "$name||notag"
                fi
                return
            fi
            ;;
        hex)
            vers="$(hex_versions "$src")"
            if [ -z "$vers" ]; then
                echo "$name||fail"; return
            fi
            latest="$(printf '%s\n' "$vers" | tail -1)"
            ;;
        *) echo "$name||skip"; return ;;
    esac

    case "$(ver_cmp "$latest" "$cur")" in
        eq) echo "$name|$latest|latest" ;;
        lt) echo "$name|$latest|latest" ;;  # 上游最高版低于当前 pin（镜像滞后），报最新
        gt)
            if [ "${cur%%.*}" != "${latest%%.*}" ]; then
                echo "$name|$latest|major"
            else
                echo "$name|$latest|up"
            fi
            ;;
    esac
}

# xargs 并发 worker：读任务行，结果写 $MOD_CHECK_RES/<name>
worker() {
    task="$1"
    name="$(printf '%s' "$task" | cut -d'|' -f1)"
    task5="$(printf '%s' "$task" | cut -d'|' -f1-5)"
    query_one "$task5" > "$MOD_CHECK_RES/$name" 2>/dev/null || echo "$name||fail" > "$MOD_CHECK_RES/$name"
}

# ---------- 审计报告 ----------
do_check() {
    [ -f "$DEPS_MK" ] || die "未找到 $DEPS_MK"

    all="$(parse_deps_mk)"
    [ -n "$all" ] || die "deps.mk 中未解析到任何 dep_* 定义"
    if [ "$#" -gt 0 ]; then
        selected=""
        while IFS= read -r line; do
            n="${line%%|*}"
            for want in "$@"; do
                [ "$n" = "$want" ] && selected="${selected}${line}
"
            done
        done <<<"$all"
        [ -n "$(printf '%s' "$selected" | tr -d '[:space:]')" ] || die "指定依赖未在 deps.mk 中定义: $*"
        all="$selected"
    fi

    graph="$(parse_graph)"
    tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/imboy-modcheck.XXXXXX")"
    trap 'rm -rf "$tmpdir"' EXIT
    export MOD_CHECK_RES="$tmpdir/res"
    mkdir -p "$MOD_CHECK_RES"

    : > "$tmpdir/tasks"
    while IFS= read -r dep; do
        name="$(printf '%s' "$dep" | cut -d'|' -f1)"
        if in_graph "$name" "$graph"; then ing="1"; else ing="0"; fi
        printf '%s|%s\n' "$dep" "$ing" >> "$tmpdir/tasks"
    done <<<"$all"

    # 并发查询
    xargs -P "$PARALLEL" -I{} bash "$0" _worker '{}' < "$tmpdir/tasks" >/dev/null 2>&1

    # 汇总
    printf '\n%-20s %-5s %-4s %-12s %-12s %s\n' "依赖" "类型" "图" "当前" "最新" "状态"
    printf '%s\n' "----------------------------------------------------------------------------------------"
    n_up=0; n_major=0; n_latest=0; n_warn=0; n_unused=0
    while IFS='|' read -r name type src cur vstyle lineno ing; do
        [ -n "$name" ] || continue
        resfile="$MOD_CHECK_RES/$name"
        if [ -s "$resfile" ]; then
            IFS='|' read -r _r l_ver l_status < "$resfile"
        else
            l_ver=""; l_status="fail"
        fi
        latest_disp="$l_ver"
        [ -n "$vstyle" ] && latest_disp="${vstyle}${l_ver}"
        case "$l_status" in
            latest) mark="✓ 最新" ;;
            up)     mark="⬆ 可升级" ;;
            major)  mark="⬆⬆ 大版本(谨慎)" ;;
            branch) mark="⚠ pin分支,上游有tag $latest_disp" ;;
            notag)  mark="⚠ pin分支(上游无tag)" ;;
            fail)   mark="✗ 查询失败" ;;
            *)      mark="? ${l_status:-unknown}" ;;
        esac
        g="✓"; unused_note=""
        if [ "$ing" = "0" ]; then
            g="—"
            unused_note=" [图外,可清理]"
            n_unused=$((n_unused+1))
        else
            case "$l_status" in
                up)     n_up=$((n_up+1)) ;;
                major)  n_major=$((n_major+1)) ;;
                latest) n_latest=$((n_latest+1)) ;;
                branch|notag|fail) n_warn=$((n_warn+1)) ;;
            esac
        fi
        printf '%-20s %-5s %-4s %-12s %-12s %s%s\n' \
            "$name" "$type" "$g" "$cur" "${l_ver:--}" "$mark" "$unused_note"
    done < "$tmpdir/tasks"

    printf '%s\n' "----------------------------------------------------------------------------------------"
    echo "图内: 可升级 $n_up · 大版本 $n_major · 已最新 $n_latest · 需注意(分支pin/查询失败) $n_warn ｜ 另有图外未用定义 $n_unused 个"
    echo "注意: pin gitee 镜像的依赖显示的是镜像内最新（可能滞后于 GitHub 上游）"
    echo "升级: make mod-up MOD=\"<名字...>\"（或 NAME=VER 指定版本）"
}

# ---------- 定点升级 ----------
do_up() {
    [ $# -ge 1 ] || die "用法: make mod-up MOD=\"name[=ver] ...\" 或 MOD=all"
    all="$(parse_deps_mk)"
    graph="$(parse_graph)"

    # MOD=all：展开为构建图内全部 dep（逐个走下方同一逻辑：已最新/分支pin 自动跳过）
    if [ "$1" = "all" ]; then
        shift
        expand=""
        while IFS='|' read -r name _t _s _c _v _l; do
            in_graph "$name" "$graph" && expand="$expand $name"
        done <<<"$all"
        # shellcheck disable=SC2086
        set -- $expand
        [ $# -gt 0 ] || die "all: 构建图内无可处理的依赖"
        echo "== mod-up all: 共 $# 个图内依赖（已最新/分支pin 将自动跳过；图外定义不动）=="
    fi
    changed=""

    for spec in "$@"; do
        name="${spec%%=*}"
        want=""
        case "$spec" in *=*) want="${spec#*=}" ;; esac
        # 显式版本允许带 v 前缀，统一去掉后按去 v 形式匹配/写回（vstyle 补风格）
        want="${want#v}"

        line="$(printf '%s\n' "$all" | grep -E "^$(printf '%s' "$name" | sed 's/[][\\.*^$]/\\&/g')\|" || true)"
        [ -n "$line" ] || { echo "⚠ 跳过 $name: 未在 deps.mk 中定义"; continue; }
        IFS='|' read -r _n type src cur vstyle lineno <<<"$line"

        if ! in_graph "$name" "$graph"; then
            echo "⚠ $name 未进构建图（图外定义），仍执行升级，请确认意图"
        fi

        if [ -z "$want" ]; then
            r="$(query_one "$name|$type|$src|$cur|$vstyle")"
            latest="$(printf '%s' "$r" | cut -d'|' -f2)"
            status="$(printf '%s' "$r" | cut -d'|' -f3)"
            case "$status" in
                latest) echo "✓ $name 已是最新 ($cur)，跳过"; continue ;;
                fail)   echo "⚠ 跳过 $name: 上游查询失败"; continue ;;
                branch|notag)
                    echo "⚠ 跳过 $name: 当前 pin 分支（可手动 mod-up $name=${vstyle}${latest} 改 pin tag）"
                    continue ;;
                up|major) [ -n "$latest" ] || { echo "⚠ 跳过 $name: 未取到最新版"; continue; }
                          want="$latest" ;;
                *) echo "⚠ 跳过 $name: $status"; continue ;;
            esac
        fi

        if [ "$want" = "$cur" ]; then
            echo "✓ $name 已是 ${vstyle}${cur}，跳过"
            continue
        fi

        case "$type" in
            git)
                [ -n "$(git_tags "$src" | grep -Fx "$want")" ] \
                    || { echo "⚠ 跳过 $name: 上游无 tag ${vstyle}${want}"; continue; }
                awk -v ln="$lineno" -v ref="${vstyle}${want}" '
                    NR == ln { if ($3 == "git") $5 = ref; print; next }
                    { print }' "$DEPS_MK" > "$DEPS_MK.tmp" && mv "$DEPS_MK.tmp" "$DEPS_MK"
                ;;
            hex)
                [ -n "$(hex_versions "$src" | grep -Fx "$want")" ] \
                    || { echo "⚠ 跳过 $name: hex 无版本 $want"; continue; }
                awk -v ln="$lineno" -v ver="$want" '
                    NR == ln { if ($3 == "hex") $4 = ver; print; next }
                    { print }' "$DEPS_MK" > "$DEPS_MK.tmp" && mv "$DEPS_MK.tmp" "$DEPS_MK"
                ;;
        esac
        echo "⬆ $name: $cur → ${vstyle}${want}（include/deps.mk 已更新）"
        changed="$changed $name"
    done

    if [ -n "$changed" ]; then
        changed="$(printf '%s' "$changed" | sed 's/^ *//')"
        echo
        echo "后续步骤："
        echo "  1. git diff include/deps.mk        # 审阅改动"
        echo "  2. 清缓存（缺一不可，erlang.mk 戳记不随目录删除失效）："
        echo "     for n in $changed; do rm -rf deps/\$n .erlang.mk/dep_built/\$n; done"
        echo "  3. make compile && make eunit-local  # 重新拉取并验证"
        echo "  4. git checkout include/deps.mk      # 如需回滚"
    else
        echo "未产生任何改动。"
    fi
}

# ---------- 入口 ----------
case "${1:-}" in
    check) shift; do_check "$@" ;;
    up)    shift; do_up "$@" ;;
    _worker) shift; worker "$@" ;;
    *) sed -n '2,12p' "$0"; echo; exit 1 ;;
esac

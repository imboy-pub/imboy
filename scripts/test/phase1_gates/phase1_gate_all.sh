#!/usr/bin/env bash
# ============================================================================
# phase1_gate_all.sh —— Q12 一期终验 20 门总控（串行执行器）
# ----------------------------------------------------------------------------
# 依据: Q12-final-gate-runners.md §1（20 门清单）+ §4（排期与串行图 L174-188）
#   门总数 20 = Backend 7 + Admin 8 + App 2 + 演练 3（设计 L185）
#   全串行一次通过约 5.5–7h + P3 真机段 0.5–1.5h/平台（设计 L186，L57）
#   默认顺序即设计 §4 串行图: Phase A(G01-G05) → B(G06-G07) → C(G08-G15)
#   → D(G16) → E(G17) → F(G18,G19,G20) ；Phase G（冻结复算）不在 20 门内。
#
# 环境互斥纪律（macOS 无 flock，一律 mkdir 自旋锁 + trap EXIT rmdir）:
#   /tmp/phase1-eunit.lock —— eunit 门 G06/G07（占 ebin+隔离库+端口三元组+pg_relay）
#   /tmp/phase1-node.lock  —— 节点/演练门 G18-G20（起后端节点/演练栈并写库）
#   /tmp/phase1-seat.lock  —— seat E2E 门；当前 20 门清单未含 seat 门，
#                              预留给 Q07 seat Playwright / P3 真机追加门
#                              （主计划 §6: 浏览器登录态、设备必须串行租用）
#   锁超时: PHASE1_LOCK_TIMEOUT 秒（默认 3600）；超时退出 75，绝不自动破坏锁。
#
# 用法:
#   bash phase1_gate_all.sh --list                 # 列出 20 门与预估时长
#   bash phase1_gate_all.sh --dry-run              # 打印将执行的命令但不执行
#   bash phase1_gate_all.sh --only G03,G07         # 门子集（仍按注册顺序执行）
#   bash phase1_gate_all.sh --continue-on-error    # 失败不中断，跑完汇总
#   bash phase1_gate_all.sh                        # 全 20 门真实串行执行
#   bash phase1_gate_all.sh --only G06,G07 --run-root /path/to/logs
#
# 日志（RUN_ROOT 优先级: --run-root 参数 > RUN_ROOT 环境变量 >
#       默认 /tmp/phase1-gates-<UTC 时间戳>）:
#   <RUN_ROOT>/<Gxx>.log    每门 stdout/stderr + GATE=… STATUS=START/END 行
#   <RUN_ROOT>/summary.tsv  汇总: gate script status start end duration_s exit_code lock
# 退出码: 0=全过；1=存在失败门；64=参数错误；75=锁超时
# ============================================================================
set -euo pipefail

GATES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$GATES_DIR/../../.." && pwd)}"

# ---------------------------------------------------------------------------
# 门注册表（顺序 = 设计 §4 串行图；字段: id|脚本|描述|仓|预计|锁）
# ---------------------------------------------------------------------------
GATE_IDS=(G01 G02 G03 G04 G05 G06 G07 G08 G09 G10 G11 G12 G13 G14 G15 G16 G17 G18 G19 G20)
GATE_SCRIPTS=(
  g01_backend_compile.sh
  g02_static_cross_repo.sh
  g03_static_generator_check.sh
  g04_static_flatten_internal.sh
  g05_static_migrations_check.sh
  g06_eunit_full_run1.sh
  g07_eunit_full_run2.sh
  g08_admin_unit_test.sh
  g09_admin_lint.sh
  g10_admin_typecheck.sh
  g11_admin_build.sh
  g12_admin_widget_build.sh
  g13_admin_widget_verify.sh
  g14_admin_widget_pairing.sh
  g15_admin_deadcode.sh
  g16_app_flutter_analyze.sh
  g17_app_flutter_test.sh
  g18_drill_golden_install.sh
  g19_drill_backup_restore_oracle.sh
  g20_drill_golden_upgrade.sh
)
GATE_DESCS=(
  "B1 后端全量编译 make compile"
  "B4 静态①跨仓一致 cross_repo"
  "B5 静态②生成器 --check"
  "B6 静态③internal 扁平 --check"
  "B7 静态④迁移文件 migrations-check"
  "B2 后端全量 eunit 第1次(端口 23980/23970/26432)"
  "B3 后端全量 eunit 第2次(RC=0 x2 收敛)"
  "A1 Admin 全量单测 bun test"
  "A2 Admin lint"
  "A3 Admin typecheck"
  "A4 Admin 生产构建"
  "A5 Admin Widget 构建"
  "A6 Admin Widget 校验"
  "A7 Admin Widget 资产配对(跨仓读)"
  "A8 Admin 死代码 knip"
  "P1 App 静态分析 flutter analyze"
  "P2 App 全量单测 flutter test"
  "F1 演练·golden-install(TOTAL<=900s)"
  "F3 演练·PG/Garage 备份恢复 oracle"
  "F2 演练·golden-upgrade(alpha.70→候选)"
)
GATE_REPOS=(
  imboy imboy imboy imboy imboy imboy imboy
  imboyadmin imboyadmin imboyadmin imboyadmin imboyadmin imboyadmin imboyadmin imboyadmin
  imboyapp imboyapp
  imboy imboy imboy
)
GATE_ETAS=(
  "1-5 min" "<1 min" "<1 min" "<1 min" "<1 min"
  "20-25 min" "20-25 min"
  "3-8 min" "1-2 min" "1-2 min" "2-3 min" "3-6 min" "~1 min" "~1 min" "1-3 min"
  "1-2 min" "~2.5 h"
  "~0.5 h" "~0.5 h" "~0.5 h"
)
GATE_LOCKS=(
  none none none none none
  eunit eunit
  none none none none none none none none
  none none
  node node node
)

LOCK_EUNIT=/tmp/phase1-eunit.lock
LOCK_NODE=/tmp/phase1-node.lock
LOCK_SEAT=/tmp/phase1-seat.lock
LOCK_TIMEOUT="${PHASE1_LOCK_TIMEOUT:-3600}"

CURRENT_LOCK=""
SUMMARY=""
RUN_ROOT_ARG=""
ONLY_RAW=""
LIST=0
HELP=0
DRY_RUN=0
CONTINUE_ON_ERROR=0

# ---------------------------------------------------------------------------
# 工具函数
# ---------------------------------------------------------------------------
usage() {
  cat <<'EOF'
phase1_gate_all.sh —— Q12 一期终验 20 门总控（串行执行器）
依据: Q12-final-gate-runners.md §1（20 门清单）+ §4（串行图）；全串行约 5.5-7h。

用法: bash phase1_gate_all.sh [选项]
  --list                 列出 20 门（ID/门/脚本/仓/预计时长/锁）与总时长口径
  --dry-run              逐门调用 runner --dry-run，只打印将执行的命令，不执行
  --only G03,G07         选择门子集（大小写不敏感；仍按设计 §4 串行顺序执行）
  --continue-on-error    门失败时不中断，跑完所选门后汇总并以非零退出
  --run-root <dir>       日志目录（优先级: 本参数 > RUN_ROOT 环境变量 >
                         默认 /tmp/phase1-gates-<UTC 时间戳>）

环境互斥锁（macOS 无 flock，mkdir 自旋锁 + trap EXIT rmdir）:
  /tmp/phase1-eunit.lock  eunit 门 G06/G07（ebin+隔离库+端口三元组）
  /tmp/phase1-node.lock   节点/演练门 G18-G20（起后端节点/演练栈并写库）
  /tmp/phase1-seat.lock   seat E2E 门（当前 20 门未含；预留给 Q07 seat/P3 追加门）
  超时: PHASE1_LOCK_TIMEOUT 秒（默认 3600），超时退出绝不自动破坏锁。

日志: <RUN_ROOT>/<Gxx>.log 每门输出 + START/END/exit_code 行；
      <RUN_ROOT>/summary.tsv 汇总（gate script status start end duration_s exit_code lock）
退出码: 0=全过 1=有失败门 64=参数错误 75=锁超时
EOF
}

info()  { printf '[gate-all] %s\n' "$*"; }
warn()  { printf '[gate-all][WARN] %s\n' "$*" >&2; }
# die <msg> [exit_code]（默认 1；参数错误 64；锁超时 75——与 usage 承诺一致）
die()   { printf '[gate-all][ERROR] %s\n' "$*" >&2; exit "${2:-1}"; }

utc_now() { date -u +%Y-%m-%dT%H:%M:%SZ; }
utc_from_epoch() {
  # 兼容 macOS(date -r) 与 GNU(date -d @epoch)
  local iso
  iso="$(date -u -d "@$1" +%Y-%m-%dT%H:%M:%SZ 2>/dev/null || date -u -r "$1" +%Y-%m-%dT%H:%M:%SZ)"
  printf '%s\n' "$iso"
}

# ---------------------------------------------------------------------------
# mkdir 自旋锁（macOS 无 flock；trap EXIT 兜底释放）
# ---------------------------------------------------------------------------
acquire_lock() {
  local lock_dir="$1" gate="$2" waited=0 holder
  while ! mkdir "$lock_dir" 2>/dev/null; do
    if [ "$waited" -eq 0 ]; then
      holder="$(cat "$lock_dir/owner" 2>/dev/null || echo '未知（疑似残留锁）')"
      warn "等待锁 ${lock_dir}（当前持有者: ${holder}）"
    fi
    sleep 5
    waited=$((waited + 5))
    if [ "$waited" -ge "$LOCK_TIMEOUT" ]; then
      die "获取锁 $lock_dir 超时（${LOCK_TIMEOUT}s）。若确认无并发持有者，可手工清理: rm -f $lock_dir/owner && rmdir $lock_dir" 75
    fi
    if [ $((waited % 300)) -eq 0 ]; then
      warn "仍在等待锁 ${lock_dir}（已等 ${waited}s）"
    fi
  done
  printf '%s pid=%s gate=%s since=%s\n' "$(hostname)" "$$" "$gate" "$(utc_now)" > "$lock_dir/owner"
  info "已获取锁 ${lock_dir}（gate=${gate}）"
}

release_lock() {
  local lock_dir="$1"
  if [ -d "$lock_dir" ]; then
    rm -f "$lock_dir/owner" 2>/dev/null || true
    rmdir "$lock_dir" 2>/dev/null || true
    info "已释放锁 $lock_dir"
  fi
}

on_exit() {
  if [ -n "$CURRENT_LOCK" ]; then
    warn "进程退出时仍持有 ${CURRENT_LOCK}，trap 兜底释放"
    release_lock "$CURRENT_LOCK"
    CURRENT_LOCK=""
  fi
}
trap on_exit EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

lock_dir_for() {
  case "$1" in
    eunit) printf '%s\n' "$LOCK_EUNIT" ;;
    node)  printf '%s\n' "$LOCK_NODE" ;;
    seat)  printf '%s\n' "$LOCK_SEAT" ;;
    *)     printf '' ;;
  esac
}

# ---------------------------------------------------------------------------
# --list
# ---------------------------------------------------------------------------
print_list() {
  printf '%-4s %-42s %-40s %-11s %-10s %s\n' "ID" "门（设计§1 编号）" "脚本" "仓" "预计" "锁"
  local i
  for i in "${!GATE_IDS[@]}"; do
    printf '%-4s %-42s %-40s %-11s %-10s %s\n' \
      "${GATE_IDS[$i]}" "${GATE_DESCS[$i]}" "${GATE_SCRIPTS[$i]}" \
      "${GATE_REPOS[$i]}" "${GATE_ETAS[$i]}" "${GATE_LOCKS[$i]}"
  done
  echo "----"
  echo "共 ${#GATE_IDS[@]} 门 = Backend 7 + Admin 8 + App 2 + 演练 3（设计 L185）"
  echo "全串行一次通过约 5.5-7h + P3 真机段 0.5-1.5h/平台（P3 不在本清单，设计 L57/L186）"
  echo "锁: eunit=${LOCK_EUNIT} node=${LOCK_NODE} seat=${LOCK_SEAT}（seat 为预留，见头部注释）"
}

# ---------------------------------------------------------------------------
# --only 解析（支持 G03,g07 混写；输出保持注册顺序）
# ---------------------------------------------------------------------------
declare -a ONLY_SET=()
has_only_filter() { [ "${#ONLY_SET[@]}" -gt 0 ]; }

parse_only() {
  local raw="$1" tok normalized found
  local -a toks=()
  IFS=',' read -r -a toks <<< "$raw"
  for tok in "${toks[@]}"; do
    normalized="$(printf '%s' "$tok" | tr -d '[:space:]' | tr '[:lower:]' '[:upper:]')"
    if [ -z "$normalized" ]; then continue; fi
    case "$normalized" in
      G[0-9][0-9]) : ;;
      *) die "--only 非法门号: ${tok}（形如 G03,G07）" 64 ;;
    esac
    found=0
    local id
    for id in "${GATE_IDS[@]}"; do
      if [ "$id" = "$normalized" ]; then found=1; break; fi
    done
    if [ "$found" -ne 1 ]; then die "--only 未知门: ${normalized}（用 --list 查看）" 64; fi
    # 去重
    local dup=0 existing
    for existing in "${ONLY_SET[@]:-}"; do
      if [ "$existing" = "$normalized" ]; then dup=1; break; fi
    done
    if [ "$dup" -eq 0 ]; then ONLY_SET+=("$normalized"); fi
  done
  if [ "${#ONLY_SET[@]}" -eq 0 ]; then die "--only 未解析出任何门号" 64; fi
}

gate_selected() {
  local gate="$1" g
  if ! has_only_filter; then return 0; fi
  for g in "${ONLY_SET[@]}"; do
    if [ "$g" = "$gate" ]; then return 0; fi
  done
  return 1
}

gate_index() {
  local gate="$1" i
  for i in "${!GATE_IDS[@]}"; do
    if [ "${GATE_IDS[$i]}" = "$gate" ]; then printf '%s' "$i"; return 0; fi
  done
  return 1
}

# ---------------------------------------------------------------------------
# 单门执行（真实模式；dry-run 由 main 直接分发）
# ---------------------------------------------------------------------------
run_gate() {
  local gate="$1" idx script desc lock lock_dir log start_s end_s rc dur status
  idx="$(gate_index "$gate")"
  script="${GATE_SCRIPTS[$idx]}"
  desc="${GATE_DESCS[$idx]}"
  lock="${GATE_LOCKS[$idx]}"
  lock_dir="$(lock_dir_for "$lock")"
  log="$RUN_ROOT/${gate}.log"

  if [ -n "$lock_dir" ]; then
    acquire_lock "$lock_dir" "$gate"
    CURRENT_LOCK="$lock_dir"
  fi

  start_s="$(date +%s)"
  printf 'GATE=%s STATUS=START utc=%s lock=%s desc=%s\n' "$gate" "$(utc_now)" "${lock_dir:-none}" "$desc" | tee -a "$log"

  set +e
  bash "$GATES_DIR/$script" >>"$log" 2>&1
  rc=$?
  set -e

  if [ -n "$lock_dir" ]; then
    release_lock "$lock_dir"
    CURRENT_LOCK=""
  fi

  end_s="$(date +%s)"
  dur=$((end_s - start_s))
  if [ "$rc" -eq 0 ]; then status=PASS; else status=FAIL; fi
  printf 'GATE=%s STATUS=END rc=%s duration=%ss utc=%s\n' "$gate" "$rc" "$dur" "$(utc_now)" | tee -a "$log"
  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$gate" "$script" "$status" "$(utc_from_epoch "$start_s")" "$(utc_from_epoch "$end_s")" "$dur" "$rc" "$lock" >> "$SUMMARY"
  printf '[%s] %s %s rc=%s duration=%ss（日志 %s）\n' "$status" "$gate" "$desc" "$rc" "$dur" "$log"
  return "$rc"
}

# ---------------------------------------------------------------------------
# 参数解析
# ---------------------------------------------------------------------------
while [ "$#" -gt 0 ]; do
  case "$1" in
    --list)             LIST=1 ;;
    --help|-h)          HELP=1 ;;
    --dry-run)          DRY_RUN=1 ;;
    --continue-on-error) CONTINUE_ON_ERROR=1 ;;
    --only)             shift; if [ "$#" -eq 0 ] || [ -z "${1:-}" ]; then die "--only 需要参数（如 G03,G07）" 64; fi; ONLY_RAW="$1" ;;
    --only=*)           ONLY_RAW="${1#*=}"; if [ -z "$ONLY_RAW" ]; then die "--only 需要参数（如 G03,G07）" 64; fi ;;
    --run-root)         shift; if [ "$#" -eq 0 ] || [ -z "${1:-}" ]; then die "--run-root 需要目录参数" 64; fi; RUN_ROOT_ARG="$1" ;;
    --run-root=*)       RUN_ROOT_ARG="${1#*=}"; if [ -z "$RUN_ROOT_ARG" ]; then die "--run-root 需要目录参数" 64; fi ;;
    *) die "未知参数: $1（--list / --help / --dry-run / --only G03,G07 / --continue-on-error / --run-root <dir>）" 64 ;;
  esac
  shift
done

if [ "$HELP" -eq 1 ]; then usage; exit 0; fi
if [ "$LIST" -eq 1 ]; then print_list; exit 0; fi
if [ -n "$ONLY_RAW" ]; then parse_only "$ONLY_RAW"; fi

# ---------------------------------------------------------------------------
# RUN_ROOT 解析与初始化
# ---------------------------------------------------------------------------
if [ -n "$RUN_ROOT_ARG" ]; then
  RUN_ROOT="$RUN_ROOT_ARG"
elif [ -n "${RUN_ROOT:-}" ]; then
  RUN_ROOT="$RUN_ROOT"
else
  RUN_ROOT="/tmp/phase1-gates-$(date -u +%Y%m%dT%H%M%SZ)"
fi
mkdir -p "$RUN_ROOT"
SUMMARY="$RUN_ROOT/summary.tsv"

# ---------------------------------------------------------------------------
# DRY-RUN：逐门调用 runner --dry-run，只打印命令不执行
# ---------------------------------------------------------------------------
if [ "$DRY_RUN" -eq 1 ]; then
  info "DRY-RUN 模式（不执行任何真实门） RUN_ROOT=$RUN_ROOT 选门: $(if has_only_filter; then IFS=','; echo "${ONLY_SET[*]}"; else echo '全部 20 门'; fi)"
  echo ""
  for gate in "${GATE_IDS[@]}"; do
    if gate_selected "$gate"; then
      idx="$(gate_index "$gate")"
      echo "══════ ${gate} ${GATE_DESCS[$idx]}（repo=${GATE_REPOS[$idx]} lock=${GATE_LOCKS[$idx]} eta=${GATE_ETAS[$idx]}）"
      bash "$GATES_DIR/${GATE_SCRIPTS[$idx]}" --dry-run
      echo ""
    fi
  done
  info "DRY-RUN 结束（未执行任何真实门；真实执行去掉 --dry-run）"
  exit 0
fi

# ---------------------------------------------------------------------------
# 真实串行执行
# ---------------------------------------------------------------------------
printf 'gate\tscript\tstatus\tstart_utc\tend_utc\tduration_s\texit_code\tlock\n' > "$SUMMARY"

info "Q12 终验 20 门总控启动 $(utc_now)"
info "RUN_ROOT=${RUN_ROOT}（每门日志 <Gxx>.log；汇总 summary.tsv）"
info "imboy=$IMBOY_ROOT head=$(git -C "$IMBOY_ROOT" rev-parse HEAD 2>/dev/null || echo '?')"
info "选门: $(if has_only_filter; then IFS=','; echo "${ONLY_SET[*]}"; else echo '全部 20 门（设计 §4 串行顺序）'; fi)"
info "continue_on_error=$CONTINUE_ON_ERROR lock_timeout=${LOCK_TIMEOUT}s"
echo ""

FAILED=0
EXECUTED=0
for gate in "${GATE_IDS[@]}"; do
  if gate_selected "$gate"; then
    if run_gate "$gate"; then
      :
    else
      FAILED=$((FAILED + 1))
      if [ "$CONTINUE_ON_ERROR" -ne 1 ]; then
        die "$gate 失败，按串行纪律停止（加 --continue-on-error 可继续其余门）"
      fi
      warn "$gate 失败，--continue-on-error 生效，继续后续门"
    fi
    EXECUTED=$((EXECUTED + 1))
  fi
done

echo ""
info "执行完毕 $(utc_now)：共 $EXECUTED 门，失败 $FAILED 门；汇总："
cat "$SUMMARY"
if [ "$FAILED" -eq 0 ]; then
  info "ALL PASS（20 门口径下的所选子集全绿）"
  exit 0
fi
die "存在 $FAILED 个失败门（详见 $SUMMARY 与各 <Gxx>.log）"

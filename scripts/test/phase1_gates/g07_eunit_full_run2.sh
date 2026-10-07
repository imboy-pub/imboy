#!/usr/bin/env bash
# ============================================================================
# G07 / 设计门 B3 —— 后端全量 eunit 第 2 次（imboy 仓）
# ----------------------------------------------------------------------------
# 依据: Q12-final-gate-runners.md §1.1 表 B3 行（L26）
#   命令: 同 B2，紧随 G06 同冻结候选重跑，连续两次 RC=0 达成才算收敛
#         （设计 L18：EO 实证形态 All NNNNN passed RC=0 ×2；L186 Phase B）
#   中途零修复零提交（设计 §4 L177 Phase B 注记）——G06 与 G07 之间不得有任何
#   commit/修复动作，否则两轮作废重跑。
# 互斥: 由总控 phase1_gate_all.sh 持有 /tmp/phase1-eunit.lock（同 G06）。
# 预计: 20–25 min
# 判定口径: 同 G06（四查全零 + All NNNNN passed RC=0）。
# 用法:
#   PGPASSWORD=... bash g07_eunit_full_run2.sh [--dry-run]
# ============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"

usage() {
  cat <<'EOF'
用法: PGPASSWORD=... bash g07_eunit_full_run2.sh [选项]
  --dry-run    只打印将执行的命令，不执行
  -h, --help   显示本帮助
门: G07（设计 B3）后端全量 eunit 第 2 次（与 G06 同候选；中途零修复零提交）
EOF
}

DRY_RUN=0
for arg in "$@"; do
  case "$arg" in
    --dry-run) DRY_RUN=1 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "ERROR: 未知参数 $arg" >&2; usage >&2; exit 64 ;;
  esac
done

if [ "$DRY_RUN" -eq 0 ]; then
  command -v make >/dev/null 2>&1 || { echo "ERROR: 缺少 make" >&2; exit 127; }
  if [ -z "${PGPASSWORD:-}" ]; then
    echo "ERROR: PGPASSWORD 未设置（设计 L25：经环境注入）" >&2
    exit 2
  fi
  [ -f "$IMBOY_ROOT/config/sys.p1-run.config" ] || {
    echo "ERROR: config/sys.p1-run.config 不存在" >&2; exit 2;
  }
fi
cd "$IMBOY_ROOT"

CMD=(make eunit-local
     EUNIT_CONFIG=config/sys.p1-run
     EUNIT_HTTP_PORT=23980
     EUNIT_HTTP_ADM_PORT=23970
     EUNIT_RELAY_PORT=26432)

if [ "$DRY_RUN" -eq 1 ]; then
  echo "[DRY-RUN] cd $IMBOY_ROOT && PGPASSWORD=*** ${CMD[*]}"
  exit 0
fi

echo "==> G07 / B3 后端全量 eunit 第 2 次 start=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo "    前置：G06 已 RC=0；两轮之间零修复零提交（否则回 G06 重跑）"
"${CMD[@]}"
echo "==> G07 / B3 后端全量 eunit 第 2 次 PASS rc=0（连续两次 RC=0 收敛达成）"

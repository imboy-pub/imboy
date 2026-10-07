#!/usr/bin/env bash
# ============================================================================
# G01 / 设计门 B1 —— 后端全量编译（imboy 仓）
# ----------------------------------------------------------------------------
# 依据: Q12-final-gate-runners.md §1.1 表 B1 行（L24）
#   命令: make compile（= app 别名；干净全量重编约 4 min，Makefile WH-02 注释口径）
# 资源: ebin（独占）；与 eunit 门（G06/G07）互斥由总控 phase1_gate_all.sh 的
#       /tmp/phase1-eunit.lock 锁纪律约束（本门自身不取锁，Phase A 首位）
# 预计: 1–5 min（串行顺序见设计 §4 L175 Phase A）
# 用法:
#   bash g01_backend_compile.sh [--dry-run]
# ============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"

usage() {
  cat <<'EOF'
用法: bash g01_backend_compile.sh [选项]
  --dry-run    只打印将执行的命令，不执行
  -h, --help   显示本帮助
门: G01（设计 B1）后端全量编译 —— make compile
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
fi
cd "$IMBOY_ROOT"

CMD=(make compile)

if [ "$DRY_RUN" -eq 1 ]; then
  echo "[DRY-RUN] cd $IMBOY_ROOT && ${CMD[*]}"
  exit 0
fi

echo "==> G01 / B1 后端全量编译 start=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
"${CMD[@]}"
echo "==> G01 / B1 后端全量编译 PASS rc=0"

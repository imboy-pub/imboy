#!/usr/bin/env bash
# ============================================================================
# G08 / 设计门 A1 —— Admin 全量单测（imboyadmin 仓，bun）
# ----------------------------------------------------------------------------
# 依据: Q12-final-gate-runners.md §1.2 表 A1 行（L42）
#   命令: bun test（= bun test --isolate，package.json script；222 个 *.test.* 文件）
# 资源: CPU/内存；无 DB/设备
# 预计: 3–8 min（串行，设计 §4 L177 Phase C 首位）
# 用法:
#   bash g08_admin_unit_test.sh [--dry-run]
# ============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"
IMBOYADMIN_ROOT="${IMBOYADMIN_ROOT:-$(cd "$IMBOY_ROOT/.." && pwd)/imboyadmin}"

usage() {
  cat <<'EOF'
用法: bash g08_admin_unit_test.sh [选项]
  --dry-run    只打印将执行的命令，不执行
  -h, --help   显示本帮助
门: G08（设计 A1）Admin 全量单测 —— bun test
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
  command -v bun >/dev/null 2>&1 || { echo "ERROR: 缺少 bun" >&2; exit 127; }
  [ -f "$IMBOYADMIN_ROOT/package.json" ] || { echo "ERROR: $IMBOYADMIN_ROOT/package.json 不存在" >&2; exit 2; }
fi
cd "$IMBOYADMIN_ROOT"

CMD=(bun test)

if [ "$DRY_RUN" -eq 1 ]; then
  echo "[DRY-RUN] cd $IMBOYADMIN_ROOT && ${CMD[*]}"
  exit 0
fi

echo "==> G08 / A1 Admin 全量单测 start=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
"${CMD[@]}"
echo "==> G08 / A1 Admin 全量单测 PASS rc=0"

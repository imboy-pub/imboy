#!/usr/bin/env bash
# ============================================================================
# G13 / 设计门 A6 —— Admin Widget 校验（imboyadmin 仓）
# ----------------------------------------------------------------------------
# 依据: Q12-final-gate-runners.md §1.2 表 A6 行（L47）
#   命令: bun run verify:widget（scripts/widget-verify.mts）
# 前置: 依赖 G12（A5）构建产物
# 资源: 读产物
# 预计: ~1 min
# 用法:
#   bash g13_admin_widget_verify.sh [--dry-run]
# ============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"
IMBOYADMIN_ROOT="${IMBOYADMIN_ROOT:-$(cd "$IMBOY_ROOT/.." && pwd)/imboyadmin}"

usage() {
  cat <<'EOF'
用法: bash g13_admin_widget_verify.sh [选项]
  --dry-run    只打印将执行的命令，不执行
  -h, --help   显示本帮助
门: G13（设计 A6）Admin Widget 校验 —— bun run verify:widget
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
fi
cd "$IMBOYADMIN_ROOT"

CMD=(bun run verify:widget)

if [ "$DRY_RUN" -eq 1 ]; then
  echo "[DRY-RUN] cd $IMBOYADMIN_ROOT && ${CMD[*]}"
  exit 0
fi

echo "==> G13 / A6 Admin Widget 校验 start=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo "    前置：G12（A5）已构建产物"
"${CMD[@]}"
echo "==> G13 / A6 Admin Widget 校验 PASS rc=0"

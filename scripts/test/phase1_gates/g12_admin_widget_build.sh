#!/usr/bin/env bash
# ============================================================================
# G12 / 设计门 A5 —— Admin Widget 构建（imboyadmin 仓）
# ----------------------------------------------------------------------------
# 依据: Q12-final-gate-runners.md §1.2 表 A5 行（L46）
#   命令: bun run build:widget
#         （widget/widget-loader/seat 三 mode vite build ×4 + widget-manifest.mts）
# 资源: 与 G11（A4）串行——同构建缓存；写 dist-widget 产物
# 预计: 3–6 min
# 用法:
#   bash g12_admin_widget_build.sh [--dry-run]
# ============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"
IMBOYADMIN_ROOT="${IMBOYADMIN_ROOT:-$(cd "$IMBOY_ROOT/.." && pwd)/imboyadmin}"

usage() {
  cat <<'EOF'
用法: bash g12_admin_widget_build.sh [选项]
  --dry-run    只打印将执行的命令，不执行
  -h, --help   显示本帮助
门: G12（设计 A5）Admin Widget 构建 —— bun run build:widget
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

CMD=(bun run build:widget)

if [ "$DRY_RUN" -eq 1 ]; then
  echo "[DRY-RUN] cd $IMBOYADMIN_ROOT && ${CMD[*]}"
  exit 0
fi

echo "==> G12 / A5 Admin Widget 构建 start=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
"${CMD[@]}"
echo "==> G12 / A5 Admin Widget 构建 PASS rc=0"

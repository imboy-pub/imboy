#!/usr/bin/env bash
# ============================================================================
# G09 / 设计门 A2 —— Admin lint（imboyadmin 仓）
# ----------------------------------------------------------------------------
# 依据: Q12-final-gate-runners.md §1.2 表 A2 行（L43）
#   命令: bun run lint（eslint .）
# 资源: 只读
# 预计: 1–2 min
# 用法:
#   bash g09_admin_lint.sh [--dry-run]
# ============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"
IMBOYADMIN_ROOT="${IMBOYADMIN_ROOT:-$(cd "$IMBOY_ROOT/.." && pwd)/imboyadmin}"

usage() {
  cat <<'EOF'
用法: bash g09_admin_lint.sh [选项]
  --dry-run    只打印将执行的命令，不执行
  -h, --help   显示本帮助
门: G09（设计 A2）Admin lint —— bun run lint
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

CMD=(bun run lint)

if [ "$DRY_RUN" -eq 1 ]; then
  echo "[DRY-RUN] cd $IMBOYADMIN_ROOT && ${CMD[*]}"
  exit 0
fi

echo "==> G09 / A2 Admin lint start=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
"${CMD[@]}"
echo "==> G09 / A2 Admin lint PASS rc=0"

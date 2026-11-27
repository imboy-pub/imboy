#!/usr/bin/env bash
# ============================================================================
# G15 / 设计门 A8 —— Admin 死代码检查（imboyadmin 仓，knip）
# ----------------------------------------------------------------------------
# 依据: Q12-final-gate-runners.md §1.2 表 A8 行（L49）
#   命令: bun run deadcode（knip）
# 注意: F10 债务（15 unused files、158 unused exports 等）须先经 Q10 入口
#       建模校准（seatMain/Widget main/Playwright 配置等可能误报）——设计 L49；
#       本门按 Q12 执行时点 knip 输出判定，不豁免未登记项。
# 资源: 只读
# 预计: 1–3 min
# 用法:
#   bash g15_admin_deadcode.sh [--dry-run]
# ============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"
IMBOYADMIN_ROOT="${IMBOYADMIN_ROOT:-$(cd "$IMBOY_ROOT/.." && pwd)/imboyadmin}"

usage() {
  cat <<'EOF'
用法: bash g15_admin_deadcode.sh [选项]
  --dry-run    只打印将执行的命令，不执行
  -h, --help   显示本帮助
门: G15（设计 A8）Admin 死代码 —— bun run deadcode（knip）
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

CMD=(bun run deadcode)

if [ "$DRY_RUN" -eq 1 ]; then
  echo "[DRY-RUN] cd $IMBOYADMIN_ROOT && ${CMD[*]}"
  exit 0
fi

echo "==> G15 / A8 Admin 死代码检查 start=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
"${CMD[@]}"
echo "==> G15 / A8 Admin 死代码检查 PASS rc=0"

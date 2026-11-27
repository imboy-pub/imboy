#!/usr/bin/env bash
# ============================================================================
# G05 / 设计门 B7 —— 静态门④ 迁移文件校验（imboy 仓，只读）
# ----------------------------------------------------------------------------
# 依据: Q12-final-gate-runners.md §1.1 表 B7 行（L30）
#   命令: make migrations-check（Makefile:437 确认；实际执行
#         bash scripts/check_migrations.sh）
#   语义: ADR-0002 —— 命名/up-down 成对/版本唯一/up 非空；编号断档仅告警。
#         当前 priv/migrations 326 文件（设计 L30 时点口径）
# 资源: 只读
# 预计: <1 min
# 用法:
#   bash g05_static_migrations_check.sh [--dry-run]
# ============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"

usage() {
  cat <<'EOF'
用法: bash g05_static_migrations_check.sh [选项]
  --dry-run    只打印将执行的命令，不执行
  -h, --help   显示本帮助
门: G05（设计 B7）静态④迁移文件 —— make migrations-check
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

CMD=(make migrations-check)

if [ "$DRY_RUN" -eq 1 ]; then
  echo "[DRY-RUN] cd $IMBOY_ROOT && ${CMD[*]}"
  exit 0
fi

echo "==> G05 / B7 静态④迁移文件校验 start=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
"${CMD[@]}"
echo "==> G05 / B7 静态④迁移文件校验 PASS rc=0"

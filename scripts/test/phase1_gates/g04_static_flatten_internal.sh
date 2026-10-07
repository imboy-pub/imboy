#!/usr/bin/env bash
# ============================================================================
# G04 / 设计门 B6 —— 静态门③ internal API 扁平校验（imboy 仓，只读）
# ----------------------------------------------------------------------------
# 依据: Q12-final-gate-runners.md §1.1 表 B6 行（L29）
#   命令: python3 api/flatten_internal.py --check
# 资源: 只读
# 预计: <1 min
# 用法:
#   bash g04_static_flatten_internal.sh [--dry-run]
# ============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"

usage() {
  cat <<'EOF'
用法: bash g04_static_flatten_internal.sh [选项]
  --dry-run    只打印将执行的命令，不执行
  -h, --help   显示本帮助
门: G04（设计 B6）静态③ internal 扁平 --check —— api/flatten_internal.py
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
  command -v python3 >/dev/null 2>&1 || { echo "ERROR: 缺少 python3" >&2; exit 127; }
  [ -f "$IMBOY_ROOT/api/flatten_internal.py" ] || { echo "ERROR: api/flatten_internal.py 不存在" >&2; exit 2; }
fi
cd "$IMBOY_ROOT"

CMD=(python3 api/flatten_internal.py --check)

if [ "$DRY_RUN" -eq 1 ]; then
  echo "[DRY-RUN] cd $IMBOY_ROOT && ${CMD[*]}"
  exit 0
fi

echo "==> G04 / B6 静态③ internal 扁平校验 start=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
"${CMD[@]}"
echo "==> G04 / B6 静态③ internal 扁平校验 PASS rc=0"

#!/usr/bin/env bash
# ============================================================================
# G03 / 设计门 B5 —— 静态门② 生成器校验（imboy 仓，只读）
# ----------------------------------------------------------------------------
# 依据: Q12-final-gate-runners.md §1.1 表 B5 行（L28）
#   命令: python3 scripts/generate_product_features.py --check
# 资源: 只读；注意任何生成器写模式一次只允许一个调用者（设计 L28）
# 预计: <1 min
# 用法:
#   bash g03_static_generator_check.sh [--dry-run]
# ============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"

usage() {
  cat <<'EOF'
用法: bash g03_static_generator_check.sh [选项]
  --dry-run    只打印将执行的命令，不执行
  -h, --help   显示本帮助
门: G03（设计 B5）静态②生成器 --check —— generate_product_features.py
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
fi
cd "$IMBOY_ROOT"

CMD=(python3 scripts/generate_product_features.py --check)

if [ "$DRY_RUN" -eq 1 ]; then
  echo "[DRY-RUN] cd $IMBOY_ROOT && ${CMD[*]}"
  exit 0
fi

echo "==> G03 / B5 静态②生成器校验 start=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
"${CMD[@]}"
echo "==> G03 / B5 静态②生成器校验 PASS rc=0"

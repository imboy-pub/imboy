#!/usr/bin/env bash
# ============================================================================
# G02 / 设计门 B4 —— 静态门① 跨仓一致（imboy 仓，只读生成物）
# ----------------------------------------------------------------------------
# 依据: Q12-final-gate-runners.md §1.1 表 B4 行（L27）
#   命令: python3 scripts/check_product_feature_cross_repo.py
#         （= make feature-cross-repo-check）
# 前置: 需 ../imboyapp、../imboyadmin 与 imboy 同父目录（设计 L27 原文）
# 资源: 读三仓生成物；不写
# 预计: <1 min
# 用法:
#   bash g02_static_cross_repo.sh [--dry-run]
# ============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"

usage() {
  cat <<'EOF'
用法: bash g02_static_cross_repo.sh [选项]
  --dry-run    只打印将执行的命令，不执行
  -h, --help   显示本帮助
门: G02（设计 B4）静态①跨仓一致 —— check_product_feature_cross_repo.py
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
  for dep in "$IMBOY_ROOT/../imboyapp" "$IMBOY_ROOT/../imboyadmin"; do
    if [ ! -d "$dep" ]; then
      echo "ERROR: 缺少同父目录仓 ${dep}（设计 L27：B4 需 ../imboyapp、../imboyadmin）" >&2
      exit 2
    fi
  done
fi
cd "$IMBOY_ROOT"

CMD=(python3 scripts/check_product_feature_cross_repo.py)

if [ "$DRY_RUN" -eq 1 ]; then
  echo "[DRY-RUN] cd $IMBOY_ROOT && ${CMD[*]}"
  exit 0
fi

echo "==> G02 / B4 静态①跨仓一致 start=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
"${CMD[@]}"
echo "==> G02 / B4 静态①跨仓一致 PASS rc=0"

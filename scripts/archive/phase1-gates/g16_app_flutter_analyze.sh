#!/usr/bin/env bash
# ============================================================================
# G16 / 设计门 P1 —— App 静态分析（imboyapp 仓，flutter analyze）
# ----------------------------------------------------------------------------
# 依据: Q12-final-gate-runners.md §1.3 表 P1 行（L55）
#   命令: flutter analyze
# 已知基线: EO 记录既有 5 条 info 级（integration_test×2 + test×3）——设计 L55；
#           Q12 执行时若超出该基线需解释，未解释失败不能豁免（主计划 §6）。
# 资源: 只读；无真机占用、无 PG 写
# 预计: 1–2 min（串行，设计 §4 L178 Phase D）
# 用法:
#   bash g16_app_flutter_analyze.sh [--dry-run]
# ============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"
IMBOYAPP_ROOT="${IMBOYAPP_ROOT:-$(cd "$IMBOY_ROOT/.." && pwd)/imboyapp}"

usage() {
  cat <<'EOF'
用法: bash g16_app_flutter_analyze.sh [选项]
  --dry-run    只打印将执行的命令，不执行
  -h, --help   显示本帮助
门: G16（设计 P1）App 静态分析 —— flutter analyze
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
  command -v flutter >/dev/null 2>&1 || { echo "ERROR: 缺少 flutter" >&2; exit 127; }
  [ -f "$IMBOYAPP_ROOT/pubspec.yaml" ] || { echo "ERROR: $IMBOYAPP_ROOT/pubspec.yaml 不存在" >&2; exit 2; }
fi
cd "$IMBOYAPP_ROOT"

CMD=(flutter analyze)

if [ "$DRY_RUN" -eq 1 ]; then
  echo "[DRY-RUN] cd $IMBOYAPP_ROOT && ${CMD[*]}"
  exit 0
fi

echo "==> G16 / P1 App 静态分析 start=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo "    基线：EO 记录既有 5 条 info 级（integration_test×2 + test×3，设计 L55）"
"${CMD[@]}"
echo "==> G16 / P1 App 静态分析 PASS rc=0"

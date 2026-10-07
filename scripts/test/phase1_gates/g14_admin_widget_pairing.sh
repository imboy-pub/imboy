#!/usr/bin/env bash
# ============================================================================
# G14 / 设计门 A7 —— Admin Widget 资产配对校验（imboyadmin 仓，跨仓读）
# ----------------------------------------------------------------------------
# 依据: Q12-final-gate-runners.md §1.2 表 A7 行（L48）
#   命令: bun run verify:widget-pairing（scripts/check-widget-asset-pairing.mts）
# 前置: 依赖 G12（A5）/G13（A6）产物 + 后端 priv/cs_widget_asset_pairing.json
# 资源: 跨仓读（imboy 仓 priv/）
# 预计: ~1 min
# 用法:
#   bash g14_admin_widget_pairing.sh [--dry-run]
# ============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"
IMBOYADMIN_ROOT="${IMBOYADMIN_ROOT:-$(cd "$IMBOY_ROOT/.." && pwd)/imboyadmin}"

usage() {
  cat <<'EOF'
用法: bash g14_admin_widget_pairing.sh [选项]
  --dry-run    只打印将执行的命令，不执行
  -h, --help   显示本帮助
门: G14（设计 A7）Admin Widget 配对 —— bun run verify:widget-pairing
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
  if [ ! -f "$IMBOY_ROOT/priv/cs_widget_asset_pairing.json" ]; then
    echo "ERROR: $IMBOY_ROOT/priv/cs_widget_asset_pairing.json 不存在（设计 L48：跨仓读后端配置）" >&2
    exit 2
  fi
fi
cd "$IMBOYADMIN_ROOT"

CMD=(bun run verify:widget-pairing)

if [ "$DRY_RUN" -eq 1 ]; then
  echo "[DRY-RUN] cd $IMBOYADMIN_ROOT && ${CMD[*]}"
  exit 0
fi

echo "==> G14 / A7 Admin Widget 资产配对 start=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo "    前置：G12/G13 已过；后端 priv/cs_widget_asset_pairing.json 存在"
"${CMD[@]}"
echo "==> G14 / A7 Admin Widget 资产配对 PASS rc=0"

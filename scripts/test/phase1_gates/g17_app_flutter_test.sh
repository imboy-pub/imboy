#!/usr/bin/env bash
# ============================================================================
# G17 / 设计门 P2 —— App 全量单测（imboyapp 仓，flutter test）
# ----------------------------------------------------------------------------
# 依据: Q12-final-gate-runners.md §1.3 表 P2 行（L56）
#   命令: flutter test（全量 831 个 *_test.dart；上轮 Q03 全量 7409 通过/6 失败）
# 预计: ~2.5 h（纳入排期；不含真机段——P3 另计 0.5–1.5h/平台）
# 资源: dart test 隔离进程；无真机占用、无 PG 写
# e2ee 备份 6 个既有失败的裁决前置（设计 §1.3 L59-63，Q12 前必须裁决并登记
# recovery.jsonl BASELINE-DEBT-REGISTER）:
#   选项一 修复：最小 diff，修复后本门全量重跑
#   选项二 范围裁决（AC24 排除项建模）：一期 profile E2EE 关闭（delivery_locked），
#          测试经显式（非静默 ignore）范围裁决豁免，二期安全回归覆盖
#   选项三 拆段：一期域全量（必须 RC=0）+ e2ee 备份域单列（附裁决）
#   失败定位：test/unit_test/service/e2ee_server_backup_service_test.dart
#            + e2ee_local_backup_boundary_test.dart（设计 L60）
# 用法:
#   bash g17_app_flutter_test.sh [--dry-run]
# ============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"
IMBOYAPP_ROOT="${IMBOYAPP_ROOT:-$(cd "$IMBOY_ROOT/.." && pwd)/imboyapp}"

usage() {
  cat <<'EOF'
用法: bash g17_app_flutter_test.sh [选项]
  --dry-run    只打印将执行的命令，不执行
  -h, --help   显示本帮助
门: G17（设计 P2）App 全量单测 —— flutter test（~2.5h）
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
fi
cd "$IMBOYAPP_ROOT"

CMD=(flutter test)

if [ "$DRY_RUN" -eq 1 ]; then
  echo "[DRY-RUN] cd $IMBOYAPP_ROOT && ${CMD[*]}"
  exit 0
fi

echo "==> G17 / P2 App 全量单测 start=$(date -u +%Y-%m-%dT%H:%M:%SZ)（预计 ~2.5h）"
echo "    裁决前置：e2ee 备份 6 个既有失败须已在 Q12 前裁决（设计 L59-63 三选一，"
echo "    记录 recovery.jsonl BASELINE-DEBT-REGISTER）；未裁决即跑视为流程违规"
"${CMD[@]}"
echo "==> G17 / P2 App 全量单测 PASS rc=0"

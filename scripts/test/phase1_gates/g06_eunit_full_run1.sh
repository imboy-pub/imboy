#!/usr/bin/env bash
# ============================================================================
# G06 / 设计门 B2 —— 后端全量 eunit 第 1 次（imboy 仓）
# ----------------------------------------------------------------------------
# 依据: Q12-final-gate-runners.md §1.1 表 B2 行（L25）
#   命令: make eunit-local EUNIT_CONFIG=config/sys.p1-run
#         EUNIT_HTTP_PORT=23980 EUNIT_HTTP_ADM_PORT=23970 EUNIT_RELAY_PORT=26432
#         （端口三元组与隔离库绑定本 run leases.json：设计 §0 L14）
#   PGPASSWORD 经环境注入（设计 L25），本脚本不落盘、不回显。
# 互斥: 由总控 phase1_gate_all.sh 持有 /tmp/phase1-eunit.lock（mkdir 自旋锁）
#       —— 占 ebin + 隔离库写入 + 端口三元组 + pg_relay 循环，与任何其他
#       ebin/写库作业互斥（设计 L25 资源占用列）。
# 预计: 20–25 min/轮（EO attempt2 实测 20 min；10885 用例、日志 ~28k 行，L25）
# 判定口径（设计 L18 / L33）:
#   - "四查全零" = failed/cancelled/skipped/error 全零（were cancelled=0）
#   - 末尾 All NNNNN passed 且 RC=0
#   - EUNIT Excl 机制排除 11 套件（elib_tsid 系 3、AG31 系 7、cs_pg 系 2），
#     Q12 不改此契约（L33）
# 执行前置（Q12 执行时确认，非本脚本职责，设计 L34）:
#   1) 隔离库 imboy_p1_20261007t003716z schema 已迁移到最新
#   2) .erlang.mk/imboy.test 标记/beam-cache 失步由 eunit-local 入口三连删自愈，
#      勿手工清 ebin 共享状态
# 用法:
#   PGPASSWORD=... bash g06_eunit_full_run1.sh [--dry-run]
# ============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"

usage() {
  cat <<'EOF'
用法: PGPASSWORD=... bash g06_eunit_full_run1.sh [选项]
  --dry-run    只打印将执行的命令，不执行
  -h, --help   显示本帮助
门: G06（设计 B2）后端全量 eunit 第 1 次（判定：四查全零 + All passed RC=0）
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
  if [ -z "${PGPASSWORD:-}" ]; then
    echo "ERROR: PGPASSWORD 未设置（设计 L25：经环境注入）" >&2
    exit 2
  fi
  [ -f "$IMBOY_ROOT/config/sys.p1-run.config" ] || {
    echo "ERROR: config/sys.p1-run.config 不存在" >&2; exit 2;
  }
fi
cd "$IMBOY_ROOT"

CMD=(make eunit-local
     EUNIT_CONFIG=config/sys.p1-run
     EUNIT_HTTP_PORT=23980
     EUNIT_HTTP_ADM_PORT=23970
     EUNIT_RELAY_PORT=26432)

if [ "$DRY_RUN" -eq 1 ]; then
  echo "[DRY-RUN] cd $IMBOY_ROOT && PGPASSWORD=*** ${CMD[*]}"
  exit 0
fi

echo "==> G06 / B2 后端全量 eunit 第 1 次 start=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo "    判定口径：连续两次 RC=0 ×2（与 G07 配对）；四查全零 + All NNNNN passed"
echo "    预计 20-25 min（EO attempt2 实测 20 min）"
"${CMD[@]}"
echo "==> G06 / B2 后端全量 eunit 第 1 次 PASS rc=0（第 2 次见 g07，中途零修复零提交）"

#!/usr/bin/env bash
# ============================================================================
# G20 / 设计门 F2 —— 演练·金升级（golden-upgrade，含回退路线）
# ----------------------------------------------------------------------------
# 依据: Q12-final-gate-runners.md §3.4（L166-168）+ §1 资产表（L134）
#   命令骨架（设计 L167）:
#     make golden-upgrade GOLDEN_ARGS="--from v1.0.0-alpha.70
#       --to-image <候选 digest> --to-ref <frozen_head>"
#   断言: 总结行 GOLDEN_UPGRADE=PASS … MIGRATION=… DATA_RETENTION=PASS SMOKE8=PASS
#     （golden_upgrade.sh L669 格式；无时限门，TOTAL 如实计时入质量总账，L168）
# 前置（TODO，Q12 执行时补齐）:
#   1) 升级前先做全量备份（回退路线，设计 L163 §3.3.5）——down 脚本不作为
#      回滚承诺；升级失败 → DROP 半迁移库 → restore_pg 回备份 + garage 恢复
#      → 8 步链复绿 → 记回退耗时（对照 RTO≤60min 建议值）。
#   2) 基线镜像可拉性验证（设计 L168 风险①）: ghcr 基线镜像
#      imboy-backend:v1.0.0-alpha.70 未必可拉，先 docker pull 验证；
#      失败则改 --from-image 本地按 tag 构建。
#   3) cleanroom 同 G18（/tmp/phase1-node.lock 由总控持有）。
# 已知风险（设计 L168）: alpha.70→82 迁移跨度大（326 迁移），auto_migrate
#   观察窗可能较长；cleanroom 网络须可达 ghcr/git remote。
# 预计: 单门 ~0.5h 量级；Phase F 三件合计 ~1.5–2.5h（设计 §4 L181）
# 必填环境变量（实跑时）:
#   GOLDEN_IMAGE_DIGEST  候选镜像 digest（形如 sha256:…，硬要求 digest）
#   GOLDEN_GIT_REF       冻结候选 git ref（frozen_head）
# 可选:
#   GOLDEN_FROM_REF（默认 v1.0.0-alpha.70——仓内最新版本 tag，设计 L167）
# 用法:
#   bash g20_drill_golden_upgrade.sh [--dry-run]
# ============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"

usage() {
  cat <<'EOF'
用法: bash g20_drill_golden_upgrade.sh [选项]
  --dry-run    只打印将执行的命令，不执行
  -h, --help   显示本帮助
门: G20（设计 F2）演练·golden-upgrade（alpha.70 → 冻结候选；数据保留断言）
必填 env: GOLDEN_IMAGE_DIGEST / GOLDEN_GIT_REF；可选: GOLDEN_FROM_REF
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

GOLDEN_FROM_REF="${GOLDEN_FROM_REF:-v1.0.0-alpha.70}"

if [ "$DRY_RUN" -eq 0 ]; then
  command -v make >/dev/null 2>&1 || { echo "ERROR: 缺少 make" >&2; exit 127; }
  missing=0
  for v in GOLDEN_IMAGE_DIGEST GOLDEN_GIT_REF; do
    if [ -z "${!v:-}" ]; then
      echo "ERROR: 必填环境变量 $v 未设置（TODO 待 Q12 执行时提供，设计 L167）" >&2
      missing=1
    fi
  done
  if [ "$missing" -eq 1 ]; then exit 2; fi
fi
cd "$IMBOY_ROOT"

GOLDEN_ARGS="--from ${GOLDEN_FROM_REF} \
--to-image ${GOLDEN_IMAGE_DIGEST:-<GOLDEN_IMAGE_DIGEST>} \
--to-ref ${GOLDEN_GIT_REF:-<GOLDEN_GIT_REF>}"

step0_preflight() {
  # TODO(Q12, 设计 L163 + L168):
  #  1) 升级前全量备份（回退路线 §3.3.5：restore 路线是唯一承诺，down 脚本不算）
  #  2) 基线镜像可拉性: docker pull ghcr.io/.../imboy-backend:${GOLDEN_FROM_REF}
  #     失败 → 改 --from-image 本地按 tag 构建（golden_upgrade.sh 支持）
  #  3) cleanroom 就绪（同 G18 方案 A）
  echo "TODO(Q12): 升级前全量备份 + 基线镜像可拉性验证 + cleanroom（设计 L163/L168/L144-145）"
  return 1
}

if [ "$DRY_RUN" -eq 1 ]; then
  echo "[DRY-RUN] 前置 TODO: 全量备份（回退路线 L163）+ 基线镜像 docker pull 验证（L168）+ cleanroom"
  echo "[DRY-RUN] cd $IMBOY_ROOT && make golden-upgrade GOLDEN_ARGS=\"$GOLDEN_ARGS\""
  echo "[DRY-RUN] 断言: 总结行 GOLDEN_UPGRADE=PASS 且 DATA_RETENTION=PASS 且 SMOKE8=PASS（golden_upgrade.sh L669）"
  echo "[DRY-RUN] 失败路线: DROP 半迁移库 → restore_pg 回备份 + garage 恢复 → 8 步链复绿 → 记回退耗时（RTO<=60min，L163）"
  exit 0
fi

echo "==> G20 / F2 演练·golden-upgrade start=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
step0_preflight || {
  echo "ERROR: 升级前置（备份/基线镜像/cleanroom）未就绪，阻断实跑（设计 L163/L168）" >&2
  exit 2
}

LOG_FILE="${G20_LOG_FILE:-$(mktemp -t g20_golden_upgrade).log}"
echo "    golden-upgrade 输出留存: $LOG_FILE"
make golden-upgrade GOLDEN_ARGS="$GOLDEN_ARGS" 2>&1 | tee "$LOG_FILE"

# 断言（总结行格式见 golden_upgrade.sh L669）：
#   GOLDEN_UPGRADE=<v> TOTAL=<n>s FROM=… TO=… PROFILE=… MIGRATION=<v> DATA_RETENTION=<v> SMOKE8=<v>
SUMMARY_LINE="$(grep -E '^GOLDEN_UPGRADE=' "$LOG_FILE" | tail -1)"
echo "    总结行: $SUMMARY_LINE"
case "$SUMMARY_LINE" in
  *GOLDEN_UPGRADE=PASS*) : ;;
  *) echo "ERROR: GOLDEN_UPGRADE!=PASS" >&2; exit 1 ;;
esac
case "$SUMMARY_LINE" in
  *MIGRATION=PASS*|*MIGRATION=OK*) : ;;
  *) echo "WARN: MIGRATION 状态非 PASS，按 golden_upgrade.sh 实际枚举值复核（L669）" >&2 ;;
esac
case "$SUMMARY_LINE" in
  *DATA_RETENTION=PASS*) : ;;
  *) echo "ERROR: DATA_RETENTION!=PASS（设计 L134：数据保留断言）" >&2; exit 1 ;;
esac
case "$SUMMARY_LINE" in
  *SMOKE8=PASS*) : ;;
  *) echo "ERROR: SMOKE8!=PASS（8 步链复绿）" >&2; exit 1 ;;
esac
echo "    TOTAL 如实计时入质量总账（无时限门，设计 L168）"
echo "==> G20 / F2 演练·golden-upgrade PASS rc=0（log+总结行留档入 manifest.drills.upgrade）"

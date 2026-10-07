#!/usr/bin/env bash
# ============================================================================
# G18 / 设计门 F1 —— 演练·空环境一键安装（golden-install）
# ----------------------------------------------------------------------------
# 依据: Q12-final-gate-runners.md §3.2（L141-150）+ §1 资产表（L133）
#   命令骨架（设计 L149）:
#     make golden-install GOLDEN_ARGS="--image-ref <候选镜像 digest> --git-ref
#       <frozen_head> --profile ci --admin-phone 139…007806
#       --admin-password '…' --keep"
#   断言（设计 L149）: 末行含 GOLDEN_INSTALL=PASS 与 RESTART_IDEMPOTENT=PASS，
#     TOTAL≤900s（BUDGET=900s，golden_install.sh L359/L374/L500-505 自带判定）。
# 前置缺口（TODO，Q12 执行时补齐）:
#   1) 候选后端镜像须先构建并钉 digest（--image-ref 硬要求 digest，tag 不收，
#      设计 L150）——CI 构建 or 本地 IMBOYENV=pro make rel + 镜像打包；
#      Q11 交付包白名单须先就绪。
#   2) DinD cleanroom（方案 A，设计 L144-145）——宿主在跑开发用 imboy_pg18 与
#      garage-local（imboy_ 前缀），golden 门 assert_cleanroom_leftovers 在宿主
#      直接跑必 die 且 leases 禁杀 foreign 容器，故须隔离 cleanroom。
# 互斥: 由总控 phase1_gate_all.sh 持有 /tmp/phase1-node.lock（起后端节点/演练栈）。
# 预计: 单门 ~0.5h 量级；Phase F 三件合计 ~1.5–2.5h（设计 §4 L181）
# 必填环境变量（实跑时）:
#   GOLDEN_IMAGE_DIGEST  候选镜像 digest（形如 sha256:…）
#   GOLDEN_GIT_REF       冻结候选 git ref（frozen_head）
#   GOLDEN_ADMIN_PHONE   演练超管手机号
#   GOLDEN_ADMIN_PASSWORD 演练超管密码（不回显、不落盘）
# 可选:
#   GOLDEN_PROFILE（默认 ci；host 需真域名+certbot，设计 L146 方案 C 需外部授权）
# 用法:
#   bash g18_drill_golden_install.sh [--dry-run]
# ============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"

usage() {
  cat <<'EOF'
用法: bash g18_drill_golden_install.sh [选项]
  --dry-run    只打印将执行的命令，不执行
  -h, --help   显示本帮助
门: G18（设计 F1）演练·golden-install（空环境安装 + restart 幂等 + TOTAL≤900s）
必填 env: GOLDEN_IMAGE_DIGEST / GOLDEN_GIT_REF / GOLDEN_ADMIN_PHONE / GOLDEN_ADMIN_PASSWORD
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

GOLDEN_PROFILE="${GOLDEN_PROFILE:-ci}"

if [ "$DRY_RUN" -eq 0 ]; then
  command -v make >/dev/null 2>&1 || { echo "ERROR: 缺少 make" >&2; exit 127; }
  missing=0
  for v in GOLDEN_IMAGE_DIGEST GOLDEN_GIT_REF GOLDEN_ADMIN_PHONE GOLDEN_ADMIN_PASSWORD; do
    if [ -z "${!v:-}" ]; then
      echo "ERROR: 必填环境变量 $v 未设置（TODO 待 Q12 执行时提供，设计 L149-150）" >&2
      missing=1
    fi
  done
  if [ "$missing" -eq 1 ]; then exit 2; fi
fi
cd "$IMBOY_ROOT"

GOLDEN_ARGS="--image-ref ${GOLDEN_IMAGE_DIGEST:-<GOLDEN_IMAGE_DIGEST>} \
--git-ref ${GOLDEN_GIT_REF:-<GOLDEN_GIT_REF>} \
--profile ${GOLDEN_PROFILE} \
--admin-phone ${GOLDEN_ADMIN_PHONE:-<GOLDEN_ADMIN_PHONE>} \
--admin-password ${GOLDEN_ADMIN_PASSWORD:-***} \
--keep"

step1_prepare_cleanroom() {
  # TODO(Q12, 设计 §3.2 方案 A L144-145): 宿主起特权 DinD 容器作 cleanroom，
  #   golden 脚本在容器内独立 docker daemon 上运行，与宿主 imboy_* 零冲突。
  # 参考骨架（执行时按实际调整并验证磁盘 ~20GB）:
  #   docker run --privileged --name p1_cleanroom -d debian:13 sleep infinity
  #   # 容器内装 docker+compose；候选仓经 bind 只读挂载或走 git clone（golden 原路径）
  #   # 注意：golden_common.sh:assert_cleanroom_leftovers 拒绝任何 imboy_*
  #   #   容器/卷/imboy-network 残留（设计 L143-144），检查的是容器内 daemon。
  echo "TODO(Q12): DinD cleanroom 准备（设计 §3.2 方案 A，L144-145）——"
  echo "  本骨架不自动创建；请在 Q12 执行前完成 cleanroom 搭建或在容器内执行本门。"
  return 1
}

if [ "$DRY_RUN" -eq 1 ]; then
  echo "[DRY-RUN] 前置 TODO: DinD cleanroom（方案 A，设计 L144-145）"
  echo "[DRY-RUN] cd $IMBOY_ROOT && make golden-install GOLDEN_ARGS=\"$GOLDEN_ARGS\""
  echo "[DRY-RUN] 断言: 末行 GOLDEN_INSTALL=PASS 且 RESTART_IDEMPOTENT=PASS 且 TOTAL<=900s（设计 L149）"
  exit 0
fi

echo "==> G18 / F1 演练·golden-install start=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
# cleanroom 前置：TODO 未落地前阻断实跑（防止误在宿主直接跑 golden 门）
step1_prepare_cleanroom || {
  echo "ERROR: cleanroom 前置未就绪，阻断实跑（设计 L143-145：宿主在跑 imboy_*，直接跑必 die）" >&2
  exit 2
}

LOG_FILE="${G18_LOG_FILE:-$(mktemp -t g18_golden_install).log}"
echo "    golden-install 输出留存: $LOG_FILE"
make golden-install GOLDEN_ARGS="$GOLDEN_ARGS" 2>&1 | tee "$LOG_FILE"

# 断言（设计 L149；总结行格式见 golden_install.sh L374）：
#   GOLDEN_INSTALL=<v> TOTAL=<n>s BUDGET=900s PROFILE=… GIT_REF=… IMAGE_DIGEST=… RESTART_IDEMPOTENT=<v>
SUMMARY_LINE="$(grep -E '^GOLDEN_INSTALL=' "$LOG_FILE" | tail -1)"
echo "    总结行: $SUMMARY_LINE"
case "$SUMMARY_LINE" in
  *GOLDEN_INSTALL=PASS*) : ;;
  *) echo "ERROR: GOLDEN_INSTALL!=PASS（设计 L149 断言失败）" >&2; exit 1 ;;
esac
case "$SUMMARY_LINE" in
  *RESTART_IDEMPOTENT=PASS*) : ;;
  *) echo "ERROR: RESTART_IDEMPOTENT!=PASS（设计 L149 断言失败）" >&2; exit 1 ;;
esac
TOTAL_S="$(printf '%s\n' "$SUMMARY_LINE" | sed -n 's/.*TOTAL=\([0-9]*\)s.*/\1/p')"
if [ -n "$TOTAL_S" ] && [ "$TOTAL_S" -le 900 ]; then
  echo "    TOTAL=${TOTAL_S}s <= 900s 预算 PASS"
else
  echo "ERROR: TOTAL=${TOTAL_S:-?}s 超 900s 预算（设计 L149）" >&2
  exit 1
fi
echo "==> G18 / F1 演练·golden-install PASS rc=0（log+总结行留档入 manifest.drills.install）"

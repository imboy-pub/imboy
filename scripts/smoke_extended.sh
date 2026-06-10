#!/usr/bin/env bash
# 扩展冒烟测试 / Extended smoke test
# 串接基础冒烟（make smoke）+ 特性开关冒烟 + 可选 Garage 附件 E2E。
# Chains base smoke (make smoke) + feature-flag smoke + optional Garage E2E.
#
# 前置 / Prerequisites:
#   后端已启动 / Backend running:  IMBOYENV=local make run
#
# 用法 / Usage:
#   bash scripts/smoke_extended.sh                 # 基础 + 特性开关
#   TOKEN=<jwt> bash scripts/smoke_extended.sh     # 额外跑 Garage 附件 E2E
#   bash scripts/smoke_extended.sh --base-only     # 仅 make smoke
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."

BASE_ONLY=0
[ "${1:-}" = "--base-only" ] && BASE_ONLY=1

PASS=()
FAIL=()
run_gate() { # run_gate <名称> <命令...>
  local name="$1"; shift
  printf '\n========== %s ==========\n' "$name"
  if "$@"; then
    PASS+=("$name")
  else
    FAIL+=("$name")
  fi
}

# 1. 节点存活（快速失败优于跑一半）
escript scripts/imboy_ctl node status >/dev/null 2>&1 \
  || { echo "✗ 后端节点不可达，请先 IMBOYENV=local make run" >&2; exit 1; }

# 2. 基础冒烟：c2c + ws + ctl
run_gate "base-smoke (make smoke)" make smoke

if [ "$BASE_ONLY" -eq 0 ]; then
  # 3. 特性开关冒烟（默认打本地，FEATURE_SMOKE_BASE_URL 可覆盖）
  run_gate "feature-flag-smoke" make feature-smoke \
    FEATURE_SMOKE_BASE_URL="${FEATURE_SMOKE_BASE_URL:-http://127.0.0.1:9800}"

  # 4. Garage 附件直传 E2E（需要 TOKEN，未设置则跳过）
  if [ -n "${TOKEN:-}" ]; then
    run_gate "garage-attachment-e2e" bash scripts/garage_e2e_test.sh
  else
    echo ""
    echo "（跳过 Garage E2E：未设置 TOKEN。取 token：escript scripts/imboy_ctl user token <uid>）"
  fi
fi

# 汇总 / Summary
printf '\n========== 汇总 / Summary ==========\n'
for g in "${PASS[@]+"${PASS[@]}"}"; do echo "  ✓ $g"; done
for g in "${FAIL[@]+"${FAIL[@]}"}"; do echo "  ✗ $g"; done
if [ "${#FAIL[@]}" -gt 0 ]; then
  echo "✗ ${#FAIL[@]} 个冒烟门失败"; exit 1
fi
echo "✓ 全部通过"

#!/usr/bin/env bash
# DCO 本地检查 / Local DCO check
# 与 CI dco-check job（backend-ci.yml）等价的本地版：检查指定范围内
# 每个 commit 是否带 Signed-off-by。提交时用 `git commit -s` 自动签名。
# Local equivalent of the CI dco-check job: every commit in range must
# carry a Signed-off-by trailer. Use `git commit -s` when committing.
#
# 用法 / Usage:
#   bash scripts/check_dco.sh              # 默认检查 origin/main..HEAD（无 origin/main 则最近 10 个）
#   bash scripts/check_dco.sh <range>      # 如 main..HEAD / HEAD~5..HEAD
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."

RANGE="${1:-}"
if [ -z "$RANGE" ]; then
  if git rev-parse --verify origin/main >/dev/null 2>&1; then
    RANGE="origin/main..HEAD"
  else
    RANGE="HEAD~10..HEAD"
  fi
fi

FAIL=0
TOTAL=0
while read -r sha; do
  [ -n "$sha" ] || continue
  TOTAL=$((TOTAL + 1))
  if ! git log -1 --format='%(trailers:key=Signed-off-by,only)' "$sha" | grep -q 'Signed-off-by'; then
    echo "✗ $(git log -1 --format='%h %s' "$sha") — 缺少 Signed-off-by"
    FAIL=$((FAIL + 1))
  fi
done < <(git rev-list "$RANGE" 2>/dev/null || true)

if [ "$TOTAL" -eq 0 ]; then
  echo "范围 ${RANGE} 内无 commit，跳过 / No commits in range, skipped"
  exit 0
fi
if [ "$FAIL" -gt 0 ]; then
  echo ""
  echo "✗ ${FAIL}/${TOTAL} 个 commit 缺少 DCO 签名。修复 / Fix:"
  echo "  git rebase --signoff ${RANGE%%..*}"
  exit 1
fi
echo "✓ ${TOTAL} 个 commit 全部带 Signed-off-by（${RANGE}）"

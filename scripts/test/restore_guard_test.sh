#!/usr/bin/env bash
# ============================================================
# 恢复脚本守卫测试 / Restore script guard tests
# ------------------------------------------------------------
# 覆盖 B-28：restore_pg.sh 的生产库守卫。
# 纯参数/守卫层测试，**不连任何数据库、不需要 docker**。
#
# 用法: bash scripts/test/restore_guard_test.sh
# ============================================================
set -uo pipefail

cd "$(dirname "$0")/../.."
SCRIPT="scripts/restore_pg.sh"
PASS=0; FAIL=0
RED='\033[0;31m'; GREEN='\033[0;32m'; NC='\033[0m'

ok()   { echo -e "${GREEN}  PASS${NC} $1"; PASS=$((PASS+1)); }
bad()  { echo -e "${RED}  FAIL${NC} $1"; echo "    输出: ${2:-<空>}"; FAIL=$((FAIL+1)); }

# 断言脚本以非零退出且输出含指定文本
assert_rejects() {
  local desc="$1" needle="$2"; shift 2
  local out rc
  out="$("$@" 2>&1 </dev/null)"; rc=$?
  if [ "$rc" -eq 0 ]; then
    bad "${desc}（期望拒绝，实际退出码 0）" "$out"
  elif ! printf '%s' "$out" | grep -q "$needle"; then
    bad "${desc}（退出码正确但未命中 '${needle}'）" "$out"
  else
    ok "$desc"
  fi
}

echo "== B-28 生产库守卫 =="

# 用一个不存在的备份文件即可：守卫必须**早于**文件存在性检查触发，
# 否则"目标是生产库"这件事会被"文件不存在"掩盖掉。
NOFILE=/tmp/definitely_missing_$$.dump

# 1) 不带 --target 时缺省就是生产库 —— 这正是最容易手滑的那条路
assert_rejects "缺省 target(=生产库) 被拒绝" "拒绝执行" \
  env POSTGRES_DB=imboy_pro bash "$SCRIPT" "$NOFILE"

# 2) 显式把 target 写成生产库，同样拒绝
assert_rejects "显式 --target 生产库被拒绝" "拒绝执行" \
  env POSTGRES_DB=imboy_pro bash "$SCRIPT" "$NOFILE" --target imboy_pro

# 3) 放行开关 + FORCE=1 同时使用被拒 —— 不允许一次绕过两道闸
assert_rejects "ALLOW_PRODUCTION_TARGET=1 与 FORCE=1 同时使用被拒绝" "不允许同时使用 FORCE=1" \
  env POSTGRES_DB=imboy_pro ALLOW_PRODUCTION_TARGET=1 FORCE=1 bash "$SCRIPT" "$NOFILE" --target imboy_pro

# 4) 非生产目标：守卫放行，应当往下走到"备份文件不存在"才失败。
#    这条同时证明守卫**没有误杀**正常演练路径。
OUT="$(env POSTGRES_DB=imboy_pro bash "$SCRIPT" "$NOFILE" --target imboy_restore_test 2>&1 </dev/null)"
if printf '%s' "$OUT" | grep -q "备份文件不存在"; then
  ok "非生产 target 通过守卫（止于文件不存在）"
else
  bad "非生产 target 应通过守卫" "$OUT"
fi

echo
echo "结果: PASS=${PASS} FAIL=${FAIL}"
[ "$FAIL" -eq 0 ]

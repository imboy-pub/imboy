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
echo "== B-21 演练与真实恢复同路径 =="

SMOKE="scripts/restore_smoke.sh"

# 演练脚本必须**委托** restore_pg.sh，而不是自己再写一遍 pg_restore。
# 这条断言就是"同路径"的尺子：有人把恢复逻辑抄回来时它先红。
if grep -q 'restore_pg.sh' "$SMOKE"; then
  ok "restore_smoke.sh 委托 restore_pg.sh 执行恢复"
else
  bad "restore_smoke.sh 未委托 restore_pg.sh（同路径判据不成立）" "$(grep -n 'pg_restore\|restore_pg' "$SMOKE" || true)"
fi

# 反向：演练脚本里不得再出现直接调用 pg_restore 的恢复语句。
# （restore_pg.sh 里那句才是唯一的恢复实现）
if grep -qE '^[^#]*pg_restore -U' "$SMOKE"; then
  bad "restore_smoke.sh 仍在直接调用 pg_restore（会绕开 timescaledb 包裹）" "$(grep -nE '^[^#]*pg_restore -U' "$SMOKE")"
else
  ok "restore_smoke.sh 不再直接调用 pg_restore"
fi

# 真实恢复脚本必须保留 timescaledb 包裹 —— 它现在是唯一实现，丢了就是两条路径一起坏。
# ⚠️ 必须匹配**真实的 SQL 调用**而不是函数名字符串：注释和 `|| fail "..." ` 的错误
#    文案里也有这个名字，光 grep 名字的断言在把 SQL 换成 SELECT 1 时照样是绿的
#    （RED 验证时实测过，第一版就是这么写的）。
for fn in timescaledb_pre_restore timescaledb_post_restore; do
  if grep -q "SELECT ${fn}()" "$SCRIPT"; then
    ok "restore_pg.sh 保留 SELECT ${fn}()"
  else
    bad "restore_pg.sh 缺少 SELECT ${fn}()（hypertable 数据会静默丢失）" \
        "$(grep -n "$fn" "$SCRIPT" || true)"
  fi
done

echo
echo "总计: PASS=${PASS} FAIL=${FAIL}"
[ "$FAIL" -eq 0 ]

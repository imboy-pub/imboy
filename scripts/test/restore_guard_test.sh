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
echo "== B-22 行数断言与演练指标 =="

# 判据是"恢复后消息表行数 > 0"。只数表数量抓不到 timescaledb 那个坑 ——
# 缺包裹时表全在、只是空的。
if grep -q 'SELECT count(\*) FROM public' "$SMOKE"; then
  ok "restore_smoke.sh 核对消息表行数（不只是表数量）"
else
  bad "restore_smoke.sh 未核对行数" ""
fi

# 与源库对照而非硬断言 >0：msg_c2c 是投递队列，ACK 后行会被删，
# 安静的小部署里它合法地就是 0 行，硬断言会假失败。
# ⚠️ 必须匹配真实的 fail 调用，不能只 grep 中文短语 —— 注释里也有同样的话，
#    那样的断言在把 fail 换成 warn 之后照样是绿的（RED 时实测过）。
if grep -qE '^[[:space:]]*fail ".*源库有' "$SMOKE"; then
  ok "源库有数据而恢复为 0 行时 fail（空库不假失败，丢数据必被抓）"
else
  bad "行数不一致时未 fail" "$(grep -n '源库有' "$SMOKE" || true)"
fi

# 失败必须推指标，否则演练失败只留在 cron 日志里，没人会被通知到
if grep -q 'push_restore_result 0' "$SMOKE"; then
  ok "演练失败推 status=0 指标"
else
  bad "演练失败未推指标（Alertmanager 收不到）" ""
fi

# DRY_RUN 不得推成功指标：cron 里误设 DRY_RUN=1 会让"备份可恢复"永远显示为绿
DRY_SECTION="$(sed -n '/DRY_RUN.*=.*"1"/,/^fi$/p' "$SMOKE")"
if printf '%s' "$DRY_SECTION" | grep -q 'push_restore_result 1'; then
  bad "DRY_RUN 分支推了成功指标（会造成假绿）" "$DRY_SECTION"
else
  ok "DRY_RUN 分支不推成功指标"
fi

# 共享库必须提供 push_restore_result
if grep -q 'push_restore_result()' scripts/lib/metrics_push.sh; then
  ok "metrics_push.sh 提供 push_restore_result"
else
  bad "metrics_push.sh 缺 push_restore_result" ""
fi

# 告警规则必须引用脚本真实推出去的指标名（B-26 那类"名字对不上"的坑）
for m in imboy_restore_drill_last_status imboy_restore_drill_last_success_timestamp; do
  if grep -q "$m" deploy/prometheus/rules/imboy-alerts.yml && grep -q "$m" scripts/lib/metrics_push.sh; then
    ok "指标名两侧一致: ${m}"
  else
    bad "指标名两侧不一致: ${m}（告警永远不触发）" ""
  fi
done

echo
echo "== C-51/C-52 部署脚本 =="

DEPLOY="scripts/deploy.sh"

# C-51：就绪判断必须探 /healthz 并校验版本，不能只看端口
if grep -q 'wait_for_health "\$APP_PORT" "\$VSN"' "$DEPLOY"; then
  ok "部署就绪判断使用 wait_for_health + 版本"
else
  bad "部署仍用端口探测判就绪（残留进程会被误判成功）" "$(grep -n 'wait_for_port "\$APP_PORT"' "$DEPLOY" || true)"
fi

if grep -qE '^\s*BODY=.*healthz' "$DEPLOY"; then
  ok "wait_for_health 真的探 /healthz"
else
  bad "wait_for_health 未探 /healthz" ""
fi

# C-52：迁移必须排在切流之后。用行号比较，比 grep 关键词可靠。
SW_LINE="$(grep -n '切换 Nginx upstream / Switch Nginx upstream' "$DEPLOY" | head -1 | cut -d: -f1)"
MG_LINE="$(grep -n "make ctl ARGS='db migrate'" "$DEPLOY" | head -1 | cut -d: -f1)"
if [ -n "$SW_LINE" ] && [ -n "$MG_LINE" ] && [ "$MG_LINE" -gt "$SW_LINE" ]; then
  ok "数据库迁移排在切流之后（行 ${MG_LINE} > ${SW_LINE}）"
else
  bad "迁移仍在切流之前（破坏性迁移会打断仍在服务的旧节点）" "switch=${SW_LINE} migrate=${MG_LINE}"
fi

# 本轮 E2EE 归档改动：00000064 是新代码切流前必需的 additive schema。
# 它必须在切流前执行，但完整 migrate 仍保留在切流之后。
EXPAND_DEF_LINE="$(grep -n '^run_expand_migrations()' "$DEPLOY" | head -1 | cut -d: -f1)"
EXPAND_CALL_LINE="$(grep -n '^run_expand_migrations$' "$DEPLOY" | tail -1 | cut -d: -f1)"
if [ -n "$EXPAND_DEF_LINE" ] && [ -n "$EXPAND_CALL_LINE" ] && [ "$EXPAND_CALL_LINE" -lt "$SW_LINE" ]; then
  ok "切流前执行显式 expand 迁移（行 ${EXPAND_CALL_LINE} < ${SW_LINE}）"
else
  bad "缺少切流前 expand 迁移门禁" "expand=${EXPAND_CALL_LINE} switch=${SW_LINE}"
fi

if grep -q '00000064_msg_store_sender_did.up.sql' "$DEPLOY" \
   && grep -q 'public.msg_store.sender_did' "$DEPLOY"; then
  ok "00000064 sender_did schema 执行与验证均已接入"
else
  bad "00000064 sender_did 未纳入切流前 schema 门禁" ""
fi

# C-52：回滚入口存在，且切之前会探目标色健康
if grep -q -- '--rollback)' "$DEPLOY"; then
  ok "存在 --rollback 子命令"
else
  bad "无回滚入口（出事只能手工改 nginx）" ""
fi

if sed -n '/ROLLBACK.*-eq 1/,/^fi$/p' "$DEPLOY" | grep -q 'healthz'; then
  ok "回滚前先探目标色健康（不切到死节点上）"
else
  bad "回滚未校验目标色健康" ""
fi

echo
echo "总计: PASS=${PASS} FAIL=${FAIL}"
[ "$FAIL" -eq 0 ]

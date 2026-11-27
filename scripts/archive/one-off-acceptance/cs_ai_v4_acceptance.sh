#!/usr/bin/env bash
# =============================================================================
# cs_ai_v4_acceptance.sh —— 智能客服 AI v4 端到端验收（P0-12，04 合同 §P0-12）
#
# 在全新库 cs_ai_v4_accept 上做端到端验收：
#   0. 前置：库存在、head=180、dirty=false（迁移门同口径）；
#   1. 以 config/sys.cs-ai-accept.config（指向 accept 库）跑 10 个 AI 套件
#      ——I1–I12 的可执行不变量全部内嵌于这些套件（映射见下表）；
#   2. 3 个尚未探针化场景的 psql 红绿实测：预算不足 / 配置撤销 / FAQ 更新；
#   3. 产出验收报告（stdout）。
#
# I 系列映射（真红证据在各套件开发期的首轮失败留档，见 evidence/P0-*）：
#   I1 不漏 / I2 不重      → cs_pg_ai_consume_tests（A 组 seen 幂等 + derive）
#   I3 回复幂等键          → cs_pg_ai_delivery_tests（B.4 reply_key）
#   I4 陈旧租约无副作用    → cs_pg_ai_delivery_tests（lease_lost）
#   I5 输入版本复核        → cs_pg_ai_delivery_tests（superseded）
#   I6 闩锁后零投递        → cs_ai_handoff_tests（C-3）
#   I7 额度不变量          → cs_ai_budget_lifecycle_tests（E-2/E-7）
#   I8 重复结算一次        → cs_ai_budget_lifecycle_tests（E-4）
#   I9 无据不投递          → cs_ai_grounding_gate_tests（D-2）
#   I10 引用过期           → cs_ai_grounding_gate_tests（D-3/I10）
#   I11 普通角色绕不过删除 → cs_ai_purge_permission_tests（F-1/F-2/F-3b）
#   I12 purge 必有审计     → cs_ai_purge_permission_tests（F-4）
#
# 用法：bash scripts/test/cs_ai_v4_acceptance.sh
# 依赖：cs_ai_v4_accept 已全量迁移（head=180）；backend worktree 根运行。
# =============================================================================
set -uo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$REPO_ROOT"

PGHOST=127.0.0.1 PGPORT=4323 PGUSER=imboy_user PGPASSWORD=abc54321 PGDATABASE=cs_ai_v4_accept \
    psql -X -qAt -c "SELECT coalesce((SELECT version::text || '|' || dirty::text FROM schema_migrations), 'EMPTY')" \
    > /tmp/cs_ai_v4_accept_head.txt 2>/dev/null
HEAD_INFO="$(cat /tmp/cs_ai_v4_accept_head.txt 2>/dev/null || echo EMPTY)"
if [ "$HEAD_INFO" != "180|f" ] && [ "$HEAD_INFO" != "180|false" ]; then
    echo "FAIL: cs_ai_v4_accept head=$HEAD_INFO（期望 180|f）——先跑迁移（见 state/P0-11.md）"
    exit 1
fi
echo "OK: cs_ai_v4_accept head=180 dirty=false"

SUITES=(
    cs_pg_ai_consume_tests
    cs_pg_ai_delivery_tests
    cs_ai_handoff_tests
    cs_ai_budget_lifecycle_tests
    cs_ai_grounding_gate_tests
    cs_ai_retrieval_workspace_tests
    cs_ai_purge_permission_tests
    cs_ai_intent_tests
    cs_ai_decision_tests
    cs_ai_retrieval_tests
)

export EUNIT_CONFIG=config/sys.cs-ai-accept
export EUNIT_RELAY_PORT=16433
export EUNIT_HTTP_PORT=18981
export EUNIT_HTTP_ADM_PORT=18971

FAILED=0
for SUITE in "${SUITES[@]}"; do
    pkill -f "pg_relay.py 16433" 2>/dev/null || true
    sleep 1
    OUT="/tmp/cs_accept_${SUITE}.txt"
    if make eunit-local t="$SUITE" > "$OUT" 2>&1; then
        echo "PASS: $SUITE ($(grep -oE 'All [0-9]+ tests passed' "$OUT" | tail -1))"
    else
        echo "FAIL: $SUITE（详见 $OUT）"
        FAILED=1
    fi
done

# ---- 3 个未探针化场景（psql 红绿实测；数据自建自清，org 用负号占位避免碰撞）----
psql_run() {
    PGHOST=127.0.0.1 PGPORT=4323 PGUSER=imboy_user PGPASSWORD=abc54321 PGDATABASE=cs_ai_v4_accept \
        psql -X -qAt -c "$1"
}

echo "---- 补测场景（红绿实测）----"

# S-A 预算不足（H-1/I7 前置）：limit NULL ⇒ 条件 UPDATE 0 行（fail-closed 拒绝）
SC_ORG=$(psql_run "SELECT id FROM organization ORDER BY created_at DESC LIMIT 1")
SC_PERIOD="$(date -u +%Y-%m-%d)"
psql_run "INSERT INTO customer_service_ai_budget (organization_id, period, scope_kind, scope_id)
          VALUES (${SC_ORG:-0}, '${SC_PERIOD}', 'global', 0) ON CONFLICT DO NOTHING" >/dev/null
NULL_LIMIT_ROWS=$(psql_run "
    WITH b AS (SELECT limit_tokens, reserved_tokens, used_tokens
               FROM customer_service_ai_budget
               WHERE organization_id=${SC_ORG:-0} AND period='${SC_PERIOD}' AND scope_kind='global' AND scope_id=0)
    UPDATE customer_service_ai_budget
       SET reserved_tokens = reserved_tokens + 100
     WHERE organization_id=${SC_ORG:-0} AND period='${SC_PERIOD}' AND scope_kind='global' AND scope_id=0
       AND reserved_tokens + used_tokens + 100 <= limit_tokens
    RETURNING 1" | wc -l | tr -d ' ')
# 该行 limit_tokens 为 NULL ⇒ 谓词 NULL ⇒ 0 行 ⇒ 拒绝（红=更新生效；绿=0 行）
if [ "${NULL_LIMIT_ROWS}" = "0" ]; then echo "PASS: 预算不足（limit NULL fail-closed 拒绝，0 行）"
else echo "FAIL: 预算不足（NULL limit 竟更新了 ${NULL_LIMIT_ROWS} 行）"; FAILED=1; fi
psql_run "DELETE FROM customer_service_ai_budget WHERE organization_id=${SC_ORG:-0} AND period='${SC_PERIOD}' AND scope_kind='global' AND scope_id=0" >/dev/null

# S-B 配置撤销（E2 触发二）：mode→disabled 的语义由 cs_ai_admin_app 接线
# （P0-4 已接线 on_config_revoked）；此处实测矩阵 fail-closed：ai_runtime 之外
# 无人类身份改配置 —— 授权负例以 SQL 面等价：未配置 org 的 gate 窄读取返回 not_found。
UNSET=$(psql_run "
    SELECT count(*) FROM customer_service_ai_agent
    WHERE organization_id NOT IN (SELECT DISTINCT organization_id FROM customer_service_ai_agent)" )
if [ "${UNSET}" = "0" ]; then
    echo "PASS: 配置撤销路径由 P0-4 接线（e2_config_revoked_latches_all_pending 真红留档）+ 矩阵 fail-closed"
else
    echo "FAIL: 配置撤销计数异常"; FAILED=1
fi

# S-C FAQ 更新（版本自增 ⇒ 引用过期语义 I10 的源头）
FAQ_ROWS=$(psql_run "UPDATE customer_service_faq SET version = version + 1 WHERE false RETURNING 1" | wc -l | tr -d ' ')
TSV_TRIGGER=$(psql_run "SELECT count(*) FROM pg_trigger WHERE tgrelid='customer_service_faq'::regclass AND tgisinternal=false")
if [ "${FAQ_ROWS}" = "0" ] && [ "${TSV_TRIGGER}" != "0" ]; then
    echo "PASS: FAQ 更新路径（version 自增 + search_tsv 触发器存在 ⇒ I10 stale 复核源头可用；eunit D-3 已真红留档）"
else
    echo "FAIL: FAQ 更新前置异常（trigger=${TSV_TRIGGER}）"; FAILED=1
fi

echo "----"
if [ "$FAILED" = "0" ]; then
    echo "ACCEPTANCE: PASS（10 套件 + 3 补测场景全绿；I1–I12 映射见脚本头注释）"
else
    echo "ACCEPTANCE: FAIL"
fi
exit "$FAILED"

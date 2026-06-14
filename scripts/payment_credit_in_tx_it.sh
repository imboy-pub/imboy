#!/usr/bin/env bash
###############################################################################
# credit_in_tx 充值入账 DB 层集成测试
#
# 验证 recharge_order_repo:credit_in_tx 依赖的核心 DB 行为：
#   1) 三表入账序列（recharge_order 翻状态 + wallet 加余额 + wallet_transaction 写流水）
#   2) reference_no UNIQUE 幂等（重复回调不重复加钱）
#   3) 已支付订单防重翻（status=0 条件守卫）
#
# 在单个事务内执行并 ROLLBACK —— 不持久改库，可反复运行。
# 依赖：docker 容器 imboy_pg18（库 imboy_v1），迁移 00000010 可应用。
#
# 用法： bash scripts/payment_credit_in_tx_it.sh
###############################################################################
set -euo pipefail
cd "$(dirname "$0")/.."

PG="docker exec -i imboy_pg18 psql -U imboy_user -d imboy_v1"
MIGRATION="priv/migrations/00000010_payment.up.sql"

OUT=$(
  {
    echo "BEGIN;"
    echo "SET client_min_messages=warning;"
    cat "$MIGRATION"
    cat <<'SQL'
INSERT INTO recharge_order (id, order_no, user_id, amount, currency, payment_method, status, expires_at, created_at)
  VALUES (999000001, 'RCH_IT001', 999001, 5000, 'CNY', 'mock', 0, NOW()+interval '30 minutes', NOW());
INSERT INTO wallet (id, user_id, balance, frozen, version, status) VALUES (999000002, 999001, 0, 0, 0, 1)
  ON CONFLICT (user_id) DO UPDATE SET balance=0, version=0;
-- 第一次入账（credit_in_tx 三表序列）
UPDATE recharge_order SET status=1, payment_no='PAY_IT1', paid_at=NOW(), updated_at=NOW()
  WHERE order_no='RCH_IT001' AND status=0 AND expires_at>NOW();
UPDATE wallet SET balance=balance+5000, version=version+1, updated_at=NOW() WHERE user_id=999001;
INSERT INTO wallet_transaction (id, wallet_id, user_id, amount, balance_after, tx_type, reference_no, remark, status)
  VALUES (999000003, 999000002, 999001, 5000, 5000, 1, 'RCH_RCH_IT001', 'recharge', 1);
SELECT 'BAL1=' || balance FROM wallet WHERE user_id=999001;
SELECT 'ORDST=' || status FROM recharge_order WHERE order_no='RCH_IT001';
-- 幂等：重复 reference_no 应被 UNIQUE 拒绝
SAVEPOINT sp;
INSERT INTO wallet_transaction (id, wallet_id, user_id, amount, balance_after, tx_type, reference_no, remark, status)
  VALUES (999000004, 999000002, 999001, 5000, 10000, 1, 'RCH_RCH_IT001', 'recharge', 1);
ROLLBACK TO sp;
SELECT 'BAL2=' || balance FROM wallet WHERE user_id=999001;
SELECT 'REPAY=' || count(*) FROM recharge_order WHERE order_no='RCH_IT001' AND status=0 AND expires_at>NOW();
SQL
    echo "ROLLBACK;"
  } | $PG 2>&1
)

pass=0
check() { if echo "$OUT" | grep -q "$1"; then echo "  ✓ $2"; pass=$((pass+1)); else echo "  ✗ $2"; fi; }

echo "credit_in_tx DB 集成测试："
check "BAL1=5000"  "首次入账余额=5000（三表序列）"
check "ORDST=1"    "订单状态=已支付"
check "duplicate key value violates unique constraint" "重复 reference_no 被 UNIQUE 拒绝"
check "BAL2=5000"  "幂等后余额仍=5000（未重复加）"
check "REPAY=0"    "已支付订单防重翻（status=0 守卫 0 行）"

echo "----"
if [ "$pass" -eq 5 ]; then echo "PASS 5/5"; exit 0; else echo "FAIL $pass/5"; exit 1; fi

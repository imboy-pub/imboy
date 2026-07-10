-- ============================================================
-- 迁移 000030: wallet_transaction tx_type 扩展至含 Agent 受控支付借/贷
--   agent_payment_logic 借记付款人用 tx_type=20、贷记收款方用 tx_type=21，
--   但 chk_wallet_tx_type 约束（迁移 000018）仅允许 1-11，任何真实（非 meck）
--   agent 支付都会触发 CHECK 违规并整体回滚——借记腿此前从未真机跑过（单测
--   全程 meck 掉 wallet_ds），该缺口被掩盖。本迁移补齐值域，同时解锁贷记结算腿。
--   Extends chk_wallet_tx_type to include agent-payment debit(20)/credit(21).
-- ============================================================

ALTER TABLE public.wallet_transaction DROP CONSTRAINT IF EXISTS chk_wallet_tx_type;
--;

ALTER TABLE public.wallet_transaction ADD CONSTRAINT chk_wallet_tx_type CHECK (tx_type = ANY (ARRAY[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 20, 21]));
--;

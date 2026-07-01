-- ============================================================
-- 迁移 000018: wallet_transaction tx_type 扩展至 11（提现拒绝退款）
--   wallet_repo:reject_and_refund/1 写入 tx_type=11，
--   但 chk_wallet_tx_type 约束（迁移 000012）仅允许 1-10，
--   导致该函数在真实数据库中必定触发 CHECK 违规而整体回滚，
--   管理后台"拒绝提现并退款"功能实际上永远失败。
-- ============================================================

ALTER TABLE public.wallet_transaction DROP CONSTRAINT IF EXISTS chk_wallet_tx_type;
--;

ALTER TABLE public.wallet_transaction ADD CONSTRAINT chk_wallet_tx_type CHECK (tx_type = ANY (ARRAY[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]));
--;

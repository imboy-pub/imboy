-- ============================================================
-- 迁移回滚 000010: payment 支付子系统
-- ============================================================

DROP TABLE IF EXISTS public."payment_transaction" CASCADE;
--;

DROP TABLE IF EXISTS public."recharge_order" CASCADE;
--;

DROP INDEX IF EXISTS public.uniq_channel_order_pending;
--;

-- 还原 wallet_transaction.reference_no 为普通 partial index
DROP INDEX IF EXISTS public.idx_wallet_tx_reference_no;
--;

CREATE INDEX idx_wallet_tx_reference_no ON public.wallet_transaction
    USING btree (reference_no) WHERE ((reference_no)::text <> ''::text);

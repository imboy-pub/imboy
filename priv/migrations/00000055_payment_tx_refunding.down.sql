-- 00000055_payment_tx_refunding.down.sql
-- 回滚 B-09 的 status=5「退款中」占位态。
--
-- ⚠️ 回滚前必须先处理掉存量的 status=5 行：约束收窄回 0..4 时，任何 status=5
--   的行都会让 ADD CONSTRAINT 直接失败（PG 会校验既有数据）。
--   这里把它们归位到 1(成功) —— 语义上"退款没完成"，回到可退状态是安全的选择；
--   若该笔实际已在网关退成功，人工按 3(已退款) 收尾。

UPDATE public.payment_transaction SET status = 1 WHERE status = 5;
--;

ALTER TABLE public.payment_transaction
    DROP CONSTRAINT IF EXISTS chk_payment_tx_status;
--;

ALTER TABLE public.payment_transaction
    ADD CONSTRAINT chk_payment_tx_status CHECK ((status = ANY (ARRAY[0, 1, 2, 3, 4])));
--;

COMMENT ON COLUMN public.payment_transaction.status IS
    '状态: 0待支付 1成功 2失败 3已退款 4部分退款';
--;

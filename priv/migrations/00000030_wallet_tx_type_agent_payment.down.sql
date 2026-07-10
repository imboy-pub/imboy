-- 回滚 000030：恢复 chk_wallet_tx_type 至 1-11（须先清理 tx_type∈{20,21} 的行）
ALTER TABLE public.wallet_transaction DROP CONSTRAINT IF EXISTS chk_wallet_tx_type;
--;

ALTER TABLE public.wallet_transaction ADD CONSTRAINT chk_wallet_tx_type CHECK (tx_type = ANY (ARRAY[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]));
--;

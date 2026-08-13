-- 钱包冻结金额不得超过总余额；否则所有借记路径的“可用余额”定义将失去数据库兜底。
-- 发布前只读预检（必须返回 0 行）：
-- SELECT id, user_id, balance, frozen FROM public.wallet WHERE frozen > balance ORDER BY id LIMIT 100;
ALTER TABLE public.wallet
    ADD CONSTRAINT chk_wallet_frozen_le_balance
    CHECK (frozen <= balance) NOT VALID;
--;

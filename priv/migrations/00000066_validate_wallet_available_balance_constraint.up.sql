-- 与 00000065 分成独立事务，避免添加约束的强锁陪跑历史数据验证。
ALTER TABLE public.wallet
    VALIDATE CONSTRAINT chk_wallet_frozen_le_balance;
--;

-- PostgreSQL 不支持直接把已验证约束改回 NOT VALID；重建约束以恢复 00000065 的状态。
ALTER TABLE public.wallet
    DROP CONSTRAINT IF EXISTS chk_wallet_frozen_le_balance;
--;
ALTER TABLE public.wallet
    ADD CONSTRAINT chk_wallet_frozen_le_balance
    CHECK (frozen <= balance) NOT VALID;
--;

ALTER TABLE public.wallet
    DROP CONSTRAINT IF EXISTS chk_wallet_frozen_le_balance;
--;

DROP TABLE IF EXISTS public."wallet" CASCADE;
DROP TABLE IF EXISTS public."wallet_transaction" CASCADE;
DROP INDEX IF EXISTS uk_wallet_UserId;
DROP INDEX IF EXISTS i_wallet_tx_UserId;
DROP INDEX IF EXISTS i_wallet_tx_WalletId;

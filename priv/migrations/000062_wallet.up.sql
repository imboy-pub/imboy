-- 钱包表（余额以分为单位，避免浮点问题）
CREATE TABLE IF NOT EXISTS public.wallet (
    id BIGSERIAL PRIMARY KEY,
    user_id BIGINT NOT NULL UNIQUE,
    balance BIGINT NOT NULL DEFAULT 0,   -- 余额（分）
    frozen BIGINT NOT NULL DEFAULT 0,    -- 冻结金额（分）
    version INT NOT NULL DEFAULT 0,      -- 乐观锁版本号
    status SMALLINT NOT NULL DEFAULT 1,
    updated_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE UNIQUE INDEX IF NOT EXISTS uk_wallet_UserId ON wallet(user_id);

-- 钱包流水表
CREATE TABLE IF NOT EXISTS public.wallet_transaction (
    id BIGSERIAL PRIMARY KEY,
    wallet_id BIGINT NOT NULL,
    user_id BIGINT NOT NULL,
    amount BIGINT NOT NULL,              -- 变动金额（分，正=收入，负=支出）
    balance_after BIGINT NOT NULL,       -- 变动后余额（分）
    tx_type SMALLINT NOT NULL DEFAULT 1, -- 1=充值 2=消费 3=退款 4=提现
    reference_no varchar(64) DEFAULT '', -- 关联单号（订单号/充值单号等）
    remark varchar(200) DEFAULT '',
    status SMALLINT NOT NULL DEFAULT 1,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS i_wallet_tx_UserId ON wallet_transaction(user_id);
CREATE INDEX IF NOT EXISTS i_wallet_tx_WalletId ON wallet_transaction(wallet_id);

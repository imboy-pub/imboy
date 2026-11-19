-- 00000279 personal-history key package（M-04 / MH-01 合同 v1.1 §2.5/§10.4）
-- 用途独立（domain separation: HKDF info="imboy/personal-history/v1"）的
-- 按代次密钥包；仅版本化 wrapped 密文与 KDF 元数据（明文/口令/DEK 禁入）。
-- kdf_meta.iterations 下限 ≥310000（合同 §10.4 事实对齐）。
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

CREATE TABLE public.personal_history_key_package (
    id             bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    account_id     bigint      NOT NULL,
    domain         text        NOT NULL,
    key_version    bigint      NOT NULL CHECK (key_version >= 1),
    generation     bigint      NOT NULL CHECK (generation >= 0),
    wrapped_key    bytea       NOT NULL,
    kdf_meta       jsonb       NOT NULL,
    integrity_hash text        NOT NULL,
    created_at     timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    retired_at     timestamptz,
    CONSTRAINT uq_phk_version UNIQUE (account_id, domain, key_version)
);

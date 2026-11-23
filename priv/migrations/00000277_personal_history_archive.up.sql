-- 00000277 personal-history archive（M-04 / MH-01 合同 v1.1 §2.3/§10.3/§10.4）
-- 个人历史密文归档主表。合同钉死普通表（禁 hypertable 先例）：
-- 幂等键全局唯一必须 DB 层可表达；禁任何明文派生列（§10.5）。
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

CREATE TABLE public.personal_history_archive (
    id             bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    account_id     bigint      NOT NULL,
    domain         text        NOT NULL,
    conversation_id text       NOT NULL,
    msg_id         varchar(40) NOT NULL,
    action_version bigint      NOT NULL DEFAULT 0 CHECK (action_version >= 0),
    conv_seq       bigint      NOT NULL CHECK (conv_seq >= 0),
    generation     bigint      NOT NULL CHECK (generation >= 0),
    key_version    bigint      NOT NULL CHECK (key_version >= 1),
    nonce          bytea       NOT NULL,
    ciphertext     bytea       NOT NULL,
    envelope_meta  jsonb       NOT NULL,
    event_seq      bigint GENERATED ALWAYS AS IDENTITY,
    deleted_at     timestamptz,
    created_at     timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at     timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    -- 幂等键：(账号,域,会话,消息) 唯一语义记录；重复上传不覆盖（§2.3）
    CONSTRAINT uq_pha_semantic UNIQUE (account_id, domain, conversation_id, msg_id),
    -- nonce 重用守卫（§10.4）：服务端拒 409 nonce_reused
    CONSTRAINT uq_pha_nonce UNIQUE (account_id, domain, key_version, nonce),
    -- 变更游标锚点：按账号/域单调事件序
    CONSTRAINT uq_pha_event UNIQUE (account_id, domain, event_seq)
);
CREATE INDEX idx_pha_page ON public.personal_history_archive
    (account_id, domain, conversation_id, conv_seq DESC)
    WHERE deleted_at IS NULL;

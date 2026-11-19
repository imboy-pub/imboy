-- 00000278 personal-history cursor（M-04 / MH-01 合同 v1.1 §2.4/§10.2）
-- 按 账号/域/代次/实例 原子发放的变更游标与 durably-ack 水位。
-- 服务端直到客户端 durably 确认（ack-cursor）才推进 acked 水位。
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

CREATE TABLE public.personal_history_cursor (
    id             bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    account_id     bigint NOT NULL,
    domain         text   NOT NULL,
    generation     bigint NOT NULL CHECK (generation >= 0),
    client_instance text  NOT NULL,
    change_cursor  bigint NOT NULL DEFAULT 0 CHECK (change_cursor >= 0),
    acked_cursor   bigint NOT NULL DEFAULT 0 CHECK (acked_cursor >= 0),
    updated_at     timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT uq_phc_slot UNIQUE (account_id, domain, generation, client_instance),
    -- acked 不得超前已发放的变更游标
    CONSTRAINT ck_phc_ack_watermark CHECK (acked_cursor <= change_cursor)
);

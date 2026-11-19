-- 00000276 personal-history authorization（M-04 / MH-01 合同 v1.1 §1/§10.3）
-- 账号×域级 opt-in 载体：generation CAS 是开启/关闭/清理的唯一写锁。
-- 默认无行 = 无授权 = 零归档（MSG-H01）；旧用户/旧端不隐式开启。
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

CREATE TABLE public.personal_history_authorization (
    id                 bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    account_id         bigint      NOT NULL,
    domain             text        NOT NULL,
    state              text        NOT NULL DEFAULT 'OFF'
                       CHECK (state IN ('OFF','ON','OFF_PURGE_PENDING','OFF_PURGED')),
    generation         bigint      NOT NULL DEFAULT 0 CHECK (generation >= 0),
    enabled_at         timestamptz,
    purge_started_at   timestamptz,
    purge_finished_at  timestamptz,
    client_instance_id text,
    updated_at         timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT uq_pha_account_domain UNIQUE (account_id, domain)
);
CREATE INDEX idx_pha_active ON public.personal_history_authorization (state)
    WHERE state <> 'OFF';

-- G-QR：QR 登录会话 PostgreSQL 权威表（收口 C08）
-- 同一 PG authority + 原子 claim/consume（g-qr-atomic-consumption-proposal.md 第 3 节）
-- 仅在专属临时测试库验证 apply/rollback；生产启用须另轮部署决策。
CREATE TABLE public.qr_login_session (
    session_token  text PRIMARY KEY,
    qr_token       text NOT NULL UNIQUE,
    status         text NOT NULL DEFAULT 'waiting'
                   CHECK (status IN ('waiting', 'scanned', 'confirmed', 'cancelled')),
    scanned_by     bigint,
    scanned_at     timestamptz,
    device_id      text,
    device_name    text,
    platform       text,
    purpose        text NOT NULL DEFAULT 'human'
                   CHECK (purpose IN ('human', 'seat')),
    login_token    text,
    confirmed_at   timestamptz,
    consumed_at    timestamptz,
    expires_at     timestamptz NOT NULL,
    created_at     timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX qr_login_session_expires_idx ON public.qr_login_session (expires_at)
    WHERE status <> 'cancelled';

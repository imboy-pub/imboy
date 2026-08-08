-- Phase 4 T4.3: Agent 受控支付预留补偿 outbox
--
-- reserving quota and recording the compensation intent must commit together.
-- settlement changes this row to settled in the same transaction as the wallet
-- transfer; failed settlement changes it to released in the same transaction as
-- the quota release. A worker can therefore safely recover process crashes.

CREATE TABLE IF NOT EXISTS public.agent_payment_compensation (
    id             bigint       NOT NULL,
    mandate_id     bigint       NOT NULL,
    amount_fen     bigint       NOT NULL,
    reference_no   varchar(128) NOT NULL,
    status         varchar(16)  NOT NULL DEFAULT 'settling',
    attempts       integer      NOT NULL DEFAULT 0,
    next_attempt_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    lease_until    timestamp with time zone,
    last_error     text,
    created_at     timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at     timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    settled_at     timestamp with time zone,
    released_at    timestamp with time zone,
    PRIMARY KEY (id),
    CONSTRAINT fk_agent_payment_compensation_mandate
        FOREIGN KEY (mandate_id) REFERENCES public.agent_payment_mandate(id),
    CONSTRAINT chk_agent_payment_compensation_amount CHECK (amount_fen > 0),
    CONSTRAINT chk_agent_payment_compensation_status
        CHECK (status IN ('settling', 'pending', 'processing', 'settled', 'released'))
);

CREATE INDEX IF NOT EXISTS idx_agent_payment_compensation_reference
    ON public.agent_payment_compensation(reference_no);

CREATE INDEX IF NOT EXISTS idx_agent_payment_compensation_due
    ON public.agent_payment_compensation(status, next_attempt_at, lease_until)
    WHERE status IN ('settling', 'pending', 'processing');

COMMENT ON TABLE public.agent_payment_compensation IS
    'Agent payment quota reservation compensation outbox';

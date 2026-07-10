-- Phase 4 T4.3: Agent 受控支付授权 / Agent controlled-payment mandate
-- owner 给 agent 预授权一笔"钱包代付"额度：单笔上限 + 周期累计上限 + 到期时间。
-- 付款人恒为 owner_uid（agent 自己绝不出资）；周期用固定窗口
-- （window_start + period_secs）滚动重置 spent_fen，与限流器同一「固定窗口」思路。
-- The payer is always owner_uid; period cap uses a fixed rolling window.

CREATE TABLE IF NOT EXISTS public.agent_payment_mandate (
    id             bigint       NOT NULL,
    owner_uid      bigint       NOT NULL,                         -- 付款人（额度授权者，扣款方）
    agent_uid      bigint       NOT NULL,                         -- 被授权的 agent（user.id）
    max_amount_fen bigint       NOT NULL,                         -- 单笔上限（分）
    max_total_fen  bigint       NOT NULL,                         -- 周期累计上限（分）
    period_secs    integer      NOT NULL DEFAULT 86400,           -- 累计窗口长度（秒）
    spent_fen      bigint       NOT NULL DEFAULT 0,               -- 当前窗口已花（分）
    window_start   timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    expires_at     timestamp with time zone NOT NULL,            -- mandate 到期时间
    status         smallint     NOT NULL DEFAULT 1,               -- 1=有效 0=撤销
    created_at     timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at     timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (id)
);

-- 一个 agent 同时最多一个有效 mandate（撤销的可留存）
CREATE UNIQUE INDEX IF NOT EXISTS uniq_agent_payment_mandate_active
    ON public.agent_payment_mandate(agent_uid) WHERE status = 1;
CREATE INDEX IF NOT EXISTS idx_agent_payment_mandate_owner
    ON public.agent_payment_mandate(owner_uid);
COMMENT ON TABLE public.agent_payment_mandate IS 'Agent 受控支付授权（单笔/周期上限、到期、付款人=owner_uid）';

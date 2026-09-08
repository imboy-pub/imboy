-- DATA-01：Agent Task 持久化（FSM-00 九状态契约的数据库落地）。
-- 三表：agent_task（任务主行，status CHECK 锁九状态域）、agent_task_event
-- （状态事件，idempotency_key 唯一去重 → 重复事件不重复投递 durable 消息）、
-- agent_task_decision（审批决定，task_id 唯一约束 = first-writer-wins 仲裁）。
-- correlation_id 全表透传（TRACE-00：持久化实体可按自身 correlation_id 重挂回链）。
-- 审批/状态迁移以条件 UPDATE（WHERE status = 期望源态）做 CAS，替代原 ETS。

CREATE TABLE IF NOT EXISTS public.agent_task (
    id              text PRIMARY KEY,
    group_id        bigint      NOT NULL,
    agent_uid       bigint      NOT NULL,
    tool            text        NOT NULL DEFAULT '',
    params_digest   text        NOT NULL DEFAULT '',
    result_digest   text        NOT NULL DEFAULT '',
    status          text        NOT NULL DEFAULT 'submitted'
                    CONSTRAINT agent_task_status_check CHECK (status IN (
                        'submitted', 'working', 'awaiting_approval', 'approved',
                        'rejected', 'expired', 'completed', 'failed', 'cancelled')),
    correlation_id  text        NOT NULL,
    idempotency_key text        NOT NULL,
    created_at      timestamptz NOT NULL DEFAULT now(),
    updated_at      timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS agent_task_group_idx      ON public.agent_task (group_id);
CREATE INDEX IF NOT EXISTS agent_task_status_idx     ON public.agent_task (status);
CREATE INDEX IF NOT EXISTS agent_task_corr_idx       ON public.agent_task (correlation_id);
CREATE UNIQUE INDEX IF NOT EXISTS agent_task_idem_uq ON public.agent_task (idempotency_key);

CREATE TABLE IF NOT EXISTS public.agent_task_event (
    id              text PRIMARY KEY,
    task_id         text        NOT NULL REFERENCES public.agent_task (id) ON DELETE CASCADE,
    status          text        NOT NULL,
    seq             bigint      NOT NULL DEFAULT 0,
    correlation_id  text        NOT NULL,
    idempotency_key text        NOT NULL,
    created_at      timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS agent_task_event_task_idx ON public.agent_task_event (task_id, seq);
CREATE UNIQUE INDEX IF NOT EXISTS agent_task_event_idem_uq
    ON public.agent_task_event (idempotency_key);

CREATE TABLE IF NOT EXISTS public.agent_task_decision (
    id              text PRIMARY KEY,
    task_id         text        NOT NULL REFERENCES public.agent_task (id) ON DELETE CASCADE,
    decision        text        NOT NULL
                    CONSTRAINT agent_task_decision_check CHECK (decision IN ('approved', 'rejected')),
    approver_uid    bigint      NOT NULL,
    correlation_id  text        NOT NULL,
    decided_at      timestamptz NOT NULL DEFAULT now()
);

-- first-writer-wins：一任务至多一条决定；并发第二个决定者 ON CONFLICT 拿到 0 行。
CREATE UNIQUE INDEX IF NOT EXISTS agent_task_decision_task_uq
    ON public.agent_task_decision (task_id);
CREATE INDEX IF NOT EXISTS agent_task_decision_corr_idx
    ON public.agent_task_decision (correlation_id);

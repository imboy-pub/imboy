-- 00000086_user_deletion_job.up.sql
-- 账号删除合规链 D-03：删除任务/墓碑表（Implementation Plan Task D-03）
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT（同 00000082 头注）。
--
-- 设计决策：
--   * 一人一行（user_id UNIQUE）：pending → running → completed/failed；
--     failed 且 attempts < 上限的可重试回 pending（应用层控制）。
--   * 不设到 user 的 FK：用户主行删除后本行必须**幸存**作为墓碑——
--     account 快照列证明"此 id 曾是账号且已按清单处置"，并防止同
--     user_id 的重复注销任务污染。
--   * claim 用 FOR UPDATE SKIP LOCKED：多节点/多 worker 并发认领互不阻塞、
--     不重复执行（见 erlang_migrate 事务包裹约束，SKIP LOCKED 在事务内有效）。
--   * attempts + claimed_by/claimed_at 支撑重试与故障归因；
--     last_error 保留最近一次失败原因（terminal failed 供运营介入）。

CREATE TABLE IF NOT EXISTS public.user_deletion_job (
    id           bigint                   NOT NULL,  -- TSID
    user_id      bigint                   NOT NULL,
    account      character varying(80)    DEFAULT '' NOT NULL,  -- 墓碑快照
    status       text                     DEFAULT 'pending' NOT NULL, -- pending|running|completed|failed
    attempts     integer                  DEFAULT 0 NOT NULL,
    last_error   text,
    claimed_by   character varying(120),
    claimed_at   timestamp with time zone,
    finished_at  timestamp with time zone,
    created_at   timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at   timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_user_deletion_job PRIMARY KEY (id),
    CONSTRAINT uq_user_deletion_job_user UNIQUE (user_id),
    CONSTRAINT ck_user_deletion_job_status
        CHECK (status = ANY (ARRAY['pending', 'running', 'completed', 'failed']))
);

CREATE INDEX IF NOT EXISTS idx_user_deletion_job_status
    ON public.user_deletion_job (status, attempts);
--;

-- 迁移 00000134: Agent Run 基础三表（Agent Run Foundation）。
-- 计划契约：docs/architecture/2026-09-16-imboy-agent-runtime-v3.1.md §9.3-9.4
--   Frozen AgentRun FSM + Frozen Run/Effect Schema Contract
--   （agent_run / agent_run_event / agent_effect；规范本
--   SHA256=05808674d4825320de867a2a8d2899fb4babddbfcea6fe4bf27a4e43a55dd6b2）。
-- 迁移契约：up=可重复执行，down=完整对称回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
-- 槽位原登记 00000133，并入 main 时改号 00000134（grant 同步改号 00000133，相对顺序不变）。
--
-- 设计决策：
--   * 本迁移逐字实现 §9.4 合同，不添加合同外对象；status CHECK 冻结 §9.3 八状态
--     created|queued|running|waiting_approval|succeeded|failed|cancelled|unknown；
--     timeout 不是状态（failed + reason_code='timeout' 表达，见 §9.3）。
--   * workspace 复合 FK 目标 uq_workspace_organization_id_id 由 00000116 建立（00000133 同款）；
--     MATCH SIMPLE 语义下 workspace_id 为 NULL（organization-scoped capability）时约束不检查。
--   * agent_run_event.actor_kind CHECK ('human','system','agent') 为 AG-A0 裁决 CS-4 的
--     冻结值域（对齐 grant_event human/system 语义并扩展 agent 执行者）。
--   * actor_id 为 text：run 事件的 actor 除 human 外还有 system worker 与 agent 本体，
--     不设 user FK；human actor 存 user id 的十进制文本。
--   * append-only 守卫复用 00000119/00000133 fn_*_event_append_only 模式
--     （ERRCODE 23514 + BEFORE UPDATE OR DELETE）；本表无 ON DELETE SET NULL 例外
--     （actor_id 为 text 无 user FK），UPDATE/DELETE 一律拒绝。
--   * DEFAULT 語义仅限合同列的既有仓内惯例（version/attempt/created_at/updated_at/detail_json）。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 1: agent_run（§9.4 L463-492）
-- ============================================================
CREATE TABLE IF NOT EXISTS agent_run (
    id                        bigint                   NOT NULL,  -- TSID
    agent_id                  bigint                   NOT NULL,
    organization_id           bigint                   NOT NULL,
    workspace_id              bigint,                             -- NULL = organization-scoped capability
    grant_id                  bigint                   NOT NULL,
    grant_version_at_start    integer                  NOT NULL,
    delegating_principal_id   bigint                   NOT NULL,
    trigger_type              text                     NOT NULL,
    trigger_id                text                     NOT NULL,
    runtime_type              text                     NOT NULL,
    status                    text                     NOT NULL,
    reason_code               text,
    version                   integer                  DEFAULT 1 NOT NULL,
    context_digest            text                     NOT NULL,
    idempotency_key           text                     NOT NULL,
    lease_owner               text,
    lease_expires_at          timestamp with time zone,
    attempt                   integer                  DEFAULT 0 NOT NULL,
    created_at                timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    queued_at                 timestamp with time zone,
    started_at                timestamp with time zone,
    finished_at               timestamp with time zone,
    updated_at                timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_agent_run PRIMARY KEY (id),
    CONSTRAINT uq_ar_org_id UNIQUE (organization_id, id),
    CONSTRAINT uq_ar_agent_org_trigger_idem
        UNIQUE (agent_id, organization_id, trigger_type, trigger_id, idempotency_key),
    CONSTRAINT ck_ar_trigger_type
        CHECK (trigger_type = ANY (ARRAY['message'::text, 'schedule'::text, 'webhook'::text])),
    CONSTRAINT ck_ar_status CHECK (status = ANY (ARRAY[
        'created'::text, 'queued'::text, 'running'::text, 'waiting_approval'::text,
        'succeeded'::text, 'failed'::text, 'cancelled'::text, 'unknown'::text])),
    CONSTRAINT ck_ar_terminal_finished CHECK (
        status NOT IN ('succeeded'::text, 'failed'::text, 'cancelled'::text)
        OR finished_at IS NOT NULL),
    CONSTRAINT ck_ar_version CHECK (version >= 1),
    CONSTRAINT ck_ar_attempt CHECK (attempt >= 0),
    CONSTRAINT ck_ar_trigger_id CHECK (trigger_id <> ''),
    CONSTRAINT ck_ar_context_digest CHECK (context_digest <> ''),
    CONSTRAINT ck_ar_idempotency_key CHECK (idempotency_key <> ''),
    CONSTRAINT fk_ar_agent FOREIGN KEY (agent_id) REFERENCES "user"(id) ON DELETE RESTRICT,
    CONSTRAINT fk_ar_organization FOREIGN KEY (organization_id)
        REFERENCES organization(id) ON DELETE RESTRICT,
    CONSTRAINT fk_ar_workspace FOREIGN KEY (organization_id, workspace_id)
        REFERENCES workspace (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_ar_grant FOREIGN KEY (grant_id) REFERENCES agent_grant(id) ON DELETE RESTRICT,
    CONSTRAINT fk_ar_delegating_principal FOREIGN KEY (delegating_principal_id)
        REFERENCES "user"(id) ON DELETE RESTRICT
);

COMMENT ON TABLE agent_run IS
    'AgentRun（§9.1/§9.4 冻结合同）：一次 Runtime 执行实例；与 ProductTask(agent_task)/MCP Task 三者可关联但绝不共表、改名或共享状态机；status 冻结 §9.3 八状态，timeout 非状态（failed + reason_code=timeout）';
COMMENT ON COLUMN agent_run.id IS '主键 TSID';
COMMENT ON COLUMN agent_run.agent_id IS '执行 Agent 的 user id（account_type=1）；RESTRICT 保护血统';
COMMENT ON COLUMN agent_run.organization_id IS 'Run 归属机构；immutable context 成员，写入后不得随默认值变化';
COMMENT ON COLUMN agent_run.workspace_id IS '可空；非空时与 organization_id 复合 FK 指向同 Org workspace（MATCH SIMPLE：NULL 不检查）；默认 Workspace 只在创建 Run 时解析候选值';
COMMENT ON COLUMN agent_run.grant_id IS 'Run 依据的 Grant；RESTRICT；运行期授权以 Grant 实时状态重检，不凭此快照放行';
COMMENT ON COLUMN agent_run.grant_version_at_start IS 'Grant 版本审计快照；不用于绕过实时重检（§9.2）';
COMMENT ON COLUMN agent_run.delegating_principal_id IS 'delegating principal（§8）；RESTRICT 保护血统';
COMMENT ON COLUMN agent_run.trigger_type IS 'message | schedule | webhook（CHECK 冻结）';
COMMENT ON COLUMN agent_run.trigger_id IS '服务端来源标识（非空）';
COMMENT ON COLUMN agent_run.runtime_type IS 'Runtime 类型 text；首版 mock | hird';
COMMENT ON COLUMN agent_run.status IS 'FSM 存储态：created|queued|running|waiting_approval|succeeded|failed|cancelled|unknown；unknown 仅显式 reconcile 转 succeeded|failed';
COMMENT ON COLUMN agent_run.reason_code IS '稳定原因码（如 timeout/validation_failed/grant_revoked/max_attempts_exceeded）；可空';
COMMENT ON COLUMN agent_run.version IS 'CAS 版本（>=1）；每次迁移 UPDATE ... WHERE id AND status AND version（§9.3）';
COMMENT ON COLUMN agent_run.context_digest IS 'immutable context 摘要（非空；不存 Prompt 原文）';
COMMENT ON COLUMN agent_run.idempotency_key IS 'trigger 幂等键（非空）；五元组 UNIQUE 支撑 duplicate trigger 返回同一 Run';
COMMENT ON COLUMN agent_run.lease_owner IS 'worker lease 持有者；节点重启后仅一个 worker 能以 DB 条件更新抢到过期 lease';
COMMENT ON COLUMN agent_run.lease_expires_at IS 'lease 到期时刻；获取/续租均为 DB 条件更新，进程锁/ETS/全局注册不作真源（§9.3）';
COMMENT ON COLUMN agent_run.attempt IS 'lease 获取次数（>=0）；上限为应用层冻结常量（AG-A0 裁决 CS-6）';
COMMENT ON COLUMN agent_run.created_at IS 'Run 创建时刻';
COMMENT ON COLUMN agent_run.queued_at IS '入队时刻（created→queued 边）';
COMMENT ON COLUMN agent_run.started_at IS '起跑时刻（queued→running 边）';
COMMENT ON COLUMN agent_run.finished_at IS '终态时刻；终态（succeeded|failed|cancelled）必须非空（CHECK）';
COMMENT ON COLUMN agent_run.updated_at IS '最近变更时刻';

CREATE INDEX IF NOT EXISTS i_ar_status_lease
    ON agent_run USING btree (status, lease_expires_at)
    WHERE status IN ('queued', 'running');
CREATE INDEX IF NOT EXISTS i_ar_agent_org_created
    ON agent_run USING btree (agent_id, organization_id, created_at DESC);
CREATE INDEX IF NOT EXISTS i_ar_grant_status
    ON agent_run USING btree (grant_id, status);

-- ============================================================
-- Phase 2: agent_run_event（§9.4 L495-498，append-only）
-- ============================================================
CREATE TABLE IF NOT EXISTS agent_run_event (
    id              bigint                   NOT NULL,  -- TSID
    run_id          bigint                   NOT NULL,
    from_status     text,
    to_status       text                     NOT NULL,
    reason_code     text,
    actor_kind      text                     NOT NULL,
    actor_id        text                     NOT NULL,
    detail_json     jsonb                    DEFAULT '{}'::jsonb NOT NULL,
    idempotency_key text                     NOT NULL,
    created_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_agent_run_event PRIMARY KEY (id),
    CONSTRAINT uq_are_run_idem UNIQUE (run_id, idempotency_key),
    CONSTRAINT ck_are_from_status CHECK (from_status IS NULL OR from_status = ANY (ARRAY[
        'created'::text, 'queued'::text, 'running'::text, 'waiting_approval'::text,
        'succeeded'::text, 'failed'::text, 'cancelled'::text, 'unknown'::text])),
    CONSTRAINT ck_are_to_status CHECK (to_status = ANY (ARRAY[
        'created'::text, 'queued'::text, 'running'::text, 'waiting_approval'::text,
        'succeeded'::text, 'failed'::text, 'cancelled'::text, 'unknown'::text])),
    CONSTRAINT ck_are_actor_kind
        CHECK (actor_kind = ANY (ARRAY['human'::text, 'system'::text, 'agent'::text])),
    CONSTRAINT fk_are_run FOREIGN KEY (run_id) REFERENCES agent_run(id) ON DELETE RESTRICT
);

COMMENT ON TABLE agent_run_event IS
    'AgentRun 事件账本（§9.4，append-only）：每次 FSM 迁移与 event 同一事务提交，审计失败则迁移回滚；禁止 UPDATE 与 DELETE';
COMMENT ON COLUMN agent_run_event.run_id IS '所属 Run；RESTRICT：有 event 血统的 Run 物理不可删';
COMMENT ON COLUMN agent_run_event.from_status IS '迁移前状态；创建事件（[*]→created，非态到态边）为 NULL';
COMMENT ON COLUMN agent_run_event.to_status IS '迁移后状态（八状态 CHECK）';
COMMENT ON COLUMN agent_run_event.reason_code IS '迁移稳定原因码；可空';
COMMENT ON COLUMN agent_run_event.actor_kind IS 'human | system | agent（AG-A0 裁决 CS-4 冻结值域）';
COMMENT ON COLUMN agent_run_event.actor_id IS '执行者标识：human=user id 十进制文本、system=worker/node 标识、agent=agent user id 十进制文本';
COMMENT ON COLUMN agent_run_event.detail_json IS 'sanitized metadata（只存脱敏元数据，§9.4）';
COMMENT ON COLUMN agent_run_event.idempotency_key IS '事件幂等键；同 Run 内唯一（UNIQUE(run_id,idempotency_key)）';

-- ============================================================
-- Phase 3: agent_effect（§9.4 L500-513）
-- ============================================================
CREATE TABLE IF NOT EXISTS agent_effect (
    id                       bigint                   NOT NULL,  -- TSID
    run_id                   bigint                   NOT NULL,
    sequence                 integer                  NOT NULL,
    tool_id                  text                     NOT NULL,
    capability               text                     NOT NULL,
    action                   text                     NOT NULL,
    resource_digest          text                     NOT NULL,
    args_digest              text                     NOT NULL,
    status                   text                     NOT NULL,
    authorization_reason     text,
    approval_ref             text,
    grant_version_checked    integer,
    external_idempotency_key text,
    result_digest            text,
    failure_code             text,
    version                  integer                  DEFAULT 1 NOT NULL,
    created_at               timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at               timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_agent_effect PRIMARY KEY (id),
    CONSTRAINT uq_ae_run_sequence UNIQUE (run_id, sequence),
    CONSTRAINT uq_ae_tool_external_idem UNIQUE (tool_id, external_idempotency_key),
    CONSTRAINT ck_ae_status CHECK (status = ANY (ARRAY[
        'created'::text, 'denied'::text, 'waiting_approval'::text, 'authorized'::text,
        'dispatching'::text, 'succeeded'::text, 'failed'::text, 'unknown'::text])),
    CONSTRAINT ck_ae_sequence CHECK (sequence >= 1),
    CONSTRAINT ck_ae_version CHECK (version >= 1),
    CONSTRAINT ck_ae_tool_id CHECK (tool_id <> ''),
    CONSTRAINT ck_ae_capability CHECK (capability <> ''),
    CONSTRAINT ck_ae_action CHECK (action <> ''),
    CONSTRAINT ck_ae_resource_digest CHECK (resource_digest <> ''),
    CONSTRAINT ck_ae_args_digest CHECK (args_digest <> ''),
    CONSTRAINT fk_ae_run FOREIGN KEY (run_id) REFERENCES agent_run(id) ON DELETE RESTRICT
);

COMMENT ON TABLE agent_effect IS
    'AgentRun Effect 账本（§9.4）：dispatch/reconcile ledger，非业务结果真源；不存 credential、完整 Prompt、完整 Tool 参数或敏感结果；dispatching 必须在调用 adapter 前持久化（§10.3）';
COMMENT ON COLUMN agent_effect.id IS '主键 TSID';
COMMENT ON COLUMN agent_effect.run_id IS '所属 Run；RESTRICT';
COMMENT ON COLUMN agent_effect.sequence IS 'Run 内单调序号（>=1），UNIQUE(run_id,sequence)';
COMMENT ON COLUMN agent_effect.tool_id IS '版本化 Tool descriptor 的稳定 tool id';
COMMENT ON COLUMN agent_effect.capability IS 'Tool descriptor capability';
COMMENT ON COLUMN agent_effect.action IS 'Tool descriptor action';
COMMENT ON COLUMN agent_effect.resource_digest IS '服务端解析的资源摘要（只存 digest）';
COMMENT ON COLUMN agent_effect.args_digest IS 'Tool 参数摘要（只存 digest）；approval 绑定四元组之一（§10.3）';
COMMENT ON COLUMN agent_effect.status IS 'created|denied|waiting_approval|authorized|dispatching|succeeded|failed|unknown（CHECK 冻结）；子状态矩阵见 AG31-04B evidence（AG-A0 裁决 CS-7）';
COMMENT ON COLUMN agent_effect.authorization_reason IS '稳定 reason code（allow/grant_revoked/grant_expired/grant_missing 等）';
COMMENT ON COLUMN agent_effect.approval_ref IS '批准凭据引用；批准时绑定 digest/version（§10.3），可空';
COMMENT ON COLUMN agent_effect.grant_version_checked IS '最后授权检查时的 Grant 版本';
COMMENT ON COLUMN agent_effect.external_idempotency_key IS '外部系统幂等 key；UNIQUE(tool_id,external_idempotency_key) 支撑 duplicate effect 不重发（§14）';
COMMENT ON COLUMN agent_effect.result_digest IS '成功结果摘要（sanitized outcome）';
COMMENT ON COLUMN agent_effect.failure_code IS '失败码；dispatch 结果不可知时记 unknown（禁止自动重发，§10.3）';
COMMENT ON COLUMN agent_effect.version IS 'CAS 版本（>=1）；Effect 所有状态写入使用 CAS（§9.4）';

-- ============================================================
-- Phase 4: agent_run_event append-only 守卫（复用 00000119/00000133 模式）
-- ============================================================
CREATE OR REPLACE FUNCTION fn_agent_run_event_append_only() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION
            'agent_run_event 是 append-only Run 血统账本，禁止 DELETE'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_agent_run_event_append_only';
    END IF;

    -- 本表 actor_id 为 text 且无 user FK（不存在 ON DELETE SET NULL 例外），UPDATE 一律拒绝。
    RAISE EXCEPTION
        'agent_run_event 是 append-only Run 血统账本，禁止 UPDATE（只允许 INSERT 追加）'
        USING ERRCODE = '23514',
              CONSTRAINT = 'trg_agent_run_event_append_only';
END;
$$;

COMMENT ON FUNCTION fn_agent_run_event_append_only() IS
    'AgentRun event append-only 守卫：DELETE 一律 23514；UPDATE 一律 23514（无例外），只允许 INSERT 追加';

DROP TRIGGER IF EXISTS trg_agent_run_event_append_only ON agent_run_event;
CREATE TRIGGER trg_agent_run_event_append_only
    BEFORE UPDATE OR DELETE ON agent_run_event
    FOR EACH ROW EXECUTE FUNCTION fn_agent_run_event_append_only();

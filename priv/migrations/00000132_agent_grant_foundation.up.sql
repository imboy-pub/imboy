-- 迁移 00000132: Agent Grant 基础四表（Agent Grant Foundation）。
-- 计划契约：docs/architecture/2026-09-16-imboy-agent-runtime-v3.1.md §7.2
--   Frozen Grant Schema Contract（agent_grant / agent_grant_workspace /
--   agent_grant_capability / agent_grant_event；规范本 SHA256=05808674d4825320de867a2a8d2899fb4babddbfcea6fe4bf27a4e43a55dd6b2）。
-- 迁移契约：up=可重复执行，down=完整对称回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策：
--   * 本迁移逐字实现 §7.2 合同，不添加合同外对象；槽位 AGENT_MIGRATION_SLOT_GRANT=00000132。
--   * DEFAULT 語义仅限合同列的既有仓内惯例（version/created_at/updated_at/constraint_json/
--     detail_json），与 00000116/00000119 同构，不引入新对象。
--   * workspace 复合 FK 目标 uq_workspace_organization_id_id 由 00000116 建立（已有 6 处消费者），
--     跨 Org 的 workspace 引用在 DB 层被复合 FK 天然拒绝（MATCH SIMPLE：NULL-org 匹配不到）。
--   * append-only 守卫复用 00000119 fn_enterprise_audit_event_append_only 模式
--     （ERRCODE 23514 + BEFORE UPDATE OR DELETE）；唯一例外是 fk_age_actor 的
--     ON DELETE SET NULL 把 actor_user_id 置 NULL。
--   * `expired` 不入库：status 只存 active|revoked，到期由 status+expires_at 实时判定，
--     INDEX (agent_id, organization_id, status, expires_at) 支撑该查询路径。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 1: agent_grant（§7.2 L262-289）
-- ============================================================
CREATE TABLE IF NOT EXISTS agent_grant (
    id                   bigint                   NOT NULL,  -- TSID
    agent_id             bigint                   NOT NULL,
    organization_id      bigint                   NOT NULL,
    delegator_user_id    bigint                   NOT NULL,
    workspace_scope_kind text                     NOT NULL,
    status               text                     NOT NULL,
    valid_from           timestamp with time zone NOT NULL,
    expires_at           timestamp with time zone NOT NULL,
    revoked_at           timestamp with time zone,
    revoked_by_user_id   bigint,
    version              integer                  DEFAULT 1 NOT NULL,
    idempotency_key      text                     NOT NULL,
    created_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_agent_grant PRIMARY KEY (id),
    CONSTRAINT uq_ag_org_id UNIQUE (organization_id, id),
    CONSTRAINT uq_ag_org_delegator_idempotency UNIQUE (organization_id, delegator_user_id, idempotency_key),
    CONSTRAINT ck_ag_workspace_scope_kind
        CHECK (workspace_scope_kind = ANY (ARRAY['none'::text, 'explicit'::text])),
    CONSTRAINT ck_ag_status CHECK (status = ANY (ARRAY['active'::text, 'revoked'::text])),
    CONSTRAINT ck_ag_validity CHECK (expires_at > valid_from),
    CONSTRAINT ck_ag_status_revoked_match CHECK (
        (status = 'revoked') = (revoked_at IS NOT NULL)
        AND (revoked_at IS NULL) = (revoked_by_user_id IS NULL)
    ),
    CONSTRAINT ck_ag_version CHECK (version >= 1),
    CONSTRAINT fk_ag_agent FOREIGN KEY (agent_id) REFERENCES "user"(id) ON DELETE RESTRICT,
    CONSTRAINT fk_ag_organization FOREIGN KEY (organization_id)
        REFERENCES organization(id) ON DELETE RESTRICT,
    CONSTRAINT fk_ag_delegator FOREIGN KEY (delegator_user_id)
        REFERENCES "user"(id) ON DELETE RESTRICT,
    CONSTRAINT fk_ag_revoked_by FOREIGN KEY (revoked_by_user_id)
        REFERENCES "user"(id) ON DELETE RESTRICT
);

COMMENT ON TABLE agent_grant IS
    'Agent Grant（§7.2 冻结合同）：Human delegator 向 Agent 授予的最大可执行边界；DEFAULT=DENY；API 有效态 pending|active|expired|revoked 由时间实时计算，expired 不入库';
COMMENT ON COLUMN agent_grant.id IS '主键 TSID';
COMMENT ON COLUMN agent_grant.agent_id IS '被授权 Agent 的 user id（运行时要求 account_type=1）';
COMMENT ON COLUMN agent_grant.organization_id IS 'Grant 归属机构；与 agent_id/delegator 同 Org（运行时校验）';
COMMENT ON COLUMN agent_grant.delegator_user_id IS '承担授权责任的 Human principal（运行时要求 Human member）';
COMMENT ON COLUMN agent_grant.workspace_scope_kind IS 'none=不限定 workspace（零行 agent_grant_workspace）| explicit=显式 workspace id 列表（至少一行）';
COMMENT ON COLUMN agent_grant.status IS '存储态只有 active|revoked；expired 不入库，到期实时判定';
COMMENT ON COLUMN agent_grant.valid_from IS '生效起始（不晚于 expires_at）';
COMMENT ON COLUMN agent_grant.expires_at IS '到期时刻；到期不依赖后台任务即被拒绝';
COMMENT ON COLUMN agent_grant.revoked_at IS '撤销时间；仅 status=revoked 非空（与 revoked_by 同空/同非空）';
COMMENT ON COLUMN agent_grant.revoked_by_user_id IS '撤销人；与 revoked_at 同空/同非空；RESTRICT 保护 lineage';
COMMENT ON COLUMN agent_grant.version IS 'CAS 版本（>=1）；所有 mutation 走 expected-version CAS';
COMMENT ON COLUMN agent_grant.idempotency_key IS '幂等键；同 (organization_id, delegator_user_id) 范围唯一';

CREATE INDEX IF NOT EXISTS i_ag_agent_org_status_expires
    ON agent_grant USING btree (agent_id, organization_id, status, expires_at);
CREATE INDEX IF NOT EXISTS i_ag_delegator_org_status
    ON agent_grant USING btree (delegator_user_id, organization_id, status);

-- ============================================================
-- Phase 2: agent_grant_workspace（§7.2 L296-303）
-- ============================================================
CREATE TABLE IF NOT EXISTS agent_grant_workspace (
    organization_id bigint NOT NULL,
    grant_id        bigint NOT NULL,
    workspace_id    bigint NOT NULL,
    CONSTRAINT pk_agent_grant_workspace PRIMARY KEY (grant_id, workspace_id),
    CONSTRAINT fk_agw_grant FOREIGN KEY (organization_id, grant_id)
        REFERENCES agent_grant (organization_id, id) ON DELETE CASCADE,
    CONSTRAINT fk_agw_workspace FOREIGN KEY (organization_id, workspace_id)
        REFERENCES workspace (organization_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE agent_grant_workspace IS
    'Agent Grant 显式 workspace 范围（§7.2）：workspace_scope_kind=none 必须零行、explicit 必须至少一行；该跨表条件由 Grant command 同一事务内锁定 Grant 验证';
COMMENT ON COLUMN agent_grant_workspace.organization_id IS '机构 id；复合 FK 保证 grant 与 workspace 同 Org';
COMMENT ON COLUMN agent_grant_workspace.grant_id IS '所属 Grant（同 Org 复合 FK 引用 agent_grant(organization_id,id)）';
COMMENT ON COLUMN agent_grant_workspace.workspace_id IS '被授予的 workspace（复合 FK 保证属于同一 Org，RESTRICT）';

CREATE INDEX IF NOT EXISTS i_agw_workspace_grant
    ON agent_grant_workspace USING btree (workspace_id, grant_id);

-- ============================================================
-- Phase 3: agent_grant_capability（§7.2 L310-317）
-- ============================================================
CREATE TABLE IF NOT EXISTS agent_grant_capability (
    grant_id       bigint NOT NULL,
    capability     text   NOT NULL,
    action         text   NOT NULL,
    resource_type  text   NOT NULL,
    constraint_json jsonb DEFAULT '{}'::jsonb NOT NULL,
    CONSTRAINT pk_agent_grant_capability PRIMARY KEY (grant_id, capability, action, resource_type),
    CONSTRAINT ck_agc_capability CHECK (capability <> ''),
    CONSTRAINT ck_agc_action CHECK (action <> ''),
    CONSTRAINT ck_agc_resource_type CHECK (resource_type <> ''),
    CONSTRAINT fk_agc_grant FOREIGN KEY (grant_id) REFERENCES agent_grant(id) ON DELETE CASCADE
);

COMMENT ON TABLE agent_grant_capability IS
    'Agent Grant capability 边界（§7.2）：capability/action 来自版本化目录；constraint_json 只能收窄资源，未知 key 在发行与执行阶段都由应用层拒绝';
COMMENT ON COLUMN agent_grant_capability.constraint_json IS '资源收窄约束 jsonb（不允许否定后再扩大；不存敏感原文）';

-- ============================================================
-- Phase 4: agent_grant_event（§7.2 L325-336，append-only）
-- ============================================================
CREATE TABLE IF NOT EXISTS agent_grant_event (
    id              bigint                   NOT NULL,  -- TSID
    grant_id        bigint                   NOT NULL,
    event_type      text                     NOT NULL,
    actor_kind      text                     NOT NULL,
    actor_user_id   bigint,
    from_version    integer,
    to_version      integer                  NOT NULL,
    detail_json     jsonb                    DEFAULT '{}'::jsonb NOT NULL,
    idempotency_key text                     NOT NULL,
    created_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_agent_grant_event PRIMARY KEY (id),
    CONSTRAINT uq_age_idempotency_key UNIQUE (idempotency_key),
    CONSTRAINT ck_age_event_type
        CHECK (event_type = ANY (ARRAY['issued'::text, 'revoked'::text, 'expiry_observed'::text])),
    CONSTRAINT ck_age_actor_kind CHECK (actor_kind = ANY (ARRAY['human'::text, 'system'::text])),
    CONSTRAINT fk_age_grant FOREIGN KEY (grant_id) REFERENCES agent_grant(id) ON DELETE RESTRICT,
    CONSTRAINT fk_age_actor FOREIGN KEY (actor_user_id) REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE agent_grant_event IS
    'Agent Grant 事件账本（§7.2，append-only）：Grant create/revoke 与 event 同一事务提交，审计失败则 mutation 回滚；禁止 UPDATE 与 DELETE';
COMMENT ON COLUMN agent_grant_event.grant_id IS '所属 Grant；RESTRICT：有 event 血统的 Grant 物理不可删';
COMMENT ON COLUMN agent_grant_event.event_type IS 'issued | revoked | expiry_observed';
COMMENT ON COLUMN agent_grant_event.actor_kind IS 'human | system';
COMMENT ON COLUMN agent_grant_event.actor_user_id IS 'Human 操作者快照；user 删除后置 NULL，不级联 Grant lineage';
COMMENT ON COLUMN agent_grant_event.from_version IS '变更前 CAS 版本（issued 首事件可为 NULL）';
COMMENT ON COLUMN agent_grant_event.to_version IS '变更后 CAS 版本';
COMMENT ON COLUMN agent_grant_event.detail_json IS 'sanitized metadata（只存脱敏元数据）';
COMMENT ON COLUMN agent_grant_event.idempotency_key IS '事件幂等键（全局唯一）';

CREATE INDEX IF NOT EXISTS i_age_grant_created
    ON agent_grant_event USING btree (grant_id, created_at, id);

-- ============================================================
-- Phase 5: append-only 守卫（复用 00000119 模式）
-- ============================================================
CREATE OR REPLACE FUNCTION fn_agent_grant_event_append_only() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION
            'agent_grant_event 是 append-only Grant 血统账本，禁止 DELETE'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_agent_grant_event_append_only';
    END IF;

    -- 唯一允许的变更：user 行被删除时，actor_user_id 由外键 ON DELETE SET NULL 置 NULL。
    -- 事件内容（grant/event_type/actor_kind/version/detail/idempotency/时间）与既有 actor 一律不可改写。
    IF (NEW.id, NEW.grant_id, NEW.event_type, NEW.actor_kind, NEW.from_version,
        NEW.to_version, NEW.detail_json, NEW.idempotency_key, NEW.created_at)
       IS DISTINCT FROM
       (OLD.id, OLD.grant_id, OLD.event_type, OLD.actor_kind, OLD.from_version,
        OLD.to_version, OLD.detail_json, OLD.idempotency_key, OLD.created_at)
       OR NEW.actor_user_id IS NOT NULL THEN
        RAISE EXCEPTION
            'agent_grant_event 是 append-only Grant 血统账本，禁止 UPDATE（仅允许 user 删除时 actor_user_id 置 NULL）'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_agent_grant_event_append_only';
    END IF;

    RETURN NEW;
END;
$$;

COMMENT ON FUNCTION fn_agent_grant_event_append_only() IS
    'Agent Grant event append-only 守卫：DELETE 一律 23514；UPDATE 一律 23514，唯一例外是 fk_age_actor 的 ON DELETE SET NULL 把 actor_user_id 置 NULL（事件内容与既有 actor 不可改写，只允许 INSERT 追加）';

DROP TRIGGER IF EXISTS trg_agent_grant_event_append_only ON agent_grant_event;
CREATE TRIGGER trg_agent_grant_event_append_only
    BEFORE UPDATE OR DELETE ON agent_grant_event
    FOR EACH ROW EXECUTE FUNCTION fn_agent_grant_event_append_only();

-- 迁移 00000119: 企业审计事件（Enterprise Audit Event）。
-- 计划契约：§4.1（enterprise_audit_event: Org/resource/action/identity/actor/detail；append-only）。
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策：
--   * 审计是 append-only 真源：UPDATE / DELETE 一律拒绝（只在 down 里随表一起移除）。
--   * actor_user_id 只作审计快照（ON DELETE SET NULL），不参与资源 owner 判定（EB-D03）。
--   * organization_id 用 RESTRICT：机构物理删除必须先走销户/导出流程，不得带走审计。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

CREATE TABLE IF NOT EXISTS enterprise_audit_event (
    id                   bigint                   NOT NULL,  -- TSID
    organization_id      bigint                   NOT NULL,
    resource_type        text                     NOT NULL,
    resource_id          bigint,
    action               text                     NOT NULL,
    business_identity_id bigint,
    actor_user_id        bigint,
    actor_role           text,
    detail               jsonb                    DEFAULT '{}'::jsonb NOT NULL,
    created_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_enterprise_audit_event PRIMARY KEY (id),
    CONSTRAINT ck_eae_action CHECK (action <> ''),
    CONSTRAINT ck_eae_resource_type CHECK (resource_type <> ''),
    CONSTRAINT fk_eae_organization FOREIGN KEY (organization_id)
        REFERENCES organization(id) ON DELETE RESTRICT,
    CONSTRAINT fk_eae_identity FOREIGN KEY (organization_id, business_identity_id)
        REFERENCES organization_business_identity (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_eae_actor FOREIGN KEY (actor_user_id) REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE enterprise_audit_event IS
    '企业审计事件（append-only 真源）：记录 Org/resource/action/identity/actor/detail；禁止 UPDATE 与 DELETE';
COMMENT ON COLUMN enterprise_audit_event.resource_type IS '资源类型（如 enterprise_message / enterprise_asset / organization_business_identity_assignment）';
COMMENT ON COLUMN enterprise_audit_event.resource_id IS '资源 ID（TSID；纯组织级事件可为空）';
COMMENT ON COLUMN enterprise_audit_event.action IS '动作名（如 message.accept / offboarding.rebind），非空';
COMMENT ON COLUMN enterprise_audit_event.business_identity_id IS '执行时的业务身份（复合 FK 保证同 Org）';
COMMENT ON COLUMN enterprise_audit_event.actor_user_id IS '执行者 user（仅审计/展示）；user 删除后置 NULL，不级联企业数据';
COMMENT ON COLUMN enterprise_audit_event.actor_role IS '执行时的角色快照（governance role 或 function，审计用文本快照）';
COMMENT ON COLUMN enterprise_audit_event.detail IS '结构化明细 jsonb（不得存放明文客户资料）';

CREATE INDEX IF NOT EXISTS i_eae_org_created ON enterprise_audit_event
    USING btree (organization_id, created_at DESC);
CREATE INDEX IF NOT EXISTS i_eae_org_resource ON enterprise_audit_event
    USING btree (organization_id, resource_type, resource_id);

CREATE OR REPLACE FUNCTION fn_enterprise_audit_event_append_only() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION
            'enterprise_audit_event 是 append-only 审计真源，禁止 DELETE'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_audit_event_append_only';
    END IF;

    -- 唯一允许的变更：user 行被删除时，actor_user_id 由外键 ON DELETE SET NULL 置 NULL。
    -- 审计内容（Org/resource/action/identity/role/detail/时间）与既有 actor 一律不可改写：
    -- 若除 actor_user_id 外的任一列变化，或 actor_user_id 被写成新的非 NULL 值，一律拒绝。
    IF (NEW.id, NEW.organization_id, NEW.resource_type, NEW.resource_id, NEW.action,
        NEW.business_identity_id, NEW.actor_role, NEW.detail, NEW.created_at)
       IS DISTINCT FROM
       (OLD.id, OLD.organization_id, OLD.resource_type, OLD.resource_id, OLD.action,
        OLD.business_identity_id, OLD.actor_role, OLD.detail, OLD.created_at)
       OR NEW.actor_user_id IS NOT NULL THEN
        RAISE EXCEPTION
            'enterprise_audit_event 是 append-only 审计真源，禁止 UPDATE（仅允许 user 删除时 actor_user_id 置 NULL）'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_audit_event_append_only';
    END IF;

    RETURN NEW;
END;
$$;

COMMENT ON FUNCTION fn_enterprise_audit_event_append_only() IS
    '企业审计 append-only 守卫：DELETE 一律 23514；UPDATE 一律 23514，唯一例外是 fk_eae_actor 的 ON DELETE SET NULL 把 actor_user_id 置 NULL（审计内容与既有 actor 不可改写，只允许 INSERT 追加）';

DROP TRIGGER IF EXISTS trg_enterprise_audit_event_append_only ON enterprise_audit_event;
CREATE TRIGGER trg_enterprise_audit_event_append_only
    BEFORE UPDATE OR DELETE ON enterprise_audit_event
    FOR EACH ROW EXECUTE FUNCTION fn_enterprise_audit_event_append_only();

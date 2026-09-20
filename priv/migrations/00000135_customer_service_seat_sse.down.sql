-- 迁移 00000135 down: 回滚 Seat SSE 分区列。
-- 数据不变量（MIG-00）：event 行数不变，仅列/索引/约束移除；workspace_id
-- 可由 session 真源重建，down 无数据损失。
-- 迁移契约：down=安全回滚、可重复执行。禁止 BEGIN/COMMIT。
--
-- 顺序：先撤触发器（守卫拒绝 DROP COLUMN 前的任何 UPDATE，且函数体引用
-- NEW.workspace_id——列删除后该引用在运行时炸 record field）→ 还原迁移 125
-- 的原始守卫函数 → 删索引/约束/列。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

DROP TRIGGER IF EXISTS trg_customer_service_event_append_only ON customer_service_event;

-- 还原迁移 125 的原始守卫（冻结列清单不含 workspace_id——列即将删除）。
CREATE OR REPLACE FUNCTION fn_customer_service_event_append_only() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION
            'customer_service_event 是 append-only 审计真源，禁止 DELETE'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_customer_service_event_append_only';
    END IF;

    IF (NEW.id, NEW.organization_id, NEW.session_id, NEW.business_identity_id,
        NEW.actor_kind, NEW.action, NEW.detail, NEW.created_at)
       IS DISTINCT FROM
       (OLD.id, OLD.organization_id, OLD.session_id, OLD.business_identity_id,
        OLD.actor_kind, OLD.action, OLD.detail, OLD.created_at)
       OR NEW.actor_user_id IS NOT NULL THEN
        RAISE EXCEPTION
            'customer_service_event 是 append-only 审计真源，禁止 UPDATE（仅允许 user 删除时 actor_user_id 置 NULL）'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_customer_service_event_append_only';
    END IF;

    RETURN NEW;
END;
$$;

DROP INDEX IF EXISTS i_cse_org_ws_id;

DO $$
BEGIN
    IF EXISTS (
        SELECT 1 FROM pg_constraint
         WHERE conname = 'fk_cse_workspace'
           AND conrelid = 'customer_service_event'::regclass)
    THEN
        ALTER TABLE customer_service_event
            DROP CONSTRAINT fk_cse_workspace;
    END IF;
END;
$$;

ALTER TABLE customer_service_event
    DROP COLUMN IF EXISTS workspace_id;

DROP TRIGGER IF EXISTS trg_customer_service_event_append_only ON customer_service_event;
CREATE TRIGGER trg_customer_service_event_append_only
    BEFORE UPDATE OR DELETE ON customer_service_event
    FOR EACH ROW EXECUTE FUNCTION fn_customer_service_event_append_only();

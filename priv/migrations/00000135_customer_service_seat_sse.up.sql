-- 迁移 00000135: Customer Service Seat SSE 分区列（event.workspace_id）。
-- 计划契约：MIG-00（schema-change-manifest §3.6）——坐席 SSE 的 (org, workspace, id)
--   键集分区需要 customer_service_event 带 workspace 维度（现状仅 uq_cse_org_id）。
-- 任务契约：BE-S01a（T-2 裁定后坐席面 org 作用域的事件流前置 DDL）。
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate
--   外层单事务包裹。
--
-- 设计决策：
--   * 纯扩张（expand）：先 ADD COLUMN nullable，再回填，最后 SET NOT NULL——
--     中途任何失败都回滚为「无列」原状，不产生半迁移行。
--   * 回填三级链（每级只填 IS NULL 行，可证明才回填）：
--       1. 会话事件自 customer_service_session 真源回填（session.workspace_id
--          NOT NULL，(org, session) 复合 FK 保证同句可联）；
--       2. 无 session 的 Org 级事件（seat.created / shop_key.issued /
--          visit_token.issued / installation.*）取 organization_default_workspace
--          （迁移 130 的显式默认）；
--       3. 无显式默认的 Org 取最小 active Workspace（与迁移 130 Phase 4 的
--          可证明回填同口径：min(active id) 是 legacy 读法的确定性纯函数）。
--     三级之后仍 NULL 的行（Org 无任何 active Workspace 却有事件——按写入
--     语义不应存在）由 SET NOT NULL 显式拒绝（fail loudly，不静默猜值）。
--   * append-only 守卫（迁移 125 trg_customer_service_event_append_only）会拒
--     绝任何 UPDATE——回填前先 DROP TRIGGER，回填后 CREATE OR REPLACE FUNCTION
--     把 workspace_id 并入冻结列清单再重建触发器：迁移之后 UPDATE 改
--     workspace_id 与改其他列同样 23514。
--   * 索引 (organization_id, workspace_id, id)：坐席 SSE 键集游标
--     （workspace 内 id > after ORDER BY id）的唯一入口；普通索引 + 短锁
--     （单实例 local 模式，CONCURRENTLY 不适用，见 MIG-00 锁与兼容）。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 1: expand——追加 nullable 列
-- ============================================================
ALTER TABLE customer_service_event
    ADD COLUMN IF NOT EXISTS workspace_id bigint;

-- ============================================================
-- Phase 2: backfill——回填前撤 append-only 触发器（125 的守卫拒绝 UPDATE）
-- ============================================================
DROP TRIGGER IF EXISTS trg_customer_service_event_append_only ON customer_service_event;

-- 2.1 会话事件：自 session 真源回填（session.workspace_id NOT NULL）。
UPDATE customer_service_event e
   SET workspace_id = s.workspace_id
  FROM customer_service_session s
 WHERE s.organization_id = e.organization_id
   AND s.id = e.session_id
   AND e.workspace_id IS NULL;

-- 2.2 无 session 的 Org 级事件：显式默认 Workspace（迁移 130）。
UPDATE customer_service_event e
   SET workspace_id = d.workspace_id
  FROM organization_default_workspace d
 WHERE d.organization_id = e.organization_id
   AND e.workspace_id IS NULL;

-- 2.3 无显式默认的 Org：最小 active Workspace（可证明的确定性回填）。
UPDATE customer_service_event e
   SET workspace_id = w.workspace_id
  FROM (
        SELECT organization_id, MIN(id) AS workspace_id
          FROM workspace
         WHERE status = 'active'
         GROUP BY organization_id
       ) w
 WHERE w.organization_id = e.organization_id
   AND e.workspace_id IS NULL;

-- ============================================================
-- Phase 3: contract——NOT NULL + 同 Org 复合 FK + 分区索引
-- ============================================================
ALTER TABLE customer_service_event
    ALTER COLUMN workspace_id SET NOT NULL;

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM pg_constraint
         WHERE conname = 'fk_cse_workspace'
           AND conrelid = 'customer_service_event'::regclass)
    THEN
        ALTER TABLE customer_service_event
            ADD CONSTRAINT fk_cse_workspace
            FOREIGN KEY (organization_id, workspace_id)
            REFERENCES workspace (organization_id, id) ON DELETE RESTRICT;
    END IF;
END;
$$;

CREATE INDEX IF NOT EXISTS i_cse_org_ws_id ON customer_service_event
    USING btree (organization_id, workspace_id, id);

COMMENT ON COLUMN customer_service_event.workspace_id IS
    '事件发生的 Workspace（SSE 分区维度；会话事件自 session 真源派生，Org 级事件取 Org 默认/最小 active Workspace）';

-- ============================================================
-- Phase 4: 重建 append-only 守卫（冻结列清单并入 workspace_id）
-- ============================================================
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

    IF (NEW.id, NEW.organization_id, NEW.workspace_id, NEW.session_id, NEW.business_identity_id,
        NEW.actor_kind, NEW.action, NEW.detail, NEW.created_at)
       IS DISTINCT FROM
       (OLD.id, OLD.organization_id, OLD.workspace_id, OLD.session_id, OLD.business_identity_id,
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

DROP TRIGGER IF EXISTS trg_customer_service_event_append_only ON customer_service_event;
CREATE TRIGGER trg_customer_service_event_append_only
    BEFORE UPDATE OR DELETE ON customer_service_event
    FOR EACH ROW EXECUTE FUNCTION fn_customer_service_event_append_only();

-- 迁移 00000130: Organization Default Workspace（显式默认工作区关系，M07 slot）。
-- Core Contract C05（Workspace Contract）/ 计划 ORG-05：
--   * 以显式同 Org 关系 `organization_default_workspace` 取代
--     「最小 active Workspace ID 推导」（legacy：eb_member_fact_pg 的
--     ORDER BY w.id LIMIT 1 读法——本迁移后该读法仅为过渡兼容，等待
--     src/features 装配层一次性切换，见 ORG-05 evidence caller inventory）。
--   * PK = organization_id：每 Org 至多一个默认（0 或 1，无多值）。
--   * 组合 FK (organization_id, workspace_id) -> workspace(organization_id, id)
--     保证默认必须同 Org（跨 Org 写入 23503 fail-closed）。
--   * 运行时守卫触发器：目标 Workspace 必须存在且 status='active'
--     （archived 目标 23514 拒绝）；organization_id 由 workspace 行**派生**
--     （单一真源，调用方传错 org 被覆盖，不产生跨 Org 脏行）。
--   * Backfill（可证明才回填）：legacy 读法是纯函数——对每个拥有
--     ≥1 个 active Org Workspace 的 Org，min(id) 恒确定可复算；
--     回填值 = min(active id)，与 legacy 读法逐 Org 等值（实测见 ORG-05
--     evidence 的 backfill 判定节）。全 archived / 无 Org Workspace 的
--     Org 不回填（legacy 读法返回 no_default_workspace，等值）。
--     数据量以现库存量上界（O(orgs) 行）有界，与 DDL 同迁移原子提交。
-- 迁移契约：up=可重复执行（IF NOT EXISTS + ON CONFLICT DO NOTHING），
--           down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 1: workspace(organization_id, id) 唯一索引（组合 FK 引用前提）
-- expand-only：organization_id 可空的个人 Workspace 行（NULL 组合互不冲突）
-- 不受影响；只加索引，不改任何既有约束。
-- ============================================================
CREATE UNIQUE INDEX IF NOT EXISTS uq_workspace_org_id
    ON workspace USING btree (organization_id, id);

-- ============================================================
-- Phase 2: organization_default_workspace 表
-- ============================================================
CREATE TABLE IF NOT EXISTS organization_default_workspace (
    organization_id bigint NOT NULL,
    workspace_id    bigint NOT NULL,
    created_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_organization_default_workspace PRIMARY KEY (organization_id),
    CONSTRAINT fk_odw_organization FOREIGN KEY (organization_id)
        REFERENCES organization(id) ON DELETE RESTRICT,
    -- 同 Org 不变量（C05）：组合 FK 指向 workspace(organization_id, id)；
    -- Workspace 无物理删除路径（00000076 计划契约），RESTRICT 兜底防御。
    CONSTRAINT fk_odw_workspace FOREIGN KEY (organization_id, workspace_id)
        REFERENCES workspace(organization_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE  organization_default_workspace IS
    'Org 显式默认 Workspace（每 Org 至多 1 条）；取代最小 active ID 推导（C05 TRANSITION）';
COMMENT ON COLUMN organization_default_workspace.organization_id IS
    '所属机构；PK 即「每 Org 至多一个默认」；机构物理删除一律 RESTRICT（C17 fail-closed）';
COMMENT ON COLUMN organization_default_workspace.workspace_id IS
    '默认 Workspace；必须同 Org（组合 FK）且 status=active（触发器运行时守卫）';
COMMENT ON COLUMN organization_default_workspace.created_at IS
    '设置时间；set 同值幂等不刷新（审计稳定）';
COMMENT ON COLUMN organization_default_workspace.updated_at IS
    '最后变更时间；replace（archive 交接/显式改设）时刷新';

-- ============================================================
-- Phase 3: 运行时守卫（BEFORE 触发器，两职合一）：
--   1. organization_id 由 workspace 行派生（调用方传值被覆盖）；
--   2. 目标 Workspace 必须存在且 status='active'（fail-closed 23514）。
-- 组合 FK 已保证行存在（23503），本触发器补足同 Org 派生 + active 语义。
-- ============================================================
CREATE OR REPLACE FUNCTION fn_organization_default_workspace_active_guard()
    RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_workspace_org bigint;
    v_status        text;
BEGIN
    SELECT organization_id, status INTO v_workspace_org, v_status
      FROM workspace
     WHERE id = NEW.workspace_id;
    IF v_workspace_org IS NULL THEN
        RAISE EXCEPTION
            'workspace % 不存在，不能设为 Org % 的默认 Workspace',
            NEW.workspace_id, NEW.organization_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_organization_default_workspace_active_guard';
    END IF;
    IF v_status IS NULL OR v_status <> 'active' THEN
        RAISE EXCEPTION
            'workspace % 不是 active（当前 %），不能担任 Org 默认 Workspace',
            NEW.workspace_id, v_status
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_organization_default_workspace_active_guard';
    END IF;
    -- 冗余 org 列以 workspace 行为准派生（单一真源；传错 org 被覆盖，
    -- 与组合 FK 双保险杜绝跨 Org 脏行）
    NEW.organization_id := v_workspace_org;
    RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_organization_default_workspace_active_guard
    ON organization_default_workspace;
CREATE TRIGGER trg_organization_default_workspace_active_guard
    BEFORE INSERT OR UPDATE OF workspace_id, organization_id
    ON organization_default_workspace
    FOR EACH ROW EXECUTE FUNCTION fn_organization_default_workspace_active_guard();

-- ============================================================
-- Phase 4: Backfill（可证明的 min-ID 等值回填）
-- legacy 读法（ORDER BY w.id LIMIT 1 ON active）是确定性纯函数：
-- 每 Org 的结果 = MIN(id) WHERE status='active'。逐一可复算 ⇒ 可回填。
-- 已有行不覆盖（ON CONFLICT DO NOTHING，可重复执行）。
-- ============================================================
INSERT INTO organization_default_workspace (organization_id, workspace_id)
SELECT w.organization_id, MIN(w.id)
  FROM workspace w
 WHERE w.organization_id IS NOT NULL
   AND w.status = 'active'
 GROUP BY w.organization_id
ON CONFLICT (organization_id) DO NOTHING;

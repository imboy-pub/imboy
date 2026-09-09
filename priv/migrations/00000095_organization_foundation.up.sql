-- 00000095_organization_foundation.up.sql
-- 墨芽习字 Step 5：Organization 租户层 + workspace.organization_id（expand-first）
-- 计划契约：docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md §6.1、Step 5
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策：
--   * 00000076 曾明确 "I11 不引入 Organization 层"；本迁移按计划 D-06 引入 Organization 作为
--     SaaS 租户边界（一个 Organization 可包含多个 Workspace），不修改任何历史迁移。
--   * expand-first：workspace.organization_id 可空、不回填、不按 owner_id 猜测机构归属、
--     不自动合并历史 Workspace（计划 §6.1 迁移策略 1/2/4）。旧 Workspace 行为完全兼容
--     （DB-ORG-03：仅新增可空列，任何现有查询语义不变）。
--   * id/owner_id 类型与 workspace 保持一致（bigint TSID）；FK 删除行为镜像 00000076 惯例
--     （CASCADE 为主），但 organization_id 用 RESTRICT：物理删除机构前必须先显式解除
--     Workspace 归属（fail-closed；机构生命周期走 status=archived 软删除，不触发物理 DELETE）。
--   * workspace.owner_id 保留为 Workspace 本地治理 Owner（计划 §6.1 迁移策略 5）。

-- ============================================================
-- Phase 1: organization 表
-- ============================================================
CREATE TABLE IF NOT EXISTS organization (
    id          bigint                       NOT NULL,  -- TSID
    name        character varying(200)       NOT NULL,
    owner_id    bigint                       NOT NULL,  -- 机构 Owner（首版唯一管理员，计划 D-14）
    status      text                         DEFAULT 'active' NOT NULL,
    branding    jsonb                        DEFAULT '{}'::jsonb NOT NULL,  -- {name,logo,primaryColor,...} 首版只存必要字段
    settings    jsonb                        DEFAULT '{}'::jsonb NOT NULL,  -- 首版只存必要字段
    created_at  timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at  timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_organization PRIMARY KEY (id),
    CONSTRAINT ck_organization_status CHECK (status = ANY (ARRAY['active'::text, 'archived'::text]))
);

COMMENT ON TABLE  organization              IS '机构（SaaS 租户边界；一个机构可含多个 Workspace；墨芽教学归属锚点）';
COMMENT ON COLUMN organization.id           IS '主键 TSID';
COMMENT ON COLUMN organization.name         IS '机构名称';
COMMENT ON COLUMN organization.owner_id     IS '机构 Owner 用户ID（首版唯一管理员；出现第二位跨 Workspace 管理员时再扩表）';
COMMENT ON COLUMN organization.status       IS '生命周期: active 正常 | archived 已归档';
COMMENT ON COLUMN organization.branding     IS '品牌配置 jsonb：{name,logo,primaryColor,...}，仅存必要字段';
COMMENT ON COLUMN organization.settings     IS '机构设置 jsonb：仅存必要字段';

CREATE INDEX IF NOT EXISTS i_organization_owner_id ON organization USING btree (owner_id);
CREATE INDEX IF NOT EXISTS i_organization_status   ON organization USING btree (status);

ALTER TABLE organization DROP CONSTRAINT IF EXISTS fk_organization_owner;
ALTER TABLE organization ADD CONSTRAINT fk_organization_owner
    FOREIGN KEY (owner_id) REFERENCES "user"(id) ON DELETE CASCADE;

-- ============================================================
-- Phase 2: workspace 增加可空 organization_id（expand-first，不回填）
-- ============================================================
ALTER TABLE workspace ADD COLUMN IF NOT EXISTS organization_id bigint;

COMMENT ON COLUMN workspace.organization_id IS '所属机构ID；可空=尚未加入任何机构（expand-first 兼容历史 Workspace，不按 owner_id 自动归属）；一个 Workspace 最多属于一个 Organization';

CREATE INDEX IF NOT EXISTS i_workspace_organization_id
    ON workspace USING btree (organization_id);

ALTER TABLE workspace DROP CONSTRAINT IF EXISTS fk_workspace_organization;
ALTER TABLE workspace ADD CONSTRAINT fk_workspace_organization
    FOREIGN KEY (organization_id) REFERENCES organization(id) ON DELETE RESTRICT;

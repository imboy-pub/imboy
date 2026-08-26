-- 00000076_workspace_foundation.up.sql
-- 双体验 v2.5.2 WP2/T3 逻辑变更①workspace + ②workspace_member（Gate W = W0）
-- 计划契约：.claude/PRPs/plans/imboy-dual-experience-v21-channel-firstclass.plan.md §七 T3（1/2）
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹，
--           文件内 COMMIT 会提前提交外层事务，其后的失败将无法回滚前置 DDL。
--
-- 设计决策：
--   * workspace 是组织级边界（I11：不引入 Organization 层）；owner_id 是主 Owner/未来计费锚点（只读）。
--   * workspace_member 复合主键 (workspace_id,user_id) 即唯一约束（计划 §七 T3.2 的
--     "唯一约束 (workspace_id,user_id)"），同时充当 project 复合外键的引用目标。
--   * user 外键删除行为镜像本仓既有惯例（00000009：ON DELETE CASCADE 为主，SET NULL 用于可空审计列）。
--   * ⚠️ 移除保护触发器（status→removed 时校验无 active 下级 Group Member）定义在 00000077：
--     其函数体引用 "group".scope 列（00000077 才引入），放在本文件会产生 down 00000077 后的
--     悬挂触发器（R6：每份迁移必须是自洽的原子单元）。
--   * W0 裁剪：本批次不建 project_member / project_milestone / project_channel_rel / project.links。

-- ============================================================
-- Phase 1: workspace 表（逻辑变更①）
-- ============================================================
CREATE TABLE IF NOT EXISTS workspace (
    id          bigint                       NOT NULL,  -- TSID
    name        character varying(200)       NOT NULL,
    logo        text                         DEFAULT '' NOT NULL,
    owner_id    bigint                       NOT NULL,  -- 主 Owner / 未来计费归属锚点（只读，不计费）
    status      text                         DEFAULT 'active' NOT NULL,
    archived_at timestamp with time zone,               -- 归档审计落点（T7）
    archived_by bigint,                                 -- 归档操作人（审计，可空）
    type        text                         DEFAULT 'project' NOT NULL,
    branding    jsonb                        DEFAULT '{}'::jsonb NOT NULL,  -- {name,logo,favicon,primaryColor}
    created_at  timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at  timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT workspace_pkey PRIMARY KEY (id),
    CONSTRAINT chk_workspace_status CHECK (status = ANY (ARRAY['active'::text, 'archived'::text]))
);

COMMENT ON TABLE  workspace          IS '工作区（组织/业务边界：管人和权限；I11 无 Organization 层）';
COMMENT ON COLUMN workspace.id       IS '主键 TSID';
COMMENT ON COLUMN workspace.owner_id IS '主 Owner 用户ID / 未来计费归属锚点（本期只读不计费）';
COMMENT ON COLUMN workspace.status   IS '生命周期: active 正常 | archived 已归档（归档后写守卫只读）';
COMMENT ON COLUMN workspace.archived_at IS '归档时间（T7 归档审计）';
COMMENT ON COLUMN workspace.archived_by IS '归档操作人（T7 归档审计）';
COMMENT ON COLUMN workspace.type     IS '工作区类型，默认 project；预留扩展，本期无值域约束';
COMMENT ON COLUMN workspace.branding IS '品牌配置 jsonb：{name,logo,favicon,primaryColor}，仅 Workspace 视图作用域（T12）';

CREATE INDEX IF NOT EXISTS i_workspace_owner_id ON workspace USING btree (owner_id);
CREATE INDEX IF NOT EXISTS i_workspace_status   ON workspace USING btree (status);

ALTER TABLE workspace DROP CONSTRAINT IF EXISTS fk_workspace_owner;
ALTER TABLE workspace ADD CONSTRAINT fk_workspace_owner
    FOREIGN KEY (owner_id) REFERENCES "user"(id) ON DELETE CASCADE;

ALTER TABLE workspace DROP CONSTRAINT IF EXISTS fk_workspace_archived_by;
ALTER TABLE workspace ADD CONSTRAINT fk_workspace_archived_by
    FOREIGN KEY (archived_by) REFERENCES "user"(id) ON DELETE SET NULL;

-- ============================================================
-- Phase 2: workspace_member 表（逻辑变更②）
-- ============================================================
-- 复合主键 (workspace_id,user_id) 即计划要求的唯一约束；
-- role 仅 owner|member|guest 三角色（§1.4.2，禁止扩展为通用 RBAC）；
-- status 仅 active|removed（软移除；重新加入写新行覆盖，不自动恢复下级关系）。
CREATE TABLE IF NOT EXISTS workspace_member (
    workspace_id bigint                       NOT NULL,
    user_id      bigint                       NOT NULL,
    role         text                         DEFAULT 'member' NOT NULL,
    invited_by   bigint,                                  -- 邀请人（可空=Owner 自建）
    joined_at    timestamp with time zone,               -- 实际加入时间（可空=邀请未确认）
    status       text                         DEFAULT 'active' NOT NULL,
    created_at   timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at   timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT workspace_member_pkey PRIMARY KEY (workspace_id, user_id),
    CONSTRAINT chk_workspace_member_role   CHECK (role = ANY (ARRAY['owner'::text, 'member'::text, 'guest'::text])),
    CONSTRAINT chk_workspace_member_status CHECK (status = ANY (ARRAY['active'::text, 'removed'::text]))
);

COMMENT ON TABLE  workspace_member            IS '工作区成员（组织访问边界；三角色 Owner/Member/Guest；区别于群成员/频道订阅者）';
COMMENT ON COLUMN workspace_member.role       IS '角色: owner 群主级治理 | member 普通成员 | guest 只读访客（无第四种角色）';
COMMENT ON COLUMN workspace_member.invited_by IS '邀请人用户ID（NULL=Owner 创建工作区时的初始记录）';
COMMENT ON COLUMN workspace_member.joined_at  IS '实际加入时间（邀请确认后写入）';
COMMENT ON COLUMN workspace_member.status     IS '状态: active 在册 | removed 已移除（软删，重新加入不自动恢复下级关系）';

ALTER TABLE workspace_member DROP CONSTRAINT IF EXISTS fk_workspace_member_workspace;
ALTER TABLE workspace_member ADD CONSTRAINT fk_workspace_member_workspace
    FOREIGN KEY (workspace_id) REFERENCES workspace(id) ON DELETE CASCADE;

ALTER TABLE workspace_member DROP CONSTRAINT IF EXISTS fk_workspace_member_user;
ALTER TABLE workspace_member ADD CONSTRAINT fk_workspace_member_user
    FOREIGN KEY (user_id) REFERENCES "user"(id) ON DELETE CASCADE;

ALTER TABLE workspace_member DROP CONSTRAINT IF EXISTS fk_workspace_member_invited_by;
ALTER TABLE workspace_member ADD CONSTRAINT fk_workspace_member_invited_by
    FOREIGN KEY (invited_by) REFERENCES "user"(id) ON DELETE SET NULL;

-- 我的 工作区 列表（user 维度过滤 + active 过滤）
CREATE INDEX IF NOT EXISTS i_workspace_member_uid_ws_status
    ON workspace_member USING btree (user_id, workspace_id, status);
-- 成员管理 / 角色矩阵查询（workspace 维度）
CREATE INDEX IF NOT EXISTS i_workspace_member_ws_role_status
    ON workspace_member USING btree (workspace_id, role, status);

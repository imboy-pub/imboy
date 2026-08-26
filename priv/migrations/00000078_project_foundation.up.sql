-- 00000078_project_foundation.up.sql
-- 双体验 v2.5.2 WP2/T3 逻辑变更⑤project(W0版) + ⑧project_task + ⑩project_event（Gate W = W0）
-- 计划契约：.claude/PRPs/plans/imboy-dual-experience-v21-channel-firstclass.plan.md §七 T3（5/8/10）
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- W0 裁剪（Scope Contract，Gate W=W0；违反即验收失败）：
--   * project 无 links 列（Resources 聚合 defer）
--   * 不建 project_member（W0 禁止落表；Project 对 active Workspace Member 可见）
--   * 不建 project_milestone（defer）、project_channel_rel（defer）
--   * 无 Project Member 触发器（⑤只保留 Workspace Membership 校验）
--
-- 设计决策：
--   * project.owner 的双重保证：
--       - 复合外键 (workspace_id,owner_id)→workspace_member(workspace_id,user_id)
--         DEFERRABLE INITIALLY DEFERRED —— 允许 T6a 同一事务"先建 Project、再建 Owner Member 记录"；
--       - 可延迟约束触发器 trg_project_owner_membership_active —— 提交时 owner 的
--         Workspace Membership 必须为 active（FK 只能保证行存在，不能保证状态）。
--   * 唯一约束 (id,workspace_id) + 索引 (workspace_id,id)：I3 一个 Workspace 可有多个 Project。
--   * project_task.assignee 的 active 校验由 T6b 应用层同事务验证（W0 查 active workspace_member）；
--     schema 层仅保留 user 存在性外键。
--   * project_event 是 Activity 唯一数据源：纯系统事件流，不含消息/公告正文（§1.4）；
--     event_type CHECK 仅列 W0 实际写入的值（未来加值需新迁移，正常演进；不引用
--     milestone_status/channel_rel 等已 defer 能力）。
--   * 日志表 actor_id 不设 user 外键（镜像本仓 app_upgrade_log 惯例：审计历史不随用户删除抹除；
--     本系统 user 为软删 status=-1，物理删除不在路径上）。

-- ============================================================
-- Phase 1: project 表（逻辑变更⑤，W0 版）
-- ============================================================
CREATE TABLE IF NOT EXISTS project (
    id           bigint                       NOT NULL,  -- TSID
    workspace_id bigint                       NOT NULL,
    name         character varying(200)       NOT NULL,
    description  text                         DEFAULT '' NOT NULL,
    owner_id     bigint                       NOT NULL,  -- W0：必须是 active workspace_member（触发器保证）
    status       text                         DEFAULT 'active' NOT NULL,
    created_at   timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at   timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT project_pkey PRIMARY KEY (id),
    CONSTRAINT uk_project_id_workspace UNIQUE (id, workspace_id),
    CONSTRAINT chk_project_status CHECK (status = ANY (ARRAY['active'::text, 'done'::text]))
);

COMMENT ON TABLE  project                 IS '项目（可选执行层：目标→任务→交付；W0 Project Lite，对 active workspace_member 可见）';
COMMENT ON COLUMN project.workspace_id    IS '所属工作区ID（I3 非空外键；一个 Workspace 可有多个 Project）';
COMMENT ON COLUMN project.owner_id        IS '项目 Owner；提交时必须是同 workspace 的 active workspace_member（W0 无 project_member）';
COMMENT ON COLUMN project.status          IS '状态: active 进行中 | done 已完成';

ALTER TABLE project DROP CONSTRAINT IF EXISTS fk_project_workspace;
ALTER TABLE project ADD CONSTRAINT fk_project_workspace
    FOREIGN KEY (workspace_id) REFERENCES workspace(id) ON DELETE CASCADE;

-- 可延迟复合外键：允许 T6a 同一事务先建 Project 再建 Owner Member 记录（§七 T3.5）
ALTER TABLE project DROP CONSTRAINT IF EXISTS fk_project_owner_membership;
ALTER TABLE project ADD CONSTRAINT fk_project_owner_membership
    FOREIGN KEY (workspace_id, owner_id) REFERENCES workspace_member(workspace_id, user_id)
    ON DELETE CASCADE
    DEFERRABLE INITIALLY DEFERRED;

-- 工作区项目列表（I5：列表查询按 workspace_id 过滤）
CREATE INDEX IF NOT EXISTS i_project_workspace_id_id ON project USING btree (workspace_id, id);

-- 可延迟约束触发器：提交时 owner 的 Workspace Membership 必须 active
CREATE OR REPLACE FUNCTION fn_project_owner_membership_active() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM workspace_member
         WHERE workspace_id = NEW.workspace_id
           AND user_id = NEW.owner_id
           AND status = 'active'
    ) THEN
        RAISE EXCEPTION
            'project Owner 校验：用户 % 不是 workspace % 的 active workspace_member，'
            '不能成为项目 % 的 owner',
            NEW.owner_id, NEW.workspace_id, NEW.id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_project_owner_membership_active',
                  HINT = '同一事务内先写入/激活该用户的 workspace_member(active) 记录，再提交项目';
    END IF;
    RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_project_owner_membership_active ON project;
CREATE CONSTRAINT TRIGGER trg_project_owner_membership_active
    AFTER INSERT OR UPDATE OF owner_id, workspace_id ON project
    DEFERRABLE INITIALLY DEFERRED
    FOR EACH ROW EXECUTE FUNCTION fn_project_owner_membership_active();

COMMENT ON FUNCTION fn_project_owner_membership_active() IS
    '项目 Owner 子集校验：owner 提交时必须是同 workspace 的 active workspace_member（W0 版，无 Project Member 触发器，可延迟到 COMMIT）';

-- ============================================================
-- Phase 2: project_task 表（逻辑变更⑧，Tasks 四态 = W0 必选项）
-- ============================================================
CREATE TABLE IF NOT EXISTS project_task (
    id          bigint                       NOT NULL,  -- TSID
    project_id  bigint                       NOT NULL,
    title       character varying(500)       NOT NULL,
    creator_id  bigint                       NOT NULL,
    assignee_id bigint,                                 -- 可空=未指派；active 校验由 T6b 同事务验证
    status      text                         DEFAULT 'todo' NOT NULL,
    sort        integer                      DEFAULT 0 NOT NULL,
    created_at  timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at  timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT project_task_pkey PRIMARY KEY (id),
    CONSTRAINT chk_project_task_status CHECK (status = ANY (ARRAY['todo'::text, 'doing'::text, 'review'::text, 'done'::text])),
    CONSTRAINT chk_project_task_sort CHECK (sort >= 0)
);

COMMENT ON TABLE  project_task            IS '项目任务（轻量执行实体：仅 title/assignee/status/排序；禁甘特图/依赖/估点/子任务）';
COMMENT ON COLUMN project_task.assignee_id IS '负责人；W0 必须来自 active workspace_member（T6b 应用层同事务校验，schema 仅保证 user 存在）';
COMMENT ON COLUMN project_task.status     IS '四态: todo 待办 | doing 进行中 | review 待评审 | done 已完成（含回退，由应用层状态机控制）';
COMMENT ON COLUMN project_task.sort       IS '手动排序值，同 status 内升序展示';

ALTER TABLE project_task DROP CONSTRAINT IF EXISTS fk_project_task_project;
ALTER TABLE project_task ADD CONSTRAINT fk_project_task_project
    FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE;

ALTER TABLE project_task DROP CONSTRAINT IF EXISTS fk_project_task_creator;
ALTER TABLE project_task ADD CONSTRAINT fk_project_task_creator
    FOREIGN KEY (creator_id) REFERENCES "user"(id) ON DELETE CASCADE;

ALTER TABLE project_task DROP CONSTRAINT IF EXISTS fk_project_task_assignee;
ALTER TABLE project_task ADD CONSTRAINT fk_project_task_assignee
    FOREIGN KEY (assignee_id) REFERENCES "user"(id) ON DELETE SET NULL;

-- 看板/列表查询：项目内按状态分组 + 排序（§七 T3.8 索引原文）
CREATE INDEX IF NOT EXISTS i_project_task_project_status_sort
    ON project_task USING btree (project_id, status, sort, id);

-- ============================================================
-- Phase 3: project_event 表（逻辑变更⑩，Activity 唯一数据源）
-- ============================================================
CREATE TABLE IF NOT EXISTS project_event (
    id          bigint                       NOT NULL,  -- TSID
    project_id  bigint                       NOT NULL,
    actor_id    bigint                       NOT NULL,
    event_type  text                         NOT NULL,
    target_id   bigint,                                 -- 事件对象（如 task id；可空）
    payload     jsonb                        DEFAULT '{}'::jsonb NOT NULL,
    created_at  timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT project_event_pkey PRIMARY KEY (id),
    CONSTRAINT chk_project_event_type CHECK (
        event_type = ANY (ARRAY[
            'project_created'::text,
            'project_status'::text,
            'task_created'::text,
            'task_status'::text,
            'task_assignee'::text
        ]))
);

COMMENT ON TABLE  project_event            IS '项目 Activity 唯一数据源（纯系统事件流；不含消息/群公告正文；业务写与事件同事务提交）';
COMMENT ON COLUMN project_event.event_type IS 'W0 实际写入的事件类型；未来扩展需新迁移修改 CHECK（正常演进；不含已 defer 的 milestone_status/channel_rel/member_change）';
COMMENT ON COLUMN project_event.actor_id   IS '操作者用户ID（审计历史，不设 user 外键）';
COMMENT ON COLUMN project_event.target_id  IS '事件目标资源ID（如 project_task.id）';

ALTER TABLE project_event DROP CONSTRAINT IF EXISTS fk_project_event_project;
ALTER TABLE project_event ADD CONSTRAINT fk_project_event_project
    FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE;

-- Activity 时间线（§七 T3.10 索引原文：分页稳定）
CREATE INDEX IF NOT EXISTS i_project_event_project_created
    ON project_event USING btree (project_id, created_at, id);

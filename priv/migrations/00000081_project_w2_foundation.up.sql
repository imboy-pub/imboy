-- 00000081_project_w2_foundation.up.sql
-- Channel-first-class W2：project_member + project_milestone + project_channel_rel + project.links
-- 计划契约：docs/planning/imboy-channel-firstclass-w2-alpha-release-execution-plan.md（ZC-01）
-- Scope 契约：docs/planning/channel-firstclass-w2-execution-ledger.md §4（H1 放行 2026-08-29）
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹，
--           文件内 COMMIT 会提前提交外层事务，其后的失败将无法回滚前置 DDL。
--
-- 设计决策：
--   * I14 同款子集模型（00000077 先例）：project_member ⊆ active workspace_member 在 DB 层
--     双向 fail-closed——写入端 trg_project_member_ws_active（可延迟，允许同事务先建成员后激活
--     Workspace 身份），移除端 trg_workspace_member_remove_guard_pm（workspace_member 移除前
--     不得残留 active project_member）。00000077 的 group_member 兜底触发器不可修改（R6：
--     每份迁移自洽），故本项目兜底触发器独立命名 _pm 落在本文件。
--   * 复合 FK 同 Workspace 强制（R3）：
--       project 已有 uk_project_id_workspace UNIQUE (id, workspace_id)（00000078）；
--       workspace_member PK (workspace_id, user_id)（00000076）；
--       channel 本迁移补 uk_channel_id_workspace UNIQUE (workspace_id, id)。
--     project_channel_rel 的 (workspace_id, channel_id) 复合 FK 使跨 Workspace 频道与
--     personal 频道（workspace_id IS NULL）在 FK 层即被拒绝。
--   * "重复成员/关联只一行"：project_member PK (project_id, user_id)、
--     project_channel_rel PK (project_id, channel_id)，幂等 UPSERT 语义由应用层实现。
--   * project_milestone 字段只允许 name/due_date/status（+ reached_at 审计）；
--     chk_project_milestone_reached 保证 status='reached' ⟺ reached_at IS NOT NULL，
--     planned→reached 单向状态机由应用层执行。
--   * project.links 为 jsonb 数组（[{name,url}]），形状由 trg_project_links_shape
--     BEFORE 触发器强制（PG CHECK 不允许子查询/集合展开，触发器是仓内既有惯例）。
--   * project_event 的 chk_project_event_type 扩入 W2 事件类型（W0 五值保留，
--     事件类型即 API 契约，后续 ZC-02/03/04 写入端不得自造值）。
--   * W0 存量回填（Owner 自动入项目）：为每个 owner 为 active workspace_member 的
--     project 回填一行 active project_member；owner 身份已失效的存量 project 跳过
--     （不阻塞迁移，留待人工治理）。ON CONFLICT DO NOTHING 保证可重复执行。
--   * 普通 CREATE INDEX（erlang_migrate 恒包事务，CONCURRENTLY 不可用，见 00000077 R2.5）；
--     本迁移全部为空表/新增列上的毫秒级元数据操作。

-- ============================================================
-- Phase 1: channel 复合唯一（project_channel_rel 复合 FK 的引用目标）
-- ============================================================
ALTER TABLE channel DROP CONSTRAINT IF EXISTS uk_channel_id_workspace;
ALTER TABLE channel ADD CONSTRAINT uk_channel_id_workspace UNIQUE (workspace_id, id);

-- ============================================================
-- Phase 2: project_member 表（W2 逻辑：Project Member 隔离）
-- ============================================================
CREATE TABLE IF NOT EXISTS project_member (
    workspace_id bigint                   NOT NULL,
    project_id   bigint                   NOT NULL,
    user_id      bigint                   NOT NULL,
    invited_by   bigint,                              -- 邀请人（可空=Owner 回填/自入）
    joined_at    timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    status       text                     DEFAULT 'active' NOT NULL,
    created_at   timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at   timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT project_member_pkey PRIMARY KEY (project_id, user_id),
    CONSTRAINT chk_project_member_status CHECK (status = ANY (ARRAY['active'::text, 'removed'::text]))
);

COMMENT ON TABLE  project_member              IS '项目成员（Project 访问边界；无 Project Role，读写权限继承 Workspace Member 角色）';
COMMENT ON COLUMN project_member.workspace_id IS '冗余 Workspace ID；与 (project_id,user_id) 一起构成复合 FK 子集约束（同 Workspace 强制）';
COMMENT ON COLUMN project_member.project_id   IS '所属项目ID（PK 前缀，唯一成员身份即"重复成员只一行"）';
COMMENT ON COLUMN project_member.user_id      IS '成员用户ID';
COMMENT ON COLUMN project_member.invited_by   IS '邀请人用户ID（NULL=Owner 存量回填/自建）';
COMMENT ON COLUMN project_member.joined_at    IS '加入时间（Project Member 无"邀请未确认"态，写入即生效）';
COMMENT ON COLUMN project_member.status       IS '状态: active 在册 | removed 已移除（软删；重新邀请由应用层覆盖激活，不自动恢复历史下级）';

-- ⚠️ 复合 FK 列序按位置配对：引用列序必须与 project 唯一索引 (id, workspace_id) 对齐，
-- 故 FK 列序为 (project_id, workspace_id)。
ALTER TABLE project_member DROP CONSTRAINT IF EXISTS fk_project_member_project;
ALTER TABLE project_member ADD CONSTRAINT fk_project_member_project
    FOREIGN KEY (project_id, workspace_id) REFERENCES project(id, workspace_id)
    ON DELETE CASCADE;

ALTER TABLE project_member DROP CONSTRAINT IF EXISTS fk_project_member_ws_member;
ALTER TABLE project_member ADD CONSTRAINT fk_project_member_ws_member
    FOREIGN KEY (workspace_id, user_id) REFERENCES workspace_member(workspace_id, user_id)
    ON DELETE CASCADE;

ALTER TABLE project_member DROP CONSTRAINT IF EXISTS fk_project_member_invited_by;
ALTER TABLE project_member ADD CONSTRAINT fk_project_member_invited_by
    FOREIGN KEY (invited_by) REFERENCES "user"(id) ON DELETE SET NULL;

-- 项目成员列表（project 维度；PK 前缀已覆盖 project_id 过滤）
-- 我的项目列表（user × workspace 维度过滤）
CREATE INDEX IF NOT EXISTS i_project_member_ws_uid_status
    ON project_member USING btree (workspace_id, user_id, status);

-- 写入端：active project_member 提交时必须存在同 workspace 的 active workspace_member
-- （复合 FK 只保证行存在，active 语义由本触发器 fail-closed；可延迟允许同事务先建成员）
CREATE OR REPLACE FUNCTION fn_project_member_ws_active_check() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.status = 'active' AND NOT EXISTS (
        SELECT 1 FROM workspace_member
         WHERE workspace_id = NEW.workspace_id
           AND user_id = NEW.user_id
           AND status = 'active'
    ) THEN
        RAISE EXCEPTION
            'project_member 子集约束（Project Member ⊆ Workspace Member）：'
            '用户 % 不是 workspace % 的 active workspace_member，不能成为 project % 的 active 成员',
            NEW.user_id, NEW.workspace_id, NEW.project_id
        USING ERRCODE = '23514',
              CONSTRAINT = 'trg_project_member_ws_active',
              HINT = '先将该用户加入 workspace（workspace_member.status=active），再写入/恢复项目成员';
    END IF;
    RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_project_member_ws_active ON project_member;
CREATE CONSTRAINT TRIGGER trg_project_member_ws_active
    AFTER INSERT OR UPDATE OF status, user_id, project_id, workspace_id ON project_member
    DEFERRABLE INITIALLY DEFERRED
    FOR EACH ROW EXECUTE FUNCTION fn_project_member_ws_active_check();

-- 移除端：workspace_member 置 removed/DELETE 时，同 workspace 不得残留该用户的
-- active project_member（正确流程：先移除项目成员再移除工作区成员，同事务完成；
-- 本触发器是 DB 层 fail-closed 兜底，与 00000077 的 group_member 兜底 _pm 互补）
CREATE OR REPLACE FUNCTION fn_workspace_member_remove_guard_pm() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_OP = 'UPDATE' AND NEW.status <> 'removed' THEN
        RETURN NEW;  -- 仅约束 active → removed 的移除方向（恢复 active 不受限）
    END IF;
    IF EXISTS (
        SELECT 1
          FROM project_member pm
         WHERE pm.workspace_id = OLD.workspace_id
           AND pm.user_id = OLD.user_id
           AND pm.status = 'active'
    ) THEN
        RAISE EXCEPTION
            'workspace_member 移除保护（W2）：用户 % 在 workspace % 仍有 active 的项目成员关系，'
            '不能移除其工作区成员身份（Project Member ⊆ Workspace Member）',
            OLD.user_id, OLD.workspace_id
        USING ERRCODE = '23514',
              CONSTRAINT = 'trg_workspace_member_remove_guard_pm',
              HINT = '同一事务内先移除该用户的项目成员（project_member），再移除工作区成员';
    END IF;
    RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_workspace_member_remove_guard_pm ON workspace_member;
CREATE CONSTRAINT TRIGGER trg_workspace_member_remove_guard_pm
    AFTER UPDATE OF status OR DELETE ON workspace_member
    DEFERRABLE INITIALLY DEFERRED
    FOR EACH ROW EXECUTE FUNCTION fn_workspace_member_remove_guard_pm();

COMMENT ON FUNCTION fn_project_member_ws_active_check() IS
    'Project 成员写入端子集校验：active project_member 提交时必须是同 workspace 的 active workspace_member（W2，可延迟到 COMMIT）';
COMMENT ON FUNCTION fn_workspace_member_remove_guard_pm() IS
    '工作区成员移除端保护（W2）：removed/DELETE 前同 workspace 不得有该用户的 active project_member（可延迟到 COMMIT）';

-- ============================================================
-- Phase 3: project_milestone 表（字段只允许 name/due_date/status）
-- ============================================================
CREATE TABLE IF NOT EXISTS project_milestone (
    id           bigint                   NOT NULL,  -- TSID
    workspace_id bigint                   NOT NULL,
    project_id   bigint                   NOT NULL,
    name         character varying(200)   NOT NULL,
    due_date     date,
    status       text                     DEFAULT 'planned' NOT NULL,
    reached_at   timestamp with time zone,
    created_at   timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at   timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT project_milestone_pkey PRIMARY KEY (id),
    CONSTRAINT chk_project_milestone_status CHECK (status = ANY (ARRAY['planned'::text, 'reached'::text])),
    CONSTRAINT chk_project_milestone_reached CHECK ((status = 'reached') = (reached_at IS NOT NULL))
);

COMMENT ON TABLE  project_milestone            IS '项目里程碑（W2：仅 name/due_date/status，禁依赖/甘特图/复杂状态机）';
COMMENT ON COLUMN project_milestone.id         IS '主键 TSID';
COMMENT ON COLUMN project_milestone.workspace_id IS '冗余 Workspace ID（复合 FK 同 Workspace 强制）';
COMMENT ON COLUMN project_milestone.status     IS '状态: planned 计划 | reached 已达成（单向 planned→reached，由应用层状态机控制）';
COMMENT ON COLUMN project_milestone.reached_at IS '达成时间（status=reached 时必须非空，CHECK 同步强制）';

ALTER TABLE project_milestone DROP CONSTRAINT IF EXISTS fk_project_milestone_project;
ALTER TABLE project_milestone ADD CONSTRAINT fk_project_milestone_project
    FOREIGN KEY (project_id, workspace_id) REFERENCES project(id, workspace_id)
    ON DELETE CASCADE;

-- 里程碑列表（project 维度 + 状态过滤，分页稳定）
CREATE INDEX IF NOT EXISTS i_project_milestone_project_status
    ON project_milestone USING btree (project_id, status, id);

-- ============================================================
-- Phase 4: project_channel_rel 表（同 Workspace 关联，重复 link 幂等）
-- ============================================================
CREATE TABLE IF NOT EXISTS project_channel_rel (
    workspace_id bigint                   NOT NULL,
    project_id   bigint                   NOT NULL,
    channel_id   bigint                   NOT NULL,
    created_by   bigint,                              -- 关联操作人（审计，可空）
    created_at   timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT project_channel_rel_pkey PRIMARY KEY (project_id, channel_id)
);

COMMENT ON TABLE  project_channel_rel              IS '项目↔频道关联（Channel-first-class W2；同 Workspace 强制，重复 link 幂等只一行）';
COMMENT ON COLUMN project_channel_rel.workspace_id IS '冗余 Workspace ID；两侧复合 FK 强制 project 与 channel 同 workspace（personal 频道不可关联）';
COMMENT ON COLUMN project_channel_rel.channel_id   IS '被关联频道ID（scope=workspace）';
COMMENT ON COLUMN project_channel_rel.created_by   IS '关联操作人用户ID（审计）';

ALTER TABLE project_channel_rel DROP CONSTRAINT IF EXISTS fk_project_channel_rel_project;
ALTER TABLE project_channel_rel ADD CONSTRAINT fk_project_channel_rel_project
    FOREIGN KEY (project_id, workspace_id) REFERENCES project(id, workspace_id)
    ON DELETE CASCADE;

ALTER TABLE project_channel_rel DROP CONSTRAINT IF EXISTS fk_project_channel_rel_channel;
ALTER TABLE project_channel_rel ADD CONSTRAINT fk_project_channel_rel_channel
    FOREIGN KEY (workspace_id, channel_id) REFERENCES channel(workspace_id, id)
    ON DELETE CASCADE;

ALTER TABLE project_channel_rel DROP CONSTRAINT IF EXISTS fk_project_channel_rel_created_by;
ALTER TABLE project_channel_rel ADD CONSTRAINT fk_project_channel_rel_created_by
    FOREIGN KEY (created_by) REFERENCES "user"(id) ON DELETE SET NULL;

-- 反查：频道的关联项目列表（PK 前缀覆盖 project 侧列表）
CREATE INDEX IF NOT EXISTS i_project_channel_rel_channel
    ON project_channel_rel USING btree (channel_id);

-- ============================================================
-- Phase 5: project.links 列 + 形状触发器（Resources 聚合载体）
-- ============================================================
ALTER TABLE project ADD COLUMN IF NOT EXISTS links jsonb DEFAULT '[]'::jsonb NOT NULL;

COMMENT ON COLUMN project.links IS 'Resources 聚合的手工链接数组 [{name,url}]（name/url 均非空字符串；形状由 trg_project_links_shape 强制）';

-- 形状校验（PG CHECK 不支持集合展开，按仓内惯例用 BEFORE 触发器 fail-closed）
CREATE OR REPLACE FUNCTION fn_project_links_shape_check() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    el jsonb;
BEGIN
    IF NEW.links IS NULL OR jsonb_typeof(NEW.links) <> 'array' THEN
        RAISE EXCEPTION
            'project.links 形状校验失败：links 必须是 jsonb 数组（收到 %）',
            COALESCE(jsonb_typeof(NEW.links), 'NULL')
        USING ERRCODE = '23514',
              CONSTRAINT = 'trg_project_links_shape';
    END IF;
    FOR el IN SELECT * FROM jsonb_array_elements(NEW.links) LOOP
        IF jsonb_typeof(el) <> 'object'
           OR (el->>'name' IS NULL) OR (el->>'url' IS NULL)
           OR length(el->>'name') = 0 OR length(el->>'url') = 0
        THEN
            RAISE EXCEPTION
                'project.links 形状校验失败：每个元素必须是含非空 name 与 url 字符串的对象（收到 %）',
                el
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_project_links_shape';
        END IF;
    END LOOP;
    RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_project_links_shape ON project;
CREATE TRIGGER trg_project_links_shape
    BEFORE INSERT OR UPDATE OF links ON project
    FOR EACH ROW EXECUTE FUNCTION fn_project_links_shape_check();

COMMENT ON FUNCTION fn_project_links_shape_check() IS
    'project.links 形状校验：必须是 [{name,url}] 数组且 name/url 非空字符串（W2 Resources 聚合载体）';

-- ============================================================
-- Phase 6: project_event CHECK 扩入 W2 事件类型（W0 五值保留）
-- ============================================================
-- 事件类型即 API 契约（Activity 聚合数据源），ZC-02/03/04 写入端不得自造值。
ALTER TABLE project_event DROP CONSTRAINT IF EXISTS chk_project_event_type;
ALTER TABLE project_event ADD CONSTRAINT chk_project_event_type CHECK (
    event_type = ANY (ARRAY[
        'project_created'::text,
        'project_status'::text,
        'task_created'::text,
        'task_status'::text,
        'task_assignee'::text,
        'member_invited'::text,
        'member_removed'::text,
        'member_owner_transferred'::text,
        'milestone_created'::text,
        'milestone_updated'::text,
        'milestone_reached'::text,
        'channel_linked'::text,
        'channel_unlinked'::text,
        'links_updated'::text
    ]));

-- ============================================================
-- Phase 7: W0 存量回填（Owner 自动入项目）
-- ============================================================
-- 仅回填 owner 仍为 active workspace_member 的 project；owner 身份已失效的存量行
-- 跳过（不阻塞迁移），由 w0_owner_backfill_invariant 不变式持续盯守。
-- ON CONFLICT DO NOTHING 保证可重复执行。
INSERT INTO project_member (workspace_id, project_id, user_id, joined_at, status)
SELECT p.workspace_id, p.id, p.owner_id,
       COALESCE(p.created_at, CURRENT_TIMESTAMP),
       'active'
FROM project p
WHERE EXISTS (
    SELECT 1 FROM workspace_member wm
     WHERE wm.workspace_id = p.workspace_id
       AND wm.user_id = p.owner_id
       AND wm.status = 'active'
)
ON CONFLICT (project_id, user_id) DO NOTHING;

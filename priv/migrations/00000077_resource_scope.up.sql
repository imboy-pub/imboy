-- 00000077_resource_scope.up.sql
-- 双体验 v2.5.2 WP2/T3 逻辑变更③channel scope + ④"group" scope + 双向子集触发器（Gate W = W0）
-- 计划契约：.claude/PRPs/plans/imboy-dual-experience-v21-channel-firstclass.plan.md §七 T3（3/4）
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策：
--   * I1 归属维度单一：scope('personal'|'workspace') + workspace_id 双向 XOR CHECK（R2.3 草稿原文）。
--   * I2 存量回填 = 零回填：ADD COLUMN ... DEFAULT 'personal'（PG 18 元数据级变更，不重写堆），
--     存量 channel/"group" 行自动 scope='personal' AND workspace_id IS NULL，不创建虚构 Workspace。
--   * XOR CHECK 隐含 scope 值域（scope='personal'/'workspace' 之外的两分支均不成立 → 拒绝）。
--   * ⚠️ "group" 是保留字表名，SQL 中必须双引号（R2.2 已实测确认）。
--   * group_notice 不加 scope：经 group 归属解析 Workspace（I12，群公告只属于 Group）。
--   * Group Member ⊆ Workspace Member（I14，W0 强制）：
--       - trg_group_member_ws_subset      —— 写入端：scope=workspace 群的 active group_member
--         在提交时必须存在同 workspace 的 active workspace_member；
--       - trg_workspace_member_remove_guard —— 移除端（逻辑变更②的触发器，依赖本文件的
--         "group".scope 列，故按 R6"每份迁移自洽"原则从 00000076 移入本文件）：workspace_member
--         置 removed 时，同 workspace 不得残留该用户的 active 下级 group_member。
--     两者均为 DEFERRABLE INITIALLY DEFERRED 约束触发器，允许 T4/T5 在同一事务内任意顺序写
--     workspace_member 与 group_member，提交时统一校验。
--   * group_member 现状（00000001_foundation）：唯一索引 uk_gid_uid(group_id,user_id)；
--     active = status=1（CHECK -1|0|1|2）；is_join 为独立维度不参与本约束；该表无外键。
--   * 普通 CREATE INDEX（经 R2.5/R6 结论：erlang_migrate 恒包事务，CONCURRENTLY 不可用，
--     索引步骤需发布窗口；本地 26/515 行毫秒级）。

-- ============================================================
-- Phase 1: channel 加 scope + workspace_id（逻辑变更③）
-- ============================================================
ALTER TABLE channel ADD COLUMN IF NOT EXISTS scope text NOT NULL DEFAULT 'personal';
ALTER TABLE channel ADD COLUMN IF NOT EXISTS workspace_id bigint;

COMMENT ON COLUMN channel.scope        IS '归属维度: personal 个人（直属用户，无 workspace）| workspace 工作区（I1，创建后不可变）';
COMMENT ON COLUMN channel.workspace_id IS '所属工作区ID；scope=personal 时必须为 NULL（XOR CHECK 强制）';

ALTER TABLE channel DROP CONSTRAINT IF EXISTS chk_channel_scope_xor;
ALTER TABLE channel ADD CONSTRAINT chk_channel_scope_xor CHECK (
    (scope = 'personal' AND workspace_id IS NULL) OR
    (scope = 'workspace' AND workspace_id IS NOT NULL));

ALTER TABLE channel DROP CONSTRAINT IF EXISTS fk_channel_workspace;
ALTER TABLE channel ADD CONSTRAINT fk_channel_workspace
    FOREIGN KEY (workspace_id) REFERENCES workspace(id) ON DELETE CASCADE;

-- 个人频道列表（"我创建的频道"，R2.3 草稿原文）
CREATE INDEX IF NOT EXISTS i_channel_scope_personal
    ON channel USING btree (creator_uid, created_at DESC)
    WHERE scope = 'personal' AND status = 1;
-- 工作区频道列表（Workspace Experience 导航）
CREATE INDEX IF NOT EXISTS i_channel_scope_ws
    ON channel USING btree (workspace_id, created_at DESC)
    WHERE scope = 'workspace' AND status = 1;

-- ============================================================
-- Phase 2: "group" 加 scope + workspace_id（逻辑变更④）
-- ============================================================
ALTER TABLE "group" ADD COLUMN IF NOT EXISTS scope text NOT NULL DEFAULT 'personal';
ALTER TABLE "group" ADD COLUMN IF NOT EXISTS workspace_id bigint;

COMMENT ON COLUMN "group".scope        IS '归属维度: personal 个人 | workspace 工作区（I1，创建后不可变）';
COMMENT ON COLUMN "group".workspace_id IS '所属工作区ID；scope=personal 时必须为 NULL（XOR CHECK 强制）';

ALTER TABLE "group" DROP CONSTRAINT IF EXISTS chk_group_scope_xor;
ALTER TABLE "group" ADD CONSTRAINT chk_group_scope_xor CHECK (
    (scope = 'personal' AND workspace_id IS NULL) OR
    (scope = 'workspace' AND workspace_id IS NOT NULL));

ALTER TABLE "group" DROP CONSTRAINT IF EXISTS fk_group_workspace;
ALTER TABLE "group" ADD CONSTRAINT fk_group_workspace
    FOREIGN KEY (workspace_id) REFERENCES workspace(id) ON DELETE CASCADE;

-- 个人群列表（owner_uid 对应 channel 索引的 creator_uid，R2.3 草稿"同款"）
CREATE INDEX IF NOT EXISTS i_group_scope_personal
    ON "group" USING btree (owner_uid, created_at DESC)
    WHERE scope = 'personal' AND status = 1;
-- 工作区群列表
CREATE INDEX IF NOT EXISTS i_group_scope_ws
    ON "group" USING btree (workspace_id, created_at DESC)
    WHERE scope = 'workspace' AND status = 1;

-- ============================================================
-- Phase 3: Group Member ⊆ Workspace Member 子集触发器（可延迟）
-- ============================================================
-- 写入端：group_member 的 active 写入/恢复（INSERT，或 UPDATE 使 status=1 / 换人换群）时，
-- 若目标 group 为 scope='workspace'，提交时必须存在同 workspace 的 active workspace_member。
-- personal 群直通（不进入 Workspace 子集约束）。
CREATE OR REPLACE FUNCTION fn_group_member_ws_subset_check() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_ws bigint;
BEGIN
    SELECT workspace_id INTO v_ws FROM "group" WHERE id = NEW.group_id;
    IF v_ws IS NULL THEN
        RETURN NEW;  -- personal 群或群不存在（群不存在会被业务层拒绝，此处放行）
    END IF;
    IF NEW.status = 1 AND NOT EXISTS (
        SELECT 1 FROM workspace_member
         WHERE workspace_id = v_ws AND user_id = NEW.user_id AND status = 'active'
    ) THEN
        RAISE EXCEPTION
            'group_member 子集约束（Group Member ⊆ Workspace Member）：'
            '用户 % 不是 workspace % 的 active workspace_member，不能成为 workspace 群 % 的 active 成员',
            NEW.user_id, v_ws, NEW.group_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_group_member_ws_subset',
                  HINT = '先将该用户加入 workspace（workspace_member.status=active），再写入/恢复群成员';
    END IF;
    RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_group_member_ws_subset ON group_member;
CREATE CONSTRAINT TRIGGER trg_group_member_ws_subset
    AFTER INSERT OR UPDATE OF status, user_id, group_id ON group_member
    DEFERRABLE INITIALLY DEFERRED
    FOR EACH ROW EXECUTE FUNCTION fn_group_member_ws_subset_check();

COMMENT ON FUNCTION fn_group_member_ws_subset_check() IS
    'Workspace 群成员写入端子集校验：active group_member 提交时必须存在同 workspace 的 active workspace_member（I14，可延迟到 COMMIT）';

-- 移除端（逻辑变更②的可延迟约束触发器，因引用 "group".scope 定义于本文件）：
-- workspace_member 置 removed（或物理 DELETE）时，同 workspace 不得残留该用户的 active
-- 下级关系。W0 下级关系 = 该 workspace 下 scope='workspace' 群的 active(status=1) group_member。
-- 移除 Workspace Member 的正确流程（§1.4.2 规则 8）：先禁用下属 Group Membership 再移除父关系，
-- 全部在同一事务内完成；本触发器是该流程的数据库层 fail-closed 兜底。
CREATE OR REPLACE FUNCTION fn_workspace_member_remove_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_OP = 'UPDATE' AND NEW.status <> 'removed' THEN
        RETURN NEW;  -- 仅约束 active → removed 的移除方向（恢复 active 不受限）
    END IF;
    IF EXISTS (
        SELECT 1
          FROM group_member gm
          JOIN "group" g ON g.id = gm.group_id
         WHERE g.workspace_id = OLD.workspace_id
           AND g.scope = 'workspace'
           AND gm.user_id = OLD.user_id
           AND gm.status = 1
    ) THEN
        RAISE EXCEPTION
            'workspace_member 移除保护：用户 % 在 workspace % 仍有 active 的 workspace 群成员关系，'
            '不能移除其工作区成员身份（Group Member ⊆ Workspace Member）',
            OLD.user_id, OLD.workspace_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_workspace_member_remove_guard',
                  HINT = '同一事务内先禁用/移除该用户在 workspace 群中的 active 成员记录，再移除工作区成员';
    END IF;
    RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_workspace_member_remove_guard ON workspace_member;
CREATE CONSTRAINT TRIGGER trg_workspace_member_remove_guard
    AFTER UPDATE OF status OR DELETE ON workspace_member
    DEFERRABLE INITIALLY DEFERRED
    FOR EACH ROW EXECUTE FUNCTION fn_workspace_member_remove_guard();

COMMENT ON FUNCTION fn_workspace_member_remove_guard() IS
    '工作区成员移除端保护：removed/DELETE 前同 workspace 不得有该用户的 active workspace 群成员（W0 下级关系=group_member，可延迟到 COMMIT）';

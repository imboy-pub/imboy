-- 迁移 00000129: Organization Department Directory（树形部门目录 + 多部门成员关系）。
-- Core Contract C10（Department Contract）/ C15（Permission Layering）：
--   * Department 属于一个 Org，支持树形 parent 与 member 多部门归属（兼职）。
--   * Department admin 是**局部目录角色**：只标记 department_member 行，
--     **不产生任何** Workspace / CS / Agent / 资源权限（不是万能 Role）。
--   * 纯 expand：不回填、不映射 class_staff/learner/Seat、不复制 Employee 表。
--   * member 必须是**同 Org** 的 organization_member（组合 FK 保证同 Org，
--     触发器保证 active）；archive 不级联撤销任何 Org/Workspace 权限。
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
-- 本迁移刻意只建两表（M04 slot 冻结范围），无第三张审计表：目录变更的可追溯性
-- 由 created_by/updated_by/updated_at/version 列承担（见各 COMMENT）。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 1: organization_department 表
-- ============================================================
CREATE TABLE IF NOT EXISTS organization_department (
    id              bigint                 NOT NULL,  -- TSID
    organization_id bigint                 NOT NULL,  -- 所属 Org（目录的租户边界）
    parent_id       bigint,                           -- 父部门；NULL=根部门
    name            character varying(200) NOT NULL,
    status          text                   DEFAULT 'active' NOT NULL,
    version         bigint                 DEFAULT 1 NOT NULL,
    created_by_user_id bigint,
    updated_by_user_id bigint,
    created_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_organization_department PRIMARY KEY (id),
    CONSTRAINT ck_organization_department_status
        CHECK (status = ANY (ARRAY['active'::text, 'archived'::text])),
    CONSTRAINT ck_organization_department_version CHECK (version >= 1),
    CONSTRAINT ck_organization_department_name CHECK (name <> ''),
    -- 环防第一层：self-parent 必拒（DB 权威）
    CONSTRAINT ck_organization_department_not_self_parent CHECK (parent_id <> id),
    CONSTRAINT fk_organization_department_org FOREIGN KEY (organization_id)
        REFERENCES organization(id) ON DELETE RESTRICT,
    CONSTRAINT fk_organization_department_parent FOREIGN KEY (parent_id)
        REFERENCES organization_department(id) ON DELETE RESTRICT,
    CONSTRAINT fk_organization_department_created_by FOREIGN KEY (created_by_user_id)
        REFERENCES "user"(id) ON DELETE SET NULL,
    CONSTRAINT fk_organization_department_updated_by FOREIGN KEY (updated_by_user_id)
        REFERENCES "user"(id) ON DELETE SET NULL
);

-- 同一 Org 内 active 部门名唯一（archived 释放名字；expand-first：不回填历史）
CREATE UNIQUE INDEX IF NOT EXISTS uq_organization_department_org_active_name
    ON organization_department USING btree (organization_id, name)
    WHERE status = 'active';
CREATE INDEX IF NOT EXISTS i_organization_department_org_parent
    ON organization_department USING btree (organization_id, parent_id);
CREATE INDEX IF NOT EXISTS i_organization_department_org_status
    ON organization_department USING btree (organization_id, status);

COMMENT ON TABLE  organization_department IS
    '树形部门目录（Org 内）；纯通讯录结构，不承载任何资源权限（C10/C15）';
COMMENT ON COLUMN organization_department.id              IS '主键 TSID';
COMMENT ON COLUMN organization_department.organization_id IS '所属机构；目录不可脱离 Org 存在；物理删除机构一律 RESTRICT（C17 fail-closed）';
COMMENT ON COLUMN organization_department.parent_id       IS '父部门 ID；NULL=根部门；禁止 self-parent 与祖先环（CHECK + 触发器双层）';
COMMENT ON COLUMN organization_department.name            IS '部门名；同 Org 内 active 部门唯一（部分唯一索引）';
COMMENT ON COLUMN organization_department.status          IS '状态: active 正常 | archived 已归档；archive 只改目录状态，不级联撤销任何 Org/Workspace 权限';
COMMENT ON COLUMN organization_department.version         IS '乐观锁版本（move/update CAS），从 1 起';
COMMENT ON COLUMN organization_department.created_by_user_id IS '创建人（审计快照）；user 删除后置 NULL，不阻断删除';
COMMENT ON COLUMN organization_department.updated_by_user_id IS '最后修改人（审计快照）；user 删除后置 NULL';

-- ============================================================
-- 环防第二层（DB 权威）：INSERT/UPDATE parent_id 时沿祖先链上行，
-- 若回到自身则拒绝（含多级祖先环）。应用层预检只是快速失败，此处是最终裁决。
-- ============================================================
CREATE OR REPLACE FUNCTION fn_organization_department_cycle_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_cursor bigint;
    v_depth  integer := 0;
BEGIN
    v_cursor := NEW.parent_id;
    WHILE v_cursor IS NOT NULL AND v_depth < 10000 LOOP
        IF v_cursor = NEW.id THEN
            RAISE EXCEPTION
                'organization_department % 的 parent 链回到自身（祖先环）',
                NEW.id
                USING ERRCODE = '23514',
                      CONSTRAINT = 'trg_organization_department_cycle_guard';
        END IF;
        SELECT parent_id INTO v_cursor
          FROM organization_department
         WHERE id = v_cursor;
        v_depth := v_depth + 1;
    END LOOP;
    -- 循环自然终止（parent 链到达根）即无环；深度上限防异常数据死循环
    RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_organization_department_cycle_guard ON organization_department;
CREATE TRIGGER trg_organization_department_cycle_guard
    BEFORE INSERT OR UPDATE OF parent_id ON organization_department
    FOR EACH ROW EXECUTE FUNCTION fn_organization_department_cycle_guard();

-- ============================================================
-- Phase 2: organization_department_member 表
-- ============================================================
-- 复合 FK (organization_id,user_id) -> organization_member 保证成员必须先是
-- **同 Org** 的组织成员；触发器 trg_organization_department_member_active_guard
-- 再要求该 membership 行 status='active'（一人可同时属多部门=兼职，C10）。
CREATE TABLE IF NOT EXISTS organization_department_member (
    organization_id bigint NOT NULL,  -- 冗余自 department 行，供组合 FK 与 org 级查询
    department_id   bigint NOT NULL,
    user_id         bigint NOT NULL,
    is_admin        boolean DEFAULT false NOT NULL,  -- 局部目录角色标记；不是权限
    added_by_user_id   bigint,
    created_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_organization_department_member PRIMARY KEY (department_id, user_id),
    CONSTRAINT fk_odm_department FOREIGN KEY (department_id)
        REFERENCES organization_department(id) ON DELETE CASCADE,
    -- 同 Org membership 不变量（C10）：组合 FK 指向 organization_member 主键
    CONSTRAINT fk_odm_organization_member FOREIGN KEY (organization_id, user_id)
        REFERENCES organization_member(organization_id, user_id) ON DELETE CASCADE,
    CONSTRAINT fk_odm_added_by FOREIGN KEY (added_by_user_id)
        REFERENCES "user"(id) ON DELETE SET NULL
);

CREATE INDEX IF NOT EXISTS i_organization_department_member_org_user
    ON organization_department_member USING btree (organization_id, user_id);
CREATE INDEX IF NOT EXISTS i_organization_department_member_user
    ON organization_department_member USING btree (user_id);

COMMENT ON TABLE  organization_department_member IS
    '部门成员关系（一人可属多部门）；is_admin 是局部目录角色标记，不产生任何资源权限（C15）';
COMMENT ON COLUMN organization_department_member.organization_id IS '所属机构（与 department.organization_id 一致，由 CHECK 锚定）；供同 Org membership 组合 FK 使用';
COMMENT ON COLUMN organization_department_member.department_id IS '部门 ID；部门行物理删除时成员行随删（目录内部一致性，非权限级联）';
COMMENT ON COLUMN organization_department_member.user_id IS '成员用户 ID；必须是同 Org 的 active organization_member（组合 FK + 触发器）';
COMMENT ON COLUMN organization_department_member.is_admin IS '部门管理员（局部目录角色）；只允许管理本部门目录成员，不授予 Workspace/CS/Agent/任何资源权限';
COMMENT ON COLUMN organization_department_member.added_by_user_id IS '加入人（审计快照）；user 删除后置 NULL';

-- ============================================================
-- department_member 写入守卫（BEFORE 触发器，两职合一）：
--   1. organization_id 由 department 行**派生**（单一真源，杜绝冗余列不一致）；
--   2. 被引用的 organization_member 必须存在且 active。
-- 组合 FK（本触发器之后执行）已保证 membership 行存在（23503），
-- 本触发器补足 status='active' 语义（23514）。
-- ============================================================
CREATE OR REPLACE FUNCTION fn_organization_department_member_active_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_department_org bigint;
    v_status         text;
BEGIN
    SELECT organization_id INTO v_department_org
      FROM organization_department
     WHERE id = NEW.department_id;
    IF v_department_org IS NULL THEN
        RAISE EXCEPTION
            'department % 不存在，不能挂成员',
            NEW.department_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_organization_department_member_active_guard';
    END IF;
    -- 冗余 org 列以 department 行为准派生（调用方传值被覆盖）
    NEW.organization_id := v_department_org;

    SELECT status INTO v_status
      FROM organization_member
     WHERE organization_id = NEW.organization_id
       AND user_id = NEW.user_id;
    IF v_status IS NULL OR v_status <> 'active' THEN
        RAISE EXCEPTION
            'user % 不是 organization % 的 active member，不能加入部门',
            NEW.user_id, NEW.organization_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_organization_department_member_active_guard';
    END IF;
    RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_organization_department_member_active_guard
    ON organization_department_member;
CREATE TRIGGER trg_organization_department_member_active_guard
    BEFORE INSERT OR UPDATE OF department_id, organization_id, user_id
    ON organization_department_member
    FOR EACH ROW EXECUTE FUNCTION fn_organization_department_member_active_guard();

-- 00000096_teaching_identity.up.sql
-- 墨芽习字 Step 6：教学身份五表 + 机构一致性约束（class_profile/class_staff/learner/
-- class_enrollment/guardian_learner）
-- 计划契约：docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md §6.2、Step 6
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策：
--   * 机构一致性（learner.organization_id == group→workspace→organization_id，计划 §6.2"必须保证"）：
--     group 经 workspace 间接归属机构（00000077 workspace_id + 00000095 organization_id），
--     无法直接建复合 FK（被引用键不在单表上），采用 DEFERRABLE INITIALLY DEFERRED 约束触发器
--     ——与 00000077 trg_group_member_ws_subset 同款仓库先例，提交时统一校验（DB-LEARNER-01）。
--     教学班级必须是"已归属机构的 workspace 下的群"：group 无 workspace 或 workspace 无机构
--     时 organization 解析为 NULL → 拒绝 enrollment（fail-closed）。
--   * learner 端防篡改 guard：learner 已有 enrollment 后禁止改 organization_id（防止事后换机构
--     使历史 enrollment 失效）。
--   * D-08/D-14：learner.user_id 可空（低龄学员无账号，不创建假账号）；UNIQUE(organization_id,
--     user_id) 用部分唯一索引 WHERE user_id IS NOT NULL——同一用户可绑不同机构的独立 learner，
--     同机构不重复绑定（DB-BIND-01）。
--   * class_staff/class_enrollment/guardian_learner 用复合主键即计划要求的 UNIQUE（镜像
--     00000076 workspace_member 先例）；class_profile 以 group_id 为 PK（与 Group 1:1，D-07）。
--   * FK 删除行为：引用 "group"/"user"/learner 的归属关系 CASCADE（容器删则关系删）；
--     learner.organization_id RESTRICT（镜像 00000095 fail-closed，机构物理删除前先清 learner）；
--     learner.user_id / account_bound_by 为可空绑定/审计列 SET NULL（镜像 00000076 惯例）。

-- ============================================================
-- Phase 1: class_profile（Group 的教学属性扩展，D-07）
-- ============================================================
CREATE TABLE IF NOT EXISTS class_profile (
    group_id    bigint                       NOT NULL,  -- PK/FK -> "group".id（1:1）
    course_type text                         DEFAULT 'hard_pen' NOT NULL,
    term        character varying(100),                 -- 学期，可空
    status      text                         DEFAULT 'active' NOT NULL,
    created_at  timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at  timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_class_profile PRIMARY KEY (group_id),
    CONSTRAINT ck_class_profile_course_type CHECK (course_type = ANY (ARRAY['hard_pen'::text, 'brush'::text, 'mixed'::text])),
    CONSTRAINT ck_class_profile_status      CHECK (status = ANY (ARRAY['active'::text, 'archived'::text]))
);

COMMENT ON TABLE  class_profile            IS '班级教学档案（Group 是沟通容器，教学属性在此；1:1 扩展，D-07）';
COMMENT ON COLUMN class_profile.group_id   IS 'PK/FK -> "group".id（班级沟通容器）';
COMMENT ON COLUMN class_profile.course_type IS '课程类型: hard_pen 硬笔 | brush 毛笔 | mixed 混合';
COMMENT ON COLUMN class_profile.term       IS '学期标识（可空）';

ALTER TABLE class_profile DROP CONSTRAINT IF EXISTS fk_class_profile_group;
ALTER TABLE class_profile ADD CONSTRAINT fk_class_profile_group
    FOREIGN KEY (group_id) REFERENCES "group"(id) ON DELETE CASCADE;

-- ============================================================
-- Phase 2: class_staff（教学角色，不从 Group 管理员推断，§5.1）
-- ============================================================
CREATE TABLE IF NOT EXISTS class_staff (
    group_id    bigint                       NOT NULL,
    user_id     bigint                       NOT NULL,
    role        text                         DEFAULT 'teacher' NOT NULL,
    status      text                         DEFAULT 'active' NOT NULL,
    created_at  timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at  timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_class_staff PRIMARY KEY (group_id, user_id),  -- 即计划要求的 UNIQUE(group_id,user_id)
    CONSTRAINT ck_class_staff_role   CHECK (role = ANY (ARRAY['manager'::text, 'teacher'::text, 'assistant'::text])),
    CONSTRAINT ck_class_staff_status CHECK (status = ANY (ARRAY['active'::text, 'removed'::text]))
);

COMMENT ON TABLE  class_staff        IS '班级教学角色（manager/teacher/assistant；聊天管理角色≠教学角色）';
COMMENT ON COLUMN class_staff.role   IS '教学角色: manager 班主任 | teacher 老师 | assistant 助教（无发布回评权）';
COMMENT ON COLUMN class_staff.status IS '状态: active 在任 | removed 已移除（软删）';

ALTER TABLE class_staff DROP CONSTRAINT IF EXISTS fk_class_staff_group;
ALTER TABLE class_staff ADD CONSTRAINT fk_class_staff_group
    FOREIGN KEY (group_id) REFERENCES "group"(id) ON DELETE CASCADE;

ALTER TABLE class_staff DROP CONSTRAINT IF EXISTS fk_class_staff_user;
ALTER TABLE class_staff ADD CONSTRAINT fk_class_staff_user
    FOREIGN KEY (user_id) REFERENCES "user"(id) ON DELETE CASCADE;

CREATE INDEX IF NOT EXISTS i_class_staff_user_status
    ON class_staff USING btree (user_id, status);

-- ============================================================
-- Phase 3: learner（学员教学档案，归属 Organization，D-08/D-09）
-- ============================================================
CREATE TABLE IF NOT EXISTS learner (
    id                bigint                       NOT NULL,  -- TSID
    organization_id   bigint                       NOT NULL,
    display_name      character varying(100)       NOT NULL,
    birth_year        integer,                                -- 只收出生年，不收完整生日
    user_id           bigint,                                 -- 可空：未来账号绑定（D-08）
    account_bound_at  timestamp with time zone,               -- 账号绑定审计
    account_bound_by  bigint,                                 -- 账号绑定操作人（审计）
    status            text                         DEFAULT 'active' NOT NULL,
    created_at        timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at        timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_learner PRIMARY KEY (id),
    CONSTRAINT ck_learner_birth_year CHECK (birth_year IS NULL OR birth_year BETWEEN 1900 AND 2100),
    CONSTRAINT ck_learner_status     CHECK (status = ANY (ARRAY['active'::text, 'archived'::text]))
);

COMMENT ON TABLE  learner                  IS '学员教学档案（归属 Organization；同机构跨 Workspace 历史连续，跨机构隔离，D-09）';
COMMENT ON COLUMN learner.organization_id  IS '所属机构ID（学员档案的租户锚点）';
COMMENT ON COLUMN learner.display_name     IS '学员显示名（仅展示用，不作身份认证依据）';
COMMENT ON COLUMN learner.birth_year       IS '出生年（可空，隐私最小化：不收完整生日）';
COMMENT ON COLUMN learner.user_id          IS '绑定的 IMBoy 用户ID（可空=未登录学员；真源绑定关系在 guardian_learner）';
COMMENT ON COLUMN learner.account_bound_at IS '账号绑定时间（审计，§6.4）';
COMMENT ON COLUMN learner.account_bound_by IS '账号绑定操作人（审计，§6.4）';

ALTER TABLE learner DROP CONSTRAINT IF EXISTS fk_learner_organization;
ALTER TABLE learner ADD CONSTRAINT fk_learner_organization
    FOREIGN KEY (organization_id) REFERENCES organization(id) ON DELETE RESTRICT;

ALTER TABLE learner DROP CONSTRAINT IF EXISTS fk_learner_user;
ALTER TABLE learner ADD CONSTRAINT fk_learner_user
    FOREIGN KEY (user_id) REFERENCES "user"(id) ON DELETE SET NULL;

ALTER TABLE learner DROP CONSTRAINT IF EXISTS fk_learner_bound_by;
ALTER TABLE learner ADD CONSTRAINT fk_learner_bound_by
    FOREIGN KEY (account_bound_by) REFERENCES "user"(id) ON DELETE SET NULL;

-- 机构内学员列表
CREATE INDEX IF NOT EXISTS i_learner_org_status
    ON learner USING btree (organization_id, status);
-- 同机构内一个 user 只能绑定一个 learner（DB-BIND-01；跨机构各自独立）
CREATE UNIQUE INDEX IF NOT EXISTS uk_learner_org_user
    ON learner USING btree (organization_id, user_id)
    WHERE user_id IS NOT NULL;

-- ============================================================
-- Phase 4: class_enrollment（学员入班，机构一致性约束核心）
-- ============================================================
CREATE TABLE IF NOT EXISTS class_enrollment (
    group_id    bigint                       NOT NULL,
    learner_id  bigint                       NOT NULL,
    status      text                         DEFAULT 'active' NOT NULL,
    joined_at   timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_class_enrollment PRIMARY KEY (group_id, learner_id),  -- 即 UNIQUE(group_id,learner_id)
    CONSTRAINT ck_class_enrollment_status CHECK (status = ANY (ARRAY['active'::text, 'removed'::text]))
);

COMMENT ON TABLE  class_enrollment        IS '学员入班记录（learner 机构 == 班级 Group 经 workspace 解析的机构，触发器强制）';
COMMENT ON COLUMN class_enrollment.status IS '状态: active 在班 | removed 已移出（软删）';

ALTER TABLE class_enrollment DROP CONSTRAINT IF EXISTS fk_class_enrollment_group;
ALTER TABLE class_enrollment ADD CONSTRAINT fk_class_enrollment_group
    FOREIGN KEY (group_id) REFERENCES "group"(id) ON DELETE CASCADE;

ALTER TABLE class_enrollment DROP CONSTRAINT IF EXISTS fk_class_enrollment_learner;
ALTER TABLE class_enrollment ADD CONSTRAINT fk_class_enrollment_learner
    FOREIGN KEY (learner_id) REFERENCES learner(id) ON DELETE CASCADE;

CREATE INDEX IF NOT EXISTS i_class_enrollment_learner
    ON class_enrollment USING btree (learner_id);

-- ============================================================
-- Phase 5: guardian_learner（监护关系，提交/查看权限真源，§5.1）
-- ============================================================
CREATE TABLE IF NOT EXISTS guardian_learner (
    guardian_uid    bigint                       NOT NULL,
    learner_id      bigint                       NOT NULL,
    relation        text                         DEFAULT 'guardian' NOT NULL,
    can_submit      boolean                      DEFAULT true NOT NULL,
    can_view_review boolean                      DEFAULT true NOT NULL,
    status          text                         DEFAULT 'active' NOT NULL,
    created_at      timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at      timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_guardian_learner PRIMARY KEY (guardian_uid, learner_id),  -- 即 UNIQUE(guardian_uid,learner_id)
    CONSTRAINT ck_guardian_learner_relation CHECK (relation = ANY (ARRAY['guardian'::text, 'other'::text])),
    CONSTRAINT ck_guardian_learner_status   CHECK (status = ANY (ARRAY['active'::text, 'removed'::text]))
);

COMMENT ON TABLE  guardian_learner             IS '监护关系（家长替低龄学员提交/查看的权限真源；非 group_member）';
COMMENT ON COLUMN guardian_learner.relation    IS '关系: guardian 法定监护人 | other 其他被授权人';
COMMENT ON COLUMN guardian_learner.can_submit  IS '是否可替该学员提交作业（家长可按孩子细分）';
COMMENT ON COLUMN guardian_learner.can_view_review IS '是否可查看该学员的已发布回评';

ALTER TABLE guardian_learner DROP CONSTRAINT IF EXISTS fk_guardian_learner_guardian;
ALTER TABLE guardian_learner ADD CONSTRAINT fk_guardian_learner_guardian
    FOREIGN KEY (guardian_uid) REFERENCES "user"(id) ON DELETE CASCADE;

ALTER TABLE guardian_learner DROP CONSTRAINT IF EXISTS fk_guardian_learner_learner;
ALTER TABLE guardian_learner ADD CONSTRAINT fk_guardian_learner_learner
    FOREIGN KEY (learner_id) REFERENCES learner(id) ON DELETE CASCADE;

CREATE INDEX IF NOT EXISTS i_guardian_learner_guardian
    ON guardian_learner USING btree (guardian_uid, status);
CREATE INDEX IF NOT EXISTS i_guardian_learner_learner
    ON guardian_learner USING btree (learner_id, status);

-- ============================================================
-- Phase 6: 机构一致性约束触发器（DB-LEARNER-01）
--   learner.organization_id 必须等于 enrollment 班级 Group 经
--   workspace 解析出的 organization_id；教学班必须挂在已归属机构的 workspace 下。
-- ============================================================
CREATE OR REPLACE FUNCTION fn_class_enrollment_org_check() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_group_org bigint;
    v_learner_org bigint;
BEGIN
    SELECT w.organization_id INTO v_group_org
      FROM "group" g
      JOIN workspace w ON w.id = g.workspace_id
     WHERE g.id = NEW.group_id;

    SELECT organization_id INTO v_learner_org
      FROM learner
     WHERE id = NEW.learner_id;

    IF v_group_org IS NULL THEN
        RAISE EXCEPTION
            '班级机构一致性：群 % 不是已归属机构的工作区群（无 workspace 或 workspace 无 organization），'
            '不能作为教学班级接收学员入班',
            NEW.group_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_class_enrollment_org_consistency',
                  HINT = '教学班级必须位于 organization_id 非空的 workspace 下';
    END IF;

    IF v_learner_org IS NULL OR v_group_org <> v_learner_org THEN
        RAISE EXCEPTION
            '班级机构一致性：学员 % 属于机构 %，但班级群 % 属于机构 %，跨机构入班被拒绝',
            NEW.learner_id, v_learner_org, NEW.group_id, v_group_org
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_class_enrollment_org_consistency',
                  HINT = 'learner.organization_id 必须等于 group->workspace->organization_id';
    END IF;

    RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_class_enrollment_org_consistency ON class_enrollment;
CREATE CONSTRAINT TRIGGER trg_class_enrollment_org_consistency
    AFTER INSERT OR UPDATE OF group_id, learner_id ON class_enrollment
    DEFERRABLE INITIALLY DEFERRED
    FOR EACH ROW EXECUTE FUNCTION fn_class_enrollment_org_check();

COMMENT ON FUNCTION fn_class_enrollment_org_check IS
    '入班机构一致性：learner.organization_id == group→workspace→organization_id（提交时校验，可延迟；fail-closed：解析不出机构即拒绝）';

-- learner 端防篡改：已入班的 learner 禁止更换机构（防事后改机构使历史 enrollment 失效）
CREATE OR REPLACE FUNCTION fn_learner_org_change_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.organization_id IS DISTINCT FROM OLD.organization_id
       AND EXISTS (SELECT 1 FROM class_enrollment WHERE learner_id = NEW.id)
    THEN
        RAISE EXCEPTION
            'learner 机构防篡改：学员 % 已有入班记录，禁止更换机构 % -> %',
            NEW.id, OLD.organization_id, NEW.organization_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_learner_org_change_guard',
                  HINT = '如需迁移学员机构，先移除其全部 class_enrollment 再变更';
    END IF;
    RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_learner_org_change_guard ON learner;
CREATE TRIGGER trg_learner_org_change_guard
    AFTER UPDATE OF organization_id ON learner
    FOR EACH ROW EXECUTE FUNCTION fn_learner_org_change_guard();

COMMENT ON FUNCTION fn_learner_org_change_guard IS
    'learner 机构防篡改：存在 class_enrollment 时禁止变更 organization_id（立即触发）';

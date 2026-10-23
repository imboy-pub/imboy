-- 00000224_retire_moya.down.sql
-- 仅恢复空表、专属字段、索引与约束；不能恢复任何历史业务数据、审计记录或媒体。
-- 不恢复路由、worker 或产品配置。以下 DDL 固化自原迁移，历史迁移保持不变。
SET LOCAL search_path = public, pg_catalog;

-- Restore structure from 00000096_teaching_identity.up.sql
CREATE TABLE IF NOT EXISTS class_profile (
    group_id    bigint                       NOT NULL,
    course_type text                         DEFAULT 'hard_pen' NOT NULL,
    term        character varying(100),
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
CREATE TABLE IF NOT EXISTS class_staff (
    group_id    bigint                       NOT NULL,
    user_id     bigint                       NOT NULL,
    role        text                         DEFAULT 'teacher' NOT NULL,
    status      text                         DEFAULT 'active' NOT NULL,
    created_at  timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at  timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_class_staff PRIMARY KEY (group_id, user_id),
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
CREATE TABLE IF NOT EXISTS learner (
    id                bigint                       NOT NULL,
    organization_id   bigint                       NOT NULL,
    display_name      character varying(100)       NOT NULL,
    birth_year        integer,
    user_id           bigint,
    account_bound_at  timestamp with time zone,
    account_bound_by  bigint,
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
CREATE INDEX IF NOT EXISTS i_learner_org_status
    ON learner USING btree (organization_id, status);
CREATE UNIQUE INDEX IF NOT EXISTS uk_learner_org_user
    ON learner USING btree (organization_id, user_id)
    WHERE user_id IS NOT NULL;
CREATE TABLE IF NOT EXISTS class_enrollment (
    group_id    bigint                       NOT NULL,
    learner_id  bigint                       NOT NULL,
    status      text                         DEFAULT 'active' NOT NULL,
    joined_at   timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_class_enrollment PRIMARY KEY (group_id, learner_id),
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
CREATE TABLE IF NOT EXISTS guardian_learner (
    guardian_uid    bigint                       NOT NULL,
    learner_id      bigint                       NOT NULL,
    relation        text                         DEFAULT 'guardian' NOT NULL,
    can_submit      boolean                      DEFAULT true NOT NULL,
    can_view_review boolean                      DEFAULT true NOT NULL,
    status          text                         DEFAULT 'active' NOT NULL,
    created_at      timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at      timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_guardian_learner PRIMARY KEY (guardian_uid, learner_id),
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

-- Restore structure from 00000097_homework_review_loop.up.sql
ALTER TABLE group_task_assignment ADD COLUMN IF NOT EXISTS learner_id bigint;
ALTER TABLE group_task_assignment ADD COLUMN IF NOT EXISTS submitted_by bigint;
COMMENT ON COLUMN group_task_assignment.learner_id   IS '教学作业学员档案ID（NULL=普通 IMBoy 群作业，兼容旧数据；教学作业提交/查看权限真源是 guardian_learner）';
COMMENT ON COLUMN group_task_assignment.submitted_by IS '最近一次提交人（快捷字段，可空；真源在 homework_submission.submitted_by）';
ALTER TABLE group_task_assignment
    DROP CONSTRAINT IF EXISTS group_task_assignment_task_id_user_id_key;
CREATE UNIQUE INDEX IF NOT EXISTS group_task_assignment_task_id_user_id_key
    ON group_task_assignment USING btree (task_id, user_id)
    WHERE learner_id IS NULL;
CREATE UNIQUE INDEX IF NOT EXISTS uk_group_task_assignment_task_learner
    ON group_task_assignment USING btree (task_id, learner_id)
    WHERE learner_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS i_group_task_assignment_learner
    ON group_task_assignment USING btree (learner_id)
    WHERE learner_id IS NOT NULL;
ALTER TABLE group_task_assignment DROP CONSTRAINT IF EXISTS fk_gta_learner;
ALTER TABLE group_task_assignment ADD CONSTRAINT fk_gta_learner
    FOREIGN KEY (learner_id) REFERENCES learner(id) ON DELETE RESTRICT;
ALTER TABLE group_task_assignment DROP CONSTRAINT IF EXISTS fk_gta_submitted_by;
ALTER TABLE group_task_assignment ADD CONSTRAINT fk_gta_submitted_by
    FOREIGN KEY (submitted_by) REFERENCES "user"(id) ON DELETE SET NULL;
CREATE OR REPLACE FUNCTION fn_gta_learner_org_check() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_task_org bigint;
    v_learner_org bigint;
BEGIN
    IF NEW.learner_id IS NULL THEN
        RETURN NEW;  -- 普通群作业：不参与教学机构约束（DB-COMPAT-01）
    END IF;
    SELECT w.organization_id INTO v_task_org
      FROM group_task gt
      JOIN "group" g ON g.id = gt.group_id
      JOIN workspace w ON w.id = g.workspace_id
     WHERE gt.task_id = NEW.task_id;
    SELECT organization_id INTO v_learner_org
      FROM learner
     WHERE id = NEW.learner_id;
    IF v_task_org IS NULL OR v_task_org <> v_learner_org THEN
        RAISE EXCEPTION
            '作业机构一致性：learner % 属于机构 %，但作业 % 所属班级群解析机构为 %，'
            '跨机构教学 assignment 被拒绝',
            NEW.learner_id, v_learner_org, NEW.task_id, v_task_org
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_gta_learner_org_consistency',
                  HINT = '教学 assignment 的 task 所属群必须与 learner 同一 Organization';
    END IF;
    RETURN NEW;
END;
$$;
DROP TRIGGER IF EXISTS trg_gta_learner_org_consistency ON group_task_assignment;
CREATE CONSTRAINT TRIGGER trg_gta_learner_org_consistency
    AFTER INSERT OR UPDATE OF task_id, learner_id ON group_task_assignment
    DEFERRABLE INITIALLY DEFERRED
    FOR EACH ROW EXECUTE FUNCTION fn_gta_learner_org_check();
COMMENT ON FUNCTION fn_gta_learner_org_check IS
    '教学 assignment 机构一致性：learner.organization_id == task→group→workspace→organization（提交时校验，可延迟；learner_id 为空的普通作业不受约束）';
CREATE TABLE IF NOT EXISTS homework_submission (
    id            bigint                       NOT NULL,
    assignment_id bigint                       NOT NULL,
    learner_id    bigint                       NOT NULL,
    submitted_by  bigint,
    attempt_no    integer                      NOT NULL,
    status        text                         DEFAULT 'submitted' NOT NULL,
    submitted_at  timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at    timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_homework_submission PRIMARY KEY (id),
    CONSTRAINT uk_homework_submission_attempt UNIQUE (assignment_id, attempt_no),
    CONSTRAINT ck_homework_submission_attempt_no CHECK (attempt_no > 0),
    CONSTRAINT ck_homework_submission_status CHECK (status = ANY (ARRAY['submitted'::text, 'withdrawn'::text]))
);
COMMENT ON TABLE  homework_submission            IS '作业提交（重练=新 attempt 新行，旧证据不更新不删除）';
COMMENT ON COLUMN homework_submission.learner_id IS '学员档案ID（触发器强制 == assignment.learner_id）';
COMMENT ON COLUMN homework_submission.submitted_by IS '提交人用户ID（can_submit=true 的监护人或学员本人；SET NULL 保留证据）';
COMMENT ON COLUMN homework_submission.attempt_no IS '第几次提交（1 起，应用层 MAX+1 分配，UNIQUE 兜底）';
COMMENT ON COLUMN homework_submission.status    IS '状态: submitted 已提交 | withdrawn 已撤回（证据保留）';
ALTER TABLE homework_submission DROP CONSTRAINT IF EXISTS fk_hs_assignment;
ALTER TABLE homework_submission ADD CONSTRAINT fk_hs_assignment
    FOREIGN KEY (assignment_id) REFERENCES group_task_assignment(id) ON DELETE CASCADE;
ALTER TABLE homework_submission DROP CONSTRAINT IF EXISTS fk_hs_learner;
ALTER TABLE homework_submission ADD CONSTRAINT fk_hs_learner
    FOREIGN KEY (learner_id) REFERENCES learner(id) ON DELETE RESTRICT;
ALTER TABLE homework_submission DROP CONSTRAINT IF EXISTS fk_hs_submitted_by;
ALTER TABLE homework_submission ADD CONSTRAINT fk_hs_submitted_by
    FOREIGN KEY (submitted_by) REFERENCES "user"(id) ON DELETE SET NULL;
CREATE INDEX IF NOT EXISTS i_homework_submission_queue
    ON homework_submission USING btree (status, submitted_at DESC);
CREATE OR REPLACE FUNCTION fn_homework_submission_learner_check() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_assignment_learner bigint;
BEGIN
    SELECT learner_id INTO v_assignment_learner
      FROM group_task_assignment
     WHERE id = NEW.assignment_id;
    IF v_assignment_learner IS NULL OR v_assignment_learner <> NEW.learner_id THEN
        RAISE EXCEPTION
            '提交学员一致性：submission.learner_id(%) 与 assignment % 的 learner_id(%) 不一致',
            NEW.learner_id, NEW.assignment_id, v_assignment_learner
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_homework_submission_learner_consistency',
                  HINT = '教学提交必须挂在 learner_id 匹配的教学 assignment 上（assignment.learner_id 不得为空）';
    END IF;
    RETURN NEW;
END;
$$;
DROP TRIGGER IF EXISTS trg_homework_submission_learner_consistency ON homework_submission;
CREATE CONSTRAINT TRIGGER trg_homework_submission_learner_consistency
    AFTER INSERT OR UPDATE OF assignment_id, learner_id ON homework_submission
    DEFERRABLE INITIALLY DEFERRED
    FOR EACH ROW EXECUTE FUNCTION fn_homework_submission_learner_check();
COMMENT ON FUNCTION fn_homework_submission_learner_check IS
    '提交学员一致性：homework_submission.learner_id == group_task_assignment.learner_id 且 assignment 必须是教学作业（提交时校验，可延迟）';
CREATE TABLE IF NOT EXISTS submission_asset (
    id            bigint                       NOT NULL,
    submission_id bigint                       NOT NULL,
    attachment_id bigint                       NOT NULL,
    kind          text                         NOT NULL,
    sort_order    integer                      DEFAULT 0 NOT NULL,
    created_by    bigint,
    created_at    timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_submission_asset PRIMARY KEY (id),
    CONSTRAINT uk_submission_asset_attachment UNIQUE (submission_id, attachment_id),
    CONSTRAINT ck_submission_asset_kind CHECK (kind = ANY (ARRAY['practice_video'::text, 'final_photo'::text]))
);
COMMENT ON TABLE  submission_asset          IS '提交附件关联（只存 attachment_id，业务表不得持久化 presigned URL）';
COMMENT ON COLUMN submission_asset.kind     IS '类型: practice_video 练字视频 | final_photo 作品照片';
COMMENT ON COLUMN submission_asset.created_by IS '添加人用户ID（审计；SET NULL 保留关联行）';
ALTER TABLE submission_asset DROP CONSTRAINT IF EXISTS fk_sa_submission;
ALTER TABLE submission_asset ADD CONSTRAINT fk_sa_submission
    FOREIGN KEY (submission_id) REFERENCES homework_submission(id) ON DELETE CASCADE;
ALTER TABLE submission_asset DROP CONSTRAINT IF EXISTS fk_sa_attachment;
ALTER TABLE submission_asset ADD CONSTRAINT fk_sa_attachment
    FOREIGN KEY (attachment_id) REFERENCES attachment(id) ON DELETE RESTRICT;
ALTER TABLE submission_asset DROP CONSTRAINT IF EXISTS fk_sa_created_by;
ALTER TABLE submission_asset ADD CONSTRAINT fk_sa_created_by
    FOREIGN KEY (created_by) REFERENCES "user"(id) ON DELETE SET NULL;
CREATE INDEX IF NOT EXISTS i_submission_asset_submission
    ON submission_asset USING btree (submission_id, kind, sort_order);
CREATE TABLE IF NOT EXISTS calligraphy_review_draft (
    id             bigint                       NOT NULL,
    submission_id  bigint                       NOT NULL,
    ai_task_id     character varying(64),
    status         text                         DEFAULT 'queued' NOT NULL,
    model_profile  character varying(100)       DEFAULT '' NOT NULL,
    prompt_version character varying(50)        DEFAULT '' NOT NULL,
    rubric_version character varying(50)        DEFAULT '' NOT NULL,
    input_digest   character varying(128)       DEFAULT '' NOT NULL,
    result_json    jsonb,
    error_code     character varying(50),
    created_at     timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    completed_at   timestamp with time zone,
    CONSTRAINT pk_calligraphy_review_draft PRIMARY KEY (id),
    CONSTRAINT ck_crd_status CHECK (status = ANY (ARRAY['queued'::text, 'running'::text, 'succeeded'::text, 'failed'::text])),
    CONSTRAINT ck_crd_completed CHECK (
        (status NOT IN ('succeeded', 'failed')) OR completed_at IS NOT NULL)
);
COMMENT ON TABLE  calligraphy_review_draft            IS 'AI 点评草稿（只生成老师草稿，D-10；家长 API 永不返回本表）';
COMMENT ON COLUMN calligraphy_review_draft.ai_task_id IS '异步任务标识（可空）';
COMMENT ON COLUMN calligraphy_review_draft.status     IS '状态: queued | running | succeeded | failed（failed 仍进老师人工队列）';
COMMENT ON COLUMN calligraphy_review_draft.result_json IS '结构化结果 jsonb（正向观察/主要问题/证据时间点/练习动作/口播提纲/需人工核对标记）';
COMMENT ON COLUMN calligraphy_review_draft.completed_at IS '完成时间（succeeded/failed 必填，CHECK 强制）';
ALTER TABLE calligraphy_review_draft DROP CONSTRAINT IF EXISTS fk_crd_submission;
ALTER TABLE calligraphy_review_draft ADD CONSTRAINT fk_crd_submission
    FOREIGN KEY (submission_id) REFERENCES homework_submission(id) ON DELETE CASCADE;
CREATE UNIQUE INDEX IF NOT EXISTS uk_crd_active_per_submission
    ON calligraphy_review_draft USING btree (submission_id)
    WHERE status IN ('queued', 'running', 'succeeded');
CREATE INDEX IF NOT EXISTS i_crd_status_created
    ON calligraphy_review_draft USING btree (status, created_at);
CREATE TABLE IF NOT EXISTS teacher_review (
    id                  bigint                       NOT NULL,
    submission_id       bigint                       NOT NULL,
    reviewer_uid        bigint,
    positive_point      text                         DEFAULT '' NOT NULL,
    focus_problem       text                         DEFAULT '' NOT NULL,
    practice_action     text                         DEFAULT '' NOT NULL,
    comment             text                         DEFAULT '' NOT NULL,
    video_attachment_id bigint,
    rework_required     boolean                      DEFAULT false NOT NULL,
    status              text                         DEFAULT 'draft' NOT NULL,
    published_at        timestamp with time zone,
    created_at          timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at          timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_teacher_review PRIMARY KEY (id),
    CONSTRAINT ck_teacher_review_status CHECK (status = ANY (ARRAY['draft'::text, 'published'::text, 'discarded'::text])),
    CONSTRAINT ck_teacher_review_published CHECK (
        status <> 'published' OR (
            reviewer_uid IS NOT NULL AND
            published_at IS NOT NULL AND
            (positive_point <> '' OR focus_problem <> '' OR practice_action <> ''
             OR comment <> '' OR video_attachment_id IS NOT NULL)))
);
COMMENT ON TABLE  teacher_review                  IS '老师回评（draft→published 一次性条件更新；discarded 保留审计）';
COMMENT ON COLUMN teacher_review.reviewer_uid     IS '回评老师用户ID（published 时 CHECK 强制非空）';
COMMENT ON COLUMN teacher_review.video_attachment_id IS '真人点评视频附件ID（FK→attachment，RESTRICT 保护）';
COMMENT ON COLUMN teacher_review.rework_required  IS '是否要求重练（true 时家长从回评进入新 attempt）';
COMMENT ON COLUMN teacher_review.status           IS '状态: draft | published | discarded';
ALTER TABLE teacher_review DROP CONSTRAINT IF EXISTS fk_tr_submission;
ALTER TABLE teacher_review ADD CONSTRAINT fk_tr_submission
    FOREIGN KEY (submission_id) REFERENCES homework_submission(id) ON DELETE CASCADE;
ALTER TABLE teacher_review DROP CONSTRAINT IF EXISTS fk_tr_reviewer;
ALTER TABLE teacher_review ADD CONSTRAINT fk_tr_reviewer
    FOREIGN KEY (reviewer_uid) REFERENCES "user"(id) ON DELETE SET NULL;
ALTER TABLE teacher_review DROP CONSTRAINT IF EXISTS fk_tr_video_attachment;
ALTER TABLE teacher_review ADD CONSTRAINT fk_tr_video_attachment
    FOREIGN KEY (video_attachment_id) REFERENCES attachment(id) ON DELETE RESTRICT;
CREATE UNIQUE INDEX IF NOT EXISTS uk_tr_published_per_submission
    ON teacher_review USING btree (submission_id)
    WHERE status = 'published';
CREATE INDEX IF NOT EXISTS i_teacher_review_submission
    ON teacher_review USING btree (submission_id, status);
CREATE INDEX IF NOT EXISTS i_teacher_review_reviewer
    ON teacher_review USING btree (reviewer_uid)
    WHERE reviewer_uid IS NOT NULL;

-- Restore structure from 00000098_submission_idempotency_withdraw.up.sql
ALTER TABLE homework_submission ADD COLUMN IF NOT EXISTS idempotency_key character varying(128);
ALTER TABLE homework_submission ADD COLUMN IF NOT EXISTS request_digest  character varying(128);
ALTER TABLE homework_submission ADD COLUMN IF NOT EXISTS withdrawn_at   timestamp with time zone;
ALTER TABLE homework_submission ADD COLUMN IF NOT EXISTS withdrawn_by       bigint;
COMMENT ON COLUMN homework_submission.idempotency_key IS '客户端幂等键（可空=内部写入；同 (submitted_by,assignment_id,idempotency_key) 唯一，重放返回原行）';
COMMENT ON COLUMN homework_submission.request_digest  IS '请求载荷摘要（sha256 hex；同 key 不同 digest 由应用层判 5460 冲突，DB 不参与唯一性）';
COMMENT ON COLUMN homework_submission.withdrawn_at   IS '撤回时间（status=withdrawn 必填）';
COMMENT ON COLUMN homework_submission.withdrawn_by   IS '撤回操作人（can_submit=true 监护人；status=withdrawn 必填；注销置 NULL 但 CHECK 阻断该路径=审计 fail-closed）';
CREATE UNIQUE INDEX IF NOT EXISTS uk_homework_submission_idempotency
    ON homework_submission USING btree (submitted_by, assignment_id, idempotency_key)
    WHERE idempotency_key IS NOT NULL;
ALTER TABLE homework_submission DROP CONSTRAINT IF EXISTS fk_hs_withdrawn_by;
ALTER TABLE homework_submission ADD CONSTRAINT fk_hs_withdrawn_by
    FOREIGN KEY (withdrawn_by) REFERENCES "user"(id) ON DELETE SET NULL;
ALTER TABLE homework_submission DROP CONSTRAINT IF EXISTS ck_homework_submission_withdraw_audit;
ALTER TABLE homework_submission ADD CONSTRAINT ck_homework_submission_withdraw_audit CHECK (
    (status = 'submitted' AND withdrawn_at IS NULL AND withdrawn_by IS NULL)
    OR
    (status = 'withdrawn' AND withdrawn_at IS NOT NULL AND withdrawn_by IS NOT NULL
                                 AND submitted_at IS NOT NULL)
);
CREATE OR REPLACE FUNCTION fn_homework_submission_withdraw_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.status = 'withdrawn' AND EXISTS (
        SELECT 1 FROM teacher_review
         WHERE submission_id = NEW.id AND status = 'published'
    ) THEN
        RAISE EXCEPTION
            '撤回互斥：submission % 已有已发布老师回评，不能撤回（计划 §7.2）',
            NEW.id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_homework_submission_withdraw_guard',
                  HINT = '已发布的回评不可被家长撤回；如需撤回内容走老师/管理员删除流程';
    END IF;
    RETURN NEW;
END;
$$;
DROP TRIGGER IF EXISTS trg_homework_submission_withdraw_guard ON homework_submission;
CREATE CONSTRAINT TRIGGER trg_homework_submission_withdraw_guard
    AFTER UPDATE OF status ON homework_submission
    DEFERRABLE INITIALLY IMMEDIATE
    FOR EACH ROW EXECUTE FUNCTION fn_homework_submission_withdraw_guard();
COMMENT ON FUNCTION fn_homework_submission_withdraw_guard IS
    '撤回互斥兜底：已有 published teacher_review 的 submission 禁止转入 withdrawn（语句结束时新鲜快照复查，拦截 READ COMMITTED 单语句守卫的竞态）';
CREATE OR REPLACE FUNCTION fn_teacher_review_publish_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_sub_status text;
BEGIN
    IF NEW.status = 'published' THEN
        SELECT status INTO v_sub_status FROM homework_submission WHERE id = NEW.submission_id;
        IF v_sub_status = 'withdrawn' THEN
            RAISE EXCEPTION
                '发布互斥：submission % 已被监护人撤回，不能发布回评（计划 §7.2：撤回后老师队列即时移除）',
                NEW.submission_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_teacher_review_publish_guard',
                  HINT = '已撤回的提交不再进入待评队列；如内容违规走删除/审核流程';
        END IF;
    END IF;
    RETURN NEW;
END;
$$;
DROP TRIGGER IF EXISTS trg_teacher_review_publish_guard ON teacher_review;
CREATE CONSTRAINT TRIGGER trg_teacher_review_publish_guard
    AFTER INSERT OR UPDATE OF status ON teacher_review
    DEFERRABLE INITIALLY IMMEDIATE
    FOR EACH ROW EXECUTE FUNCTION fn_teacher_review_publish_guard();
COMMENT ON FUNCTION fn_teacher_review_publish_guard IS
    '发布互斥兜底：withdrawn 的 submission 禁止发布 teacher_review（与撤回侧触发器构成双向互斥）';

-- Restore structure from 00000099_teaching_admin_audit.up.sql
CREATE TABLE IF NOT EXISTS teaching_admin_audit (
    id             bigint                       NOT NULL,
    action         text                         NOT NULL,
    operator_uid   bigint                       NOT NULL,
    learner_id     bigint                       NOT NULL,
    target_user_id bigint,
    detail         jsonb                        DEFAULT '{}'::jsonb NOT NULL,
    created_at     timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_teaching_admin_audit PRIMARY KEY (id),
    CONSTRAINT ck_teaching_admin_audit_action CHECK (
        action = ANY (ARRAY['bind_learner'::text, 'unbind_learner'::text,
                            'learner_archive'::text, 'data_export'::text, 'data_delete'::text])),
    CONSTRAINT ck_teaching_admin_audit_uids CHECK (operator_uid >= 0)
);
COMMENT ON TABLE  teaching_admin_audit             IS '教学管理动作审计（不可变合规日志：绑定/解绑/归档/导出/删除；§9.3）';
COMMENT ON COLUMN teaching_admin_audit.action      IS '动作: bind_learner|unbind_learner|learner_archive|data_export|data_delete（新动作经迁移扩枚举）';
COMMENT ON COLUMN teaching_admin_audit.operator_uid IS '操作人用户ID（裸列无FK）；0=sentinel=操作人账号已删除/匿名化（应用层匿名化时改写为0，禁置NULL）';
COMMENT ON COLUMN teaching_admin_audit.learner_id  IS '学员档案ID（裸列无FK；审计行先于实体存活，learner 物理删除被 RESTRICT 链约束，删除请求走匿名化）';
COMMENT ON COLUMN teaching_admin_audit.target_user_id IS '目标用户ID（bind 目标/解绑原账号）；NULL=动作无目标用户；0=sentinel=目标账号已删除/匿名化';
COMMENT ON COLUMN teaching_admin_audit.detail      IS '结构化上下文 jsonb（role/why/request_id 等）；禁止存 display_name/出生年/视频URL 等儿童 PII';
CREATE INDEX IF NOT EXISTS i_teaching_admin_audit_learner
    ON teaching_admin_audit USING btree (learner_id, created_at DESC);
CREATE INDEX IF NOT EXISTS i_teaching_admin_audit_operator
    ON teaching_admin_audit USING btree (operator_uid, created_at DESC);

-- Restore structure from 00000100_teaching_sentinel_unify.up.sql
ALTER TABLE homework_submission DROP CONSTRAINT IF EXISTS fk_hs_withdrawn_by;
ALTER TABLE teacher_review      DROP CONSTRAINT IF EXISTS fk_tr_reviewer;
ALTER TABLE homework_submission DROP CONSTRAINT IF EXISTS ck_homework_submission_withdrawn_by_sentinel;
ALTER TABLE homework_submission ADD CONSTRAINT ck_homework_submission_withdrawn_by_sentinel
    CHECK (withdrawn_by IS NULL OR withdrawn_by >= 0);
ALTER TABLE teacher_review DROP CONSTRAINT IF EXISTS ck_teacher_review_reviewer_uid_sentinel;
ALTER TABLE teacher_review ADD CONSTRAINT ck_teacher_review_reviewer_uid_sentinel
    CHECK (reviewer_uid IS NULL OR reviewer_uid >= 0);
COMMENT ON COLUMN homework_submission.withdrawn_by IS
    '撤回操作人（can_submit=true 监护人；status=withdrawn 必填）；裸列无FK；0=sentinel=操作人账号已删除/匿名化（应用层匿名化时改写为0，禁置NULL）';
COMMENT ON COLUMN teacher_review.reviewer_uid IS
    '回评老师用户ID（published 时 CHECK 强制非空）；裸列无FK；0=sentinel=老师账号已删除/匿名化（published 行保留回评内容与 0 sentinel，禁置NULL）';

-- Restore structure from 00000105_teaching_review_asset.up.sql
CREATE TABLE IF NOT EXISTS review_asset (
    id            bigint                       NOT NULL,
    review_id     bigint                       NOT NULL,
    attachment_id bigint                       NOT NULL,
    kind          text                         NOT NULL,
    sort_order    integer                      DEFAULT 0 NOT NULL,
    created_by    bigint                       NOT NULL,
    created_at    timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_review_asset PRIMARY KEY (id),
    CONSTRAINT uk_review_asset_attachment UNIQUE (attachment_id),
    CONSTRAINT ck_review_asset_kind CHECK (kind = ANY (ARRAY['feedback_image'::text, 'feedback_video'::text])),
    CONSTRAINT ck_review_asset_created_by_sentinel CHECK (created_by >= 0)
);
COMMENT ON TABLE  review_asset              IS '老师回评多媒体关联（0-1 feedback_video + 0-3 feedback_image；video_attachment_id 旧列为兼容读窗口，非写入真源）';
COMMENT ON COLUMN review_asset.review_id    IS '回评ID（FK→teacher_review，CASCADE：review 删除连带解除关联）';
COMMENT ON COLUMN review_asset.attachment_id IS '附件ID（全表 UNIQUE：一附件至多绑一 review；FK RESTRICT 保护）';
COMMENT ON COLUMN review_asset.kind         IS '类型: feedback_video 反馈视频（单 review 至多 1）| feedback_image 反馈图片（单 review 至多 3，触发器约束）';
COMMENT ON COLUMN review_asset.created_by   IS '创建人用户ID（审计；0=sentinel 注销语义，禁置NULL——00000100 同款）';
ALTER TABLE review_asset DROP CONSTRAINT IF EXISTS fk_ra_review;
ALTER TABLE review_asset ADD CONSTRAINT fk_ra_review
    FOREIGN KEY (review_id) REFERENCES teacher_review(id) ON DELETE CASCADE;
ALTER TABLE review_asset DROP CONSTRAINT IF EXISTS fk_ra_attachment;
ALTER TABLE review_asset ADD CONSTRAINT fk_ra_attachment
    FOREIGN KEY (attachment_id) REFERENCES attachment(id) ON DELETE RESTRICT;
CREATE INDEX IF NOT EXISTS i_review_asset_review
    ON review_asset USING btree (review_id, kind, sort_order);
CREATE UNIQUE INDEX IF NOT EXISTS uk_review_asset_one_video_per_review
    ON review_asset USING btree (review_id)
    WHERE kind = 'feedback_video';
CREATE OR REPLACE FUNCTION fn_review_asset_image_cap() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_count integer;
BEGIN
    SELECT count(*) INTO v_count
      FROM review_asset
     WHERE review_id = NEW.review_id AND kind = 'feedback_image';
    IF v_count > 3 THEN
        RAISE EXCEPTION
            '回评图片上限：review % 已关联 % 张反馈图片（上限 3）',
            NEW.review_id, v_count
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_review_asset_image_cap',
                  HINT = '单个回评最多 3 张 feedback_image（P0-4 媒体约束）';
    END IF;
    RETURN NEW;
END;
$$;
DROP TRIGGER IF EXISTS trg_review_asset_image_cap ON review_asset;
CREATE CONSTRAINT TRIGGER trg_review_asset_image_cap
    AFTER INSERT OR UPDATE OF review_id, kind ON review_asset
    DEFERRABLE INITIALLY IMMEDIATE
    FOR EACH ROW EXECUTE FUNCTION fn_review_asset_image_cap();
COMMENT ON FUNCTION fn_review_asset_image_cap IS
    '回评图片上限兜底：单 review 的 feedback_image 计数 >3 拒绝（语句结束时新鲜快照复查，多行插入同步拦截）';

-- Restore structure from 00000106_group_task_idempotency.up.sql
ALTER TABLE group_task ADD COLUMN IF NOT EXISTS idempotency_key character varying(128);
ALTER TABLE group_task ADD COLUMN IF NOT EXISTS request_digest  character varying(128);
COMMENT ON COLUMN group_task.idempotency_key IS '教学作业发布幂等键（可空=普通群作业/内部写入；同 (creator_id,group_id,idempotency_key) 唯一，重放返回原 task+assignments 集合）';
COMMENT ON COLUMN group_task.request_digest  IS '请求载荷摘要（sha256 hex；同 key 不同 digest 由应用层判 5460 冲突，DB 不参与唯一性）';
CREATE UNIQUE INDEX IF NOT EXISTS uk_group_task_idempotency
    ON group_task USING btree (creator_id, group_id, idempotency_key)
    WHERE idempotency_key IS NOT NULL;

-- Restore structure from 00000110_char_reviews.up.sql
ALTER TABLE teacher_review ADD COLUMN IF NOT EXISTS char_reviews jsonb;
COMMENT ON COLUMN teacher_review.char_reviews IS
    '逐字点评字卡数组（jsonb；[{index,char,grade,comment}]≤50 项，应用层白名单校验；NULL=无逐字数据，不渲染字卡区）';

-- Restore structure from 00000151_moya_subscribe_grant.up.sql
CREATE TABLE IF NOT EXISTS moya_subscribe_grant (
    id                     bigserial                   PRIMARY KEY,
    uid                    bigint                      NOT NULL,
    template_id            text                        NOT NULL,
    status                 text                        DEFAULT 'pending' NOT NULL,
    created_at             timestamp with time zone    DEFAULT CURRENT_TIMESTAMP NOT NULL,
    consumed_at            timestamp with time zone,
    consumed_by_submission bigint,
    CONSTRAINT ck_moya_subscribe_grant_status
        CHECK (status = ANY (ARRAY['pending'::text, 'consumed'::text]))
);
COMMENT ON TABLE  moya_subscribe_grant                 IS
    '订阅消息一次性授权额度（客户端 requestSubscribeMessage accept 一次记一行 pending；下发成功一行变 consumed）';
COMMENT ON COLUMN moya_subscribe_grant.uid             IS '授权用户（guardian_uid；openid 不入库，send 时反查 sso_identity）';
COMMENT ON COLUMN moya_subscribe_grant.template_id     IS '微信一次性订阅模板 ID（配置而非常量）';
COMMENT ON COLUMN moya_subscribe_grant.status          IS 'pending 未消费 | consumed 已消费（条件 UPDATE 防并发双发）';
COMMENT ON COLUMN moya_subscribe_grant.consumed_at     IS '消费时间（=订阅消息下发成功时间）';
COMMENT ON COLUMN moya_subscribe_grant.consumed_by_submission IS '消费该额度的 homework_submission.id（审计线索）';
ALTER TABLE moya_subscribe_grant DROP CONSTRAINT IF EXISTS fk_moya_subscribe_grant_uid;
ALTER TABLE moya_subscribe_grant ADD CONSTRAINT fk_moya_subscribe_grant_uid
    FOREIGN KEY (uid) REFERENCES "user"(id) ON DELETE CASCADE;
CREATE INDEX IF NOT EXISTS i_moya_subscribe_grant_consume
    ON moya_subscribe_grant USING btree (uid, template_id, status, id DESC);

-- Restore structure from 00000152_moya_invite_code.up.sql
CREATE TABLE IF NOT EXISTS moya_invite_code (
    code        text                     NOT NULL,
    group_id    bigint                   NOT NULL,
    created_by  bigint                   NOT NULL,
    status      text                     DEFAULT 'active' NOT NULL,
    expires_at  timestamp with time zone,
    created_at  timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_moya_invite_code PRIMARY KEY (code),
    CONSTRAINT ck_moya_invite_code_status
        CHECK (status = ANY (ARRAY['active'::text, 'revoked'::text]))
);
COMMENT ON TABLE  moya_invite_code          IS
    '老师班级邀请码（绑班级不绑个人；一码多人可用、可撤销；家长凭码进确认页后显式选 learner 绑监护）';
COMMENT ON COLUMN moya_invite_code.code       IS '10 位 Crockford Base32（CSPRNG 生成，排除 I/L/O/U 手抄混淆字符；应用层生成，码即主键）';
COMMENT ON COLUMN moya_invite_code.group_id   IS '绑定的班级 "group".id（JOIN 经 workspace 归属 organization）';
COMMENT ON COLUMN moya_invite_code.created_by IS '生成者 "user".id（守卫：该班 active class_staff 或 org owner）';
COMMENT ON COLUMN moya_invite_code.status     IS 'active 有效 | revoked 已撤销（撤销后凭码访问统一按 not_found 折叠，防探测）';
COMMENT ON COLUMN moya_invite_code.expires_at IS '过期时间（NULL=不过期；过期判定读取时计算，status 不翻转）';
ALTER TABLE moya_invite_code DROP CONSTRAINT IF EXISTS fk_moya_invite_code_group;
ALTER TABLE moya_invite_code ADD CONSTRAINT fk_moya_invite_code_group
    FOREIGN KEY (group_id) REFERENCES "group"(id) ON DELETE CASCADE;
ALTER TABLE moya_invite_code DROP CONSTRAINT IF EXISTS fk_moya_invite_code_created_by;
ALTER TABLE moya_invite_code ADD CONSTRAINT fk_moya_invite_code_created_by
    FOREIGN KEY (created_by) REFERENCES "user"(id) ON DELETE CASCADE;
CREATE INDEX IF NOT EXISTS i_moya_invite_code_group_status
    ON moya_invite_code USING btree (group_id, status);

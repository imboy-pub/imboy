-- 00000097_homework_review_loop.up.sql
-- 墨芽习字 Step 7：assignment 教学扩展 + 回课闭环四表
-- （homework_submission / submission_asset / calligraphy_review_draft / teacher_review）
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策：
--   * 现有唯一约束 group_task_assignment_task_id_user_id_key（00000001，(task_id,user_id) 全量）
--     改造为**同名部分唯一索引**（WHERE learner_id IS NULL）——保留现有错误契约
--     （test/repo/group_task_assignment_repo_tests.erl:292 断言 unique_violation 约束名），普通群作业
--     行为完全不变（DB-COMPAT-01）；另增 (task_id,learner_id) WHERE learner_id IS NOT NULL
--     部分唯一索引，使"一位家长替两个孩子接收同一 task"可行、"同一 learner 不重复收教学作业"（DB-ASSIGN-01）。
--   * user_deletion_executor（现有）会物理 DELETE 该用户的 group_task_assignment 行：本迁移
--     submission→assignment 用 CASCADE 保持该流程不阻塞；引用 "user" 的可空审计列
--     （submitted_by/created_by/reviewer_uid）用 SET NULL，均不阻塞用户删除。
--   * 引用 learner/attachment 的归属列用 RESTRICT：儿童提交证据与附件不可被静默连带删除
--     （fail-closed；物理清理须走 Step 10 删除传播与 §9.3 删除请求流程）。
--   * Organization 一致性（§6.3 不变量"assignment、learner、Group 最终解析到同一 Organization"）：
--     task_id(HashID) → group_task → "group" → workspace → organization 多跳无法建复合 FK，
--     采用 DEFERRABLE 约束触发器（00000077/00000096 同款先例）。
--   * "一个 submission 最多一个有效 AI 草稿 / 一个已发布回评"用部分唯一索引实现；
--     发布完整性（reviewer/published_at/至少一种反馈内容）用 CHECK 实现（DB-REVIEW-01）。

-- ============================================================
-- Phase 1: group_task_assignment 教学扩展（兼容旧群作业，expand-first）
-- ============================================================
ALTER TABLE group_task_assignment ADD COLUMN IF NOT EXISTS learner_id bigint;
ALTER TABLE group_task_assignment ADD COLUMN IF NOT EXISTS submitted_by bigint;

COMMENT ON COLUMN group_task_assignment.learner_id   IS '教学作业学员档案ID（NULL=普通 IMBoy 群作业，兼容旧数据；教学作业提交/查看权限真源是 guardian_learner）';
COMMENT ON COLUMN group_task_assignment.submitted_by IS '最近一次提交人（快捷字段，可空；真源在 homework_submission.submitted_by）';

-- 唯一约束改造：删全量约束，建同名部分唯一索引（普通群作业分支）
ALTER TABLE group_task_assignment
    DROP CONSTRAINT IF EXISTS group_task_assignment_task_id_user_id_key;
CREATE UNIQUE INDEX IF NOT EXISTS group_task_assignment_task_id_user_id_key
    ON group_task_assignment USING btree (task_id, user_id)
    WHERE learner_id IS NULL;
-- 教学作业分支：同 task 每 learner 至多一个教学 assignment
CREATE UNIQUE INDEX IF NOT EXISTS uk_group_task_assignment_task_learner
    ON group_task_assignment USING btree (task_id, learner_id)
    WHERE learner_id IS NOT NULL;

-- 学员作业历史热路径
CREATE INDEX IF NOT EXISTS i_group_task_assignment_learner
    ON group_task_assignment USING btree (learner_id)
    WHERE learner_id IS NOT NULL;

ALTER TABLE group_task_assignment DROP CONSTRAINT IF EXISTS fk_gta_learner;
ALTER TABLE group_task_assignment ADD CONSTRAINT fk_gta_learner
    FOREIGN KEY (learner_id) REFERENCES learner(id) ON DELETE RESTRICT;

ALTER TABLE group_task_assignment DROP CONSTRAINT IF EXISTS fk_gta_submitted_by;
ALTER TABLE group_task_assignment ADD CONSTRAINT fk_gta_submitted_by
    FOREIGN KEY (submitted_by) REFERENCES "user"(id) ON DELETE SET NULL;

-- 教学 assignment 的 Organization 一致性：
-- learner_id 非空时，task 所属 Group 经 workspace 解析的机构必须等于 learner 机构
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

-- ============================================================
-- Phase 2: homework_submission（多次提交/attempt，重练不覆盖历史）
-- ============================================================
CREATE TABLE IF NOT EXISTS homework_submission (
    id            bigint                       NOT NULL,  -- TSID
    assignment_id bigint                       NOT NULL,
    learner_id    bigint                       NOT NULL,  -- 冗余自 assignment，触发器保证一致
    submitted_by  bigint,                                 -- 提交人（家长/学员uid；可空=提交账号已注销）
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

-- 老师待评队列热路径
CREATE INDEX IF NOT EXISTS i_homework_submission_queue
    ON homework_submission USING btree (status, submitted_at DESC);

-- submission 与 assignment 的 learner 一致性（一个 submission 只属于一个 assignment 和 learner）
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

-- ============================================================
-- Phase 3: submission_asset（提交附件关联，只存 attachment_id）
-- ============================================================
CREATE TABLE IF NOT EXISTS submission_asset (
    id            bigint                       NOT NULL,  -- TSID
    submission_id bigint                       NOT NULL,
    attachment_id bigint                       NOT NULL,
    kind          text                         NOT NULL,
    sort_order    integer                      DEFAULT 0 NOT NULL,
    created_by    bigint,                                 -- 添加人（可空=账号已注销，审计保留行）
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

-- ============================================================
-- Phase 4: calligraphy_review_draft（AI 草稿，仅老师可见，§7.3/D-10）
-- ============================================================
CREATE TABLE IF NOT EXISTS calligraphy_review_draft (
    id             bigint                       NOT NULL,  -- TSID
    submission_id  bigint                       NOT NULL,
    ai_task_id     character varying(64),                  -- 异步任务标识（可空=未入队/同步失败前）
    status         text                         DEFAULT 'queued' NOT NULL,
    model_profile  character varying(100)       DEFAULT '' NOT NULL,
    prompt_version character varying(50)        DEFAULT '' NOT NULL,
    rubric_version character varying(50)        DEFAULT '' NOT NULL,
    input_digest   character varying(128)       DEFAULT '' NOT NULL,
    result_json    jsonb,                                  -- 仅结构化结果，不存模型思维过程
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

-- 一个 submission 同时最多一个有效 AI 草稿（failed 可重试新行；succeeded 后重跑=新 attempt 新 submission）
CREATE UNIQUE INDEX IF NOT EXISTS uk_crd_active_per_submission
    ON calligraphy_review_draft USING btree (submission_id)
    WHERE status IN ('queued', 'running', 'succeeded');

-- Worker 队列扫描
CREATE INDEX IF NOT EXISTS i_crd_status_created
    ON calligraphy_review_draft USING btree (status, created_at);

-- ============================================================
-- Phase 5: teacher_review（老师回评，发布约束 + 单已发布回评）
-- ============================================================
CREATE TABLE IF NOT EXISTS teacher_review (
    id                  bigint                       NOT NULL,  -- TSID
    submission_id       bigint                       NOT NULL,
    reviewer_uid        bigint,                                 -- 发布时必填（draft 可空=未定稿）
    positive_point      text                         DEFAULT '' NOT NULL,
    focus_problem       text                         DEFAULT '' NOT NULL,
    practice_action     text                         DEFAULT '' NOT NULL,
    comment             text                         DEFAULT '' NOT NULL,
    video_attachment_id bigint,                                 -- 真人点评视频（可空）
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

-- 一个 submission 最多一个已发布回评（重复发布由 Step 9 条件更新 + 本索引兜底）
CREATE UNIQUE INDEX IF NOT EXISTS uk_tr_published_per_submission
    ON teacher_review USING btree (submission_id)
    WHERE status = 'published';

-- 老师工作台热路径
CREATE INDEX IF NOT EXISTS i_teacher_review_submission
    ON teacher_review USING btree (submission_id, status);
CREATE INDEX IF NOT EXISTS i_teacher_review_reviewer
    ON teacher_review USING btree (reviewer_uid)
    WHERE reviewer_uid IS NOT NULL;

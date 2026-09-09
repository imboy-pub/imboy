-- 00000098_submission_idempotency_withdraw.up.sql
-- 墨芽习字 R2：homework_submission 补幂等/撤回审计列，对齐 Step 4 冻结契约（§7.2 幂等与状态机）
-- 计划契约：docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md §7.2；
--           schema gap 审计（用户现场确认，STEP-08/schema-gap.md 由 Agent B 补写）
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策：
--   * 幂等键三元组 (submitted_by, assignment_id, idempotency_key) 部分唯一索引：
--     同家长同作业同键只允许一条 submission，重放命中返回原行（应用层 ON CONFLICT DO NOTHING + 回读）；
--     request_digest 不进唯一键——同 key 不同载荷由应用层回读比对后拒绝（错误码 5460 语义），
--     DB 层只保证"同 key 不会产生第二行"。
--   * idempotency_key 可空：兼容无幂等键的内部/测试写入；唯一性仅在键非空时生效。
--     submitted_by 被注销置 NULL 后该行自然退出幂等竞争（NULL 在唯一索引中互不冲突），符合注销语义。
--   * withdrawn_by FK ON DELETE SET NULL：与 learner.user_id / homework_submission.submitted_by 注销语义一致。
--     ⚠️ 已知交互：撤回后账号被物理删除时 SET NULL 会触发 withdraw_audit CHECK 使删除事务失败——
--     这是 fail-closed（撤回审计完整性优先），删除流程需先改派/匿名化（见 STEP-08-DB/notes.md）。
--   * CHECK 语义：submitted 态 withdrawn 两列必须为空（撤回不可逆，重提交=新 attempt 新行）；
--     withdrawn 态 withdrawn_at/withdrawn_by 必须非空且 submitted_at 非空（后者本表已 NOT NULL，显式声明自文档化）。

ALTER TABLE homework_submission ADD COLUMN IF NOT EXISTS idempotency_key character varying(128);
ALTER TABLE homework_submission ADD COLUMN IF NOT EXISTS request_digest  character varying(128);
ALTER TABLE homework_submission ADD COLUMN IF NOT EXISTS withdrawn_at   timestamp with time zone;
ALTER TABLE homework_submission ADD COLUMN IF NOT EXISTS withdrawn_by       bigint;

COMMENT ON COLUMN homework_submission.idempotency_key IS '客户端幂等键（可空=内部写入；同 (submitted_by,assignment_id,idempotency_key) 唯一，重放返回原行）';
COMMENT ON COLUMN homework_submission.request_digest  IS '请求载荷摘要（sha256 hex；同 key 不同 digest 由应用层判 5460 冲突，DB 不参与唯一性）';
COMMENT ON COLUMN homework_submission.withdrawn_at   IS '撤回时间（status=withdrawn 必填）';
COMMENT ON COLUMN homework_submission.withdrawn_by   IS '撤回操作人（can_submit=true 监护人；status=withdrawn 必填；注销置 NULL 但 CHECK 阻断该路径=审计 fail-closed）';

-- 有效唯一约束（幂等键非空时生效）
CREATE UNIQUE INDEX IF NOT EXISTS uk_homework_submission_idempotency
    ON homework_submission USING btree (submitted_by, assignment_id, idempotency_key)
    WHERE idempotency_key IS NOT NULL;

ALTER TABLE homework_submission DROP CONSTRAINT IF EXISTS fk_hs_withdrawn_by;
ALTER TABLE homework_submission ADD CONSTRAINT fk_hs_withdrawn_by
    FOREIGN KEY (withdrawn_by) REFERENCES "user"(id) ON DELETE SET NULL;

-- 撤回状态机一致性 CHECK
ALTER TABLE homework_submission DROP CONSTRAINT IF EXISTS ck_homework_submission_withdraw_audit;
ALTER TABLE homework_submission ADD CONSTRAINT ck_homework_submission_withdraw_audit CHECK (
    (status = 'submitted' AND withdrawn_at IS NULL AND withdrawn_by IS NULL)
    OR
    (status = 'withdrawn' AND withdrawn_at IS NOT NULL AND withdrawn_by IS NOT NULL
                                 AND submitted_at IS NOT NULL)
);

-- ============================================================
-- 撤回/发布互斥 DB backstop（并发实测发现并修复的竞态）
--   实测：发布事务仅 FOR UPDATE 锁 submission 行未改行时，撤回方的单语句守卫
--   （UPDATE ... AND NOT EXISTS published）在 READ COMMITTED 下按语句起始快照评估，
--   看不到发布事务随后 COMMIT 的 published review -> 撤回穿透，出现
--   withdrawn+published 非法并存。触发器在语句结束时用新鲜语句快照复查，双向封堵。
--   应用层仍应采用"先 FOR UPDATE 锁行再条件更新"配方（见 STEP-08-DB/notes.md），
--   触发器是无视应用配方的兜底。
-- ============================================================
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

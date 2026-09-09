-- 00000098_submission_idempotency_withdraw.down.sql
-- 安全回滚：仅撤除本迁移新增的列/索引/约束（00000097 建立的 homework_submission 原有结构不动）。
-- 注意：down 会丢失幂等键/撤回审计数据（expand 列，无历史回填负担）。

DROP TRIGGER IF EXISTS trg_teacher_review_publish_guard ON teacher_review;
DROP FUNCTION IF EXISTS fn_teacher_review_publish_guard();
DROP TRIGGER IF EXISTS trg_homework_submission_withdraw_guard ON homework_submission;
DROP FUNCTION IF EXISTS fn_homework_submission_withdraw_guard();

ALTER TABLE homework_submission DROP CONSTRAINT IF EXISTS ck_homework_submission_withdraw_audit;
ALTER TABLE homework_submission DROP CONSTRAINT IF EXISTS fk_hs_withdrawn_by;
DROP INDEX IF EXISTS uk_homework_submission_idempotency;

ALTER TABLE homework_submission DROP COLUMN IF EXISTS withdrawn_by;
ALTER TABLE homework_submission DROP COLUMN IF EXISTS withdrawn_at;
ALTER TABLE homework_submission DROP COLUMN IF EXISTS request_digest;
ALTER TABLE homework_submission DROP COLUMN IF EXISTS idempotency_key;

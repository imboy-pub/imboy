-- 00000100_teaching_sentinel_unify.down.sql
-- 回滚到 00000097/98 的「FK ON DELETE SET NULL」语义。
--
-- down 决策（任务书两候选之外的正确第三态，理由如下）：
--   候选 b「先 UPDATE 0→NULL 再重建 FK」不可行：sentinel 0 的 withdrawn 行改 NULL 会违反
--   ck_homework_submission_withdraw_audit（withdrawn 态强制 withdrawn_by 非空），published 回评
--   改 NULL 违反 ck_teacher_review_published——UPDATE 直接 check_violation，链走不通。
--   候选 a「直接重建 FK」在有 0 值行时 ADD CONSTRAINT 校验存量失败（FK violation，无 user id=0），
--   且错误信息对运维不友好。
--   故采用：**预检 fail-fast**——存在 sentinel 0 行时拒绝回滚（审计痕迹不可逆抹除，须先导出/处置，
--   与 00000099 down 的「先导出再删」同口径）；无 0 值行时干净重建 FK（等价候选 a 的可行域）。
--   该限制是审计不可变设计的必然，非缺陷：0 行本身就是"必须保留的操作人痕迹"。

DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM homework_submission WHERE withdrawn_by = 0)
       OR EXISTS (SELECT 1 FROM teacher_review WHERE reviewer_uid = 0) THEN
        RAISE EXCEPTION 'sentinel 0 audit rows exist (homework_submission.withdrawn_by / teacher_review.reviewer_uid): export or archive audit rows before downgrade; silent NULL rewrite would destroy operator trace';
    END IF;
END $$;

ALTER TABLE homework_submission DROP CONSTRAINT IF EXISTS ck_homework_submission_withdrawn_by_sentinel;
ALTER TABLE teacher_review      DROP CONSTRAINT IF EXISTS ck_teacher_review_reviewer_uid_sentinel;

-- 恢复 97/98 原语义（同名约束重建，重复执行 down 亦安全）
ALTER TABLE homework_submission DROP CONSTRAINT IF EXISTS fk_hs_withdrawn_by;
ALTER TABLE homework_submission ADD CONSTRAINT fk_hs_withdrawn_by
    FOREIGN KEY (withdrawn_by) REFERENCES "user"(id) ON DELETE SET NULL;

ALTER TABLE teacher_review DROP CONSTRAINT IF EXISTS fk_tr_reviewer;
ALTER TABLE teacher_review ADD CONSTRAINT fk_tr_reviewer
    FOREIGN KEY (reviewer_uid) REFERENCES "user"(id) ON DELETE SET NULL;

-- 恢复原列注释（97/98 文案）
COMMENT ON COLUMN homework_submission.withdrawn_by IS
    '撤回操作人（can_submit=true 监护人；status=withdrawn 必填；注销置 NULL 但 CHECK 阻断该路径=审计 fail-closed）';
COMMENT ON COLUMN teacher_review.reviewer_uid IS
    '回评老师用户ID（published 时 CHECK 强制非空）';

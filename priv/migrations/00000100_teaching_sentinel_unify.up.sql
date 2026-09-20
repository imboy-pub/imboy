-- 00000100_teaching_sentinel_unify.up.sql
-- 墨芽习字：教学审计操作人列从「FK SET NULL」统一为 00000099 的 sentinel 语义
-- （裸列 + CHECK(col IS NULL OR col>=0) + 0=已匿名化）。
-- 覆盖两列：
--   homework_submission.withdrawn_by（00000098 引入，fk_hs_withdrawn_by ON DELETE SET NULL）
--   teacher_review.reviewer_uid      （00000097 引入，fk_tr_reviewer     ON DELETE SET NULL）
--
-- 动机（对齐 00000099 基准）：
--   1) SET NULL 会抹掉操作人痕迹：撤回/回评是审计动作，操作人账号注销后审计行必须保留原 uid，
--      由应用层匿名化流程显式 UPDATE→0（sentinel），而不是 FK 静默置 NULL；
--   2) 解除 fail-closed 交互：00000098 时代 withdrawn_by 的 SET NULL 撞
--      ck_homework_submission_withdraw_audit（withdrawn 态强制非空）会使用户删除事务失败；
--      ck_teacher_review_published（published 态 reviewer_uid 非空）同理。裸列后用户删除不再被
--      审计行阻塞，痕迹也不丢——两面同时成立；
--   3) 替代方案「FK + 预插 user(id)=0 占位行」否决：触发 sync_fts_user() 等账号触发器、
--      污染账号空间（同 00000099 决策理由）。
--
-- 迁移契约：up=可重复执行（DROP IF EXISTS + ADD 同名约束，净效果幂等）；
--           禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--           存量数据零改写：仅换约束，既有 uid 值原样保留。

-- ① 摘除 FK（操作人列从此不由引用完整性背书，审计行先于账号存活）
ALTER TABLE homework_submission DROP CONSTRAINT IF EXISTS fk_hs_withdrawn_by;
ALTER TABLE teacher_review      DROP CONSTRAINT IF EXISTS fk_tr_reviewer;

-- ② sentinel CHECK（99 同款风格：NULL=动作本无操作人/未定稿；0=操作人已注销并匿名化；负数拒收）
ALTER TABLE homework_submission DROP CONSTRAINT IF EXISTS ck_homework_submission_withdrawn_by_sentinel;
ALTER TABLE homework_submission ADD CONSTRAINT ck_homework_submission_withdrawn_by_sentinel
    CHECK (withdrawn_by IS NULL OR withdrawn_by >= 0);

ALTER TABLE teacher_review DROP CONSTRAINT IF EXISTS ck_teacher_review_reviewer_uid_sentinel;
ALTER TABLE teacher_review ADD CONSTRAINT ck_teacher_review_reviewer_uid_sentinel
    CHECK (reviewer_uid IS NULL OR reviewer_uid >= 0);

-- ③ 注释对齐 sentinel 语义（原 97/98 文案描述的「注销置 NULL」路径已不存在）
COMMENT ON COLUMN homework_submission.withdrawn_by IS
    '撤回操作人（can_submit=true 监护人；status=withdrawn 必填）；裸列无FK；0=sentinel=操作人账号已删除/匿名化（应用层匿名化时改写为0，禁置NULL）';
COMMENT ON COLUMN teacher_review.reviewer_uid IS
    '回评老师用户ID（published 时 CHECK 强制非空）；裸列无FK；0=sentinel=老师账号已删除/匿名化（published 行保留回评内容与 0 sentinel，禁置NULL）';

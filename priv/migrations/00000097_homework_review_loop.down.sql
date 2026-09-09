-- 00000097_homework_review_loop.down.sql
-- 安全回滚：先删四张回课表与触发器/函数，再收缩 group_task_assignment 扩展列，
-- 最后恢复 00000001 原全量唯一约束 group_task_assignment_task_id_user_id_key。
-- 若存在教学作业数据（同 task 同 user 多行）导致原约束无法恢复，则显式失败并提示，
-- 绝不静默删除数据。

-- Phase 5→4→3→2: 删回课表（触发器随表删除，函数显式清理）
DROP TABLE IF EXISTS teacher_review;
DROP TABLE IF EXISTS calligraphy_review_draft;
DROP TABLE IF EXISTS submission_asset;
DROP TRIGGER IF EXISTS trg_homework_submission_learner_consistency ON homework_submission;
DROP FUNCTION IF EXISTS fn_homework_submission_learner_check();
DROP TABLE IF EXISTS homework_submission;

-- Phase 1 收缩: assignment 触发器/索引/FK/列
DROP TRIGGER IF EXISTS trg_gta_learner_org_consistency ON group_task_assignment;
DROP FUNCTION IF EXISTS fn_gta_learner_org_check();

DROP INDEX IF EXISTS i_group_task_assignment_learner;
DROP INDEX IF EXISTS uk_group_task_assignment_task_learner;
DROP INDEX IF EXISTS group_task_assignment_task_id_user_id_key;  -- 部分唯一索引（同名占位）

ALTER TABLE group_task_assignment DROP CONSTRAINT IF EXISTS fk_gta_submitted_by;
ALTER TABLE group_task_assignment DROP CONSTRAINT IF EXISTS fk_gta_learner;
ALTER TABLE group_task_assignment DROP COLUMN IF EXISTS submitted_by;
ALTER TABLE group_task_assignment DROP COLUMN IF EXISTS learner_id;

-- 恢复 00000001 原全量唯一约束（先做数据兼容预检，fail with clear message）
DO $$
BEGIN
    IF EXISTS (
        SELECT 1
          FROM group_task_assignment
         GROUP BY task_id, user_id
        HAVING count(*) > 1
    ) THEN
        RAISE EXCEPTION
            'down 00000097 无法恢复原唯一约束 group_task_assignment_task_id_user_id_key：'
            '存在同 (task_id,user_id) 多行（教学作业数据）。请先人工清理教学 assignment 行再回滚'
            USING ERRCODE = 'P0001',
                  HINT = '本 down 迁移不删除任何业务数据';
    END IF;
END;
$$;

ALTER TABLE group_task_assignment
    DROP CONSTRAINT IF EXISTS group_task_assignment_task_id_user_id_key;
ALTER TABLE group_task_assignment
    ADD CONSTRAINT group_task_assignment_task_id_user_id_key UNIQUE (task_id, user_id);

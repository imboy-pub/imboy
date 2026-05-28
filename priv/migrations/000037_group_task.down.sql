DROP INDEX IF EXISTS idx_group_task_deleted_at;
DROP INDEX IF EXISTS idx_group_task_group_status_alive;
ALTER TABLE public."group_task" DROP COLUMN IF EXISTS "deleted_at";

DROP TABLE IF EXISTS public."group_task" CASCADE;
DROP TABLE IF EXISTS public."group_task_assignment" CASCADE;
DROP INDEX IF EXISTS idx_group_task_group_id;
DROP INDEX IF EXISTS idx_group_task_task_id;
DROP INDEX IF EXISTS idx_group_task_status;
DROP INDEX IF EXISTS idx_group_task_assignment_task_id;
DROP INDEX IF EXISTS idx_group_task_assignment_user_id;
DROP INDEX IF EXISTS idx_group_task_assignment_status;

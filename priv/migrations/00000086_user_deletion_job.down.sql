-- 00000086_user_deletion_job.down.sql
DROP INDEX IF EXISTS public.idx_user_deletion_job_status;
DROP TABLE IF EXISTS public.user_deletion_job;
--;

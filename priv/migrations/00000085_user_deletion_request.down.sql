-- 00000085_user_deletion_request.down.sql
-- 回滚 00000085：注销请求窄记录表（D-01）

DROP INDEX IF EXISTS public.idx_user_deletion_request_status;
DROP TABLE IF EXISTS public.user_deletion_request;
--;

-- 回滚 000039: 移除 moment_post.location 与 at_uids
-- Rollback: drop moment_post.location and at_uids

ALTER TABLE public.moment_post DROP COLUMN IF EXISTS location;
ALTER TABLE public.moment_post DROP COLUMN IF EXISTS at_uids;

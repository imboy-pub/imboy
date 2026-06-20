-- ============================================================
-- 回滚 000013: 移除 attachment 的 scope/scope_ref 列
-- ============================================================

ALTER TABLE public.attachment DROP COLUMN IF EXISTS scope_ref;
--;

ALTER TABLE public.attachment DROP COLUMN IF EXISTS scope;

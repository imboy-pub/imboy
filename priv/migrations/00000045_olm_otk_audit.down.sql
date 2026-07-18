-- 00000045_olm_otk_audit.down.sql
-- 回滚 OTK 审计字段。已消费但未清理的 claimed 记录连同审计信息一并丢失。

DROP INDEX IF EXISTS public.idx_olm_otk_consumed;
DROP INDEX IF EXISTS public.idx_olm_otk_available;

ALTER TABLE public.olm_one_time_key
    DROP CONSTRAINT IF EXISTS chk_olm_otk_status;

ALTER TABLE public.olm_one_time_key
    DROP COLUMN IF EXISTS claimed_by,
    DROP COLUMN IF EXISTS consumed_at,
    DROP COLUMN IF EXISTS status;

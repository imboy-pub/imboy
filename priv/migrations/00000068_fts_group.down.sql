-- 00000068_fts_group.down.sql
-- 回滚群组全文搜索表 + 触发器 + 公开群分类

BEGIN;

DROP TRIGGER IF EXISTS trg_fts_group_update ON public."group";
DROP FUNCTION IF EXISTS fts_group_trigger();
DROP TABLE IF EXISTS public.fts_group;

ALTER TABLE public."group" DROP COLUMN IF EXISTS is_featured;
ALTER TABLE public."group" DROP COLUMN IF EXISTS category_id;

DROP TABLE IF EXISTS public.group_category;

COMMIT;
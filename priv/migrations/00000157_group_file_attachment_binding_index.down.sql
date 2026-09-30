-- 回滚索引，不改动文件或附件 / Roll back the index without changing files.
SET lock_timeout = '5s';
SET statement_timeout = '15min';

DROP INDEX IF EXISTS public.idx_attachment_group_file_binding;

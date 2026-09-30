-- 群文件附件绑定查询 / Bound group-file attachment lookup.
-- 外层迁移器管理事务；本迁移只加索引，不修改资料归属。
SET lock_timeout = '5s';
SET statement_timeout = '15min';

CREATE INDEX IF NOT EXISTS idx_attachment_group_file_binding
    ON public.attachment (group_file_id, scope_ref, id)
    WHERE scope = 'group' AND status >= 0 AND group_file_id IS NOT NULL;

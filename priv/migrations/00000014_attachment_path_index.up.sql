-- ============================================================
-- 迁移 000014: attachment.path 索引
--   view_url -> attachment_repo:find_by_path/1 是 C 端受限附件下载热路径
--   （WHERE path = $1 AND status >= 0），path 列原本无索引 → 全表扫描。
--   部分索引仅覆盖 status >= 0（正常/禁用，排除软删 -1），与查询条件对齐。
-- ============================================================

CREATE INDEX IF NOT EXISTS i_attachment_path
    ON public.attachment USING btree (path)
    WHERE (status >= 0);

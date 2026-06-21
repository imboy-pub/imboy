-- ============================================================
-- 回滚 000015: path 唯一约束 -> md5 唯一约束
--   注意：若回滚时存在 md5 重复行，ADD uk_attachment_md5 会失败（需先清理）。
-- ============================================================

ALTER TABLE public.attachment
    DROP CONSTRAINT IF EXISTS uk_attachment_path;
--;

CREATE INDEX IF NOT EXISTS i_attachment_path
    ON public.attachment USING btree (path)
    WHERE (status >= 0);
--;

ALTER TABLE public.attachment
    ADD CONSTRAINT uk_attachment_md5 UNIQUE (md5);

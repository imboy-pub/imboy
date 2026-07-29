-- 00000052_attachment_cipher.down.sql
--
-- ⚠️ 回滚会**丢失「哪些对象是密文」这一事实**：cipher 列被删除后，
--   已加密上传的行与明文行不再可区分，而它们的 file_hash256/size 是密文语义。
--   仅在「尚未有任何加密上传」时回滚才是无损的。

DROP INDEX IF EXISTS public.idx_attachment_plaintext_backlog;

ALTER TABLE public.attachment DROP COLUMN IF EXISTS cipher;

COMMENT ON COLUMN public.attachment.file_hash256 IS '附件文件哈希（SHA-256 hex；历史行为旧 MD5，仅作完整性参考）';

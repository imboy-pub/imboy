-- 00000075_legacy_attachment_encryption.down.sql
--
-- ⚠️ 回滚会**永久丢失存量附件的解密能力**：legacy_key 是服务端保存的唯一一份
--   content key。scripts/migrate_legacy_attachments.erl 已把 Garage 中的明文
--   原地覆盖为 AES-256-GCM 密文——列一旦删除，这些附件的密钥再无第二处副本，
--   客户端将无法解密，且不可逆。
--
-- 仅在以下两种情况回滚是无损的：
--   1) 迁移脚本从未执行（不存在 cipher IS NOT NULL AND legacy_key IS NOT NULL 的行）；或
--   2) 已先把这些行的 legacy_key 导出到库外安全保存。
--
-- 回滚前建议先自检并导出：
--   SELECT count(*) FROM public.attachment
--    WHERE cipher IS NOT NULL AND legacy_key IS NOT NULL;   -- 非 0 则有损
--   \copy (SELECT id, legacy_key FROM public.attachment
--           WHERE legacy_key IS NOT NULL) TO 'legacy_keys.csv' CSV HEADER
--
-- 幂等：DROP COLUMN IF EXISTS 可安全重跑。

ALTER TABLE public.attachment DROP COLUMN IF EXISTS legacy_key;

-- 00000075_legacy_attachment_encryption.up.sql
-- 存量明文附件迁移：新增 legacy_key 列存储加密密钥。
--
-- 背景：
--   E2EE 启用前上传的附件以明文存储在 Garage S3 中（cipher IS NULL）。
--   迁移脚本 scripts/migrate_legacy_attachments.erl 逐条：
--     1. 从 Garage 读取明文
--     2. AES-256-GCM 加密（随机 content key）
--     3. 写回 Garage（覆盖原 object key）
--     4. 更新 cipher = 'AES-256-GCM'
--     5. 将 content key 经 postgre_aes_key 加密后存入 legacy_key 列
--
-- legacy_key 列：
--   经 postgre_aes_key 加密的 base64 编码 content key。
--   客户端通过 view_url 获取此 key 后解密附件内容。
--   NULL = 未迁移的明文附件或原生 E2EE 附件（无服务器端 key）。
--
-- 幂等：
--   ALTER TABLE ... ADD COLUMN IF NOT EXISTS 可安全重跑。
--   迁移脚本只处理 cipher IS NULL 的行，已处理的不会重复。

ALTER TABLE public.attachment
    ADD COLUMN IF NOT EXISTS legacy_key text DEFAULT NULL;

COMMENT ON COLUMN public.attachment.legacy_key IS
    '存量明文附件迁移后的加密密钥（经 postgre_aes_key 加密的 base64）。
     NULL = 未迁移的明文附件，或原生 E2EE 附件（服务端不持有 key）。
     非 NULL 时客户端下载后需用此 key 解密。';
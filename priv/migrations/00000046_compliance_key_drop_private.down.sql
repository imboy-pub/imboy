-- 00000046_compliance_key_drop_private.down.sql
-- 回滚：重建 compliance_key.private_key_encrypted 可空列。
-- 注意：DROP 期间的历史私钥数据不可恢复；重建后该列对所有现存行均为 NULL。

ALTER TABLE public.compliance_key ADD COLUMN IF NOT EXISTS private_key_encrypted text;

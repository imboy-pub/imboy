-- 00000046_compliance_key_drop_private.up.sql
-- 零信任改造（线 A）：彻底下线 compliance_key.private_key_encrypted 列。
--
-- 背景：
--   compliance 私钥服务端持有 = 运营方/管理员可解密所有 compliance 模式密文，
--   破坏纯端到端语义。改造后合规私钥仅由审计方在本地（HSM / 离线介质）保管，
--   服务端永不接收、永不落盘。
--
-- 安全性：
--   - 该列是 nullable text，无 NOT NULL 约束，DROP 不影响任何 NOT NULL 数据。
--   - 该列无生产代码读取方（compliance_key_repo:find_by_key_id/1 是死代码，仅单测引用）。
--   - compliance_key_repo:create/3 改造后不再写入此列。
--
-- 回滚：见 00000046_compliance_key_drop_private.down.sql（重建可空列，历史数据不可恢复）。

ALTER TABLE public.compliance_key DROP COLUMN IF EXISTS private_key_encrypted;

-- 迁移 000074: 提升 E2EE 备份 KDF 迭代下限
--
-- 客户端已使用 310000 次 PBKDF2-HMAC-SHA256 迭代，但 DB CHECK 约束
-- 仍允许 100000 次。攻击者若降级客户端迭代数到 100k，可在拿到备份密文后
-- 以更低成本暴力破解弱口令。
-- 将下限提升到 310000 与客户端一致，消除降级攻击面。
--
-- 注意：此迁移会失败如果存在 kdf_iterations < 310000 的存量行。
-- 生产环境应先确保无低迭代备份，或先 update 再执行此迁移。

ALTER TABLE public.e2ee_key_backups
    DROP CONSTRAINT IF EXISTS chk_e2ee_key_backups_iterations;

ALTER TABLE public.e2ee_key_backups
    ADD CONSTRAINT chk_e2ee_key_backups_iterations
    CHECK (kdf_iterations >= 310000);
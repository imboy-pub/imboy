-- 00000074_e2ee_backup_kdf_lower_bound.down.sql
--
-- 回滚：把 E2EE 备份的 KDF 迭代下限从 310000 还原为 00000036 定义的 100000。
--
-- 无数据风险：310000 >= 100000，所有存量行必然满足还原后的约束，
--   不会因 CHECK 校验失败而中断回滚。
--
-- ⚠️ 安全影响：还原后 100000 次迭代的降级攻击面重新打开（原因见 .up.sql）。
--   仅在确认客户端已不依赖 310k 下限时才应回滚。

ALTER TABLE public.e2ee_key_backups
    DROP CONSTRAINT IF EXISTS chk_e2ee_key_backups_iterations;

ALTER TABLE public.e2ee_key_backups
    ADD CONSTRAINT chk_e2ee_key_backups_iterations
    CHECK (kdf_iterations >= 100000);

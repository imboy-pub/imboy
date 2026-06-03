-- ============================================================
-- Rollback 000070: 与 up 严格逆序、对称
-- 同样由外层单事务包裹，整体原子回滚。
-- 注意：DROP COLUMN 将丢失 user.dnd_enabled 数据；
--       段4 的回填属业务数据变更，删列即清除，无独立回滚步骤。
-- ============================================================

-- 逆序段3：删索引
DROP INDEX IF EXISTS i_dnd_rule_Status;

-- 逆序段2：删字段
ALTER TABLE public."user" DROP COLUMN IF EXISTS dnd_enabled;

-- 逆序段1：删唯一索引 + 删表（DROP TABLE CASCADE 已含索引清理，显式 DROP 仅作防御）
DROP INDEX IF EXISTS uk_dnd_rule_UserId;
DROP TABLE IF EXISTS public.user_dnd_rule CASCADE;

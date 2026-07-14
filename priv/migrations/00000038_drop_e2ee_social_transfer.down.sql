-- 回滚 000038：不可逆。
--   自研 E2EE 社交恢复/设备传输已从代码库删除，无法通过回滚迁移恢复到可用状态
--   （表结构可重建，但 handler/logic/ds/repo 代码已不存在，功能不会复活）。
--   如确需恢复该功能，请从版本控制历史检出对应代码与 00000004_social 等原始迁移。
--   Irreversible: feature code removed; recreating tables alone won't restore it.
SELECT 1;

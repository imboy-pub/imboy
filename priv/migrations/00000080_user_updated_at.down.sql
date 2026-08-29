-- 回滚 00000080：移除 updated_at 列。
-- 注意：若目标库在本次迁移前已手工拥有该列（含真实数据），执行 down
-- 会连带删除该列——生产环境执行前请先确认数据去向。
ALTER TABLE public."user" DROP COLUMN IF EXISTS updated_at;

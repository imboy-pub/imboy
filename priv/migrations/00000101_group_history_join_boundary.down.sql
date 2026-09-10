-- 对称 down：移除 join boundary 模型。
-- 注意：staging.conv_seq 与 group_member_generation 中由 M1/运行期产生的边界数据
-- 随对象删除；wallet 式真钱语义不存在于此表，down 允许直接删（数据语义可弃）。
-- 分配计数器（msg_store_seq）不动：序列只增不回退，gap 语义允许。

DROP INDEX IF EXISTS public.uq_gmg_one_open_generation;
DROP INDEX IF EXISTS public.idx_gmg_group_user_open;
DROP TABLE IF EXISTS public.group_member_generation;

-- staging 表不由迁移创建（运行时 ensure_table_exists/0），down 只摘本迁移加的列，
-- 不得 DROP 表本身（存量部署表内可能有未归档 staging 行）。
ALTER TABLE IF EXISTS public.msg_store_staging DROP COLUMN IF EXISTS conv_seq;

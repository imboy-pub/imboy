-- 00000206 down: 回到“序列不由迁移持有”的形态（与 00000198 及之前一致）。
-- 注意: 删除后新注册会 {error, no_ids}（这正是 F1 的症状），仅用于迁移往返测试；
--       测试库可由 account_ds:init/0 以同一 DDL 重建。
DROP SEQUENCE IF EXISTS public.imboy_account_id_seq;

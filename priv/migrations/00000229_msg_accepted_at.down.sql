-- 00000229_msg_accepted_at.down.sql
-- 回滚 R03 的 accepted_at 列。
--   回滚后重新丢失服务端首次接受时间这一可信事实（D04），旧行 NULL 兼容读
--   回退 server_ts/created_at 的低可信语义恢复为唯一来源。已落库的接受
--   时间不可恢复。物理 down 仅限隔离迁移演练库（计划第 5 节 R03 回滚约束）。

ALTER TABLE IF EXISTS public.msg_store_staging
    DROP COLUMN IF EXISTS expire_at,
    DROP COLUMN IF EXISTS accepted_at;

ALTER TABLE IF EXISTS public.msg_store
    DROP COLUMN IF EXISTS accepted_at;

ALTER TABLE IF EXISTS public.msg_c2g
    DROP COLUMN IF EXISTS accepted_at;

ALTER TABLE IF EXISTS public.msg_c2c
    DROP COLUMN IF EXISTS accepted_at;

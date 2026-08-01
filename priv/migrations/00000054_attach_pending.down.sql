-- 00000054_attach_pending.down.sql
-- 回滚 attach_pending 表。
--
-- 表内只有"已签发 presign 但未 confirm"的登记行，属可重建的瞬态数据：
-- 丢弃后已确认的附件（attachment 表）不受影响，代价仅是回滚前遗留的
-- 未确认对象重新变得不可清理——与本迁移之前的行为一致。

DROP INDEX IF EXISTS public.i_attach_pending_created_at;

--;

DROP TABLE IF EXISTS public.attach_pending;

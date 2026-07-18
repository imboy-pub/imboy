-- 00000042_olm_prekeys.down.sql
-- 回滚：删除 Olm 三张表。客户端已上报的身份/prekey 数据丢失。

DROP TABLE IF EXISTS public.olm_fallback_key CASCADE;
DROP TABLE IF EXISTS public.olm_one_time_key CASCADE;
DROP TABLE IF EXISTS public.olm_identity CASCADE;

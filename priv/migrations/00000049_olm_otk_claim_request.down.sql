-- 00000049_olm_otk_claim_request.down.sql
-- 回滚 E2EE-062 的 OTK claim 幂等租约。
--   回滚后 claim 重新失去幂等性：客户端重试与恶意重放都会再消费 OTK，
--   目标用户的池可被定向耗尽。已发放的租约信息不可恢复。

DROP INDEX IF EXISTS public.uk_olm_otk_claim_request;

ALTER TABLE public.olm_one_time_key
    DROP COLUMN IF EXISTS claim_request_id;

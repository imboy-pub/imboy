-- 00000047_trust_event_freshness.down.sql
-- 回滚 E2EE-014 trust event freshness 列与幂等索引。
--   freshness / 幂等 / 版本快照信息丢失；user_device 代数与版本计数丢失。

DROP INDEX IF EXISTS public.uk_trust_audit_event_id;

ALTER TABLE public.trust_audit
    DROP COLUMN IF EXISTS event_id,
    DROP COLUMN IF EXISTS issued_at,
    DROP COLUMN IF EXISTS expires_at,
    DROP COLUMN IF EXISTS actor_device_generation,
    DROP COLUMN IF EXISTS target_identity_version;

ALTER TABLE public.user_device
    DROP COLUMN IF EXISTS device_generation,
    DROP COLUMN IF EXISTS identity_version;

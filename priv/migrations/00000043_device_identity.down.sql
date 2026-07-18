-- 00000043_device_identity.down.sql
-- 回滚 ADR 03 device-identity 扩展。已上报的 capabilities / trust_state / 扩展签名丢失。
-- device_id 宽度回退 128 → 40：若存量已有 >40 字符设备标识将失败（回滚前需确认）。

DROP INDEX IF EXISTS public.idx_user_device_capabilities;
DROP INDEX IF EXISTS public.idx_user_device_uid_active;

ALTER TABLE public.user_device
    DROP CONSTRAINT IF EXISTS chk_user_device_device_type,
    DROP CONSTRAINT IF EXISTS chk_user_device_trust_state;

ALTER TABLE public.user_device
    ALTER COLUMN device_id TYPE varchar(40);

ALTER TABLE public.user_device
    DROP COLUMN IF EXISTS identity_signed_at,
    DROP COLUMN IF EXISTS identity_signature,
    DROP COLUMN IF EXISTS identity_blob,
    DROP COLUMN IF EXISTS trust_state,
    DROP COLUMN IF EXISTS capabilities;

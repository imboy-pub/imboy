-- 00000047_trust_event_freshness.up.sql
-- E2EE-014（ADR 16 §3.3.1 scoped waiver）：trust event freshness / 唯一性 / 幂等。
--   为 trust_audit 事件流补 freshness(issued_at/expires_at)、幂等键(event_id 唯一)、
--   actor 设备代数与 target 身份版本快照；为 user_device 补 device_generation /
--   identity_version 单调计数（重注册 / 身份轮换 +1，不回退）。
--   新增列均可空 / 带默认，legacy 行不受影响（向后兼容）；event_id 唯一走 partial index
--   仅约束 NOT NULL 行，legacy NULL 行不冲突。
-- Non-breaking additive migration; freezes see ADR 16 §3.3.1.

-- 1. user_device：设备代数与身份版本（单调，不回退）
ALTER TABLE public.user_device
    ADD COLUMN IF NOT EXISTS device_generation integer NOT NULL DEFAULT 1,
    ADD COLUMN IF NOT EXISTS identity_version  integer NOT NULL DEFAULT 1;

COMMENT ON COLUMN public.user_device.device_generation IS '物理设备重注册单调代数（+1，不回退），防旧设备重放，见 ADR 16 §3.3.1';
COMMENT ON COLUMN public.user_device.identity_version  IS '密码学身份轮换单调版本（+1，不回退），防身份键回退，见 ADR 16 §3.3.1';

-- 2. trust_audit：freshness / 幂等 / 快照字段（legacy 行留空）
ALTER TABLE public.trust_audit
    ADD COLUMN IF NOT EXISTS event_id                varchar(64),
    ADD COLUMN IF NOT EXISTS issued_at               bigint,
    ADD COLUMN IF NOT EXISTS expires_at              bigint,
    ADD COLUMN IF NOT EXISTS actor_device_generation integer,
    ADD COLUMN IF NOT EXISTS target_identity_version integer;

COMMENT ON COLUMN public.trust_audit.event_id                IS '客户端生成的全局唯一幂等键，同 event_id 重放返回原结果，见 ADR 16 §3.3.1';
COMMENT ON COLUMN public.trust_audit.issued_at               IS '事件签发时刻（epoch ms），freshness 下界';
COMMENT ON COLUMN public.trust_audit.expires_at              IS '事件有效期上界（epoch ms）';
COMMENT ON COLUMN public.trust_audit.actor_device_generation IS 'actor 设备代数快照';
COMMENT ON COLUMN public.trust_audit.target_identity_version IS 'target 身份版本快照，防回退';

-- 3. event_id 幂等唯一：仅约束非空行，legacy NULL 行不参与（向后兼容）
CREATE UNIQUE INDEX IF NOT EXISTS uk_trust_audit_event_id
    ON public.trust_audit (event_id) WHERE event_id IS NOT NULL;

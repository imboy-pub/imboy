-- 00000043_device_identity.up.sql
-- ADR 03：Device Identity 一等公民化（列语义见 ADR 03 §3）。
--   user_device 只扩展「非密码学」派生列（capabilities / trust_state / 扩展签名 blob）；
--   Olm 身份键仍独立在 olm_identity 表（00000042），不合并进 user_device（ADR 03 §2.2）。
-- Extends user_device with non-cryptographic device-identity columns per ADR 03.
-- 冻结项（不可单方面变更）：user_device 的 identity 相关列。变更须走 supersedes 流程。

-- 1. 新增列
ALTER TABLE public.user_device
    ADD COLUMN IF NOT EXISTS capabilities       text[]      NOT NULL DEFAULT '{}',
    ADD COLUMN IF NOT EXISTS trust_state        varchar(20) NOT NULL DEFAULT 'unverified',
    ADD COLUMN IF NOT EXISTS identity_blob      jsonb,
    ADD COLUMN IF NOT EXISTS identity_signature text,
    ADD COLUMN IF NOT EXISTS identity_signed_at timestamptz;

-- 2. trust_state 状态约束（状态机见 ADR 06）
ALTER TABLE public.user_device
    DROP CONSTRAINT IF EXISTS chk_user_device_trust_state,
    ADD  CONSTRAINT chk_user_device_trust_state
         CHECK (trust_state IN ('unverified','verified','revoked'));

-- 3. device_type 扩展取值（phone/ipad/desktop/watch 等 5+ 设备场景）
ALTER TABLE public.user_device
    DROP CONSTRAINT IF EXISTS chk_user_device_device_type,
    ADD  CONSTRAINT chk_user_device_device_type CHECK (
        device_type IN ('','web','phone','tablet','ipad','desktop','watch',
                        'ios','android','macos','windows','linux')
    );

-- 4. 对齐 device_id 宽度：40 → 128（与 olm_identity.device_id 一致，varchar 增宽无重写）
ALTER TABLE public.user_device
    ALTER COLUMN device_id TYPE varchar(128);

-- 5. 索引
CREATE INDEX IF NOT EXISTS idx_user_device_uid_active
    ON public.user_device (user_id) WHERE status = 1;
CREATE INDEX IF NOT EXISTS idx_user_device_capabilities
    ON public.user_device USING gin (capabilities);

COMMENT ON COLUMN public.user_device.capabilities       IS '设备支持的协议套件短名数组（olm/megolm/rsa-oaep/mls）';
COMMENT ON COLUMN public.user_device.trust_state        IS '设备信任态，状态机见 ADR 06';
COMMENT ON COLUMN public.user_device.identity_blob      IS '规范化的待签名设备身份负载（jsonb）';
COMMENT ON COLUMN public.user_device.identity_signature IS '对 identity_blob 的 Ed25519 签名（base64）';
COMMENT ON COLUMN public.user_device.identity_signed_at IS '签名时间戳，用于轮换审计';

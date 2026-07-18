-- 00000042_olm_prekeys.up.sql
-- 单聊 Olm (X3DH + Double Ratchet) 支持：身份键、one-time keys、fallback key。
--
-- 设计：
--   - olm_identity：每设备 1 条，长期身份键（ed25519 签名键 + curve25519 身份键）。
--   - olm_one_time_key：每设备 N 条，X3DH prekey；claim 即删（message_type='one_time'）。
--   - olm_fallback_key：每设备 1 条（覆盖式），OTK 耗尽时兜底（message_type='fallback'）。
--
-- 零信任：服务端只存公钥侧（curve25519 公钥与签名），无私钥；私钥由客户端 pickle 加密落本地。
-- claim 语义：SELECT ... FOR UPDATE SKIP LOCKED + DELETE 原子消费，防并发领取同一 OTK。

CREATE TABLE IF NOT EXISTS public.olm_identity (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    device_id varchar(128) NOT NULL,
    ed25519_key text NOT NULL,
    curve25519_key text NOT NULL,
    signature text NOT NULL,
    created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT olm_identity_pkey PRIMARY KEY (id),
    CONSTRAINT olm_identity_user_device_key UNIQUE (user_id, device_id)
);

CREATE INDEX IF NOT EXISTS idx_olm_identity_user_id ON public.olm_identity USING btree (user_id);

COMMENT ON TABLE public.olm_identity IS 'Olm 设备身份键（X3DH 长期身份，仅公钥侧）';
COMMENT ON COLUMN public.olm_identity.ed25519_key IS 'Ed25519 签名公钥（base64）';
COMMENT ON COLUMN public.olm_identity.curve25519_key IS 'Curve25519 身份公钥（base64）';
COMMENT ON COLUMN public.olm_identity.signature IS '对 curve25519_key 的 Ed25519 签名（base64），防服务端篡改';

CREATE TABLE IF NOT EXISTS public.olm_one_time_key (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    device_id varchar(128) NOT NULL,
    key_id varchar(64) NOT NULL,
    key_base64 text NOT NULL,
    created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT olm_one_time_key_pkey PRIMARY KEY (id),
    CONSTRAINT olm_one_time_key_user_device_keyid UNIQUE (user_id, device_id, key_id)
);

CREATE INDEX IF NOT EXISTS idx_olm_one_time_key_device ON public.olm_one_time_key USING btree (user_id, device_id);

COMMENT ON TABLE public.olm_one_time_key IS 'Olm one-time key（X3DH prekey，claim 即删）';

CREATE TABLE IF NOT EXISTS public.olm_fallback_key (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    device_id varchar(128) NOT NULL,
    key_id varchar(64) NOT NULL,
    key_base64 text NOT NULL,
    created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT olm_fallback_key_pkey PRIMARY KEY (id),
    CONSTRAINT olm_fallback_key_user_device UNIQUE (user_id, device_id)
);

COMMENT ON TABLE public.olm_fallback_key IS 'Olm fallback key（OTK 耗尽兜底，每设备覆盖式 1 条）';

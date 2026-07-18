-- 00000045_olm_otk_audit.up.sql
-- ADR 03 §6/§8：OTK claim 从「即删」改为「UPDATE 审计」，保留消费痕迹供追查与低水位统计。
--   00000042 的 claim 语义是 SELECT FOR UPDATE SKIP LOCKED + DELETE（即删）；
--   本迁移补审计字段，B.3 后端改 claim 为 UPDATE status='claimed'（不删），配 cleanup worker
--   定期清理超期 consumed 记录。migration 只加字段/索引；claim 语义切换在后端逻辑层（B.3）。
-- Adds audit columns to olm_one_time_key so OTK claim becomes UPDATE-not-DELETE per ADR 03.

ALTER TABLE public.olm_one_time_key
    ADD COLUMN IF NOT EXISTS status      varchar(20) NOT NULL DEFAULT 'available',
    ADD COLUMN IF NOT EXISTS consumed_at timestamptz,
    ADD COLUMN IF NOT EXISTS claimed_by  bigint;        -- 领取方 user_id（审计）

ALTER TABLE public.olm_one_time_key
    DROP CONSTRAINT IF EXISTS chk_olm_otk_status,
    ADD  CONSTRAINT chk_olm_otk_status CHECK (status IN ('available','claimed'));

-- 可用 OTK 查询走部分索引（claim 只扫 available，与 00000042 的按设备取键一致）
CREATE INDEX IF NOT EXISTS idx_olm_otk_available
    ON public.olm_one_time_key (user_id, device_id) WHERE status = 'available';
-- cleanup worker 按 consumed_at 清理已消费键
CREATE INDEX IF NOT EXISTS idx_olm_otk_consumed
    ON public.olm_one_time_key (consumed_at) WHERE status = 'claimed';

COMMENT ON COLUMN public.olm_one_time_key.status      IS 'available=未领取 / claimed=已消费（不删，供审计与 cleanup）';
COMMENT ON COLUMN public.olm_one_time_key.consumed_at IS 'claim 消费时间戳；cleanup worker 据此清理';
COMMENT ON COLUMN public.olm_one_time_key.claimed_by  IS '领取方 user_id（审计谁消费了此 OTK）';

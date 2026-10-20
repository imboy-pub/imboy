-- direct-v1 DOWN 阻断（沿 V5 M4 惯例）：已有 direct 数据禁止回退。
DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM attach_direct_attempt LIMIT 1) THEN
        RAISE EXCEPTION 'DV1_DOWN_BLOCKED_USE_FORWARD_FIX';
    END IF;
    IF EXISTS (SELECT 1 FROM attach_pending WHERE direct_state IS NOT NULL LIMIT 1) THEN
        RAISE EXCEPTION 'DV1_DOWN_BLOCKED_USE_FORWARD_FIX';
    END IF;
    IF EXISTS (SELECT 1 FROM attachment WHERE storage_physical_key IS NOT NULL LIMIT 1)
       OR EXISTS (SELECT 1 FROM enterprise_asset WHERE storage_physical_key IS NOT NULL LIMIT 1)
       OR EXISTS (SELECT 1 FROM attachment_object_deletion WHERE storage_physical_key IS NOT NULL LIMIT 1)
       OR EXISTS (SELECT 1 FROM enterprise_asset_delete_queue WHERE storage_physical_key IS NOT NULL LIMIT 1) THEN
        RAISE EXCEPTION 'DV1_DOWN_BLOCKED_USE_FORWARD_FIX';
    END IF;
END
$$;

DROP FUNCTION IF EXISTS fn_dv1_intent_capture_physical(text);
DROP FUNCTION IF EXISTS fn_dv1_winner_pin(text, integer, text, text);
DROP FUNCTION IF EXISTS fn_dv1_attempt_transition(text, integer, text, text, text);
DROP FUNCTION IF EXISTS fn_dv1_attempt_create(text, text, text, text, text, text, text);

DROP TABLE IF EXISTS attach_direct_attempt;

ALTER TABLE enterprise_asset_delete_queue
    DROP CONSTRAINT IF EXISTS ck_dv1_ebdq_pkey_shape,
    DROP COLUMN IF EXISTS storage_physical_key;
ALTER TABLE attachment_object_deletion
    DROP CONSTRAINT IF EXISTS ck_dv1_aod_direct_identity,
    DROP CONSTRAINT IF EXISTS ck_dv1_aod_pkey_shape,
    DROP COLUMN IF EXISTS storage_physical_key;
ALTER TABLE enterprise_asset
    DROP CONSTRAINT IF EXISTS ck_dv1_ea_pkey_shape,
    DROP COLUMN IF EXISTS storage_physical_key;
ALTER TABLE attachment
    DROP CONSTRAINT IF EXISTS ck_dv1_attachment_pkey_shape,
    DROP COLUMN IF EXISTS storage_physical_key;

ALTER TABLE attach_pending
    DROP CONSTRAINT IF EXISTS ck_dv1_confirmed_winner,
    DROP CONSTRAINT IF EXISTS ck_dv1_direct_sync,
    DROP CONSTRAINT IF EXISTS ck_dv1_direct_protocol,
    DROP CONSTRAINT IF EXISTS ck_dv1_pending_direct_state,
    DROP COLUMN IF EXISTS direct_state;

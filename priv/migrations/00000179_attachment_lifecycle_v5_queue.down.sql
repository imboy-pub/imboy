-- Down for 00000178 (V5.5 M2-queue).
-- Empty-schema down drops functions/indexes/columns in reverse; any queue
-- activity (non-blocked jobs, leases, tombstones, EB recovery snapshots,
-- intent audit rows) blocks the down.
DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM attachment_object_deletion
               WHERE job_state IN ('ready', 'leased', 'retry_wait')
                  OR storage_driver IS NOT NULL
                  OR attempts > 0
                  OR lease_token IS NOT NULL)
       OR EXISTS (SELECT 1 FROM enterprise_asset_delete_queue
               WHERE job_state IN ('ready', 'leased', 'retry_wait')
                  OR storage_driver IS NOT NULL
                  OR client_upload_id IS NOT NULL
                  OR lease_token IS NOT NULL)
       OR EXISTS (SELECT 1 FROM attachment_lifecycle_v5_audit) THEN
        RAISE EXCEPTION 'V5_DOWN_BLOCKED_USE_FORWARD_FIX'
            USING HINT = 'queue activity or tombstones require forward fix';
    END IF;
END;
$$;

DROP FUNCTION IF EXISTS fn_v5_metadata_delete_guard();
DROP FUNCTION IF EXISTS fn_v5_queue_job_guard();
DROP FUNCTION IF EXISTS fn_v5_eb_audit_actor_guard();
DROP FUNCTION IF EXISTS fn_v5_eb_registration_guard();
DROP FUNCTION IF EXISTS fn_v5_pending_session_guard();
DROP FUNCTION IF EXISTS fn_v5_first_bind_guard();
DROP FUNCTION IF EXISTS fn_v5_attachment_identity_immutable();
DROP FUNCTION IF EXISTS fn_v5_attachment_location_immutable();
DROP FUNCTION IF EXISTS fn_v5_copy_eb_recovery_tombstone(bigint, bigint, bigint);
DROP FUNCTION IF EXISTS fn_v5_proof_activate_ordinary(text, text, text, text, text);
DROP FUNCTION IF EXISTS fn_v5_reaper_eb(integer);
DROP FUNCTION IF EXISTS fn_v5_ack_eb(bigint, bigint, bigint, uuid, boolean);
DROP FUNCTION IF EXISTS fn_v5_claim_eb(uuid);
DROP FUNCTION IF EXISTS fn_v5_reaper_ordinary(integer);
DROP FUNCTION IF EXISTS fn_v5_ack_ordinary(text, uuid, boolean);
DROP FUNCTION IF EXISTS fn_v5_claim_ordinary(uuid);
DROP FUNCTION IF EXISTS fn_v5_intent_eb_delete(bigint, bigint, bigint, text);
DROP FUNCTION IF EXISTS fn_v5_intent_ordinary_delete(text, text);
DROP FUNCTION IF EXISTS fn_v5_confirm_pending(text, bigint);
DROP FUNCTION IF EXISTS fn_v5_pin_pending_content(text, jsonb);
DROP FUNCTION IF EXISTS fn_v5_register_eb(bigint, bigint, bigint, text, uuid, jsonb, text,
    timestamptz, timestamptz);
DROP FUNCTION IF EXISTS fn_v5_register_pending(text, text, text, bigint, uuid, jsonb,
    timestamptz, timestamptz, text, text, text, text);
DROP FUNCTION IF EXISTS fn_v5_lock_identity(text, text, text, text);
DROP TABLE IF EXISTS attachment_lifecycle_v5_audit;

DROP INDEX IF EXISTS i_v5_ebdq_leased_expiry;
DROP INDEX IF EXISTS i_v5_ebdq_ready;
DROP INDEX IF EXISTS i_v5_aod_leased_expiry;
DROP INDEX IF EXISTS i_v5_aod_ready;
DROP INDEX IF EXISTS uq_v5_eb_deleted_client_upload;

ALTER TABLE enterprise_asset_delete_queue
    DROP CONSTRAINT IF EXISTS ck_v5_ebdq_done_ts;
ALTER TABLE attachment_object_deletion
    DROP CONSTRAINT IF EXISTS ck_v5_aod_done_ts;
ALTER TABLE enterprise_asset_delete_queue
    DROP CONSTRAINT IF EXISTS ck_v5_ebdq_identity;
ALTER TABLE attachment_object_deletion
    DROP CONSTRAINT IF EXISTS ck_v5_aod_identity;
ALTER TABLE enterprise_asset_delete_queue
    DROP CONSTRAINT IF EXISTS ck_v5_ebdq_lease;
ALTER TABLE attachment_object_deletion
    DROP CONSTRAINT IF EXISTS ck_v5_aod_lease;
ALTER TABLE enterprise_asset_delete_queue
    DROP CONSTRAINT IF EXISTS ck_v5_ebdq_attempts;
ALTER TABLE attachment_object_deletion
    DROP CONSTRAINT IF EXISTS ck_v5_aod_state;
ALTER TABLE enterprise_asset_delete_queue
    DROP CONSTRAINT IF EXISTS ck_v5_ebdq_state;
ALTER TABLE enterprise_asset_delete_queue
    DROP CONSTRAINT IF EXISTS ck_v5_eb_recovery_snapshot;

ALTER TABLE enterprise_asset_delete_queue
    DROP COLUMN IF EXISTS completed_at,
    DROP COLUMN IF EXISTS last_error_code,
    DROP COLUMN IF EXISTS lease_until,
    DROP COLUMN IF EXISTS lease_token,
    DROP COLUMN IF EXISTS next_retry_at,
    DROP COLUMN IF EXISTS attempts,
    DROP COLUMN IF EXISTS job_state,
    DROP COLUMN IF EXISTS storage_locator_sha256,
    DROP COLUMN IF EXISTS storage_container,
    DROP COLUMN IF EXISTS storage_namespace,
    DROP COLUMN IF EXISTS storage_driver,
    DROP COLUMN IF EXISTS upload_registration,
    DROP COLUMN IF EXISTS client_upload_id;

ALTER TABLE attachment_object_deletion
    DROP COLUMN IF EXISTS last_error_code,
    DROP COLUMN IF EXISTS lease_until,
    DROP COLUMN IF EXISTS lease_token,
    DROP COLUMN IF EXISTS job_state,
    DROP COLUMN IF EXISTS storage_locator_sha256,
    DROP COLUMN IF EXISTS storage_container,
    DROP COLUMN IF EXISTS storage_namespace,
    DROP COLUMN IF EXISTS storage_driver;

-- Down for 00000177 (V5.5 M1-expand).
-- Empty-schema down removes the added constraints/columns/indexes in reverse.
-- Any persisted V5 identity/session state blocks the down (forward-fix only).
DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM attach_pending
               WHERE client_upload_id IS NOT NULL OR upload_registration IS NOT NULL
                  OR upload_content IS NOT NULL OR session_deadline IS NOT NULL
                  OR upload_expires_at IS NOT NULL OR upload_drained_at IS NOT NULL
                  OR confirmed_attachment_id IS NOT NULL OR gc_verified)
       OR EXISTS (SELECT 1 FROM enterprise_asset
               WHERE client_upload_id IS NOT NULL OR upload_registration IS NOT NULL
                  OR upload_content IS NOT NULL OR session_deadline IS NOT NULL
                  OR upload_expires_at IS NOT NULL OR upload_drained_at IS NOT NULL
                  OR gc_verified OR storage_driver IS NOT NULL)
       OR EXISTS (SELECT 1 FROM attachment
               WHERE storage_driver IS NOT NULL OR gc_verified
                  OR lifecycle_state <> 'live') THEN
        RAISE EXCEPTION 'V5_DOWN_BLOCKED_USE_FORWARD_FIX'
            USING HINT = 'persisted V5 identity/session state requires forward fix';
    END IF;
END;
$$;

DROP INDEX IF EXISTS uq_v5_pending_client_upload;
DROP INDEX IF EXISTS uq_v5_eb_client_upload;

ALTER TABLE attach_pending
    DROP CONSTRAINT IF EXISTS ck_v5_pending_times;
ALTER TABLE enterprise_asset
    DROP CONSTRAINT IF EXISTS ck_v5_eb_times;
ALTER TABLE attach_pending
    DROP CONSTRAINT IF EXISTS ck_v5_pending_registration;
ALTER TABLE enterprise_asset
    DROP CONSTRAINT IF EXISTS ck_v5_eb_registration;
ALTER TABLE attach_pending
    DROP CONSTRAINT IF EXISTS ck_v5_pending_content;
ALTER TABLE enterprise_asset
    DROP CONSTRAINT IF EXISTS ck_v5_eb_content;
ALTER TABLE attachment
    DROP CONSTRAINT IF EXISTS ck_v5_attachment_locator;
ALTER TABLE attach_pending
    DROP CONSTRAINT IF EXISTS ck_v5_pending_locator;
ALTER TABLE enterprise_asset
    DROP CONSTRAINT IF EXISTS ck_v5_eb_locator;
ALTER TABLE attachment
    DROP CONSTRAINT IF EXISTS ck_v5_attachment_gc;
ALTER TABLE attach_pending
    DROP CONSTRAINT IF EXISTS ck_v5_pending_gc;
ALTER TABLE enterprise_asset
    DROP CONSTRAINT IF EXISTS ck_v5_eb_gc;
ALTER TABLE attachment
    DROP CONSTRAINT IF EXISTS ck_v5_attachment_location;
ALTER TABLE attach_pending
    DROP CONSTRAINT IF EXISTS ck_v5_pending_location;
ALTER TABLE enterprise_asset
    DROP CONSTRAINT IF EXISTS ck_v5_eb_location;
ALTER TABLE attach_pending
    DROP CONSTRAINT IF EXISTS ck_v5_session_link;
ALTER TABLE attach_pending
    DROP CONSTRAINT IF EXISTS fk_v5_pending_confirmed;
ALTER TABLE attach_pending
    DROP CONSTRAINT IF EXISTS ck_v5_pending_state;
ALTER TABLE enterprise_asset
    DROP CONSTRAINT IF EXISTS ck_v5_eb_lifecycle;
ALTER TABLE attachment
    DROP CONSTRAINT IF EXISTS ck_v5_attachment_lifecycle;

ALTER TABLE attach_pending
    DROP COLUMN IF EXISTS gc_verified,
    DROP COLUMN IF EXISTS confirmed_attachment_id,
    DROP COLUMN IF EXISTS upload_drained_at,
    DROP COLUMN IF EXISTS upload_expires_at,
    DROP COLUMN IF EXISTS session_state,
    DROP COLUMN IF EXISTS scope_ref,
    DROP COLUMN IF EXISTS session_deadline,
    DROP COLUMN IF EXISTS upload_content,
    DROP COLUMN IF EXISTS upload_registration,
    DROP COLUMN IF EXISTS client_upload_id,
    DROP COLUMN IF EXISTS storage_locator_sha256,
    DROP COLUMN IF EXISTS storage_container,
    DROP COLUMN IF EXISTS storage_namespace,
    DROP COLUMN IF EXISTS storage_driver;

ALTER TABLE enterprise_asset
    DROP COLUMN IF EXISTS gc_verified,
    DROP COLUMN IF EXISTS lifecycle_state,
    DROP COLUMN IF EXISTS storage_locator_sha256,
    DROP COLUMN IF EXISTS storage_container,
    DROP COLUMN IF EXISTS storage_namespace,
    DROP COLUMN IF EXISTS storage_driver,
    DROP COLUMN IF EXISTS upload_drained_at,
    DROP COLUMN IF EXISTS upload_expires_at,
    DROP COLUMN IF EXISTS session_deadline,
    DROP COLUMN IF EXISTS upload_content,
    DROP COLUMN IF EXISTS upload_registration,
    DROP COLUMN IF EXISTS client_upload_id;

ALTER TABLE attachment
    DROP COLUMN IF EXISTS gc_verified,
    DROP COLUMN IF EXISTS lifecycle_state,
    DROP COLUMN IF EXISTS storage_locator_sha256,
    DROP COLUMN IF EXISTS storage_container,
    DROP COLUMN IF EXISTS storage_namespace,
    DROP COLUMN IF EXISTS storage_driver;

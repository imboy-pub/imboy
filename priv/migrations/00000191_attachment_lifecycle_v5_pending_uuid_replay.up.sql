-- Forward fix: UUID retries retain the original key and immutable declaration.
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

CREATE OR REPLACE FUNCTION fn_v5_register_pending(
    p_object_key text, p_bucket text, p_scope text, p_creator bigint,
    p_client_upload_id uuid, p_registration jsonb,
    p_deadline timestamptz, p_expiry timestamptz,
    p_driver text, p_namespace text, p_container text, p_locator text)
RETURNS TABLE(out_object_key text, out_session_state text, out_client_upload_id uuid)
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_existing attach_pending%ROWTYPE;
BEGIN
    IF p_client_upload_id IS NULL THEN
        RAISE EXCEPTION 'v5 register requires client_upload_id' USING ERRCODE = '23514';
    END IF;
    IF p_registration IS NULL OR jsonb_typeof(p_registration) <> 'object'
       OR NOT (p_registration ? 'subject') OR NOT (p_registration ? 'declaration') THEN
        RAISE EXCEPTION 'v5 register requires subject+declaration snapshot'
            USING ERRCODE = '23514';
    END IF;
    IF p_deadline IS NULL OR p_expiry IS NULL OR p_expiry > p_deadline THEN
        RAISE EXCEPTION 'v5 register requires deadline and expiry<=deadline'
            USING ERRCODE = '23514';
    END IF;
    IF (p_driver IS NULL) <> (p_namespace IS NULL) OR (p_driver IS NULL) <> (p_container IS NULL)
       OR (p_driver IS NOT NULL AND (p_namespace = '' OR p_container = '')) THEN
        RAISE EXCEPTION 'v5 register location must be all-NULL or complete'
            USING ERRCODE = '23514';
    END IF;
    -- UUID lock precedes row and physical-identity locks. Replays never adopt
    -- the new proposed key, location or capability window.
    PERFORM pg_advisory_xact_lock(hashtextextended(
        'v5-pending-upload:' || p_client_upload_id::text, 169));
    SELECT * INTO v_existing FROM attach_pending
        WHERE client_upload_id = p_client_upload_id FOR UPDATE;
    IF FOUND THEN
        IF v_existing.upload_registration IS DISTINCT FROM p_registration
           OR v_existing.creator_user_id IS DISTINCT FROM p_creator
           OR v_existing.scope IS DISTINCT FROM p_scope
           OR v_existing.bucket IS DISTINCT FROM p_bucket THEN
            RAISE EXCEPTION 'v5 registration snapshot mismatch on retry'
                USING ERRCODE = '23510';
        END IF;
        RETURN QUERY SELECT v_existing.object_key, v_existing.session_state,
                             v_existing.client_upload_id;
        RETURN;
    END IF;
    -- A different UUID may not take an existing key, even after expiry/delete.
    IF EXISTS (SELECT 1 FROM attach_pending WHERE object_key = p_object_key) THEN
        RAISE EXCEPTION 'v5 key reassignment forbidden' USING ERRCODE = '23510';
    END IF;
    PERFORM fn_v5_lock_identity(p_driver, p_namespace, p_container, p_object_key);
    IF EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path = p_object_key) THEN
        RAISE EXCEPTION 'v5 key tombstoned' USING ERRCODE = '23505';
    END IF;
    -- UUID dedup against ordinary + enterprise queue tombstones.
    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue
               WHERE client_upload_id = p_client_upload_id) THEN
        RAISE EXCEPTION 'v5 client_upload_id tombstoned in enterprise queue'
            USING ERRCODE = '23505';
    END IF;
    INSERT INTO attach_pending(object_key, bucket, scope, creator_user_id,
                               client_upload_id, upload_registration,
                               session_deadline, upload_expires_at,
                               storage_driver, storage_namespace, storage_container,
                               storage_locator_sha256)
    VALUES (p_object_key, p_bucket, p_scope, p_creator,
            p_client_upload_id, p_registration,
            p_deadline, p_expiry,
            p_driver, p_namespace, p_container, p_locator)
;
    RETURN QUERY SELECT p_object_key, 'registered'::text, p_client_upload_id;
END;
$$;

-- V5.5 attachment lifecycle M2-queue: extend both existing queues, install all
-- restricted protocol/guard FUNCTION DEFINITIONS (M3 attaches them later).
-- Contract: docs/architecture/attachment-lifecycle-v5/03-migration-contract.md
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

-- ---------------------------------------------------------------- queues
ALTER TABLE attachment_object_deletion
    ADD COLUMN storage_driver text,
    ADD COLUMN storage_namespace text,
    ADD COLUMN storage_container text,
    ADD COLUMN storage_locator_sha256 text,
    ADD COLUMN job_state text NOT NULL DEFAULT 'blocked',
    ADD COLUMN lease_token uuid,
    ADD COLUMN lease_until timestamptz,
    ADD COLUMN last_error_code text DEFAULT 'legacy_location_unverified';

ALTER TABLE enterprise_asset_delete_queue
    ADD COLUMN client_upload_id uuid,
    ADD COLUMN upload_registration jsonb,
    ADD COLUMN storage_driver text,
    ADD COLUMN storage_namespace text,
    ADD COLUMN storage_container text,
    ADD COLUMN storage_locator_sha256 text,
    ADD COLUMN job_state text NOT NULL DEFAULT 'blocked',
    ADD COLUMN attempts integer NOT NULL DEFAULT 0,
    ADD COLUMN next_retry_at timestamptz NOT NULL DEFAULT now(),
    ADD COLUMN lease_token uuid,
    ADD COLUMN lease_until timestamptz,
    ADD COLUMN last_error_code text DEFAULT 'legacy_location_unverified',
    ADD COLUMN completed_at timestamptz;

-- Existing ordinary rows: completed_at non-NULL maps done; others blocked with
-- legacy_location_unverified; attempts never reset.
UPDATE attachment_object_deletion
    SET job_state = CASE WHEN completed_at IS NOT NULL THEN 'done' ELSE 'blocked' END,
        last_error_code = CASE WHEN completed_at IS NOT NULL THEN NULL
                               ELSE 'legacy_location_unverified' END;

ALTER TABLE attachment_object_deletion
    ADD CONSTRAINT ck_v5_aod_state CHECK (job_state IN
        ('ready', 'leased', 'retry_wait', 'blocked', 'done'));
ALTER TABLE enterprise_asset_delete_queue
    ADD CONSTRAINT ck_v5_ebdq_state CHECK (job_state IN
        ('ready', 'leased', 'retry_wait', 'blocked', 'done'));
ALTER TABLE enterprise_asset_delete_queue
    ADD CONSTRAINT ck_v5_ebdq_attempts CHECK (attempts >= 0);

ALTER TABLE attachment_object_deletion
    ADD CONSTRAINT ck_v5_aod_lease CHECK (
        (job_state = 'leased' AND lease_token IS NOT NULL AND lease_until IS NOT NULL)
        OR (job_state <> 'leased' AND lease_token IS NULL AND lease_until IS NULL));
ALTER TABLE enterprise_asset_delete_queue
    ADD CONSTRAINT ck_v5_ebdq_lease CHECK (
        (job_state = 'leased' AND lease_token IS NOT NULL AND lease_until IS NOT NULL)
        OR (job_state <> 'leased' AND lease_token IS NULL AND lease_until IS NULL));

-- Executable states require complete proven identity; done keeps tombstone.
ALTER TABLE attachment_object_deletion
    ADD CONSTRAINT ck_v5_aod_identity CHECK (
        job_state NOT IN ('ready', 'leased', 'retry_wait')
        OR (storage_driver IS NOT NULL AND storage_driver IN ('garage', 'disk')
            AND storage_namespace IS NOT NULL AND storage_namespace <> ''
            AND storage_container IS NOT NULL AND storage_container <> ''
            AND storage_locator_sha256 ~ '^[0-9a-f]{64}$'));
ALTER TABLE enterprise_asset_delete_queue
    ADD CONSTRAINT ck_v5_ebdq_identity CHECK (
        job_state NOT IN ('ready', 'leased', 'retry_wait')
        OR (storage_driver IS NOT NULL AND storage_driver IN ('garage', 'disk')
            AND storage_namespace IS NOT NULL AND storage_namespace <> ''
            AND storage_container IS NOT NULL AND storage_container <> ''
            AND storage_locator_sha256 ~ '^[0-9a-f]{64}$'));
ALTER TABLE attachment_object_deletion
    ADD CONSTRAINT ck_v5_aod_done_ts CHECK (
        job_state <> 'done' OR completed_at IS NOT NULL);
ALTER TABLE enterprise_asset_delete_queue
    ADD CONSTRAINT ck_v5_ebdq_done_ts CHECK (
        job_state <> 'done' OR completed_at IS NOT NULL);

-- EB recovery tombstone snapshot pairing + joint uniqueness.
ALTER TABLE enterprise_asset_delete_queue
    ADD CONSTRAINT ck_v5_eb_recovery_snapshot CHECK (
        (client_upload_id IS NULL AND upload_registration IS NULL)
        OR (client_upload_id IS NOT NULL AND upload_registration IS NOT NULL
            AND jsonb_typeof(upload_registration) = 'object'));
CREATE UNIQUE INDEX uq_v5_eb_deleted_client_upload
    ON enterprise_asset_delete_queue(client_upload_id)
    WHERE client_upload_id IS NOT NULL;

CREATE INDEX i_v5_aod_ready
    ON attachment_object_deletion(next_retry_at, path)
    WHERE job_state IN ('ready', 'retry_wait');
CREATE INDEX i_v5_aod_leased_expiry
    ON attachment_object_deletion(lease_until, path)
    WHERE job_state = 'leased';
CREATE INDEX i_v5_ebdq_ready
    ON enterprise_asset_delete_queue(next_retry_at, organization_id, workspace_id, asset_id)
    WHERE job_state IN ('ready', 'retry_wait');
CREATE INDEX i_v5_ebdq_leased_expiry
    ON enterprise_asset_delete_queue(lease_until, organization_id, workspace_id, asset_id)
    WHERE job_state = 'leased';

-- Intent audit trail (written by restricted functions only).
CREATE TABLE attachment_lifecycle_v5_audit (
    id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    acted_at timestamptz NOT NULL DEFAULT now(),
    queue_domain text NOT NULL CHECK (queue_domain IN ('ordinary', 'enterprise')),
    object_key text NOT NULL,
    storage_namespace text,
    action text NOT NULL,
    reason text,
    actor text NOT NULL,
    details jsonb
);
REVOKE ALL ON attachment_lifecycle_v5_audit FROM PUBLIC;

-- ---------------------------------------------------------------- roles
DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'v5_fn_owner') THEN
        CREATE ROLE v5_fn_owner NOLOGIN NOINHERIT;
    END IF;
    IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'v5_worker') THEN
        CREATE ROLE v5_worker NOLOGIN NOINHERIT;
    END IF;
    IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'v5_proof') THEN
        CREATE ROLE v5_proof NOLOGIN NOINHERIT;
    END IF;
    IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'v5_app_runtime') THEN
        CREATE ROLE v5_app_runtime NOLOGIN NOINHERIT;
    END IF;
END;
$$;

-- Restricted functions run as v5_fn_owner (SECURITY DEFINER): grant the
-- minimum table rights the protocol needs; business roles get none of these.
GRANT SELECT, INSERT, UPDATE ON attach_pending TO v5_fn_owner;
GRANT SELECT, INSERT, UPDATE ON attachment TO v5_fn_owner;
GRANT SELECT, INSERT, UPDATE ON enterprise_asset TO v5_fn_owner;
GRANT SELECT, INSERT, UPDATE ON attachment_object_deletion TO v5_fn_owner;
GRANT SELECT, INSERT, UPDATE ON enterprise_asset_delete_queue TO v5_fn_owner;
GRANT SELECT, INSERT ON attachment_lifecycle_v5_audit TO v5_fn_owner;
GRANT SELECT ON "user" TO v5_fn_owner;
GRANT SELECT ON workspace TO v5_fn_owner;
GRANT SELECT ON group_file TO v5_fn_owner;
GRANT SELECT ON workspace_attachment_retention TO v5_fn_owner;
GRANT SELECT ON enterprise_attachment_retention TO v5_fn_owner;
GRANT SELECT ON enterprise_retention_hold TO v5_fn_owner;
GRANT USAGE ON SCHEMA public TO v5_fn_owner, v5_worker, v5_proof, v5_app_runtime;

-- ---------------------------------------------------------------- shared lock helper
CREATE FUNCTION fn_v5_lock_identity(p_driver text, p_root text, p_container text,
                                    p_key text) RETURNS void
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
BEGIN
    PERFORM pg_advisory_xact_lock(hashtextextended(
        jsonb_build_array(p_driver, p_root, p_container, p_key)::text, 1685));
END;
$$;
ALTER FUNCTION fn_v5_lock_identity(text, text, text, text) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_lock_identity(text, text, text, text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_lock_identity(text, text, text, text)
    TO v5_app_runtime, v5_worker;

-- ---------------------------------------------------------------- register/pin/confirm
-- Ordinary session register: UUID dedup, immutable snapshot, frozen identity.
CREATE FUNCTION fn_v5_register_pending(
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
    -- Cross-domain identity lock (namespace is the root id here).
    PERFORM fn_v5_lock_identity(p_driver, p_namespace, p_container, p_object_key);
    -- Same key never reassigned.
    SELECT * INTO v_existing FROM attach_pending WHERE object_key = p_object_key;
    IF FOUND THEN
        IF v_existing.client_upload_id IS DISTINCT FROM p_client_upload_id THEN
            RAISE EXCEPTION 'v5 key reassignment forbidden' USING ERRCODE = '23510';
        END IF;
        IF v_existing.upload_registration IS DISTINCT FROM p_registration THEN
            RAISE EXCEPTION 'v5 registration snapshot mismatch on retry'
                USING ERRCODE = '23510';
        END IF;
        RETURN QUERY SELECT v_existing.object_key, v_existing.session_state,
                             v_existing.client_upload_id;
        RETURN;
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
    ON CONFLICT (object_key) DO NOTHING;
    RETURN QUERY SELECT p_object_key, 'registered'::text, p_client_upload_id;
END;
$$;
ALTER FUNCTION fn_v5_register_pending(text, text, text, bigint, uuid, jsonb,
    timestamptz, timestamptz, text, text, text, text) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_register_pending(text, text, text, bigint, uuid, jsonb,
    timestamptz, timestamptz, text, text, text, text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_register_pending(text, text, text, bigint, uuid, jsonb,
    timestamptz, timestamptz, text, text, text, text) TO v5_app_runtime;

-- Enterprise session register: UUID dedup across live + queue tombstones,
-- immutable snapshot, subject kind must match provided identity lock domain.
CREATE FUNCTION fn_v5_register_eb(
    p_org bigint, p_ws bigint, p_asset bigint, p_object_key text,
    p_client_upload_id uuid, p_registration jsonb,
    p_subject_kind text,
    p_deadline timestamptz, p_expiry timestamptz)
RETURNS TABLE(out_session_state text, out_client_upload_id uuid)
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a enterprise_asset%ROWTYPE;
BEGIN
    IF p_client_upload_id IS NULL OR p_registration IS NULL
       OR jsonb_typeof(p_registration) <> 'object'
       OR NOT (p_registration ? 'subject') OR NOT (p_registration ? 'declaration')
       OR p_subject_kind NOT IN ('user', 'seat', 'visitor_session', 'application') THEN
        RAISE EXCEPTION 'v5 eb register requires uuid+snapshot+subject kind'
            USING ERRCODE = '23514';
    END IF;
    IF (p_registration->'subject'->>'kind') IS DISTINCT FROM p_subject_kind THEN
        RAISE EXCEPTION 'v5 eb registration subject kind conflict'
            USING ERRCODE = '23510';
    END IF;
    IF p_deadline IS NULL OR p_expiry IS NULL OR p_expiry > p_deadline THEN
        RAISE EXCEPTION 'v5 eb register requires deadline and expiry<=deadline'
            USING ERRCODE = '23514';
    END IF;
    PERFORM pg_advisory_xact_lock(hashtextextended(
        'v5-eb-upload:' || p_client_upload_id::text, 169));
    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue
               WHERE client_upload_id = p_client_upload_id) THEN
        RAISE EXCEPTION 'v5 eb client_upload_id tombstoned'
            USING ERRCODE = '23505';
    END IF;
    SELECT * INTO v_a FROM enterprise_asset
        WHERE organization_id = p_org AND workspace_id = p_ws AND id = p_asset
        FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 eb register requires pre-created pending asset row'
            USING ERRCODE = 'P0002';
    END IF;
    IF v_a.client_upload_id IS NOT NULL
       AND v_a.client_upload_id IS DISTINCT FROM p_client_upload_id THEN
        RAISE EXCEPTION 'v5 eb identity reassignment forbidden'
            USING ERRCODE = '23510';
    END IF;
    IF v_a.upload_registration IS NOT NULL
       AND v_a.upload_registration IS DISTINCT FROM p_registration THEN
        RAISE EXCEPTION 'v5 eb registration snapshot mismatch'
            USING ERRCODE = '23510';
    END IF;
    UPDATE enterprise_asset
        SET client_upload_id = p_client_upload_id,
            upload_registration = p_registration,
            session_deadline = p_deadline,
            upload_expires_at = p_expiry
        WHERE organization_id = p_org AND workspace_id = p_ws AND id = p_asset
          AND client_upload_id IS NULL AND upload_registration IS NULL;
    RETURN QUERY SELECT 'registered'::text, p_client_upload_id;
END;
$$;
ALTER FUNCTION fn_v5_register_eb(bigint, bigint, bigint, text, uuid, jsonb, text,
    timestamptz, timestamptz) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_register_eb(bigint, bigint, bigint, text, uuid, jsonb,
    text, timestamptz, timestamptz) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_register_eb(bigint, bigint, bigint, text, uuid,
    jsonb, text, timestamptz, timestamptz) TO v5_app_runtime;

-- Content pin: NULL -> receipt exactly once; same receipt idempotent.
CREATE FUNCTION fn_v5_pin_pending_content(p_object_key text, p_content jsonb)
RETURNS boolean LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_row attach_pending%ROWTYPE;
BEGIN
    SELECT * INTO v_row FROM attach_pending WHERE object_key = p_object_key
        FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 pin unknown session' USING ERRCODE = 'P0002';
    END IF;
    IF v_row.upload_content IS NULL THEN
        UPDATE attach_pending SET upload_content = p_content
            WHERE object_key = p_object_key;
        RETURN TRUE;
    END IF;
    IF v_row.upload_content = p_content THEN
        RETURN TRUE;  -- idempotent replay of identical receipt
    END IF;
    RAISE EXCEPTION 'v5 content already pinned to different receipt'
        USING ERRCODE = '23510';
END;
$$;
ALTER FUNCTION fn_v5_pin_pending_content(text, jsonb) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_pin_pending_content(text, jsonb) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_pin_pending_content(text, jsonb) TO v5_app_runtime;

-- Confirm: registered -> confirmed linking a pre-created matching asset row.
CREATE FUNCTION fn_v5_confirm_pending(p_object_key text, p_attachment_id bigint)
RETURNS integer LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_p attach_pending%ROWTYPE; v_a attachment%ROWTYPE; v_count integer;
BEGIN
    SELECT * INTO v_p FROM attach_pending WHERE object_key = p_object_key FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 confirm unknown session' USING ERRCODE = 'P0002';
    END IF;
    SELECT * INTO v_a FROM attachment WHERE id = p_attachment_id FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 confirm unknown asset' USING ERRCODE = 'P0002';
    END IF;
    IF v_p.session_state = 'confirmed' THEN
        IF v_p.confirmed_attachment_id = p_attachment_id THEN
            RETURN v_p.confirmed_attachment_id;  -- legal replay returns original
        END IF;
        RAISE EXCEPTION 'v5 confirmed link immutable' USING ERRCODE = '23510';
    END IF;
    IF v_p.session_state <> 'registered' THEN
        RAISE EXCEPTION 'v5 confirm illegal from %', v_p.session_state
            USING ERRCODE = '23510';
    END IF;
    -- Identity/owner/scope match: pending path vs asset path, creator, scope.
    IF v_a.path IS DISTINCT FROM v_p.object_key THEN
        RAISE EXCEPTION 'v5 confirm key mismatch' USING ERRCODE = '23510';
    END IF;
    IF v_a.creator_user_id IS DISTINCT FROM v_p.creator_user_id THEN
        RAISE EXCEPTION 'v5 confirm owner mismatch' USING ERRCODE = '23510';
    END IF;
    IF v_a.scope IS DISTINCT FROM v_p.scope THEN
        RAISE EXCEPTION 'v5 confirm scope mismatch' USING ERRCODE = '23510';
    END IF;
    IF (v_a.storage_driver, v_a.storage_namespace, v_a.storage_container,
        v_a.storage_locator_sha256)
       IS DISTINCT FROM
       (v_p.storage_driver, v_p.storage_namespace, v_p.storage_container,
        v_p.storage_locator_sha256) THEN
        RAISE EXCEPTION 'v5 confirm location mismatch' USING ERRCODE = '23510';
    END IF;
    IF v_p.upload_content IS NULL THEN
        RAISE EXCEPTION 'v5 confirm requires pinned content receipt'
            USING ERRCODE = '23510';
    END IF;
    UPDATE attach_pending SET session_state = 'confirmed',
            confirmed_attachment_id = p_attachment_id
        WHERE object_key = p_object_key AND session_state = 'registered';
    GET DIAGNOSTICS v_count = ROW_COUNT;
    IF v_count <> 1 THEN
        RAISE EXCEPTION 'v5 confirm lost race' USING ERRCODE = '40001';
    END IF;
    RETURN p_attachment_id;
END;
$$;
ALTER FUNCTION fn_v5_confirm_pending(text, bigint) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_confirm_pending(text, bigint) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_confirm_pending(text, bigint) TO v5_app_runtime;

-- ---------------------------------------------------------------- intent (ordinary)
CREATE FUNCTION fn_v5_intent_ordinary_delete(p_path text, p_reason text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a attachment%ROWTYPE; v_p attach_pending%ROWTYPE; v_bucket text;
BEGIN
    SELECT * INTO v_a FROM attachment WHERE path = p_path FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 intent unknown attachment' USING ERRCODE = 'P0002';
    END IF;
    -- Eligibility conjunction, re-checked under lock; unknown => not eligible.
    IF v_a.lifecycle_state <> 'live' THEN
        RAISE EXCEPTION 'v5 intent non-live asset' USING ERRCODE = '23510';
    END IF;
    IF NOT v_a.gc_verified THEN
        RAISE EXCEPTION 'v5 intent requires proven reference-zero (gc_verified)'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM workspace_attachment_retention
               WHERE attachment_id = v_a.id) THEN
        RAISE EXCEPTION 'v5 intent blocked by workspace retention'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM enterprise_attachment_retention
               WHERE attachment_id = v_a.id
                 AND (hold_state = 'held'
                      OR purge_state IS DISTINCT FROM 'purged')) THEN
        RAISE EXCEPTION 'v5 intent blocked by EAR' USING ERRCODE = '23510';
    END IF;
    IF v_a.group_file_id IS NOT NULL
       AND EXISTS (SELECT 1 FROM group_file WHERE id = v_a.group_file_id) THEN
        RAISE EXCEPTION 'v5 intent blocked by live group_file binding'
            USING ERRCODE = '23510';
    END IF;
    SELECT * INTO v_p FROM attach_pending WHERE object_key = p_path;
    IF FOUND AND (v_p.upload_expires_at IS NULL OR v_p.upload_expires_at > now()) THEN
        RAISE EXCEPTION 'v5 intent upload capability not exhausted'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path = p_path) THEN
        RAISE EXCEPTION 'v5 intent job already exists (tombstone)'
            USING ERRCODE = '23505';
    END IF;
    -- Object lock ordering: identity lock, then queue path lock (seed 168).
    PERFORM fn_v5_lock_identity(v_a.storage_driver, v_a.storage_namespace,
                                v_a.storage_container, v_a.path);
    PERFORM pg_advisory_xact_lock(hashtextextended(v_a.path, 168));
    -- Cross-domain same-identity rejection.
    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue q
               WHERE q.storage_namespace = v_a.storage_namespace
                 AND q.storage_container = v_a.storage_container
                 AND q.object_key = v_a.path) THEN
        RAISE EXCEPTION 'v5 intent cross-domain identity conflict'
            USING ERRCODE = '23510';
    END IF;
    UPDATE attachment SET lifecycle_state = 'delete_intent'
        WHERE id = v_a.id AND lifecycle_state = 'live';
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 intent lost eligibility race' USING ERRCODE = '40001';
    END IF;
    INSERT INTO attachment_object_deletion
        (path, bucket, storage_driver, storage_namespace, storage_container,
         storage_locator_sha256, job_state, next_retry_at, last_error_code)
    VALUES (v_a.path, v_a.url, v_a.storage_driver, v_a.storage_namespace,
            v_a.storage_container, v_a.storage_locator_sha256,
            'ready', now(), NULL);
    INSERT INTO attachment_lifecycle_v5_audit
        (queue_domain, object_key, storage_namespace, action, reason, actor, details)
    VALUES ('ordinary', v_a.path, v_a.storage_namespace, 'delete_intent',
            p_reason, current_user,
            jsonb_build_object('attachment_id', v_a.id,
                               'locator', v_a.storage_locator_sha256));
END;
$$;
ALTER FUNCTION fn_v5_intent_ordinary_delete(text, text) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_intent_ordinary_delete(text, text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_intent_ordinary_delete(text, text) TO v5_app_runtime;

-- ---------------------------------------------------------------- intent (enterprise)
CREATE FUNCTION fn_v5_intent_eb_delete(p_org bigint, p_ws bigint, p_asset bigint,
                                       p_reason text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a enterprise_asset%ROWTYPE;
BEGIN
    SELECT * INTO v_a FROM enterprise_asset
        WHERE organization_id = p_org AND workspace_id = p_ws AND id = p_asset
        FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 intent unknown enterprise asset' USING ERRCODE = 'P0002';
    END IF;
    IF v_a.lifecycle_state <> 'live' THEN
        RAISE EXCEPTION 'v5 intent non-live enterprise asset' USING ERRCODE = '23510';
    END IF;
    IF v_a.retain_until IS NOT NULL AND v_a.retain_until > now() THEN
        RAISE EXCEPTION 'v5 intent enterprise retention active' USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM enterprise_retention_hold h
               WHERE h.organization_id = p_org AND h.workspace_id = p_ws
                 AND h.released_at IS NULL) THEN
        RAISE EXCEPTION 'v5 intent blocked by active workspace hold'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue
               WHERE organization_id = p_org AND workspace_id = p_ws
                 AND asset_id = p_asset) THEN
        RAISE EXCEPTION 'v5 intent enterprise job already exists'
            USING ERRCODE = '23505';
    END IF;
    PERFORM pg_advisory_xact_lock(hashtextextended(
        'v5-eb-upload:' || coalesce(v_a.client_upload_id::text, p_asset::text), 169));
    UPDATE enterprise_asset SET lifecycle_state = 'delete_intent'
        WHERE id = p_asset AND lifecycle_state = 'live';
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 intent lost eligibility race' USING ERRCODE = '40001';
    END IF;
    INSERT INTO enterprise_asset_delete_queue
        (organization_id, workspace_id, asset_id, object_key,
         client_upload_id, upload_registration,
         storage_driver, storage_namespace, storage_container,
         storage_locator_sha256, job_state, next_retry_at, last_error_code)
    VALUES (p_org, p_ws, p_asset, v_a.object_key,
            v_a.client_upload_id, v_a.upload_registration,
            v_a.storage_driver, v_a.storage_namespace, v_a.storage_container,
            v_a.storage_locator_sha256, 'ready', now(), NULL);
    INSERT INTO attachment_lifecycle_v5_audit
        (queue_domain, object_key, storage_namespace, action, reason, actor, details)
    VALUES ('enterprise', v_a.object_key, v_a.storage_namespace, 'delete_intent',
            p_reason, current_user,
            jsonb_build_object('org', p_org, 'ws', p_ws, 'asset', p_asset));
END;
$$;
ALTER FUNCTION fn_v5_intent_eb_delete(bigint, bigint, bigint, text)
    OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_intent_eb_delete(bigint, bigint, bigint, text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_intent_eb_delete(bigint, bigint, bigint, text)
    TO v5_app_runtime;

-- ---------------------------------------------------------------- claim / ack / reaper
CREATE FUNCTION fn_v5_claim_ordinary(p_token uuid)
RETURNS TABLE(out_path text, out_bucket text, out_storage_driver text,
              out_storage_namespace text, out_storage_container text,
              out_storage_locator_sha256 text, out_attempts integer)
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
BEGIN
    WITH candidate AS (
        SELECT q.path FROM attachment_object_deletion q
        WHERE attempts < 8
          AND job_state IN ('ready', 'retry_wait') AND next_retry_at <= clock_timestamp()
        ORDER BY next_retry_at, path
        FOR UPDATE SKIP LOCKED LIMIT 1)
    UPDATE attachment_object_deletion q
        SET job_state = 'leased', lease_token = p_token,
            lease_until = clock_timestamp() + interval '120 seconds',
            attempts = q.attempts + 1, last_error_code = NULL
        FROM candidate c WHERE q.path = c.path
        RETURNING q.path, q.bucket, q.storage_driver, q.storage_namespace,
                  q.storage_container, q.storage_locator_sha256, q.attempts;
END;
$$;
ALTER FUNCTION fn_v5_claim_ordinary(uuid) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_claim_ordinary(uuid) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_claim_ordinary(uuid) TO v5_worker;

CREATE FUNCTION fn_v5_ack_ordinary(p_path text, p_token uuid, p_ok boolean)
RETURNS text LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_state text; v_attempts integer; v_delay interval;
DECLARE v_backoff integer[] := ARRAY[30, 120, 600, 1800, 7200, 21600, 43200];
BEGIN
    SELECT job_state, attempts INTO v_state, v_attempts
        FROM attachment_object_deletion WHERE path = p_path FOR UPDATE;
    IF NOT FOUND THEN
        RETURN 'missing';
    END IF;
    IF v_state <> 'leased' THEN
        RETURN 'not_leased';
    END IF;
    IF p_ok THEN
        UPDATE attachment_object_deletion
            SET job_state = 'done', completed_at = now(),
                lease_token = NULL, lease_until = NULL, last_error_code = NULL
            WHERE path = p_path
              AND lease_token = p_token AND job_state = 'leased';
        IF NOT FOUND THEN RETURN 'stale_token'; END IF;
        INSERT INTO attachment_lifecycle_v5_audit
            (queue_domain, object_key, action, reason, actor)
        VALUES ('ordinary', p_path, 'delete_done', 'worker_ack', current_user);
        RETURN 'done';
    END IF;
    v_delay := v_backoff[least(v_attempts, 7)] * (1 + random() * 0.2) * interval '1 second';
    UPDATE attachment_object_deletion
        SET job_state = CASE WHEN v_attempts >= 8 THEN 'blocked' ELSE 'retry_wait' END,
            next_retry_at = now() + v_delay,
            lease_token = NULL, lease_until = NULL,
            last_error_code = CASE WHEN v_attempts >= 8 THEN 'max_attempts'
                                   ELSE 'delete_failed' END
        WHERE path = p_path
          AND lease_token = p_token AND job_state = 'leased';
    IF NOT FOUND THEN RETURN 'stale_token'; END IF;
    RETURN CASE WHEN v_attempts >= 8 THEN 'blocked' ELSE 'retry_wait' END;
END;
$$;
ALTER FUNCTION fn_v5_ack_ordinary(text, uuid, boolean) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_ack_ordinary(text, uuid, boolean) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_ack_ordinary(text, uuid, boolean) TO v5_worker;

CREATE FUNCTION fn_v5_reaper_ordinary(p_limit integer DEFAULT 100)
RETURNS integer LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_count integer;
BEGIN
    WITH expired AS (
        SELECT path FROM attachment_object_deletion
        WHERE job_state = 'leased' AND lease_until < clock_timestamp()
        ORDER BY lease_until, path LIMIT least(p_limit, 100)
        FOR UPDATE SKIP LOCKED)
    UPDATE attachment_object_deletion q
        SET job_state = CASE WHEN q.attempts >= 8 THEN 'blocked' ELSE 'retry_wait' END,
            next_retry_at = now(),
            lease_token = NULL, lease_until = NULL,
            last_error_code = CASE WHEN q.attempts >= 8 THEN 'max_attempts'
                                   ELSE 'lease_expired' END
        FROM expired e WHERE q.path = e.path;
    GET DIAGNOSTICS v_count = ROW_COUNT;
    RETURN v_count;
END;
$$;
ALTER FUNCTION fn_v5_reaper_ordinary(integer) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_reaper_ordinary(integer) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_reaper_ordinary(integer) TO v5_worker;

CREATE FUNCTION fn_v5_claim_eb(p_token uuid)
RETURNS TABLE(out_organization_id bigint, out_workspace_id bigint, out_asset_id bigint,
              out_object_key text, out_storage_driver text, out_storage_namespace text,
              out_storage_container text, out_storage_locator_sha256 text,
              out_attempts integer)
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
BEGIN
    WITH candidate AS (
        SELECT q.organization_id, q.workspace_id, q.asset_id
        FROM enterprise_asset_delete_queue q
        WHERE attempts < 8
          AND job_state IN ('ready', 'retry_wait') AND next_retry_at <= clock_timestamp()
        ORDER BY next_retry_at, organization_id, workspace_id, asset_id
        FOR UPDATE SKIP LOCKED LIMIT 1)
    UPDATE enterprise_asset_delete_queue q
        SET job_state = 'leased', lease_token = p_token,
            lease_until = clock_timestamp() + interval '120 seconds',
            attempts = q.attempts + 1, last_error_code = NULL
        FROM candidate c
        WHERE (q.organization_id, q.workspace_id, q.asset_id)
            = (c.organization_id, c.workspace_id, c.asset_id)
        RETURNING q.organization_id, q.workspace_id, q.asset_id, q.object_key,
                  q.storage_driver, q.storage_namespace, q.storage_container,
                  q.storage_locator_sha256, q.attempts;
END;
$$;
ALTER FUNCTION fn_v5_claim_eb(uuid) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_claim_eb(uuid) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_claim_eb(uuid) TO v5_worker;

CREATE FUNCTION fn_v5_ack_eb(p_org bigint, p_ws bigint, p_asset bigint,
                             p_token uuid, p_ok boolean)
RETURNS text LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_state text; v_attempts integer; v_key text;
DECLARE v_backoff integer[] := ARRAY[30, 120, 600, 1800, 7200, 21600, 43200];
DECLARE v_delay interval;
BEGIN
    SELECT job_state, attempts, object_key INTO v_state, v_attempts, v_key
        FROM enterprise_asset_delete_queue
        WHERE organization_id = p_org AND workspace_id = p_ws AND asset_id = p_asset
        FOR UPDATE;
    IF NOT FOUND THEN RETURN 'missing'; END IF;
    IF v_state <> 'leased' THEN RETURN 'not_leased'; END IF;
    IF p_ok THEN
        UPDATE enterprise_asset_delete_queue
            SET job_state = 'done', completed_at = now(),
                lease_token = NULL, lease_until = NULL, last_error_code = NULL
        WHERE organization_id = p_org AND workspace_id = p_ws AND asset_id = p_asset
          AND lease_token = p_token AND job_state = 'leased';
        IF NOT FOUND THEN RETURN 'stale_token'; END IF;
        INSERT INTO attachment_lifecycle_v5_audit
            (queue_domain, object_key, action, reason, actor)
        VALUES ('enterprise', v_key, 'delete_done', 'worker_ack', current_user);
        RETURN 'done';
    END IF;
    v_delay := v_backoff[least(v_attempts, 7)] * (1 + random() * 0.2) * interval '1 second';
    UPDATE enterprise_asset_delete_queue
        SET job_state = CASE WHEN v_attempts >= 8 THEN 'blocked' ELSE 'retry_wait' END,
            next_retry_at = now() + v_delay,
            lease_token = NULL, lease_until = NULL,
            last_error_code = CASE WHEN v_attempts >= 8 THEN 'max_attempts'
                                   ELSE 'delete_failed' END
    WHERE organization_id = p_org AND workspace_id = p_ws AND asset_id = p_asset
      AND lease_token = p_token AND job_state = 'leased';
    IF NOT FOUND THEN RETURN 'stale_token'; END IF;
    RETURN CASE WHEN v_attempts >= 8 THEN 'blocked' ELSE 'retry_wait' END;
END;
$$;
ALTER FUNCTION fn_v5_ack_eb(bigint, bigint, bigint, uuid, boolean) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_ack_eb(bigint, bigint, bigint, uuid, boolean) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_ack_eb(bigint, bigint, bigint, uuid, boolean)
    TO v5_worker;

CREATE FUNCTION fn_v5_reaper_eb(p_limit integer DEFAULT 100)
RETURNS integer LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_count integer;
BEGIN
    WITH expired AS (
        SELECT organization_id, workspace_id, asset_id FROM enterprise_asset_delete_queue
        WHERE job_state = 'leased' AND lease_until < clock_timestamp()
        ORDER BY lease_until, organization_id, workspace_id, asset_id
        LIMIT least(p_limit, 100) FOR UPDATE SKIP LOCKED)
    UPDATE enterprise_asset_delete_queue q
        SET job_state = CASE WHEN q.attempts >= 8 THEN 'blocked' ELSE 'retry_wait' END,
            next_retry_at = now(),
            lease_token = NULL, lease_until = NULL,
            last_error_code = CASE WHEN q.attempts >= 8 THEN 'max_attempts'
                                   ELSE 'lease_expired' END
        FROM expired e
        WHERE (q.organization_id, q.workspace_id, q.asset_id)
            = (e.organization_id, e.workspace_id, e.asset_id);
    GET DIAGNOSTICS v_count = ROW_COUNT;
    RETURN v_count;
END;
$$;
ALTER FUNCTION fn_v5_reaper_eb(integer) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_reaper_eb(integer) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_reaper_eb(integer) TO v5_worker;

-- ---------------------------------------------------------------- proof
-- Legacy blocked job first activation: zero attempts, never leased, proof role.
CREATE FUNCTION fn_v5_proof_activate_ordinary(p_path text, p_driver text,
    p_namespace text, p_container text, p_locator text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_row attachment_object_deletion%ROWTYPE;
BEGIN
    SELECT * INTO v_row FROM attachment_object_deletion WHERE path = p_path FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 proof unknown job' USING ERRCODE = 'P0002';
    END IF;
    IF v_row.job_state <> 'blocked' OR v_row.attempts <> 0
       OR v_row.lease_token IS NOT NULL OR v_row.lease_until IS NOT NULL
       OR v_row.completed_at IS NOT NULL
       OR v_row.last_error_code IS DISTINCT FROM 'legacy_location_unverified' THEN
        RAISE EXCEPTION 'v5 proof activation criteria not met' USING ERRCODE = '23510';
    END IF;
    IF p_driver IS NULL OR p_namespace IS NULL OR p_namespace = ''
       OR p_container IS NULL OR p_container = '' OR p_locator !~ '^[0-9a-f]{64}$' THEN
        RAISE EXCEPTION 'v5 proof incomplete identity' USING ERRCODE = '23514';
    END IF;
    IF v_row.storage_driver IS NOT NULL OR v_row.storage_namespace IS NOT NULL
       OR v_row.storage_container IS NOT NULL THEN
        RAISE EXCEPTION 'v5 proof can only fill NULL location once'
            USING ERRCODE = '23510';
    END IF;
    -- Re-verify current eligibility of the attachment metadata.
    IF NOT EXISTS (SELECT 1 FROM attachment a
                   WHERE a.path = p_path AND a.lifecycle_state IN
                         ('live', 'delete_intent')) THEN
        RAISE EXCEPTION 'v5 proof metadata missing or wrong state'
            USING ERRCODE = '23510';
    END IF;
    PERFORM fn_v5_lock_identity(p_driver, p_namespace, p_container, p_path);
    UPDATE attachment_object_deletion
        SET storage_driver = p_driver, storage_namespace = p_namespace,
            storage_container = p_container, storage_locator_sha256 = p_locator,
            job_state = 'ready', next_retry_at = now(), last_error_code = NULL
        WHERE path = p_path;
    INSERT INTO attachment_lifecycle_v5_audit
        (queue_domain, object_key, storage_namespace, action, reason, actor, details)
    VALUES ('ordinary', p_path, p_namespace, 'proof_activate', 'legacy_location',
            current_user, jsonb_build_object('locator', p_locator));
END;
$$;
ALTER FUNCTION fn_v5_proof_activate_ordinary(text, text, text, text, text)
    OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_proof_activate_ordinary(text, text, text, text, text)
    FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_proof_activate_ordinary(text, text, text, text, text)
    TO v5_proof;

-- ---------------------------------------------------------------- EB tombstone copy
-- Purge companion: copy UUID + registration from live row into queue row
-- atomically (used by eb purge before metadata delete).
CREATE FUNCTION fn_v5_copy_eb_recovery_tombstone(p_org bigint, p_ws bigint,
                                                 p_asset bigint)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a enterprise_asset%ROWTYPE;
BEGIN
    SELECT * INTO v_a FROM enterprise_asset
        WHERE organization_id = p_org AND workspace_id = p_ws AND id = p_asset
        FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 tombstone source asset missing' USING ERRCODE = 'P0002';
    END IF;
    IF v_a.client_upload_id IS NULL THEN
        -- Source NULL identity: queue recovery fields must both stay NULL.
        UPDATE enterprise_asset_delete_queue
            SET client_upload_id = NULL, upload_registration = NULL
            WHERE organization_id = p_org AND workspace_id = p_ws AND asset_id = p_asset
              AND client_upload_id IS NULL;
        IF NOT FOUND THEN
            RAISE EXCEPTION 'v5 tombstone queue row missing' USING ERRCODE = 'P0002';
        END IF;
        RETURN;
    END IF;
    UPDATE enterprise_asset_delete_queue
        SET client_upload_id = v_a.client_upload_id,
            upload_registration = v_a.upload_registration
    WHERE organization_id = p_org AND workspace_id = p_ws AND asset_id = p_asset;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 tombstone queue row missing' USING ERRCODE = 'P0002';
    END IF;
END;
$$;
ALTER FUNCTION fn_v5_copy_eb_recovery_tombstone(bigint, bigint, bigint)
    OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_copy_eb_recovery_tombstone(bigint, bigint, bigint)
    FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_copy_eb_recovery_tombstone(bigint, bigint, bigint)
    TO v5_app_runtime;

-- ================================================================ M3 guard
-- Trigger FUNCTION DEFINITIONS installed here (migration_function_install_
-- contract); triggers are ATTACHED only by guards-contract.sql (W4-04).
CREATE FUNCTION fn_v5_attachment_location_immutable() RETURNS trigger
LANGUAGE plpgsql SET search_path = public AS $$
BEGIN
    IF (NEW.storage_driver, NEW.storage_namespace, NEW.storage_container,
        NEW.storage_locator_sha256)
       IS DISTINCT FROM
       (OLD.storage_driver, OLD.storage_namespace, OLD.storage_container,
        OLD.storage_locator_sha256) THEN
        RAISE EXCEPTION 'v5 attachment location immutable after fill'
            USING ERRCODE = '23510';
    END IF;
    RETURN NEW;
END;
$$;

CREATE FUNCTION fn_v5_attachment_identity_immutable() RETURNS trigger
LANGUAGE plpgsql SET search_path = public AS $$
BEGIN
    IF NEW.creator_user_id IS DISTINCT FROM OLD.creator_user_id
       OR NEW.scope IS DISTINCT FROM OLD.scope
       OR NEW.cipher IS DISTINCT FROM OLD.cipher
       OR NEW.anchor_msg_id IS DISTINCT FROM OLD.anchor_msg_id
       OR (NEW.path IS DISTINCT FROM OLD.path AND OLD.status <> -1)
       OR NEW.lifecycle_state NOT IN ('live', 'delete_intent', 'deleted') THEN
        RAISE EXCEPTION 'v5 attachment identity/owner/scope/cipher immutable'
            USING ERRCODE = '23510';
    END IF;
    -- life forward-only
    IF (OLD.lifecycle_state = 'delete_intent' AND NEW.lifecycle_state = 'live')
       OR (OLD.lifecycle_state = 'deleted' AND NEW.lifecycle_state <> 'deleted') THEN
        RAISE EXCEPTION 'v5 life state forward-only' USING ERRCODE = '23510';
    END IF;
    -- gc_verified cannot flip to true without full location.
    IF NEW.gc_verified AND (NEW.storage_driver IS NULL OR NEW.storage_namespace IS NULL
                            OR NEW.storage_container IS NULL) THEN
        RAISE EXCEPTION 'v5 gc_verified requires complete location'
            USING ERRCODE = '23514';
    END IF;
    RETURN NEW;
END;
$$;

-- First-bind: NULL -> value once; bound -> different value rejected; same value ok.
CREATE FUNCTION fn_v5_first_bind_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path = public AS $$
BEGIN
    IF TG_TABLE_NAME = 'attachment' THEN
        IF OLD.group_file_id IS NOT NULL
           AND NEW.group_file_id IS DISTINCT FROM OLD.group_file_id THEN
            RAISE EXCEPTION 'v5 group_file_id first-bind only'
                USING ERRCODE = '23510';
        END IF;
        IF OLD.scope_ref IS NOT NULL AND NEW.scope_ref IS DISTINCT FROM OLD.scope_ref THEN
            RAISE EXCEPTION 'v5 scope_ref first-bind only' USING ERRCODE = '23510';
        END IF;
        IF OLD.anchor_conv_seq IS NOT NULL
           AND NEW.anchor_conv_seq IS DISTINCT FROM OLD.anchor_conv_seq THEN
            RAISE EXCEPTION 'v5 anchor_conv_seq first-bind only'
                USING ERRCODE = '23510';
        END IF;
        IF NEW.group_file_id IS DISTINCT FROM OLD.group_file_id
           AND NEW.lifecycle_state <> 'live' THEN
            RAISE EXCEPTION 'v5 first-bind requires live' USING ERRCODE = '23510';
        END IF;
    END IF;
    RETURN NEW;
END;
$$;

CREATE FUNCTION fn_v5_pending_session_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path = public AS $$
BEGIN
    -- State machine (full legal set): registered->{confirmed,rejected,expired};
    -- rejected|expired->{delete_intent}; delete_intent->{deleted}; others fixed.
    IF TG_OP = 'UPDATE' THEN
        IF NOT (
            (OLD.session_state = NEW.session_state)
            OR (OLD.session_state = 'registered'
                AND NEW.session_state IN ('confirmed', 'rejected', 'expired'))
            OR (OLD.session_state IN ('rejected', 'expired')
                AND NEW.session_state = 'delete_intent')
            OR (OLD.session_state = 'delete_intent'
                AND NEW.session_state = 'deleted')) THEN
            RAISE EXCEPTION 'v5 illegal session transition % -> %',
                OLD.session_state, NEW.session_state USING ERRCODE = '23510';
        END IF;
        IF NEW.object_key IS DISTINCT FROM OLD.object_key THEN
            RAISE EXCEPTION 'v5 session object_key immutable (lifetime no-reuse)'
                USING ERRCODE = '23510';
        END IF;
        -- Registration snapshot immutable after first write.
        IF OLD.upload_registration IS NOT NULL
           AND NEW.upload_registration IS DISTINCT FROM OLD.upload_registration THEN
            RAISE EXCEPTION 'v5 registration snapshot immutable' USING ERRCODE = '23510';
        END IF;
        -- Content pin immutable after fill; cannot clear.
        IF OLD.upload_content IS NOT NULL
           AND NEW.upload_content IS DISTINCT FROM OLD.upload_content THEN
            RAISE EXCEPTION 'v5 content pin immutable' USING ERRCODE = '23510';
        END IF;
        -- client_upload_id immutable in both directions on UPDATE (old NULL
        -- cannot be fabricated by any visitor; new ids only via register INSERT).
        IF NEW.client_upload_id IS DISTINCT FROM OLD.client_upload_id THEN
            RAISE EXCEPTION 'v5 client_upload_id immutable' USING ERRCODE = '23510';
        END IF;
        -- Deadline cannot extend; expiry cannot shrink.
        IF NEW.session_deadline IS DISTINCT FROM OLD.session_deadline THEN
            RAISE EXCEPTION 'v5 session deadline immutable' USING ERRCODE = '23510';
        END IF;
        IF NEW.upload_expires_at IS NOT NULL AND OLD.upload_expires_at IS NOT NULL
           AND NEW.upload_expires_at < OLD.upload_expires_at THEN
            RAISE EXCEPTION 'v5 upload expiry cannot shrink' USING ERRCODE = '23510';
        END IF;
    END IF;
    IF TG_OP = 'INSERT' THEN
        IF NEW.session_state NOT IN ('registered') THEN
            RAISE EXCEPTION 'v5 new sessions start registered' USING ERRCODE = '23510';
        END IF;
        IF NEW.session_state = 'confirmed' AND NEW.confirmed_attachment_id IS NULL THEN
            RAISE EXCEPTION 'v5 confirmed requires link' USING ERRCODE = '23514';
        END IF;
        IF NEW.confirmed_attachment_id IS NOT NULL
           AND NEW.upload_registration IS NULL THEN
            RAISE EXCEPTION 'v5 confirmed legacy session cannot be forged'
                USING ERRCODE = '23510';
        END IF;
    END IF;
    IF (NEW.session_state = 'confirmed') != (NEW.confirmed_attachment_id IS NOT NULL) THEN
        RAISE EXCEPTION 'v5 confirmed iff linked' USING ERRCODE = '23514';
    END IF;
    RETURN NEW;
END;
$$;

CREATE FUNCTION fn_v5_eb_registration_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path = public AS $$
BEGIN
    IF TG_OP = 'UPDATE' THEN
        IF OLD.client_upload_id IS NOT NULL
           AND NEW.client_upload_id IS DISTINCT FROM OLD.client_upload_id THEN
            RAISE EXCEPTION 'v5 eb client_upload_id immutable' USING ERRCODE = '23510';
        END IF;
        IF OLD.upload_registration IS NOT NULL
           AND NEW.upload_registration IS DISTINCT FROM OLD.upload_registration THEN
            RAISE EXCEPTION 'v5 eb registration immutable' USING ERRCODE = '23510';
        END IF;
        IF OLD.upload_content IS NOT NULL
           AND NEW.upload_content IS DISTINCT FROM OLD.upload_content THEN
            RAISE EXCEPTION 'v5 eb content pin immutable' USING ERRCODE = '23510';
        END IF;
    END IF;
    RETURN NEW;
END;
$$;

-- Audit-actor anonymization: uploaded_by_user_id NULL allowed only when the
-- parent user row is gone in this same transaction (FK would have failed
-- otherwise); substituting a different user or NULLing a living parent is
-- rejected. Business ownership columns remain untouched by this guard.
CREATE FUNCTION fn_v5_eb_audit_actor_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path = public AS $$
BEGIN
    IF TG_OP = 'UPDATE'
       AND NEW.uploaded_by_user_id IS DISTINCT FROM OLD.uploaded_by_user_id THEN
        IF NEW.uploaded_by_user_id IS NOT NULL THEN
            RAISE EXCEPTION 'v5 audit actor substitution forbidden'
                USING ERRCODE = '23510';
        END IF;
        IF OLD.uploaded_by_user_id IS NOT NULL
           AND EXISTS (SELECT 1 FROM "user" u WHERE u.id = OLD.uploaded_by_user_id) THEN
            RAISE EXCEPTION 'v5 audit actor NULL only after parent user deleted'
                USING ERRCODE = '23510';
        END IF;
        IF (NEW.organization_id, NEW.workspace_id, NEW.object_key, NEW.status,
            NEW.lifecycle_state, NEW.storage_driver, NEW.storage_namespace,
            NEW.storage_container, NEW.storage_locator_sha256, NEW.client_upload_id,
            NEW.upload_registration)
           IS DISTINCT FROM
           (OLD.organization_id, OLD.workspace_id, OLD.object_key, OLD.status,
            OLD.lifecycle_state, OLD.storage_driver, OLD.storage_namespace,
            OLD.storage_container, OLD.storage_locator_sha256, OLD.client_upload_id,
            OLD.upload_registration) THEN
            RAISE EXCEPTION 'v5 audit anonymization cannot alter protected fields'
                USING ERRCODE = '23510';
        END IF;
    END IF;
    RETURN NEW;
END;
$$;

CREATE FUNCTION fn_v5_queue_job_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path = public AS $$
BEGIN
    IF TG_OP = 'UPDATE' THEN
        IF (NEW.storage_driver, NEW.storage_namespace, NEW.storage_container,
            NEW.storage_locator_sha256)
           IS DISTINCT FROM
           (OLD.storage_driver, OLD.storage_namespace, OLD.storage_container,
            OLD.storage_locator_sha256) THEN
            RAISE EXCEPTION 'v5 job identity immutable after fill'
                USING ERRCODE = '23510';
        END IF;
        IF NEW.job_state = 'done' AND OLD.job_state <> 'done'
           AND NEW.completed_at IS NULL THEN
            RAISE EXCEPTION 'v5 done requires completed_at' USING ERRCODE = '23514';
        END IF;
        IF OLD.job_state = 'done' AND NEW.job_state <> 'done' THEN
            RAISE EXCEPTION 'v5 done tombstone cannot revive'
                USING ERRCODE = '23510';
        END IF;
        IF TG_TABLE_NAME = 'attachment_object_deletion' THEN
            IF NEW.attempts < OLD.attempts THEN
                RAISE EXCEPTION 'v5 attempts cannot reset' USING ERRCODE = '23510';
            END IF;
        END IF;
    END IF;
    IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION 'v5 job rows are permanent tombstones (no DELETE)'
            USING ERRCODE = '23510';
    END IF;
    RETURN NEW;
END;
$$;

-- Metadata delete guard (attached by M3): attachment row DELETE allowed only
-- after delete_intent committed AND a matching queue tombstone row exists.
CREATE FUNCTION fn_v5_metadata_delete_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path = public AS $$
BEGIN
    IF OLD.lifecycle_state <> 'delete_intent' THEN
        RAISE EXCEPTION 'v5 metadata delete requires committed delete_intent'
            USING ERRCODE = '23510';
    END IF;
    IF NOT EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path = OLD.path) THEN
        RAISE EXCEPTION 'v5 metadata delete requires queue tombstone'
            USING ERRCODE = '23510';
    END IF;
    RETURN OLD;
END;
$$;

REVOKE ALL ON FUNCTION fn_v5_attachment_location_immutable() FROM PUBLIC;
REVOKE ALL ON FUNCTION fn_v5_attachment_identity_immutable() FROM PUBLIC;
REVOKE ALL ON FUNCTION fn_v5_first_bind_guard() FROM PUBLIC;
REVOKE ALL ON FUNCTION fn_v5_pending_session_guard() FROM PUBLIC;
REVOKE ALL ON FUNCTION fn_v5_eb_registration_guard() FROM PUBLIC;
REVOKE ALL ON FUNCTION fn_v5_eb_audit_actor_guard() FROM PUBLIC;
REVOKE ALL ON FUNCTION fn_v5_queue_job_guard() FROM PUBLIC;
REVOKE ALL ON FUNCTION fn_v5_metadata_delete_guard() FROM PUBLIC;

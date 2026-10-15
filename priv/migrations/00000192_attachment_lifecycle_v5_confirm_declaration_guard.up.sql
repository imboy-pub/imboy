-- Freeze the original business scope at SQL confirmation, including direct writers.
SET LOCAL lock_timeout='5s';
SET LOCAL statement_timeout='15min';

CREATE OR REPLACE FUNCTION fn_v5_confirm_pending(p_object_key text, p_attachment_id bigint)
RETURNS bigint LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_p attach_pending%ROWTYPE; v_a attachment%ROWTYPE; v_count integer; v_decl jsonb; v_subject jsonb; v_ref text;
BEGIN
    SELECT * INTO v_p FROM attach_pending WHERE object_key = p_object_key FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 confirm unknown session' USING ERRCODE = 'P0002';
    END IF;
    SELECT * INTO v_a FROM attachment WHERE id = p_attachment_id FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 confirm unknown asset' USING ERRCODE = 'P0002';
    END IF;
    -- W2-03 屏障：GC intent 先提交（delete_intent/deleted）时 confirm 落败。
    IF v_a.lifecycle_state IS NOT NULL AND v_a.lifecycle_state <> 'live' THEN
        RAISE EXCEPTION 'v5 confirm on non-live asset' USING ERRCODE = '23510';
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
    -- The immutable registration remains authoritative for all SQL writers.
    v_decl := v_p.upload_registration->'declaration';
    v_subject := v_p.upload_registration->'subject';
    IF v_subject->>'kind' = 'user' AND v_p.creator_user_id > 0 THEN
        IF v_subject->>'principal' IS DISTINCT FROM v_p.creator_user_id::text THEN
            RAISE EXCEPTION 'v5 confirm registration subject mismatch' USING ERRCODE='23510';
        END IF;
        v_ref := nullif(v_decl->>'scope_ref','');
    ELSIF v_subject->>'kind' = 'application' AND v_p.creator_user_id = 0 THEN
        IF coalesce(v_subject->>'principal','') !~ '^[1-9][0-9]*$'
           OR coalesce(v_subject->>'org_id','') !~ '^[1-9][0-9]*$'
           OR v_subject->>'principal' IS DISTINCT FROM v_decl->>'application_id'
           OR v_subject->>'org_id' IS DISTINCT FROM v_decl->>'org_id' THEN
            RAISE EXCEPTION 'v5 confirm application subject mismatch' USING ERRCODE='23510';
        END IF;
        v_ref := 'org:' || (v_decl->>'org_id') || '/app:' || (v_decl->>'application_id');
    ELSE
        RAISE EXCEPTION 'v5 confirm unproven registration subject' USING ERRCODE='23510';
    END IF;
    IF (v_a.scope IN ('group','channel','c2c') AND v_ref IS NULL)
       OR v_decl->>'scope' IS DISTINCT FROM v_a.scope
       OR v_ref IS DISTINCT FROM nullif(v_a.scope_ref,'') THEN
        RAISE EXCEPTION 'v5 confirm frozen scope declaration mismatch' USING ERRCODE='23510';
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

-- Direct DML must obey the same pending -> asset lock and frozen declaration.
CREATE OR REPLACE FUNCTION fn_v5_pending_session_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path = public AS $$
DECLARE v_p attach_pending%ROWTYPE; v_a attachment%ROWTYPE; v_decl jsonb; v_subject jsonb; v_ref text;
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
        -- The row lock on pending is already held by UPDATE. Never permit
        -- same-statement substitution of the identity used for confirmation.
        IF (NEW.bucket, NEW.scope, NEW.creator_user_id, NEW.storage_driver,
            NEW.storage_namespace, NEW.storage_container, NEW.storage_locator_sha256)
           IS DISTINCT FROM
           (OLD.bucket, OLD.scope, OLD.creator_user_id, OLD.storage_driver,
            OLD.storage_namespace, OLD.storage_container, OLD.storage_locator_sha256) THEN
            RAISE EXCEPTION 'v5 pending frozen identity immutable' USING ERRCODE='23510';
        END IF;
        IF OLD.confirmed_attachment_id IS NOT NULL
           AND NEW.confirmed_attachment_id IS DISTINCT FROM OLD.confirmed_attachment_id THEN
            RAISE EXCEPTION 'v5 confirmed link immutable' USING ERRCODE='23510';
        END IF;
        IF OLD.session_state = 'registered' AND NEW.session_state = 'confirmed' THEN
            v_p := OLD;
            SELECT * INTO v_a FROM attachment
                WHERE id = NEW.confirmed_attachment_id FOR UPDATE;
            IF NOT FOUND THEN
                RAISE EXCEPTION 'v5 confirm unknown asset' USING ERRCODE='23510';
            END IF;
            IF v_a.lifecycle_state IS NOT NULL AND v_a.lifecycle_state <> 'live' THEN
                RAISE EXCEPTION 'v5 confirm on non-live asset' USING ERRCODE='23510';
            END IF;
            -- The immutable registration remains authoritative for all SQL writers.
            v_decl := v_p.upload_registration->'declaration';
            v_subject := v_p.upload_registration->'subject';
            IF v_subject->>'kind' = 'user' AND v_p.creator_user_id > 0 THEN
                IF v_subject->>'principal' IS DISTINCT FROM v_p.creator_user_id::text THEN
                    RAISE EXCEPTION 'v5 confirm registration subject mismatch' USING ERRCODE='23510';
                END IF;
                v_ref := nullif(v_decl->>'scope_ref','');
            ELSIF v_subject->>'kind' = 'application' AND v_p.creator_user_id = 0 THEN
                IF coalesce(v_subject->>'principal','') !~ '^[1-9][0-9]*$'
                   OR coalesce(v_subject->>'org_id','') !~ '^[1-9][0-9]*$'
                   OR v_subject->>'principal' IS DISTINCT FROM v_decl->>'application_id'
                   OR v_subject->>'org_id' IS DISTINCT FROM v_decl->>'org_id' THEN
                    RAISE EXCEPTION 'v5 confirm application subject mismatch' USING ERRCODE='23510';
                END IF;
                v_ref := 'org:' || (v_decl->>'org_id') || '/app:' || (v_decl->>'application_id');
            ELSE
                RAISE EXCEPTION 'v5 confirm unproven registration subject' USING ERRCODE='23510';
            END IF;
            IF (v_a.scope IN ('group','channel','c2c') AND v_ref IS NULL)
               OR v_decl->>'scope' IS DISTINCT FROM v_a.scope
               OR v_ref IS DISTINCT FROM nullif(v_a.scope_ref,'') THEN
                RAISE EXCEPTION 'v5 confirm frozen scope declaration mismatch' USING ERRCODE='23510';
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

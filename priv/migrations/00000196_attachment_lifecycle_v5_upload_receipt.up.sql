-- 上传准入与写前复核：只在短事务内固定实际 receipt，绝不持锁等待存储。
-- Upload admission and pre-I/O recheck use the same frozen receipt and lock order.
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

-- Shared prelocks also cover existing confirm/intent paths, whose original
-- bodies formerly took rows before I. Legacy NULL locations gain only locks.
CREATE FUNCTION fn_v5_lock_ordinary_upload(p_key text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_i record; v_current record;
BEGIN
    SELECT * INTO v_i FROM (
        SELECT storage_driver,storage_namespace,storage_container,storage_locator_sha256,
               scope,coalesce(upload_registration->'declaration'->>'scope_ref',scope_ref) AS scope_ref
            FROM attach_pending WHERE object_key=p_key
        UNION ALL
        SELECT storage_driver,storage_namespace,storage_container,storage_locator_sha256,
               scope,scope_ref FROM attachment WHERE path=p_key
    ) candidates LIMIT 1;
    IF FOUND THEN
        PERFORM w.id FROM workspace w WHERE
            (v_i.scope='group' AND EXISTS (SELECT 1 FROM "group" g
                WHERE g.id::text=v_i.scope_ref AND g.scope='workspace' AND g.workspace_id=w.id))
            OR (v_i.scope='channel' AND EXISTS (SELECT 1 FROM channel c
                WHERE c.id::text=v_i.scope_ref AND c.scope='workspace' AND c.workspace_id=w.id))
            ORDER BY w.id FOR UPDATE;
        PERFORM fn_v5_lock_identity(v_i.storage_driver,v_i.storage_namespace,
                                    v_i.storage_container,p_key);
    END IF;
    PERFORM pg_advisory_xact_lock(hashtextextended(p_key,168));
    PERFORM object_key FROM attach_pending WHERE object_key=p_key FOR UPDATE;
    PERFORM id FROM attachment WHERE path=p_key ORDER BY id FOR UPDATE;
    SELECT * INTO v_current FROM (
        SELECT storage_driver,storage_namespace,storage_container,storage_locator_sha256,
               scope,coalesce(upload_registration->'declaration'->>'scope_ref',scope_ref) AS scope_ref
            FROM attach_pending WHERE object_key=p_key
        UNION ALL
        SELECT storage_driver,storage_namespace,storage_container,storage_locator_sha256,
               scope,scope_ref FROM attachment WHERE path=p_key
    ) candidates LIMIT 1;
    IF v_current IS DISTINCT FROM v_i THEN
        RAISE EXCEPTION 'v5 upload identity changed during lock acquisition' USING ERRCODE='40001';
    END IF;
END;
$$;

CREATE FUNCTION fn_v5_lock_eb_upload(p_org bigint,p_ws bigint,p_asset bigint)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_before enterprise_asset%ROWTYPE; v_a enterprise_asset%ROWTYPE;
BEGIN
    PERFORM id FROM workspace WHERE organization_id=p_org AND id=p_ws FOR UPDATE;
    SELECT * INTO v_before FROM enterprise_asset
        WHERE organization_id=p_org AND workspace_id=p_ws AND id=p_asset;
    IF NOT FOUND THEN RETURN; END IF;
    PERFORM pg_advisory_xact_lock(hashtextextended(
        'v5-eb-upload:' || coalesce(v_before.client_upload_id::text,p_asset::text),169));
    PERFORM fn_v5_lock_identity(v_before.storage_driver,v_before.storage_namespace,
                                v_before.storage_container,v_before.object_key);
    SELECT * INTO v_a FROM enterprise_asset
        WHERE organization_id=p_org AND workspace_id=p_ws AND id=p_asset FOR UPDATE;
    IF (v_a.object_key,v_a.client_upload_id,v_a.storage_driver,v_a.storage_namespace,
        v_a.storage_container,v_a.storage_locator_sha256)
       IS DISTINCT FROM (v_before.object_key,v_before.client_upload_id,v_before.storage_driver,
        v_before.storage_namespace,v_before.storage_container,v_before.storage_locator_sha256) THEN
        RAISE EXCEPTION 'v5 upload identity changed during lock acquisition' USING ERRCODE='40001';
    END IF;
END;
$$;

-- A shared predicate rejects malformed/extra receipt fields and checks the
-- applicable declaration. Ordinary E2EE has no plaintext hash declaration.
CREATE FUNCTION fn_v5_upload_receipt_matches(p_content jsonb, p_declaration jsonb)
RETURNS boolean LANGUAGE sql IMMUTABLE SET search_path = public AS $$
    SELECT coalesce(
        jsonb_typeof(p_content) = 'object'
        AND p_content - ARRAY['version','sha256','size_bytes','mime'] = '{}'::jsonb
        AND p_content->'version' = '1'::jsonb
        AND jsonb_typeof(p_content->'sha256') = 'string'
        AND p_content->>'sha256' ~ '^[0-9a-f]{64}$'
        AND jsonb_typeof(p_content->'size_bytes') = 'string'
        AND p_content->>'size_bytes' ~ '^(0|[1-9][0-9]*)$'
        AND jsonb_typeof(p_content->'mime') = 'string'
        AND length(p_content->>'mime') > 0
        AND jsonb_typeof(p_declaration) = 'object'
        AND p_content->>'mime' = p_declaration->>'mime'
        AND (p_declaration->>'size_bytes' IS NULL
             OR p_content->>'size_bytes' = p_declaration->>'size_bytes')
        AND (p_declaration->>'object_hash' IS NULL
             OR p_content->>'sha256' = p_declaration->>'object_hash')
        AND (p_declaration->>'sha256' IS NULL
             OR p_content->>'sha256' = p_declaration->>'sha256'), false)
$$;

CREATE FUNCTION fn_v5_pending_content_gate(p_key text, p_content jsonb,
                                           p_identity jsonb, p_recheck boolean)
RETURNS boolean LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_before attach_pending%ROWTYPE; v_p attach_pending%ROWTYPE;
        v_a attachment%ROWTYPE; v_identity jsonb;
BEGIN
    SELECT * INTO v_before FROM attach_pending WHERE object_key=p_key;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 upload unknown session' USING ERRCODE='P0002';
    END IF;
    PERFORM fn_v5_lock_ordinary_upload(p_key);
    SELECT * INTO v_p FROM attach_pending WHERE object_key=p_key FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 upload unknown session' USING ERRCODE='P0002';
    END IF;
    IF (v_p.storage_driver,v_p.storage_namespace,v_p.storage_container,
        v_p.storage_locator_sha256,v_p.upload_registration,v_p.client_upload_id)
       IS DISTINCT FROM
       (v_before.storage_driver,v_before.storage_namespace,v_before.storage_container,
        v_before.storage_locator_sha256,v_before.upload_registration,v_before.client_upload_id)
       OR v_p.storage_driver IS NULL OR v_p.storage_namespace IS NULL
       OR v_p.storage_container IS NULL OR v_p.storage_locator_sha256 IS NULL THEN
        RAISE EXCEPTION 'v5 upload frozen location unavailable' USING ERRCODE='22023';
    END IF;
    v_identity := jsonb_build_object('driver',v_p.storage_driver,
        'namespace',v_p.storage_namespace,'container',v_p.storage_container,
        'key',p_key,'locator_digest',v_p.storage_locator_sha256);
    IF p_recheck AND p_identity IS DISTINCT FROM v_identity THEN
        RAISE EXCEPTION 'v5 upload frozen location mismatch' USING ERRCODE='22023';
    END IF;
    -- clock_timestamp is evaluated after lock waits, unlike transaction now().
    IF v_p.client_upload_id IS NULL OR v_p.upload_registration IS NULL
       OR v_p.session_deadline IS NULL OR v_p.upload_expires_at IS NULL
       OR least(v_p.session_deadline,v_p.upload_expires_at) <= clock_timestamp()
       OR (NOT p_recheck AND v_p.session_state <> 'registered')
       OR (p_recheck AND v_p.session_state NOT IN ('registered','confirmed')) THEN
        RAISE EXCEPTION 'v5 upload session not writable' USING ERRCODE='55000';
    END IF;
    IF EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path=p_key)
       OR EXISTS (SELECT 1 FROM enterprise_asset_delete_queue q
            WHERE q.client_upload_id=v_p.client_upload_id
               OR (q.storage_driver=v_p.storage_driver
                   AND q.storage_namespace=v_p.storage_namespace
                   AND q.storage_container=v_p.storage_container AND q.object_key=p_key))
       OR EXISTS (SELECT 1 FROM enterprise_asset a
            WHERE a.storage_driver=v_p.storage_driver
              AND a.storage_namespace=v_p.storage_namespace
              AND a.storage_container=v_p.storage_container AND a.object_key=p_key) THEN
        RAISE EXCEPTION 'v5 upload identity tombstoned or shared' USING ERRCODE='23505';
    END IF;
    SELECT * INTO v_a FROM attachment WHERE path=p_key FOR UPDATE;
    IF FOUND THEN
        IF v_a.lifecycle_state <> 'live'
           OR (v_a.storage_driver,v_a.storage_namespace,v_a.storage_container,
               v_a.storage_locator_sha256)
              IS DISTINCT FROM (v_p.storage_driver,v_p.storage_namespace,
                                v_p.storage_container,v_p.storage_locator_sha256)
           OR (v_p.session_state='confirmed' AND v_p.confirmed_attachment_id<>v_a.id) THEN
            RAISE EXCEPTION 'v5 upload asset not writable' USING ERRCODE='55000';
        END IF;
    ELSIF v_p.session_state='confirmed' THEN
        RAISE EXCEPTION 'v5 upload confirmed asset missing' USING ERRCODE='55000';
    END IF;
    IF NOT fn_v5_upload_receipt_matches(p_content,v_p.upload_registration->'declaration')
       OR (v_p.upload_content IS NOT NULL AND v_p.upload_content IS DISTINCT FROM p_content)
       OR (p_recheck AND v_p.upload_content IS DISTINCT FROM p_content) THEN
        RAISE EXCEPTION 'v5 upload receipt or declaration mismatch' USING ERRCODE='23510';
    END IF;
    IF NOT p_recheck AND v_p.upload_content IS NULL THEN
        UPDATE attach_pending SET upload_content=p_content WHERE object_key=p_key;
    END IF;
    RETURN true;
END;
$$;

CREATE OR REPLACE FUNCTION fn_v5_pin_pending_content(p_object_key text, p_content jsonb)
RETURNS boolean LANGUAGE sql SECURITY DEFINER SET search_path = public AS $$
    SELECT fn_v5_pending_content_gate(p_object_key,p_content,NULL,false)
$$;
CREATE FUNCTION fn_v5_recheck_pending_content(p_key text, p_content jsonb, p_identity jsonb)
RETURNS boolean LANGUAGE sql SECURITY DEFINER SET search_path = public AS $$
    SELECT fn_v5_pending_content_gate(p_key,p_content,p_identity,true)
$$;

CREATE FUNCTION fn_v5_eb_content_gate(p_org bigint, p_ws bigint, p_asset bigint,
    p_content jsonb, p_subject jsonb, p_expiry timestamptz, p_identity jsonb, p_recheck boolean)
RETURNS boolean LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_before enterprise_asset%ROWTYPE; v_a enterprise_asset%ROWTYPE; v_identity jsonb;
BEGIN
    PERFORM fn_v5_lock_eb_upload(p_org,p_ws,p_asset);
    PERFORM id FROM workspace WHERE organization_id=p_org AND id=p_ws FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 upload unknown workspace' USING ERRCODE='P0002';
    END IF;
    SELECT * INTO v_before FROM enterprise_asset
        WHERE organization_id=p_org AND workspace_id=p_ws AND id=p_asset;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 upload unknown enterprise asset' USING ERRCODE='P0002';
    END IF;
    PERFORM pg_advisory_xact_lock(hashtextextended(
        'v5-eb-upload:' || coalesce(v_before.client_upload_id::text,p_asset::text),169));
    PERFORM fn_v5_lock_identity(v_before.storage_driver,v_before.storage_namespace,
                                v_before.storage_container,v_before.object_key);
    SELECT * INTO v_a FROM enterprise_asset
        WHERE organization_id=p_org AND workspace_id=p_ws AND id=p_asset FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 upload unknown enterprise asset' USING ERRCODE='P0002';
    END IF;
    IF (v_a.object_key,v_a.storage_driver,v_a.storage_namespace,v_a.storage_container,
        v_a.storage_locator_sha256,v_a.client_upload_id,v_a.upload_registration)
       IS DISTINCT FROM
       (v_before.object_key,v_before.storage_driver,v_before.storage_namespace,
        v_before.storage_container,v_before.storage_locator_sha256,
        v_before.client_upload_id,v_before.upload_registration)
       OR v_a.storage_driver IS NULL OR v_a.storage_namespace IS NULL
       OR v_a.storage_container IS NULL OR v_a.storage_locator_sha256 IS NULL THEN
        RAISE EXCEPTION 'v5 upload frozen location unavailable' USING ERRCODE='22023';
    END IF;
    v_identity := jsonb_build_object('driver',v_a.storage_driver,
        'namespace',v_a.storage_namespace,'container',v_a.storage_container,
        'key',v_a.object_key,'locator_digest',v_a.storage_locator_sha256);
    IF p_recheck AND p_identity IS DISTINCT FROM v_identity THEN
        RAISE EXCEPTION 'v5 upload frozen location mismatch' USING ERRCODE='22023';
    END IF;
    IF v_a.client_upload_id IS NULL OR v_a.upload_registration IS NULL
       OR v_a.session_deadline IS NULL OR v_a.upload_expires_at IS NULL
       OR least(v_a.session_deadline,v_a.upload_expires_at) <= clock_timestamp()
       OR p_expiry IS NULL OR p_expiry <= clock_timestamp() OR p_expiry > v_a.upload_expires_at
       OR v_a.lifecycle_state <> 'live' OR v_a.pending_object_delete
       OR (NOT p_recheck AND v_a.status <> 'pending_confirm')
       OR (p_recheck AND v_a.status NOT IN ('pending_confirm','active')) THEN
        RAISE EXCEPTION 'v5 upload enterprise session not writable' USING ERRCODE='55000';
    END IF;
    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue q
          WHERE (q.organization_id=p_org AND q.workspace_id=p_ws AND q.asset_id=p_asset)
             OR q.client_upload_id=v_a.client_upload_id
             OR (q.storage_driver=v_a.storage_driver AND q.storage_namespace=v_a.storage_namespace
                 AND q.storage_container=v_a.storage_container AND q.object_key=v_a.object_key))
       OR EXISTS (SELECT 1 FROM attachment_object_deletion q
          WHERE q.storage_driver=v_a.storage_driver AND q.storage_namespace=v_a.storage_namespace
            AND q.storage_container=v_a.storage_container AND q.path=v_a.object_key)
       OR EXISTS (SELECT 1 FROM attachment a
          WHERE a.storage_driver=v_a.storage_driver AND a.storage_namespace=v_a.storage_namespace
            AND a.storage_container=v_a.storage_container AND a.path=v_a.object_key)
       OR EXISTS (SELECT 1 FROM attach_pending p
          WHERE p.storage_driver=v_a.storage_driver AND p.storage_namespace=v_a.storage_namespace
            AND p.storage_container=v_a.storage_container AND p.object_key=v_a.object_key) THEN
        RAISE EXCEPTION 'v5 upload enterprise identity tombstoned or shared' USING ERRCODE='23505';
    END IF;
    IF v_a.upload_registration->'subject' IS DISTINCT FROM p_subject
       OR NOT fn_v5_upload_receipt_matches(p_content,v_a.upload_registration->'declaration')
       OR p_content->>'sha256' IS DISTINCT FROM v_a.object_hash
       OR p_content->>'mime' IS DISTINCT FROM v_a.mime
       OR p_content->>'size_bytes' IS DISTINCT FROM v_a.size_bytes::text
       OR (v_a.upload_content IS NOT NULL AND v_a.upload_content IS DISTINCT FROM p_content)
       OR (p_recheck AND v_a.upload_content IS DISTINCT FROM p_content) THEN
        RAISE EXCEPTION 'v5 upload enterprise receipt or declaration mismatch' USING ERRCODE='23510';
    END IF;
    IF NOT p_recheck AND v_a.upload_content IS NULL THEN
        UPDATE enterprise_asset SET upload_content=p_content
            WHERE organization_id=p_org AND workspace_id=p_ws AND id=p_asset;
    END IF;
    RETURN true;
END;
$$;

CREATE FUNCTION fn_v5_pin_eb_content(p_org bigint,p_ws bigint,p_asset bigint,
    p_content jsonb,p_subject jsonb,p_expiry timestamptz)
RETURNS boolean LANGUAGE sql SECURITY DEFINER SET search_path = public AS $$
    SELECT fn_v5_eb_content_gate(p_org,p_ws,p_asset,p_content,p_subject,p_expiry,NULL,false)
$$;
CREATE FUNCTION fn_v5_recheck_eb_content(p_org bigint,p_ws bigint,p_asset bigint,
    p_content jsonb,p_subject jsonb,p_expiry timestamptz,p_identity jsonb)
RETURNS boolean LANGUAGE sql SECURITY DEFINER SET search_path = public AS $$
    SELECT fn_v5_eb_content_gate(p_org,p_ws,p_asset,p_content,p_subject,p_expiry,p_identity,true)
$$;

-- SECURITY DEFINER ownership/ACL stays explicit; internal gates are not callable
-- by runtime roles. UPDATE(id) is needed for SELECT ... FOR UPDATE on the parent.
GRANT SELECT ON "group",channel TO v5_fn_owner;
GRANT UPDATE(id) ON workspace TO v5_fn_owner;
ALTER FUNCTION fn_v5_upload_receipt_matches(jsonb,jsonb) OWNER TO v5_fn_owner;
ALTER FUNCTION fn_v5_pending_content_gate(text,jsonb,jsonb,boolean) OWNER TO v5_fn_owner;
ALTER FUNCTION fn_v5_pin_pending_content(text,jsonb) OWNER TO v5_fn_owner;
ALTER FUNCTION fn_v5_recheck_pending_content(text,jsonb,jsonb) OWNER TO v5_fn_owner;
ALTER FUNCTION fn_v5_eb_content_gate(bigint,bigint,bigint,jsonb,jsonb,timestamptz,jsonb,boolean) OWNER TO v5_fn_owner;
ALTER FUNCTION fn_v5_pin_eb_content(bigint,bigint,bigint,jsonb,jsonb,timestamptz) OWNER TO v5_fn_owner;
ALTER FUNCTION fn_v5_recheck_eb_content(bigint,bigint,bigint,jsonb,jsonb,timestamptz,jsonb) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_upload_receipt_matches(jsonb,jsonb) FROM PUBLIC;
REVOKE ALL ON FUNCTION fn_v5_pending_content_gate(text,jsonb,jsonb,boolean) FROM PUBLIC;
REVOKE ALL ON FUNCTION fn_v5_pin_pending_content(text,jsonb) FROM PUBLIC;
REVOKE ALL ON FUNCTION fn_v5_recheck_pending_content(text,jsonb,jsonb) FROM PUBLIC;
REVOKE ALL ON FUNCTION fn_v5_eb_content_gate(bigint,bigint,bigint,jsonb,jsonb,timestamptz,jsonb,boolean) FROM PUBLIC;
REVOKE ALL ON FUNCTION fn_v5_pin_eb_content(bigint,bigint,bigint,jsonb,jsonb,timestamptz) FROM PUBLIC;
REVOKE ALL ON FUNCTION fn_v5_recheck_eb_content(bigint,bigint,bigint,jsonb,jsonb,timestamptz,jsonb) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_pin_pending_content(text,jsonb),
    fn_v5_recheck_pending_content(text,jsonb,jsonb),
    fn_v5_pin_eb_content(bigint,bigint,bigint,jsonb,jsonb,timestamptz),
    fn_v5_recheck_eb_content(bigint,bigint,bigint,jsonb,jsonb,timestamptz,jsonb)
    TO v5_app_runtime;

-- Existing protocol functions retain all eligibility/state/queue logic.

CREATE OR REPLACE FUNCTION fn_v5_confirm_pending(p_object_key text, p_attachment_id bigint)
RETURNS bigint LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_p attach_pending%ROWTYPE; v_a attachment%ROWTYPE; v_count integer; v_decl jsonb; v_subject jsonb; v_ref text;
BEGIN
    PERFORM fn_v5_lock_ordinary_upload(p_object_key);
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

CREATE OR REPLACE FUNCTION fn_v5_intent_pending_delete(p_object_key text,
                                                       p_reason text, p_mode text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_p attach_pending%ROWTYPE; v_a attachment%ROWTYPE; v_ready boolean;
BEGIN
    PERFORM fn_v5_lock_ordinary_upload(p_object_key);
    IF p_mode NOT IN ('reject', 'sweep') THEN
        RAISE EXCEPTION 'v5 pending intent unknown mode' USING ERRCODE = '23514';
    END IF;
    SELECT * INTO v_p FROM attach_pending WHERE object_key = p_object_key FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 pending intent unknown session' USING ERRCODE = 'P0002';
    END IF;
    IF p_mode = 'sweep' THEN
        IF v_p.session_state NOT IN ('registered', 'rejected', 'expired') THEN
            RAISE EXCEPTION 'v5 pending sweep illegal from %', v_p.session_state
                USING ERRCODE = '23510';
        END IF;
        IF v_p.upload_expires_at IS NULL OR v_p.upload_expires_at > now() THEN
            RAISE EXCEPTION 'v5 pending intent upload capability not exhausted'
                USING ERRCODE = '23510';
        END IF;
        IF v_p.session_state = 'registered' THEN
            UPDATE attach_pending SET session_state = 'expired'
                WHERE object_key = p_object_key AND session_state = 'registered';
            IF NOT FOUND THEN
                RAISE EXCEPTION 'v5 pending intent lost eligibility race'
                    USING ERRCODE = '40001';
            END IF;
        END IF;
    ELSE
        IF v_p.session_state <> 'registered' THEN
            RAISE EXCEPTION 'v5 pending reject illegal from %', v_p.session_state
                USING ERRCODE = '23510';
        END IF;
    END IF;
    SELECT * INTO v_a FROM attachment WHERE path = p_object_key;
    IF FOUND AND v_a.lifecycle_state <> 'deleted' THEN
        RAISE EXCEPTION 'v5 pending intent on metadata-backed asset'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path = p_object_key) THEN
        RAISE EXCEPTION 'v5 intent job already exists (tombstone)'
            USING ERRCODE = '23505';
    END IF;
    PERFORM fn_v5_lock_identity(v_p.storage_driver, v_p.storage_namespace,
                                v_p.storage_container, v_p.object_key);
    PERFORM pg_advisory_xact_lock(hashtextextended(v_p.object_key, 168));
    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue q
               WHERE q.storage_namespace = v_p.storage_namespace
                 AND q.storage_container = v_p.storage_container
                 AND q.object_key = v_p.object_key) THEN
        RAISE EXCEPTION 'v5 intent cross-domain identity conflict'
            USING ERRCODE = '23510';
    END IF;
    IF p_mode = 'reject' THEN
        UPDATE attach_pending SET session_state = 'rejected'
            WHERE object_key = p_object_key AND session_state = 'registered';
        IF NOT FOUND THEN
            RAISE EXCEPTION 'v5 pending intent lost eligibility race'
                USING ERRCODE = '40001';
        END IF;
    END IF;
    UPDATE attach_pending SET session_state = 'delete_intent'
        WHERE object_key = p_object_key
          AND session_state IN ('rejected', 'expired');
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 pending intent lost eligibility race' USING ERRCODE = '40001';
    END IF;
    -- W4-01 身份分支：完整 → ready；缺失 → blocked（proof 激活），不以 CK 暴露。
    v_ready := v_p.storage_driver IS NOT NULL AND v_p.storage_namespace IS NOT NULL
        AND v_p.storage_container IS NOT NULL
        AND v_p.storage_locator_sha256 ~ '^[0-9a-f]{64}$';
    INSERT INTO attachment_object_deletion
        (path, bucket, storage_driver, storage_namespace, storage_container,
         storage_locator_sha256, job_state, next_retry_at, last_error_code)
    VALUES (v_p.object_key, v_p.bucket, v_p.storage_driver, v_p.storage_namespace,
            v_p.storage_container, v_p.storage_locator_sha256,
            CASE WHEN v_ready THEN 'ready' ELSE 'blocked' END,
            CASE WHEN v_ready THEN now() ELSE now() END,
            CASE WHEN v_ready THEN NULL ELSE 'legacy_location_unverified' END);
    INSERT INTO attachment_lifecycle_v5_audit
        (queue_domain, object_key, storage_namespace, action, reason, actor, details)
    VALUES ('ordinary', v_p.object_key, v_p.storage_namespace, 'delete_intent',
            p_reason, current_user,
            jsonb_build_object('pending_session', true, 'mode', p_mode,
                               'creator', v_p.creator_user_id,
                               'job_state', CASE WHEN v_ready THEN 'ready'
                                                 ELSE 'blocked' END));
END;
$$;

CREATE OR REPLACE FUNCTION fn_v5_intent_ordinary_delete(p_path text, p_reason text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a attachment%ROWTYPE; v_p attach_pending%ROWTYPE; v_bucket text;
BEGIN
    PERFORM fn_v5_lock_ordinary_upload(p_path);
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
    -- W3-01：锁后资格复核（02 §4——取锁前的候选查询不构成授权）：
    -- 绑定事务与 intent 在同一对象锁上线性化，先提交者唯一胜出。
    IF v_a.group_file_id IS NOT NULL
       AND EXISTS (SELECT 1 FROM group_file gf WHERE gf.id = v_a.group_file_id) THEN
        RAISE EXCEPTION 'v5 intent blocked by live group_file binding'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM user_collect uc
               WHERE uc.attach_file_hash256 <> '' AND uc.attach_file_hash256 = v_a.file_hash256
                 AND uc.status = 1) THEN
        RAISE EXCEPTION 'v5 intent blocked by collect reference'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM moment_post mp
               WHERE position(v_a.path in mp.media::text) > 0) THEN
        RAISE EXCEPTION 'v5 intent blocked by moment media reference'
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

CREATE OR REPLACE FUNCTION fn_v5_intent_purged_delete(p_path text, p_reason text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a attachment%ROWTYPE; v_ready boolean;
BEGIN
    PERFORM fn_v5_lock_ordinary_upload(p_path);
    SELECT * INTO v_a FROM attachment WHERE path = p_path FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 purged intent unknown attachment' USING ERRCODE = 'P0002';
    END IF;
    IF v_a.lifecycle_state <> 'live' THEN
        RAISE EXCEPTION 'v5 purged intent non-live asset' USING ERRCODE = '23510';
    END IF;
    IF NOT EXISTS (
        SELECT 1 FROM enterprise_attachment_retention ear
        WHERE ear.attachment_id = v_a.id AND ear.purge_state = 'purged'
    ) THEN
        RAISE EXCEPTION 'v5 purged intent requires EAR purge terminal state'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path = p_path) THEN
        RAISE EXCEPTION 'v5 intent job already exists (tombstone)'
            USING ERRCODE = '23505';
    END IF;
    PERFORM fn_v5_lock_identity(v_a.storage_driver, v_a.storage_namespace,
                                v_a.storage_container, v_a.path);
    PERFORM pg_advisory_xact_lock(hashtextextended(v_a.path, 168));
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
        RAISE EXCEPTION 'v5 purged intent lost eligibility race' USING ERRCODE = '40001';
    END IF;
    v_ready := v_a.storage_driver IS NOT NULL AND v_a.storage_namespace IS NOT NULL
        AND v_a.storage_container IS NOT NULL
        AND v_a.storage_locator_sha256 ~ '^[0-9a-f]{64}$';
    INSERT INTO attachment_object_deletion
        (path, bucket, storage_driver, storage_namespace, storage_container,
         storage_locator_sha256, job_state, next_retry_at, last_error_code)
    VALUES (v_a.path, v_a.url, v_a.storage_driver, v_a.storage_namespace,
            v_a.storage_container, v_a.storage_locator_sha256,
            CASE WHEN v_ready THEN 'ready' ELSE 'blocked' END,
            CASE WHEN v_ready THEN now() ELSE now() END,
            CASE WHEN v_ready THEN NULL ELSE 'legacy_location_unverified' END);
    INSERT INTO attachment_lifecycle_v5_audit
        (queue_domain, object_key, storage_namespace, action, reason, actor, details)
    VALUES ('ordinary', v_a.path, v_a.storage_namespace, 'delete_intent',
            p_reason, current_user,
            jsonb_build_object('attachment_id', v_a.id, 'purge_domain', 'ear',
                               'job_state', CASE WHEN v_ready THEN 'ready'
                                                  ELSE 'blocked' END));
END;
$$;

CREATE OR REPLACE FUNCTION fn_v5_intent_eb_delete(p_org bigint, p_ws bigint,
                                                  p_asset bigint, p_reason text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a enterprise_asset%ROWTYPE; v_ready boolean;
BEGIN
    PERFORM fn_v5_lock_eb_upload(p_org,p_ws,p_asset);
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
    -- W4-04 双向查重（02 §4）：普通队列已有同物理身份（含 legacy blocked
    -- 接管行）时拒绝——身份永不复用。
    v_ready := v_a.storage_driver IS NOT NULL AND v_a.storage_driver IN ('garage', 'disk')
        AND v_a.storage_namespace IS NOT NULL AND v_a.storage_namespace <> ''
        AND v_a.storage_container IS NOT NULL AND v_a.storage_container <> ''
        AND v_a.storage_locator_sha256 ~ '^[0-9a-f]{64}$';
    INSERT INTO enterprise_asset_delete_queue
        (organization_id, workspace_id, asset_id, object_key,
         client_upload_id, upload_registration,
         storage_driver, storage_namespace, storage_container,
         storage_locator_sha256, job_state, next_retry_at, last_error_code)
    VALUES (p_org, p_ws, p_asset, v_a.object_key,
            v_a.client_upload_id, v_a.upload_registration,
            v_a.storage_driver, v_a.storage_namespace, v_a.storage_container,
            v_a.storage_locator_sha256,
            CASE WHEN v_ready THEN 'ready' ELSE 'blocked' END,
            CASE WHEN v_ready THEN now() ELSE now() END,
            CASE WHEN v_ready THEN NULL ELSE 'legacy_location_unverified' END);
    INSERT INTO attachment_lifecycle_v5_audit
        (queue_domain, object_key, storage_namespace, action, reason, actor, details)
    VALUES ('enterprise', v_a.object_key, v_a.storage_namespace, 'delete_intent',
            p_reason, current_user,
            jsonb_build_object('org', p_org, 'ws', p_ws, 'asset', p_asset,
                               'job_state', CASE WHEN v_ready THEN 'ready'
                                                  ELSE 'blocked' END));
END;
$$;

ALTER FUNCTION fn_v5_lock_ordinary_upload(text) OWNER TO v5_fn_owner;
ALTER FUNCTION fn_v5_lock_eb_upload(bigint,bigint,bigint) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_lock_ordinary_upload(text) FROM PUBLIC;
REVOKE ALL ON FUNCTION fn_v5_lock_eb_upload(bigint,bigint,bigint) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_lock_ordinary_upload(text),
    fn_v5_lock_eb_upload(bigint,bigint,bigint) TO v5_app_runtime;

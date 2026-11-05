-- Internal binding and reference guards. No application role can call bind yet.
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

ALTER TABLE public.enterprise_asset_direct_link
    ADD COLUMN binding_state text NOT NULL DEFAULT 'reserved',
    ADD COLUMN winner_fence jsonb,
    ADD COLUMN scan_receipt_sha256 text,
    ADD COLUMN scan_ruleset_digest text,
    ADD CONSTRAINT ck_eb_direct_binding_state CHECK (
        (binding_state='reserved' AND winner_fence IS NULL AND scan_receipt_sha256 IS NULL AND scan_ruleset_digest IS NULL)
        OR (binding_state='bound' AND winner_fence IS NOT NULL AND scan_receipt_sha256 IS NOT NULL
            AND scan_ruleset_digest IS NOT NULL AND jsonb_typeof(winner_fence)='object'
            AND scan_receipt_sha256 ~ '^[0-9a-f]{64}$' AND scan_ruleset_digest ~ '^[0-9a-f]{64}$'));

DO $$ BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname='eb_direct_binding_owner') THEN
        CREATE ROLE eb_direct_binding_owner NOLOGIN NOINHERIT;
    ELSIF EXISTS (SELECT 1 FROM pg_roles WHERE rolname='eb_direct_binding_owner'
                  AND (rolcanlogin OR rolinherit OR rolsuper OR rolcreatedb OR rolcreaterole
                       OR rolreplication OR rolbypassrls))
       OR EXISTS (SELECT 1 FROM pg_auth_members m JOIN pg_roles r
                  ON r.oid=m.roleid OR r.oid=m.member WHERE r.rolname='eb_direct_binding_owner') THEN
        RAISE EXCEPTION 'unsafe enterprise binding owner role' USING ERRCODE='23510';
    END IF;
END $$;
DO $$
DECLARE v_role oid := (SELECT oid FROM pg_roles WHERE rolname='eb_direct_binding_owner');
BEGIN
    IF EXISTS (
        SELECT 1 FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace
        LEFT JOIN LATERAL aclexplode(c.relacl) a ON true
        WHERE n.nspname='public' AND c.relname IN
            ('enterprise_asset','enterprise_asset_direct_link','attach_pending','attachment','attach_direct_attempt')
          AND (c.relowner=v_role OR a.grantee=v_role)
    ) OR EXISTS (
        SELECT 1 FROM pg_attribute t JOIN pg_class c ON c.oid=t.attrelid
        JOIN pg_namespace n ON n.oid=c.relnamespace CROSS JOIN LATERAL aclexplode(t.attacl) a
        WHERE n.nspname='public' AND c.relname IN
            ('enterprise_asset','enterprise_asset_direct_link','attach_pending','attachment','attach_direct_attempt')
          AND a.grantee=v_role
    ) OR EXISTS (
        SELECT 1 FROM pg_namespace n CROSS JOIN LATERAL aclexplode(n.nspacl) a
        WHERE n.nspname='public' AND a.grantee=v_role
    ) OR EXISTS (
        SELECT 1 FROM pg_proc p JOIN pg_namespace n ON n.oid=p.pronamespace
        CROSS JOIN LATERAL aclexplode(p.proacl) a
        WHERE n.nspname='public' AND p.proname IN
            ('fn_eb_direct_workspace_prelock','fn_eb_direct_scope_lock','fn_v5_lock_ordinary_upload')
          AND a.grantee=v_role
    ) THEN
        RAISE EXCEPTION 'enterprise binding owner has preexisting grants' USING ERRCODE='23510';
    END IF;
END $$;
GRANT USAGE ON SCHEMA public TO eb_direct_binding_owner;
GRANT SELECT ON public.enterprise_asset, public.enterprise_asset_direct_link,
    public.attach_pending, public.attachment, public.attach_direct_attempt TO eb_direct_binding_owner;
GRANT UPDATE (status, upload_content, version, updated_at) ON public.enterprise_asset TO eb_direct_binding_owner;
GRANT UPDATE (binding_state, winner_fence, scan_receipt_sha256, scan_ruleset_digest)
    ON public.enterprise_asset_direct_link TO eb_direct_binding_owner;
GRANT UPDATE (attempt_no) ON public.attach_direct_attempt TO eb_direct_binding_owner;
GRANT EXECUTE ON FUNCTION public.fn_eb_direct_workspace_prelock(text),
    public.fn_eb_direct_scope_lock(bigint,bigint,bigint,jsonb), public.fn_v5_lock_ordinary_upload(text)
    TO eb_direct_binding_owner;

-- The application verifies the Worker HMAC before calling this internal-only
-- function. This transaction checks the signed deadline and immutable winner.
CREATE FUNCTION public.fn_eb_direct_bind(p_org bigint, p_workspace bigint, p_asset bigint,
    p_subject jsonb, p_fence jsonb, p_receipt_sha text, p_ruleset_sha text, p_receipt_expiry bigint)
RETURNS bigint LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
DECLARE
    v_key text := p_fence->>'object_key';
    v_pending public.attach_pending%ROWTYPE;
    v_attachment public.attachment%ROWTYPE;
    v_attempt public.attach_direct_attempt%ROWTYPE;
    v_asset public.enterprise_asset%ROWTYPE;
    v_link public.enterprise_asset_direct_link%ROWTYPE;
    v_content jsonb;
BEGIN
    IF v_key IS NULL OR coalesce(p_receipt_sha,'') !~ '^[0-9a-f]{64}$'
       OR coalesce(p_ruleset_sha,'') !~ '^[0-9a-f]{64}$' OR p_receipt_expiry IS NULL THEN
        RAISE EXCEPTION 'invalid enterprise direct proof' USING ERRCODE='23510';
    END IF;
    PERFORM public.fn_eb_direct_workspace_prelock(v_key);
    PERFORM public.fn_v5_lock_ordinary_upload(v_key);
    SELECT * INTO v_pending FROM public.attach_pending WHERE object_key=v_key;
    IF NOT FOUND OR v_pending.direct_state <> 'CONFIRMED' OR v_pending.session_state <> 'confirmed' THEN
        RAISE EXCEPTION 'enterprise direct winner unavailable' USING ERRCODE='23510';
    END IF;
    SELECT * INTO v_attachment FROM public.attachment WHERE id=v_pending.confirmed_attachment_id AND path=v_key;
    IF NOT FOUND THEN RAISE EXCEPTION 'enterprise direct attachment missing' USING ERRCODE='23510'; END IF;
    SELECT * INTO v_attempt FROM public.attach_direct_attempt
     WHERE object_key=v_key AND attempt_state='WINNER' FOR SHARE;
    IF NOT FOUND THEN RAISE EXCEPTION 'enterprise direct attempt missing' USING ERRCODE='23510'; END IF;
    SELECT * INTO v_asset FROM public.enterprise_asset
     WHERE id=p_asset AND organization_id=p_org AND workspace_id=p_workspace FOR UPDATE;
    IF NOT FOUND THEN RAISE EXCEPTION 'enterprise direct asset missing' USING ERRCODE='23510'; END IF;
    SELECT * INTO v_link FROM public.enterprise_asset_direct_link
     WHERE asset_id=p_asset AND object_key=v_key FOR UPDATE;
    IF NOT FOUND OR v_link.organization_id IS DISTINCT FROM p_org
       OR v_link.workspace_id IS DISTINCT FROM p_workspace OR v_link.subject IS DISTINCT FROM p_subject
       OR v_asset.object_key IS DISTINCT FROM v_key
       OR v_asset.upload_registration->>'protocol' IS DISTINCT FROM 'direct-v1'
       OR v_asset.upload_registration->'subject' IS DISTINCT FROM p_subject
       OR v_asset.conversation_id IS DISTINCT FROM v_link.conversation_id
       OR v_asset.lifecycle_state <> 'live' OR v_asset.pending_object_delete
       OR v_asset.status NOT IN ('pending_confirm','active') THEN
        RAISE EXCEPTION 'enterprise direct scope mismatch' USING ERRCODE='23510';
    END IF;
    IF NOT public.fn_eb_direct_scope_lock(p_org,p_workspace,v_link.conversation_id,p_subject) THEN
        RAISE EXCEPTION 'enterprise direct subject unauthorized' USING ERRCODE='23510';
    END IF;
    IF v_attachment.lifecycle_state <> 'live' OR v_attachment.status < 0
       OR v_pending.creator_user_id IS DISTINCT FROM v_attachment.creator_user_id
       OR v_pending.scope IS DISTINCT FROM v_attachment.scope
       OR (v_pending.storage_driver,v_pending.storage_namespace,v_pending.storage_container,v_pending.storage_locator_sha256)
          IS DISTINCT FROM (v_attachment.storage_driver,v_attachment.storage_namespace,v_attachment.storage_container,v_attachment.storage_locator_sha256)
       OR (v_asset.storage_driver,v_asset.storage_namespace,v_asset.storage_container,v_asset.storage_locator_sha256)
          IS DISTINCT FROM (v_attachment.storage_driver,v_attachment.storage_namespace,v_attachment.storage_container,v_attachment.storage_locator_sha256)
       OR (v_attempt.target_driver,v_attempt.target_namespace,v_attempt.target_container,v_attempt.target_key)
          IS DISTINCT FROM (v_attachment.storage_driver,v_attachment.storage_namespace,v_attachment.storage_container,v_attachment.storage_physical_key)
       OR v_attempt.proof_sha256 IS DISTINCT FROM v_attachment.file_hash256
       OR v_attempt.proof_size IS DISTINCT FROM v_attachment.size
       OR v_asset.object_hash IS DISTINCT FROM v_attachment.file_hash256
       OR v_asset.size_bytes IS DISTINCT FROM v_attachment.size
       OR v_asset.mime IS DISTINCT FROM v_attachment.mime_type
       OR v_pending.direct_expected->>'sha256' IS DISTINCT FROM v_attachment.file_hash256
       OR v_pending.direct_expected->>'size_bytes' IS DISTINCT FROM v_attachment.size::text
       OR v_pending.direct_expected->>'mime' IS DISTINCT FROM v_attachment.mime_type THEN
        RAISE EXCEPTION 'enterprise direct winner mismatch' USING ERRCODE='23510';
    END IF;
    IF p_fence->>'attachment_id' IS DISTINCT FROM v_attachment.id::text
       OR p_fence->>'attempt_no' IS DISTINCT FROM v_attempt.attempt_no::text
       OR p_fence->>'dispatch_cas_uuid' IS DISTINCT FROM v_attempt.dispatch_cas_uuid::text
       OR p_fence->>'creator_user_id' IS DISTINCT FROM v_pending.creator_user_id::text
       OR p_fence->>'scope' IS DISTINCT FROM v_pending.scope
       OR p_fence->'upload_registration' IS DISTINCT FROM v_pending.upload_registration
       OR p_fence->>'storage_driver' IS DISTINCT FROM v_attachment.storage_driver
       OR p_fence->>'storage_namespace' IS DISTINCT FROM v_attachment.storage_namespace
       OR p_fence->>'storage_container' IS DISTINCT FROM v_attachment.storage_container
       OR p_fence->>'storage_locator_sha256' IS DISTINCT FROM v_attachment.storage_locator_sha256
       OR p_fence->>'storage_physical_key' IS DISTINCT FROM v_attachment.storage_physical_key
       OR p_fence->>'file_hash256' IS DISTINCT FROM v_attachment.file_hash256
       OR p_fence->>'size' IS DISTINCT FROM v_attachment.size::text
       OR p_fence->>'mime_type' IS DISTINCT FROM v_attachment.mime_type THEN
        RAISE EXCEPTION 'enterprise direct scan fence mismatch' USING ERRCODE='23510';
    END IF;
    IF to_timestamp(p_receipt_expiry) <= clock_timestamp() THEN
        RAISE EXCEPTION 'enterprise direct scan expired' USING ERRCODE='23510';
    END IF;
    IF v_link.binding_state='bound' THEN
        IF v_asset.status <> 'active' OR v_link.winner_fence IS DISTINCT FROM p_fence THEN
            RAISE EXCEPTION 'enterprise direct binding conflict' USING ERRCODE='23510';
        END IF;
        RETURN p_asset;
    END IF;
    IF v_link.binding_state <> 'reserved' OR v_asset.status <> 'pending_confirm' THEN
        RAISE EXCEPTION 'enterprise direct binding state mismatch' USING ERRCODE='23510';
    END IF;
    v_content := jsonb_build_object('version','1','sha256',v_attachment.file_hash256,
        'size_bytes',v_attachment.size::text,'mime',v_attachment.mime_type);
    UPDATE public.enterprise_asset_direct_link SET binding_state='bound', winner_fence=p_fence,
        scan_receipt_sha256=p_receipt_sha, scan_ruleset_digest=p_ruleset_sha WHERE asset_id=p_asset;
    UPDATE public.enterprise_asset SET upload_content=v_content, status='active',
        version=version+1, updated_at=now() WHERE id=p_asset;
    IF NOT public.fn_eb_direct_scope_lock(p_org,p_workspace,v_link.conversation_id,p_subject)
       OR to_timestamp(p_receipt_expiry) <= clock_timestamp() THEN
        RAISE EXCEPTION 'enterprise direct authorization or scan expired' USING ERRCODE='23510';
    END IF;
    RETURN p_asset;
END;
$$;
ALTER FUNCTION public.fn_eb_direct_bind(bigint,bigint,bigint,jsonb,jsonb,text,text,bigint)
    OWNER TO eb_direct_binding_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_bind(bigint,bigint,bigint,jsonb,jsonb,text,text,bigint) FROM PUBLIC;

-- A runtime role cannot activate a direct EB asset without the internal bind.
CREATE FUNCTION public.fn_eb_direct_activation_guard() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
BEGIN
    -- Until a direct-aware retirement transaction exists, no caller may
    -- release the EB reference by editing status or lifecycle directly.
    IF TG_OP='UPDATE' AND OLD.upload_registration->>'protocol'='direct-v1'
       AND (NEW.status='deleted' OR NEW.lifecycle_state IN ('delete_intent','deleted'))
       AND (OLD.status IS DISTINCT FROM NEW.status OR OLD.lifecycle_state IS DISTINCT FROM NEW.lifecycle_state) THEN
        RAISE EXCEPTION 'enterprise direct retirement unavailable' USING ERRCODE='23510';
    END IF;
    IF NEW.status='active' AND NEW.upload_registration->>'protocol'='direct-v1' THEN
        IF TG_OP='INSERT' THEN
            RAISE EXCEPTION 'enterprise direct scan binding required' USING ERRCODE='23510';
        END IF;
        IF (OLD.status IS DISTINCT FROM NEW.status
            OR OLD.upload_registration IS DISTINCT FROM NEW.upload_registration)
           AND NOT EXISTS (SELECT 1 FROM public.enterprise_asset_direct_link l
                           WHERE l.asset_id=NEW.id AND l.binding_state='bound') THEN
            RAISE EXCEPTION 'enterprise direct scan binding required' USING ERRCODE='23510';
        END IF;
    END IF;
    RETURN NEW;
END;
$$;
ALTER FUNCTION public.fn_eb_direct_activation_guard() OWNER TO eb_direct_scope_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_activation_guard() FROM PUBLIC;
CREATE TRIGGER eb_direct_activation_guard BEFORE INSERT OR UPDATE OF status, upload_registration, lifecycle_state ON public.enterprise_asset
    FOR EACH ROW EXECUTE FUNCTION public.fn_eb_direct_activation_guard();

-- A live EB reference and ordinary GC linearize on the attachment row lock.
CREATE FUNCTION public.fn_eb_direct_gc_guard() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
BEGIN
    IF NEW.lifecycle_state IN ('delete_intent','deleted')
       AND OLD.lifecycle_state IS DISTINCT FROM NEW.lifecycle_state
       AND EXISTS (SELECT 1 FROM public.enterprise_asset_direct_link l
                   JOIN public.enterprise_asset e ON e.id=l.asset_id
                   WHERE l.object_key=OLD.path
                     AND (e.status IS DISTINCT FROM 'deleted' OR e.lifecycle_state IS DISTINCT FROM 'deleted')) THEN
        RAISE EXCEPTION 'enterprise direct reference still live' USING ERRCODE='23510';
    END IF;
    RETURN NEW;
END;
$$;
ALTER FUNCTION public.fn_eb_direct_gc_guard() OWNER TO eb_direct_scope_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_gc_guard() FROM PUBLIC;
CREATE TRIGGER eb_direct_gc_guard BEFORE UPDATE OF lifecycle_state ON public.attachment
    FOR EACH ROW EXECUTE FUNCTION public.fn_eb_direct_gc_guard();

-- Until direct-aware deletion exists, the legacy EB queue must not delete the
-- winner using its logical key.
CREATE FUNCTION public.fn_eb_direct_legacy_queue_guard() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
BEGIN
    IF EXISTS (SELECT 1 FROM public.enterprise_asset_direct_link l WHERE l.asset_id=NEW.asset_id) THEN
        RAISE EXCEPTION 'enterprise direct deletion requires ordinary lifecycle' USING ERRCODE='23510';
    END IF;
    RETURN NEW;
END;
$$;
ALTER FUNCTION public.fn_eb_direct_legacy_queue_guard() OWNER TO eb_direct_scope_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_legacy_queue_guard() FROM PUBLIC;
CREATE TRIGGER eb_direct_legacy_queue_guard BEFORE INSERT ON public.enterprise_asset_delete_queue
    FOR EACH ROW EXECUTE FUNCTION public.fn_eb_direct_legacy_queue_guard();

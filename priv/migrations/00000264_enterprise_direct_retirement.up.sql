SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

-- The marker is visible only to the guarded trigger and the retirement owner.
-- A transaction-local setting is not an authorization boundary.
CREATE TABLE public.enterprise_direct_retire_guard (
    tx_id bigint NOT NULL,
    backend_pid integer NOT NULL,
    asset_id bigint NOT NULL,
    PRIMARY KEY (tx_id, backend_pid, asset_id)
);
REVOKE ALL ON public.enterprise_direct_retire_guard FROM PUBLIC;
CREATE TABLE public.enterprise_direct_purge_wait (
    organization_id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    item_kind text NOT NULL CHECK (item_kind IN ('message','asset')),
    item_id bigint NOT NULL,
    attempts integer NOT NULL DEFAULT 1 CHECK (attempts > 0),
    next_retry_at timestamptz NOT NULL,
    PRIMARY KEY (organization_id,workspace_id,item_kind,item_id)
);
REVOKE ALL ON public.enterprise_direct_purge_wait FROM PUBLIC;
CREATE TABLE public.enterprise_direct_retirement_audit (
    asset_id bigint PRIMARY KEY,
    organization_id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    object_key text NOT NULL,
    binding_state text NOT NULL CHECK (binding_state IN ('reserved','bound')),
    scan_receipt_sha256 text,
    scan_ruleset_digest text,
    retired_at timestamptz NOT NULL DEFAULT clock_timestamp(),
    CHECK ((binding_state='reserved' AND scan_receipt_sha256 IS NULL AND scan_ruleset_digest IS NULL)
       OR (binding_state='bound' AND scan_receipt_sha256 IS NOT NULL AND scan_ruleset_digest IS NOT NULL
           AND scan_receipt_sha256 ~ '^[0-9a-f]{64}$' AND scan_ruleset_digest ~ '^[0-9a-f]{64}$'))
);
REVOKE ALL ON public.enterprise_direct_retirement_audit FROM PUBLIC;

DO $$ BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname='eb_direct_retire_owner') THEN
        CREATE ROLE eb_direct_retire_owner NOLOGIN NOINHERIT;
    ELSIF EXISTS (SELECT 1 FROM pg_roles WHERE rolname='eb_direct_retire_owner'
                  AND (rolcanlogin OR rolinherit OR rolsuper OR rolcreatedb OR rolcreaterole
                       OR rolreplication OR rolbypassrls))
       OR EXISTS (SELECT 1 FROM pg_auth_members m JOIN pg_roles r
                  ON r.oid=m.roleid OR r.oid=m.member WHERE r.rolname='eb_direct_retire_owner') THEN
        RAISE EXCEPTION 'unsafe enterprise retirement owner' USING ERRCODE='23510';
    END IF;
END $$;
DO $$
DECLARE v_role oid := (SELECT oid FROM pg_roles WHERE rolname='eb_direct_retire_owner');
BEGIN
    IF EXISTS (
        SELECT 1 FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace
        LEFT JOIN LATERAL aclexplode(c.relacl) a ON true
        WHERE n.nspname='public' AND (c.relowner=v_role OR a.grantee=v_role)
    ) OR EXISTS (
        SELECT 1 FROM pg_attribute t JOIN pg_class c ON c.oid=t.attrelid
        JOIN pg_namespace n ON n.oid=c.relnamespace CROSS JOIN LATERAL aclexplode(t.attacl) a
        WHERE n.nspname='public' AND a.grantee=v_role
    ) OR EXISTS (
        SELECT 1 FROM pg_namespace n CROSS JOIN LATERAL aclexplode(n.nspacl) a
        WHERE n.nspname='public' AND a.grantee=v_role
    ) OR EXISTS (
        SELECT 1 FROM pg_proc p JOIN pg_namespace n ON n.oid=p.pronamespace
        CROSS JOIN LATERAL aclexplode(p.proacl) a
        WHERE n.nspname='public' AND a.grantee=v_role
    ) THEN
        RAISE EXCEPTION 'enterprise retirement owner has preexisting grants' USING ERRCODE='23510';
    END IF;
END $$;
GRANT USAGE ON SCHEMA public TO eb_direct_retire_owner;
GRANT SELECT ON public.workspace, public.enterprise_asset, public.enterprise_asset_direct_link,
    public.attachment, public.attach_pending, public.attach_direct_attempt,
    public.enterprise_retention_hold TO eb_direct_retire_owner;
GRANT UPDATE (id) ON public.workspace TO eb_direct_retire_owner;
GRANT UPDATE (status, lifecycle_state) ON public.enterprise_asset TO eb_direct_retire_owner;
GRANT UPDATE (id) ON public.attachment TO eb_direct_retire_owner;
GRANT UPDATE (asset_id) ON public.enterprise_asset_direct_link TO eb_direct_retire_owner;
GRANT UPDATE (direct_state, session_state) ON public.attach_pending TO eb_direct_retire_owner;
GRANT UPDATE (attempt_no) ON public.attach_direct_attempt TO eb_direct_retire_owner;
GRANT DELETE ON public.enterprise_asset_direct_link TO eb_direct_retire_owner;
GRANT SELECT, INSERT, DELETE ON public.enterprise_direct_retire_guard TO eb_direct_retire_owner;
GRANT SELECT, INSERT, UPDATE, DELETE ON public.enterprise_direct_purge_wait TO eb_direct_retire_owner;
GRANT SELECT, INSERT ON public.enterprise_direct_retirement_audit TO eb_direct_retire_owner;
GRANT SELECT ON public.enterprise_message TO eb_direct_retire_owner;
GRANT SELECT ON public.enterprise_direct_retire_guard TO eb_direct_scope_owner;
GRANT EXECUTE ON FUNCTION public.fn_v5_lock_ordinary_upload(text),
    public.fn_v5_intent_ordinary_delete(text,text) TO eb_direct_retire_owner;

CREATE OR REPLACE FUNCTION public.fn_eb_direct_activation_guard() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
BEGIN
    IF TG_OP='UPDATE' AND OLD.upload_registration->>'protocol'='direct-v1'
       AND (NEW.status='deleted' OR NEW.lifecycle_state IN ('delete_intent','deleted'))
       AND (OLD.status IS DISTINCT FROM NEW.status OR OLD.lifecycle_state IS DISTINCT FROM NEW.lifecycle_state)
       AND NOT EXISTS (SELECT 1 FROM public.enterprise_direct_retire_guard
                       WHERE tx_id=txid_current() AND backend_pid=pg_backend_pid() AND asset_id=OLD.id) THEN
        RAISE EXCEPTION 'enterprise direct retirement requires guarded transaction' USING ERRCODE='23510';
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

-- Called only within the existing workspace-locked purge transaction. Failure
-- rolls back both the metadata retirement and ordinary deletion intent.
CREATE FUNCTION public.fn_eb_direct_retire(p_org bigint, p_workspace bigint, p_asset bigint)
RETURNS text LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
DECLARE
    v_key text;
    v_asset public.enterprise_asset%ROWTYPE;
    v_link public.enterprise_asset_direct_link%ROWTYPE;
    v_attachment public.attachment%ROWTYPE;
    v_pending public.attach_pending%ROWTYPE;
    v_attempt public.attach_direct_attempt%ROWTYPE;
BEGIN
    IF coalesce(current_setting('imboy.enterprise_purge',true),'') <> 'on' THEN
        RAISE EXCEPTION 'enterprise purge context required' USING ERRCODE='23510';
    END IF;
    PERFORM 1 FROM public.workspace WHERE organization_id=p_org AND id=p_workspace FOR UPDATE;
    IF NOT FOUND THEN RAISE EXCEPTION 'workspace missing' USING ERRCODE='P0002'; END IF;
    SELECT object_key INTO v_key FROM public.enterprise_asset_direct_link
     WHERE asset_id=p_asset AND organization_id=p_org AND workspace_id=p_workspace;
    IF NOT FOUND THEN RAISE EXCEPTION 'enterprise direct link missing' USING ERRCODE='P0002'; END IF;
    PERFORM public.fn_v5_lock_ordinary_upload(v_key);
    SELECT * INTO v_attachment FROM public.attachment WHERE path=v_key FOR UPDATE;
    SELECT * INTO v_asset FROM public.enterprise_asset
     WHERE id=p_asset AND organization_id=p_org AND workspace_id=p_workspace FOR UPDATE;
    SELECT * INTO v_link FROM public.enterprise_asset_direct_link WHERE asset_id=p_asset FOR UPDATE;
    IF NOT FOUND OR v_link.object_key IS DISTINCT FROM v_key
       OR v_asset.id IS NULL OR v_asset.object_key IS DISTINCT FROM v_key
       OR v_asset.upload_registration->>'protocol' IS DISTINCT FROM 'direct-v1'
       OR v_asset.lifecycle_state <> 'live' THEN
        RAISE EXCEPTION 'enterprise direct retirement scope mismatch' USING ERRCODE='23510';
    END IF;
    IF (v_asset.message_id IS NOT NULL AND v_asset.retain_until IS NULL)
       OR v_asset.retain_until > clock_timestamp()
       OR EXISTS (SELECT 1 FROM public.enterprise_retention_hold h
                   WHERE h.organization_id=p_org AND h.workspace_id=p_workspace AND h.released_at IS NULL
                     AND (h.scope_type='workspace'
                          OR (h.scope_type='conversation' AND h.scope_conversation_id=v_asset.conversation_id)
                          OR (h.scope_type='message' AND h.scope_message_id=v_asset.message_id))) THEN
        RAISE EXCEPTION 'enterprise direct retention active' USING ERRCODE='23510';
    END IF;
    SELECT * INTO v_pending FROM public.attach_pending WHERE object_key=v_key;
    IF NOT FOUND THEN RAISE EXCEPTION 'enterprise direct session missing' USING ERRCODE='23510'; END IF;
    IF v_link.binding_state='reserved' THEN
        -- With no attempt there was never an upload capability or physical key.
        IF v_asset.status <> 'pending_confirm' OR v_attachment.id IS NOT NULL
           OR v_pending.direct_state NOT IN ('RESERVED','DECLARED')
           OR v_pending.session_state <> 'registered' OR v_pending.session_deadline IS NULL
           OR v_pending.session_deadline > clock_timestamp()
           OR EXISTS (SELECT 1 FROM public.attach_direct_attempt WHERE object_key=v_key) THEN
            RAISE EXCEPTION 'enterprise direct pending session still active' USING ERRCODE='23510';
        END IF;
        UPDATE public.attach_pending SET direct_state='EXPIRED',session_state='expired'
         WHERE object_key=v_key;
    ELSIF v_link.binding_state='bound' THEN
        SELECT * INTO v_attempt FROM public.attach_direct_attempt
         WHERE object_key=v_key AND attempt_state='WINNER' FOR SHARE;
        IF NOT FOUND THEN RAISE EXCEPTION 'enterprise direct winner missing' USING ERRCODE='23510'; END IF;
        IF v_pending.upload_expires_at IS NULL OR v_pending.upload_expires_at > clock_timestamp() THEN
            RAISE EXCEPTION 'enterprise direct capability not exhausted' USING ERRCODE='23510';
        END IF;
        IF v_asset.status <> 'active' OR v_attachment.id IS NULL OR v_attachment.lifecycle_state <> 'live'
           OR v_pending.direct_state <> 'CONFIRMED'
           OR v_pending.confirmed_attachment_id IS DISTINCT FROM v_attachment.id
           OR (v_attempt.target_driver,v_attempt.target_namespace,v_attempt.target_container,v_attempt.target_key)
              IS DISTINCT FROM (v_attachment.storage_driver,v_attachment.storage_namespace,
                                v_attachment.storage_container,v_attachment.storage_physical_key)
           OR v_link.winner_fence->>'attachment_id' IS DISTINCT FROM v_attachment.id::text
           OR v_link.winner_fence->>'attempt_no' IS DISTINCT FROM v_attempt.attempt_no::text
           OR v_link.winner_fence->>'dispatch_cas_uuid' IS DISTINCT FROM v_attempt.dispatch_cas_uuid::text
           OR v_link.winner_fence->>'storage_physical_key' IS DISTINCT FROM v_attachment.storage_physical_key
           OR v_link.winner_fence->>'storage_locator_sha256' IS DISTINCT FROM v_attachment.storage_locator_sha256 THEN
            RAISE EXCEPTION 'enterprise direct winner identity mismatch' USING ERRCODE='23510';
        END IF;
    ELSE
        RAISE EXCEPTION 'enterprise direct binding state invalid' USING ERRCODE='23510';
    END IF;
    INSERT INTO public.enterprise_direct_retire_guard VALUES (txid_current(),pg_backend_pid(),p_asset);
    INSERT INTO public.enterprise_direct_retirement_audit
        (asset_id,organization_id,workspace_id,object_key,binding_state,scan_receipt_sha256,scan_ruleset_digest)
    VALUES (v_asset.id,p_org,p_workspace,v_key,v_link.binding_state,
            v_link.scan_receipt_sha256,v_link.scan_ruleset_digest);
    UPDATE public.enterprise_asset SET status='deleted', lifecycle_state='deleted' WHERE id=p_asset;
    DELETE FROM public.enterprise_asset_direct_link WHERE asset_id=p_asset;
    IF v_link.binding_state='bound' THEN
        PERFORM public.fn_v5_intent_ordinary_delete(v_key, 'enterprise retention purge');
    END IF;
    DELETE FROM public.enterprise_direct_retire_guard
     WHERE tx_id=txid_current() AND backend_pid=pg_backend_pid() AND asset_id=p_asset;
    RETURN v_key;
END;
$$;
ALTER FUNCTION public.fn_eb_direct_retire(bigint,bigint,bigint) OWNER TO eb_direct_retire_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_retire(bigint,bigint,bigint) FROM PUBLIC;
CREATE FUNCTION public.fn_eb_direct_purge_candidates(p_org bigint,p_workspace bigint,
    p_message_ids bigint[],p_asset_ids bigint[]) RETURNS TABLE(asset_id bigint,object_key text,binding_state text)
LANGUAGE sql SECURITY DEFINER SET search_path = public, pg_temp AS $$
    SELECT a.id,l.object_key,l.binding_state FROM public.enterprise_asset a
    LEFT JOIN public.enterprise_asset_direct_link l ON l.asset_id=a.id
    WHERE a.organization_id=p_org AND a.workspace_id=p_workspace
      AND (a.upload_registration->>'protocol'='direct-v1' OR l.asset_id IS NOT NULL)
      AND (a.message_id=ANY(p_message_ids) OR a.id=ANY(p_asset_ids))
    ORDER BY l.object_key,a.id
$$;
ALTER FUNCTION public.fn_eb_direct_purge_candidates(bigint,bigint,bigint[],bigint[])
    OWNER TO eb_direct_retire_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_purge_candidates(bigint,bigint,bigint[],bigint[]) FROM PUBLIC;
-- Keep this proof identical to attachment_reference_guard_repo; the purge
-- role must not receive unrestricted UPDATE on gc_verified.
CREATE FUNCTION public.fn_eb_direct_refresh_gc(p_org bigint,p_workspace bigint,p_asset bigint)
RETURNS boolean LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
DECLARE v_key text; v_proven boolean;
BEGIN
    IF coalesce(current_setting('imboy.enterprise_purge',true),'') <> 'on' THEN
        RAISE EXCEPTION 'enterprise purge context required' USING ERRCODE='23510';
    END IF;
    SELECT l.object_key INTO v_key FROM public.enterprise_asset_direct_link l
     JOIN public.enterprise_asset e ON e.id=l.asset_id
     WHERE l.organization_id=p_org AND l.workspace_id=p_workspace AND l.asset_id=p_asset
       AND e.organization_id=p_org AND e.workspace_id=p_workspace;
    IF NOT FOUND THEN RAISE EXCEPTION 'enterprise direct link missing' USING ERRCODE='P0002'; END IF;
    PERFORM public.fn_v5_lock_ordinary_upload(v_key);
    UPDATE public.attachment a SET gc_verified = (
        a.cipher IS NULL AND a.storage_driver IS NOT NULL AND a.storage_namespace IS NOT NULL
        AND a.storage_container IS NOT NULL AND a.storage_locator_sha256 IS NOT NULL
        AND (a.group_file_id IS NULL OR NOT EXISTS (SELECT 1 FROM public.group_file gf WHERE gf.id=a.group_file_id))
        AND NOT EXISTS (SELECT 1 FROM public.user_collect uc WHERE uc.attach_file_hash256 <> ''
            AND a.file_hash256=ANY(string_to_array(uc.attach_file_hash256,',')) AND uc.status=1)
        AND NOT EXISTS (SELECT 1 FROM public.group_album_photo gap WHERE gap.photo_url=a.path AND gap.status=1)
        AND NOT public.fn_v5_channel_reference_blocks(a.path)
        AND NOT public.fn_v5_forward_reference_blocks(a.path)
        AND NOT public.fn_v5_moment_reference_blocks(a.path)
        AND NOT public.fn_talk_record_war_blocks(a.id)
        AND NOT EXISTS (SELECT 1 FROM public.enterprise_attachment_retention ear WHERE ear.attachment_id=a.id
            AND (ear.hold_state='held' OR ear.purge_state IS DISTINCT FROM 'purged'))
        AND NOT EXISTS (SELECT 1 FROM public.talk_record_attachment_ref trar WHERE trar.attachment_id=a.id
            AND trar.state='bound' AND (trar.lease_until IS NULL OR trar.lease_until>now()))
    ) WHERE a.path=v_key RETURNING a.gc_verified INTO v_proven;
    IF NOT FOUND THEN RAISE EXCEPTION 'confirmed attachment missing' USING ERRCODE='P0002'; END IF;
    RETURN v_proven;
END;
$$;
ALTER FUNCTION public.fn_eb_direct_refresh_gc(bigint,bigint,bigint) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_refresh_gc(bigint,bigint,bigint) FROM PUBLIC;
CREATE FUNCTION public.fn_eb_direct_purge_deferred(p_org bigint,p_workspace bigint,p_kind text,p_id bigint)
RETURNS boolean LANGUAGE sql SECURITY DEFINER SET search_path = public, pg_temp AS $$
    SELECT EXISTS (SELECT 1 FROM public.enterprise_direct_purge_wait
                   WHERE organization_id=p_org AND workspace_id=p_workspace
                     AND item_kind=p_kind AND item_id=p_id AND next_retry_at>clock_timestamp())
$$;
ALTER FUNCTION public.fn_eb_direct_purge_deferred(bigint,bigint,text,bigint) OWNER TO eb_direct_retire_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_purge_deferred(bigint,bigint,text,bigint) FROM PUBLIC;
CREATE FUNCTION public.fn_eb_direct_purge_defer(p_org bigint,p_workspace bigint,p_kind text,p_id bigint)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
BEGIN
    IF coalesce(current_setting('imboy.enterprise_purge',true),'') <> 'on'
       OR NOT ((p_kind='message' AND EXISTS (SELECT 1 FROM public.enterprise_message
                   WHERE organization_id=p_org AND workspace_id=p_workspace AND id=p_id))
           OR (p_kind='asset' AND EXISTS (SELECT 1 FROM public.enterprise_asset
                   WHERE organization_id=p_org AND workspace_id=p_workspace AND id=p_id))) THEN
        RAISE EXCEPTION 'invalid enterprise purge deferral' USING ERRCODE='23510';
    END IF;
    INSERT INTO public.enterprise_direct_purge_wait
        (organization_id,workspace_id,item_kind,item_id,next_retry_at)
    VALUES (p_org,p_workspace,p_kind,p_id,clock_timestamp()+interval '1 minute')
    ON CONFLICT (organization_id,workspace_id,item_kind,item_id)
    DO UPDATE SET attempts=least(public.enterprise_direct_purge_wait.attempts::bigint+1,2147483647)::integer,
                  next_retry_at=clock_timestamp()+interval '1 minute';
END;
$$;
ALTER FUNCTION public.fn_eb_direct_purge_defer(bigint,bigint,text,bigint) OWNER TO eb_direct_retire_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_purge_defer(bigint,bigint,text,bigint) FROM PUBLIC;
CREATE FUNCTION public.fn_eb_direct_purge_clear(p_org bigint,p_workspace bigint,p_kind text,p_id bigint)
RETURNS void LANGUAGE sql SECURITY DEFINER SET search_path = public, pg_temp AS $$
    DELETE FROM public.enterprise_direct_purge_wait WHERE organization_id=p_org
       AND workspace_id=p_workspace AND item_kind=p_kind AND item_id=p_id
$$;
ALTER FUNCTION public.fn_eb_direct_purge_clear(bigint,bigint,text,bigint) OWNER TO eb_direct_retire_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_purge_clear(bigint,bigint,text,bigint) FROM PUBLIC;
DO $$ BEGIN
    IF to_regrole('imboy_enterprise_purge_worker') IS NOT NULL THEN
        GRANT EXECUTE ON FUNCTION public.fn_eb_direct_retire(bigint,bigint,bigint),
            public.fn_eb_direct_purge_candidates(bigint,bigint,bigint[],bigint[]),
            public.fn_eb_direct_refresh_gc(bigint,bigint,bigint),
            public.fn_eb_direct_purge_deferred(bigint,bigint,text,bigint),
            public.fn_eb_direct_purge_defer(bigint,bigint,text,bigint),
            public.fn_eb_direct_purge_clear(bigint,bigint,text,bigint)
            TO imboy_enterprise_purge_worker;
    END IF;
END $$;

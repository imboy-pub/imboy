-- Internal reservation only; no grant, confirmation or public route is added here.
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

CREATE TABLE public.enterprise_asset_direct_link (
    asset_id bigint PRIMARY KEY REFERENCES public.enterprise_asset(id),
    object_key text NOT NULL UNIQUE REFERENCES public.attach_pending(object_key),
    organization_id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    conversation_id bigint NOT NULL,
    subject jsonb NOT NULL CHECK (jsonb_typeof(subject) = 'object'),
    created_at timestamptz NOT NULL DEFAULT now()
);
REVOKE ALL ON public.enterprise_asset_direct_link FROM PUBLIC;
GRANT SELECT, INSERT ON public.enterprise_asset_direct_link TO v5_fn_owner;

-- Row locks retain the current business scope through the calling transaction.
-- This does not authenticate a visitor token; the function remains internal.
DO $$ BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'eb_direct_scope_owner') THEN
        CREATE ROLE eb_direct_scope_owner NOLOGIN NOINHERIT;
    ELSIF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'eb_direct_scope_owner'
                  AND (rolcanlogin OR rolinherit OR rolsuper OR rolcreatedb OR rolcreaterole
                       OR rolreplication OR rolbypassrls))
       OR EXISTS (SELECT 1 FROM pg_auth_members m JOIN pg_roles r
                  ON r.oid=m.roleid OR r.oid=m.member WHERE r.rolname='eb_direct_scope_owner') THEN
        RAISE EXCEPTION 'unsafe enterprise scope owner role' USING ERRCODE = '23510';
    END IF;
END $$;
DO $$
DECLARE v_role oid := (SELECT oid FROM pg_roles WHERE rolname='eb_direct_scope_owner');
BEGIN
    IF EXISTS (
        SELECT 1 FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace
        LEFT JOIN LATERAL aclexplode(c.relacl) a ON true
        WHERE n.nspname='public' AND c.relname IN (
            'workspace','enterprise_conversation','customer_service_session','customer_service_seat',
            'customer_service_visit_token','customer_service_widget_installation',
            'organization_member','organization_business_identity',
            'organization_business_identity_assignment','enterprise_asset')
          AND (c.relowner=v_role OR a.grantee=v_role)
    ) OR EXISTS (
        SELECT 1 FROM pg_attribute t JOIN pg_class c ON c.oid=t.attrelid
        JOIN pg_namespace n ON n.oid=c.relnamespace
        CROSS JOIN LATERAL aclexplode(t.attacl) a
        WHERE n.nspname='public' AND c.relname IN (
            'workspace','enterprise_conversation','customer_service_session','customer_service_seat',
            'customer_service_visit_token','customer_service_widget_installation',
            'organization_member','organization_business_identity',
            'organization_business_identity_assignment','enterprise_asset') AND a.grantee=v_role
    ) OR EXISTS (
        SELECT 1 FROM pg_namespace n CROSS JOIN LATERAL aclexplode(n.nspacl) a
        WHERE n.nspname='public' AND a.grantee=v_role
    ) THEN
        RAISE EXCEPTION 'enterprise scope owner has preexisting grants' USING ERRCODE = '23510';
    END IF;
END $$;
GRANT USAGE ON SCHEMA public TO eb_direct_scope_owner;
GRANT SELECT ON public.enterprise_conversation, public.customer_service_session,
    public.customer_service_seat, public.customer_service_visit_token,
    public.customer_service_widget_installation, public.organization_member,
    public.organization_business_identity, public.organization_business_identity_assignment
    TO eb_direct_scope_owner;
GRANT SELECT ON public.workspace TO eb_direct_scope_owner;
GRANT SELECT ON public.enterprise_asset, public.enterprise_asset_direct_link TO eb_direct_scope_owner;
GRANT UPDATE (id) ON public.enterprise_asset TO eb_direct_scope_owner;
GRANT UPDATE (asset_id) ON public.enterprise_asset_direct_link TO eb_direct_scope_owner;
GRANT UPDATE (id) ON public.enterprise_conversation TO eb_direct_scope_owner;
GRANT UPDATE (status) ON public.customer_service_session, public.organization_member,
    public.organization_business_identity, public.organization_business_identity_assignment TO eb_direct_scope_owner;
GRANT UPDATE (enabled) ON public.customer_service_seat TO eb_direct_scope_owner;
GRANT UPDATE (revoked_at) ON public.customer_service_visit_token TO eb_direct_scope_owner;
GRANT UPDATE (status) ON public.customer_service_widget_installation TO eb_direct_scope_owner;
GRANT UPDATE (id) ON public.workspace TO eb_direct_scope_owner;

CREATE FUNCTION public.fn_eb_direct_scope_lock(p_org bigint, p_workspace bigint,
    p_conversation bigint, p_subject jsonb) RETURNS boolean
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
DECLARE
    v_actor bigint;
    v_session bigint;
    v_token bigint;
    v_installation bigint;
    v_token_expires timestamptz;
    v_token_revoked timestamptz;
    v_contact bigint;
    v_intake bigint;
    v_identity bigint;
    v_function text;
    v_kind text := p_subject->>'kind';
BEGIN
    BEGIN
        v_actor := (p_subject->>'principal')::bigint;
        IF v_kind = 'visitor_session' THEN
            v_session := (p_subject->>'session_id')::bigint;
            v_token := (p_subject->>'token_id')::bigint;
            v_installation := (p_subject->>'installation_id')::bigint;
        END IF;
    EXCEPTION WHEN invalid_text_representation OR numeric_value_out_of_range THEN
        RETURN false;
    END;
    IF v_actor IS NULL OR v_actor <= 0 OR p_subject->>'org_id' IS DISTINCT FROM p_org::text THEN
        RETURN false;
    END IF;
    PERFORM 1 FROM public.workspace WHERE id=p_workspace AND organization_id=p_org FOR KEY SHARE;
    IF NOT FOUND THEN RETURN false; END IF;
    -- Session creation takes an FK key-share lock on this row; FOR UPDATE also
    -- prevents a newer session appearing between the selection and the commit.
    SELECT contact_id, business_identity_id INTO v_contact, v_intake
      FROM public.enterprise_conversation
     WHERE id=p_conversation AND organization_id=p_org AND workspace_id=p_workspace FOR UPDATE;
    IF NOT FOUND THEN RETURN false; END IF;
    IF v_kind = 'visitor_session' THEN
        IF v_session IS NULL OR v_token IS NULL OR v_installation IS NULL
           OR v_session <= 0 OR v_token <= 0 OR v_installation <= 0 THEN RETURN false; END IF;
        PERFORM 1 FROM public.customer_service_widget_installation
         WHERE id=v_installation AND organization_id=p_org AND status='active' AND revoked_at IS NULL FOR SHARE;
        IF NOT FOUND THEN RETURN false; END IF;
        SELECT expires_at, revoked_at INTO v_token_expires, v_token_revoked FROM public.customer_service_visit_token
         WHERE id=v_token AND organization_id=p_org AND contact_id=v_actor
           AND widget_installation_id=v_installation FOR SHARE;
        IF NOT FOUND THEN RETURN false; END IF;
        -- Match visitor-read: installation/current credential before session/original token.
        PERFORM 1 FROM public.customer_service_session s
          JOIN public.customer_service_visit_token original
            ON original.organization_id=s.organization_id AND original.id=s.visit_token_id
         WHERE s.id=v_session AND s.organization_id=p_org AND s.workspace_id=p_workspace
           AND s.conversation_id=p_conversation AND s.contact_id=v_actor AND s.contact_id=v_contact
           AND original.widget_installation_id=v_installation AND original.contact_id=s.contact_id
         FOR SHARE OF s, original;
        IF NOT FOUND THEN RETURN false; END IF;
        RETURN v_token_expires > clock_timestamp()
            AND (v_token_revoked IS NULL OR v_token_revoked > clock_timestamp());
    ELSIF v_kind IS DISTINCT FROM 'seat' THEN
        RETURN false;
    END IF;
    PERFORM 1 FROM public.organization_member
     WHERE organization_id=p_org AND user_id=v_actor AND status='active' FOR SHARE;
    IF NOT FOUND THEN RETURN false; END IF;
    SELECT function_key INTO v_function FROM public.organization_business_identity
     WHERE organization_id=p_org AND id=v_intake AND status='active' FOR SHARE;
    IF NOT FOUND THEN RETURN false; END IF;
    v_identity := v_intake;
    IF v_function = 'customer_service' THEN
        SELECT business_identity_id INTO v_identity FROM public.customer_service_session
         WHERE organization_id=p_org AND workspace_id=p_workspace AND conversation_id=p_conversation
         ORDER BY id DESC LIMIT 1 FOR SHARE;
        IF NOT FOUND THEN v_identity := v_intake; END IF;
        IF v_identity IS NULL THEN RETURN false; END IF;
        PERFORM 1 FROM public.organization_business_identity
         WHERE organization_id=p_org AND id=v_identity AND function_key='customer_service'
           AND status='active' FOR SHARE;
        IF NOT FOUND THEN RETURN false; END IF;
        PERFORM 1 FROM public.customer_service_seat
         WHERE organization_id=p_org AND business_identity_id=v_identity AND enabled FOR SHARE;
        IF NOT FOUND THEN RETURN false; END IF;
    ELSIF v_function <> 'sales' THEN
        RETURN false;
    END IF;
    PERFORM 1 FROM public.organization_business_identity_assignment
     WHERE organization_id=p_org AND business_identity_id=v_identity
       AND user_id=v_actor AND status='active' FOR SHARE;
    RETURN FOUND;
END;
$$;
ALTER FUNCTION public.fn_eb_direct_scope_lock(bigint, bigint, bigint, jsonb) OWNER TO eb_direct_scope_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_scope_lock(bigint, bigint, bigint, jsonb) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_eb_direct_scope_lock(bigint, bigint, bigint, jsonb) TO v5_fn_owner;

-- Re-read the linked asset after locking it; the caller's scan snapshot is not authority.
CREATE FUNCTION public.fn_eb_direct_bound_scope_lock(p_org bigint, p_workspace bigint,
    p_conversation bigint, p_asset bigint, p_key text, p_subject jsonb) RETURNS boolean
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
BEGIN
    PERFORM 1 FROM public.workspace WHERE id=p_workspace AND organization_id=p_org FOR KEY SHARE;
    IF NOT FOUND THEN RETURN false; END IF;
    PERFORM 1 FROM public.enterprise_asset e
      JOIN public.enterprise_asset_direct_link l ON l.asset_id=e.id
     WHERE e.id=p_asset AND e.organization_id=p_org AND e.workspace_id=p_workspace
       AND e.conversation_id=p_conversation AND e.object_key=p_key
       AND e.status='pending_confirm' AND e.lifecycle_state='live'
       AND e.upload_registration->>'protocol'='direct-v1'
       AND e.upload_registration->'subject'=p_subject
       AND l.object_key=p_key AND l.organization_id=p_org AND l.workspace_id=p_workspace
       AND l.conversation_id=p_conversation AND l.subject=p_subject FOR SHARE OF e,l;
    IF NOT FOUND THEN RETURN false; END IF;
    RETURN public.fn_eb_direct_scope_lock(p_org, p_workspace, p_conversation, p_subject);
END;
$$;
ALTER FUNCTION public.fn_eb_direct_bound_scope_lock(bigint, bigint, bigint, bigint, text, jsonb)
    OWNER TO eb_direct_scope_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_bound_scope_lock(bigint, bigint, bigint, bigint, text, jsonb)
    FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_eb_direct_bound_scope_lock(bigint, bigint, bigint, bigint, text, jsonb)
    TO v5_fn_owner;

-- Direct pin and EB registration must acquire the workspace before pending.
CREATE FUNCTION public.fn_eb_direct_workspace_prelock(p_key text) RETURNS void
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
DECLARE v_org bigint; v_workspace bigint;
BEGIN
    SELECT organization_id, workspace_id INTO v_org, v_workspace
      FROM public.enterprise_asset_direct_link WHERE object_key=p_key;
    IF NOT FOUND THEN RETURN; END IF;
    PERFORM 1 FROM public.workspace WHERE id=v_workspace AND organization_id=v_org FOR KEY SHARE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'enterprise direct workspace unavailable' USING ERRCODE='23510';
    END IF;
    IF NOT EXISTS (SELECT 1 FROM public.enterprise_asset_direct_link
                   WHERE object_key=p_key AND organization_id=v_org AND workspace_id=v_workspace) THEN
        RAISE EXCEPTION 'enterprise direct link changed' USING ERRCODE='40001';
    END IF;
END;
$$;
ALTER FUNCTION public.fn_eb_direct_workspace_prelock(text) OWNER TO eb_direct_scope_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_workspace_prelock(text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_eb_direct_workspace_prelock(text) TO v5_fn_owner;

-- Called in the same transaction that creates the EB V5 pending asset.  The
-- subject is read from that immutable registration, never from a client token.
CREATE FUNCTION public.fn_eb_direct_reserve(p_org bigint, p_workspace bigint, p_asset bigint,
    p_subject jsonb)
RETURNS text LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
DECLARE
    v_asset public.enterprise_asset%ROWTYPE;
    v_pending public.attach_pending%ROWTYPE;
    v_subject jsonb;
    v_kind text;
    v_actor text;
    v_session text;
    v_cid uuid;
    v_count bigint;
BEGIN
    PERFORM 1 FROM public.workspace WHERE id=p_workspace AND organization_id=p_org FOR KEY SHARE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'enterprise direct workspace unavailable' USING ERRCODE = '23510';
    END IF;
    SELECT client_upload_id INTO v_cid FROM public.enterprise_asset
     WHERE id = p_asset AND organization_id = p_org AND workspace_id = p_workspace;
    IF NOT FOUND OR v_cid IS NULL THEN
        RAISE EXCEPTION 'enterprise direct asset is not reservable' USING ERRCODE = '23510';
    END IF;
    -- Match the existing EB registration lock order: UUID before the asset row.
    PERFORM pg_advisory_xact_lock(hashtextextended('v5-eb-upload:' || v_cid::text, 169));
    PERFORM pg_advisory_xact_lock(hashtextextended(v_cid::text, 169));
    SELECT * INTO v_asset FROM public.enterprise_asset
     WHERE id = p_asset AND organization_id = p_org AND workspace_id = p_workspace
     FOR UPDATE;
    IF NOT FOUND OR v_asset.status <> 'pending_confirm' OR v_asset.lifecycle_state <> 'live'
       OR v_asset.client_upload_id IS DISTINCT FROM v_cid OR v_asset.conversation_id IS NULL THEN
        RAISE EXCEPTION 'enterprise direct asset is not reservable' USING ERRCODE = '23510';
    END IF;
    v_subject := v_asset.upload_registration->'subject';
    v_kind := v_subject->>'kind';
    v_actor := v_subject->>'principal';
    v_session := v_subject->>'session_id';
    IF v_asset.upload_registration->>'protocol' IS DISTINCT FROM 'direct-v1'
       OR v_subject IS DISTINCT FROM p_subject
       OR coalesce(v_kind, '') NOT IN ('seat', 'visitor_session')
       OR coalesce(v_actor, '') !~ '^[1-9][0-9]*$'
       OR v_subject->>'org_id' IS DISTINCT FROM p_org::text
       OR (v_kind = 'visitor_session' AND coalesce(v_session, '') !~ '^[1-9][0-9]*$')
       OR (v_kind = 'seat' AND v_session IS NOT NULL)
       OR (v_kind = 'seat' AND v_asset.uploaded_by_user_id::text IS DISTINCT FROM v_actor)
       OR v_asset.upload_registration->'declaration'->>'conversation_id'
          IS DISTINCT FROM v_asset.conversation_id::text
       OR v_asset.session_deadline IS NULL OR v_asset.session_deadline <= clock_timestamp()
       OR v_asset.storage_driver IS DISTINCT FROM 'garage'
       OR coalesce(v_asset.storage_namespace, '') = '' OR coalesce(v_asset.storage_container, '') = ''
       OR coalesce(v_asset.storage_locator_sha256, '') !~ '^[0-9a-f]{64}$' THEN
        RAISE EXCEPTION 'invalid enterprise direct identity' USING ERRCODE = '23510';
    END IF;
    IF NOT public.fn_eb_direct_scope_lock(p_org, p_workspace, v_asset.conversation_id, v_subject) THEN
        RAISE EXCEPTION 'enterprise direct subject unauthorized' USING ERRCODE = '23510';
    END IF;
    PERFORM pg_advisory_xact_lock(hashtextextended(
        'eb-direct:' || p_org::text || ':' || v_kind || ':' || v_actor, 170));
    PERFORM public.fn_v5_lock_identity(v_asset.storage_driver, v_asset.storage_namespace,
                                       v_asset.storage_container, v_asset.object_key);
    PERFORM pg_advisory_xact_lock(hashtextextended(v_asset.object_key, 168));
    SELECT * INTO v_pending FROM public.attach_pending WHERE client_upload_id = v_cid FOR UPDATE;
    IF FOUND THEN
        IF v_pending.object_key IS DISTINCT FROM v_asset.object_key
           OR v_pending.direct_state IS NULL
           OR v_pending.session_state NOT IN ('registered', 'confirmed')
           OR NOT EXISTS (SELECT 1 FROM public.enterprise_asset_direct_link l
                          WHERE l.asset_id = p_asset AND l.object_key = v_pending.object_key
                            AND l.organization_id = p_org AND l.workspace_id = p_workspace
                            AND l.conversation_id = v_asset.conversation_id AND l.subject = v_subject) THEN
            RAISE EXCEPTION 'enterprise direct reservation conflict' USING ERRCODE = '23510';
        END IF;
        RETURN v_pending.object_key;
    END IF;
    IF EXISTS (SELECT 1 FROM public.enterprise_asset_delete_queue q
               WHERE q.client_upload_id=v_cid OR q.asset_id=p_asset OR q.object_key=v_asset.object_key)
       OR EXISTS (SELECT 1 FROM public.attachment_object_deletion q WHERE q.path=v_asset.object_key)
       OR EXISTS (SELECT 1 FROM public.attachment a WHERE a.path=v_asset.object_key) THEN
        RAISE EXCEPTION 'enterprise direct identity already used' USING ERRCODE = '23510';
    END IF;
    SELECT count(*) INTO v_count FROM public.attach_pending p
     WHERE p.upload_registration->'subject'->>'kind' = 'enterprise'
       AND p.upload_registration->'subject'->>'org_id' = p_org::text
       AND p.upload_registration->'subject'->>'actor_kind' = v_kind
       AND p.upload_registration->'subject'->>'principal' = v_actor
       AND p.direct_state IN ('RESERVED', 'DECLARED', 'GRANTED', 'FINALIZING')
       AND p.session_deadline > clock_timestamp();
    IF v_count >= 4 THEN
        RAISE EXCEPTION 'enterprise direct quota exceeded' USING ERRCODE = '23514';
    END IF;
    INSERT INTO public.attach_pending
        (object_key, bucket, scope, creator_user_id, client_upload_id, session_state,
         direct_state, upload_registration, session_deadline, storage_driver,
         storage_namespace, storage_container, storage_locator_sha256)
    VALUES
        (v_asset.object_key, v_asset.storage_container, 'private', 0, v_cid, 'registered',
         'RESERVED', jsonb_build_object('protocol', 'direct-v1',
             'subject', jsonb_build_object('kind', 'enterprise', 'actor_kind', v_kind,
                 'principal', v_actor, 'org_id', p_org::text, 'session_id', v_session),
             'declaration', jsonb_build_object('scope', 'private', 'scope_ref', '',
                 'asset_id', p_asset::text, 'workspace_id', p_workspace::text,
                 'conversation_id', v_asset.conversation_id::text, 'mime', v_asset.mime,
                 'file_name', coalesce(v_asset.file_name, 'file'), 'purpose', 'attachment')),
         v_asset.session_deadline, v_asset.storage_driver, v_asset.storage_namespace,
         v_asset.storage_container, v_asset.storage_locator_sha256);
    INSERT INTO public.enterprise_asset_direct_link
        (asset_id, object_key, organization_id, workspace_id, conversation_id, subject)
    VALUES (p_asset, v_asset.object_key, p_org, p_workspace, v_asset.conversation_id, v_subject);
    RETURN v_asset.object_key;
END;
$$;
ALTER FUNCTION public.fn_eb_direct_reserve(bigint, bigint, bigint, jsonb) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_reserve(bigint, bigint, bigint, jsonb) FROM PUBLIC;

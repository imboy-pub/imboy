-- Dedicated, independently provisioned proof identity. No runtime role may
-- write or impersonate this role; its credential is never delivered to app.
DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname='eb_direct_proof_worker') THEN
        CREATE ROLE eb_direct_proof_worker NOLOGIN NOINHERIT;
    END IF;
    IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname='eb_direct_proof_worker'
               AND (rolcanlogin OR rolsuper OR rolcreatedb OR rolcreaterole OR rolreplication OR rolbypassrls OR rolinherit))
       OR EXISTS (SELECT 1 FROM pg_auth_members m JOIN pg_roles r ON r.oid=m.roleid
                  JOIN pg_roles member ON member.oid=m.member
                  WHERE r.rolname='eb_direct_proof_worker' AND member.rolname<>'imboy_eb_proof_worker')
       OR EXISTS (SELECT 1 FROM pg_auth_members m JOIN pg_roles r ON r.oid=m.member
                  WHERE r.rolname='eb_direct_proof_worker')
       OR EXISTS (SELECT 1 FROM pg_roles r WHERE r.rolname='imboy_eb_proof_worker'
                  AND (NOT r.rolcanlogin OR r.rolsuper OR r.rolcreatedb OR r.rolcreaterole OR r.rolreplication OR r.rolbypassrls OR r.rolinherit))
       OR EXISTS (SELECT 1 FROM pg_auth_members m JOIN pg_roles member ON member.oid=m.member
                  JOIN pg_roles granted ON granted.oid=m.roleid
                  WHERE member.rolname='imboy_eb_proof_worker' AND granted.rolname<>'eb_direct_proof_worker')
       OR EXISTS (SELECT 1 FROM pg_roles untrusted CROSS JOIN pg_roles target
                  WHERE untrusted.rolname IN ('v5_app_runtime','v5_worker','v5_proof')
                    AND target.rolname IN ('eb_direct_proof_worker','imboy_eb_proof_worker')
                    AND pg_has_role(untrusted.oid,target.oid,'MEMBER'))
       OR EXISTS (SELECT 1 FROM pg_roles login_role CROSS JOIN pg_roles runtime_role CROSS JOIN pg_roles target
                  WHERE login_role.rolcanlogin AND NOT login_role.rolsuper
                    AND runtime_role.rolname IN ('v5_app_runtime','v5_worker','v5_proof')
                    AND target.rolname IN ('eb_direct_proof_worker','imboy_eb_proof_worker')
                    AND pg_has_role(login_role.oid,runtime_role.oid,'MEMBER')
                    AND pg_has_role(login_role.oid,target.oid,'MEMBER')) THEN
        RAISE EXCEPTION 'unsafe enterprise proof worker role' USING ERRCODE='23510';
    END IF;
END $$;
CREATE TABLE public.enterprise_direct_scan_proof (
    asset_id bigint PRIMARY KEY REFERENCES public.enterprise_asset(id),
    object_key text NOT NULL,
    winner_fence jsonb NOT NULL,
    receipt_sha256 text NOT NULL CHECK (receipt_sha256 ~ '^[0-9a-f]{64}$'),
    ruleset_sha256 text NOT NULL CHECK (ruleset_sha256 ~ '^[0-9a-f]{64}$'),
    expires_at timestamptz NOT NULL,
    created_at timestamptz NOT NULL DEFAULT clock_timestamp()
);
CREATE TABLE public.enterprise_direct_scan_retry (
    asset_id bigint PRIMARY KEY REFERENCES public.enterprise_asset(id),
    failures integer NOT NULL CHECK (failures BETWEEN 1 AND 1000),
    retry_after timestamptz NOT NULL
);
REVOKE ALL ON public.enterprise_direct_scan_proof FROM PUBLIC, v5_app_runtime, v5_worker, v5_proof, eb_direct_proof_worker;
REVOKE ALL ON public.enterprise_direct_scan_retry FROM PUBLIC, v5_app_runtime, v5_worker, v5_proof, eb_direct_proof_worker;
GRANT SELECT, INSERT, UPDATE ON public.enterprise_direct_scan_proof TO eb_direct_binding_owner;
GRANT SELECT, INSERT, UPDATE, DELETE ON public.enterprise_direct_scan_retry TO eb_direct_binding_owner;
GRANT USAGE ON SCHEMA public TO eb_direct_proof_worker;

-- Projection contains only frozen metadata; credentials come from the proof
-- worker's own registry, not the caller or a client-provided endpoint.
CREATE FUNCTION public.fn_eb_direct_scan_projection(p_asset bigint)
RETURNS TABLE(organization_id bigint, workspace_id bigint, asset_id bigint,
              subject jsonb, fence jsonb)
LANGUAGE sql SECURITY DEFINER SET search_path=pg_catalog,public AS $$
    SELECT l.organization_id,l.workspace_id,l.asset_id,l.subject,
      jsonb_build_object('object_key',p.object_key,'attachment_id',a.id,
        'creator_user_id',p.creator_user_id,'scope',p.scope,'upload_registration',p.upload_registration,
        'attempt_no',w.attempt_no,'dispatch_cas_uuid',w.dispatch_cas_uuid,
        'storage_driver',a.storage_driver,'storage_namespace',a.storage_namespace,
        'storage_container',a.storage_container,'storage_locator_sha256',a.storage_locator_sha256,
        'storage_physical_key',a.storage_physical_key,'file_hash256',a.file_hash256,
        'size',a.size,'mime_type',a.mime_type)
    FROM public.enterprise_asset_direct_link l
    JOIN public.enterprise_asset e ON e.id=l.asset_id
    JOIN public.attach_pending p ON p.object_key=l.object_key
    JOIN public.attachment a ON a.id=p.confirmed_attachment_id AND a.path=p.object_key
    JOIN public.attach_direct_attempt w ON w.object_key=p.object_key AND w.attempt_state='WINNER'
    WHERE l.asset_id=p_asset AND l.binding_state='reserved' AND e.status='pending_confirm'
      AND e.lifecycle_state='live' AND p.direct_state='CONFIRMED' AND p.session_state='confirmed'
      AND a.lifecycle_state='live' AND a.status>=0
      AND p.creator_user_id=a.creator_user_id AND p.scope=a.scope
      AND (p.storage_driver,p.storage_namespace,p.storage_container,p.storage_locator_sha256)
          IS NOT DISTINCT FROM (a.storage_driver,a.storage_namespace,a.storage_container,a.storage_locator_sha256)
      AND (w.target_driver,w.target_namespace,w.target_container,w.target_key)
          IS NOT DISTINCT FROM (a.storage_driver,a.storage_namespace,a.storage_container,a.storage_physical_key)
      AND w.proof_sha256=a.file_hash256 AND w.proof_size=a.size
      AND p.direct_expected->>'sha256'=a.file_hash256 AND p.direct_expected->>'size_bytes'=a.size::text
      AND p.direct_expected->>'mime'=a.mime_type
$$;
ALTER FUNCTION public.fn_eb_direct_scan_projection(bigint) OWNER TO eb_direct_binding_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_scan_projection(bigint) FROM PUBLIC,v5_app_runtime,v5_worker,v5_proof;
GRANT EXECUTE ON FUNCTION public.fn_eb_direct_scan_projection(bigint) TO eb_direct_proof_worker;

-- Only the separately authenticated worker can publish a scan result. It must
-- scan the projection's exact winner before calling this function.
CREATE FUNCTION public.fn_eb_direct_record_scan(p_asset bigint,p_fence jsonb,p_receipt text,p_ruleset text,p_expiry bigint)
RETURNS boolean LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public AS $$
DECLARE v_fence jsonb;
BEGIN
    IF coalesce(p_receipt,'') !~ '^[0-9a-f]{64}$' OR coalesce(p_ruleset,'') !~ '^[0-9a-f]{64}$'
       OR p_expiry IS NULL OR to_timestamp(p_expiry)<=clock_timestamp()
       OR to_timestamp(p_expiry)>clock_timestamp()+interval '10 minutes' THEN RETURN false; END IF;
    SELECT fence INTO v_fence FROM public.fn_eb_direct_scan_projection(p_asset);
    IF NOT FOUND OR v_fence IS DISTINCT FROM p_fence THEN RETURN false; END IF;
    INSERT INTO public.enterprise_direct_scan_proof(asset_id,object_key,winner_fence,receipt_sha256,ruleset_sha256,expires_at)
    VALUES(p_asset,p_fence->>'object_key',p_fence,p_receipt,p_ruleset,to_timestamp(p_expiry))
    ON CONFLICT(asset_id) DO UPDATE SET object_key=EXCLUDED.object_key,winner_fence=EXCLUDED.winner_fence,
        receipt_sha256=EXCLUDED.receipt_sha256,ruleset_sha256=EXCLUDED.ruleset_sha256,
        expires_at=EXCLUDED.expires_at,created_at=clock_timestamp();
    DELETE FROM public.enterprise_direct_scan_retry WHERE asset_id=p_asset;
    RETURN true;
END $$;
ALTER FUNCTION public.fn_eb_direct_record_scan(bigint,jsonb,text,text,bigint) OWNER TO eb_direct_binding_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_record_scan(bigint,jsonb,text,text,bigint) FROM PUBLIC,v5_app_runtime,v5_worker,v5_proof;
GRANT EXECUTE ON FUNCTION public.fn_eb_direct_record_scan(bigint,jsonb,text,text,bigint) TO eb_direct_proof_worker;

CREATE FUNCTION public.fn_eb_direct_scan_candidates(p_limit integer)
RETURNS TABLE(asset_id bigint) LANGUAGE sql SECURITY DEFINER SET search_path=pg_catalog,public AS $$
    SELECT l.asset_id FROM public.enterprise_asset_direct_link l
    JOIN public.attach_pending p ON p.object_key=l.object_key
    LEFT JOIN public.enterprise_direct_scan_proof s ON s.asset_id=l.asset_id
    LEFT JOIN public.enterprise_direct_scan_retry r ON r.asset_id=l.asset_id
    WHERE l.binding_state='reserved' AND p.direct_state='CONFIRMED'
      AND (s.asset_id IS NULL OR s.expires_at<=clock_timestamp())
      AND (r.asset_id IS NULL OR r.retry_after<=clock_timestamp())
    ORDER BY l.asset_id LIMIT greatest(0,least(coalesce(p_limit,0),50))
$$;
ALTER FUNCTION public.fn_eb_direct_scan_candidates(integer) OWNER TO eb_direct_binding_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_scan_candidates(integer) FROM PUBLIC,v5_app_runtime,v5_worker,v5_proof;
GRANT EXECUTE ON FUNCTION public.fn_eb_direct_scan_candidates(integer) TO eb_direct_proof_worker;

CREATE FUNCTION public.fn_eb_direct_scan_failed(p_asset bigint)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public AS $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM public.enterprise_asset_direct_link l
                   JOIN public.attach_pending p ON p.object_key=l.object_key
                   WHERE l.asset_id=p_asset AND l.binding_state='reserved' AND p.direct_state='CONFIRMED') THEN
        RETURN;
    END IF;
    INSERT INTO public.enterprise_direct_scan_retry(asset_id,failures,retry_after)
    VALUES(p_asset,1,clock_timestamp()+interval '60 seconds')
    ON CONFLICT(asset_id) DO UPDATE SET failures=least(enterprise_direct_scan_retry.failures+1,1000),
        retry_after=clock_timestamp()+least(3600,60*power(2,least(enterprise_direct_scan_retry.failures,6))) * interval '1 second';
END $$;
ALTER FUNCTION public.fn_eb_direct_scan_failed(bigint) OWNER TO eb_direct_binding_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_scan_failed(bigint) FROM PUBLIC,v5_app_runtime,v5_worker,v5_proof;
GRANT EXECUTE ON FUNCTION public.fn_eb_direct_scan_failed(bigint) TO eb_direct_proof_worker;

CREATE FUNCTION public.fn_eb_direct_proof_view(p_org bigint,p_workspace bigint,p_asset bigint)
RETURNS TABLE(fence jsonb,receipt_sha256 text,ruleset_sha256 text,expires_at bigint)
LANGUAGE sql SECURITY DEFINER SET search_path=pg_catalog,public AS $$
    SELECT s.winner_fence,s.receipt_sha256,s.ruleset_sha256,extract(epoch FROM s.expires_at)::bigint
    FROM public.enterprise_direct_scan_proof s JOIN public.enterprise_asset_direct_link l ON l.asset_id=s.asset_id
    WHERE l.organization_id=p_org AND l.workspace_id=p_workspace AND l.asset_id=p_asset
      AND s.expires_at>clock_timestamp()
$$;
ALTER FUNCTION public.fn_eb_direct_proof_view(bigint,bigint,bigint) OWNER TO eb_direct_binding_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_proof_view(bigint,bigint,bigint) FROM PUBLIC,v5_worker,v5_proof,eb_direct_proof_worker;
GRANT EXECUTE ON FUNCTION public.fn_eb_direct_proof_view(bigint,bigint,bigint) TO v5_app_runtime;

-- Preserve 263 source while inserting the independent ledger check under the
-- same asset/attachment locks and before activation.
DO $$
DECLARE src text; needle text := '    IF to_timestamp(p_receipt_expiry) <= clock_timestamp() THEN'; replacement text;
BEGIN
    SELECT pg_get_functiondef('public.fn_eb_direct_bind(bigint,bigint,bigint,jsonb,jsonb,text,text,bigint)'::regprocedure) INTO src;
    IF position(needle IN src)=0 THEN RAISE EXCEPTION 'binding function shape changed'; END IF;
    replacement := $part$    PERFORM 1 FROM public.enterprise_direct_scan_proof s
      WHERE s.asset_id=p_asset AND s.object_key=v_key AND s.winner_fence=p_fence
        AND s.receipt_sha256=p_receipt_sha AND s.ruleset_sha256=p_ruleset_sha
        AND s.expires_at=to_timestamp(p_receipt_expiry) AND s.expires_at>clock_timestamp() FOR SHARE;
    IF NOT FOUND THEN RAISE EXCEPTION 'enterprise direct independent proof required' USING ERRCODE='23510'; END IF;
$part$ || needle;
    EXECUTE replace(src,needle,replacement);
END $$;
GRANT EXECUTE ON FUNCTION public.fn_eb_direct_bind(bigint,bigint,bigint,jsonb,jsonb,text,text,bigint) TO v5_app_runtime;

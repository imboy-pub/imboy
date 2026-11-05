-- Liveness is scoped to the exact frozen location and is not a scan verdict.
CREATE TABLE public.enterprise_direct_worker_heartbeat (
    namespace text NOT NULL CHECK (namespace <> ''),
    locator_sha256 text NOT NULL CHECK (locator_sha256 ~ '^[0-9a-f]{64}$'),
    worker_id text NOT NULL CHECK (worker_id ~ '^[0-9a-f]{32}$'),
    seen_at timestamptz NOT NULL,
    PRIMARY KEY (namespace, locator_sha256, worker_id)
);
REVOKE ALL ON public.enterprise_direct_worker_heartbeat FROM PUBLIC, v5_app_runtime,
    v5_worker, v5_proof, eb_direct_proof_worker;
GRANT SELECT, INSERT, UPDATE, DELETE ON public.enterprise_direct_worker_heartbeat TO eb_direct_binding_owner;

CREATE FUNCTION public.fn_eb_direct_worker_heartbeat(p_namespace text, p_locator text, p_worker text)
RETURNS boolean LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public AS $$
BEGIN
    INSERT INTO public.enterprise_direct_worker_heartbeat(namespace,locator_sha256,worker_id,seen_at)
    VALUES (p_namespace,p_locator,p_worker,clock_timestamp())
    ON CONFLICT (namespace,locator_sha256,worker_id) DO UPDATE SET seen_at=EXCLUDED.seen_at;
    RETURN true;
END $$;
ALTER FUNCTION public.fn_eb_direct_worker_heartbeat(text,text,text) OWNER TO eb_direct_binding_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_worker_heartbeat(text,text,text) FROM PUBLIC,v5_app_runtime,v5_worker,v5_proof;
GRANT EXECUTE ON FUNCTION public.fn_eb_direct_worker_heartbeat(text,text,text) TO eb_direct_proof_worker;

CREATE FUNCTION public.fn_eb_direct_worker_offline(p_namespace text, p_locator text, p_worker text)
RETURNS void LANGUAGE sql SECURITY DEFINER SET search_path=pg_catalog,public AS $$
    DELETE FROM public.enterprise_direct_worker_heartbeat
    WHERE namespace=p_namespace AND locator_sha256=p_locator AND worker_id=p_worker
$$;
ALTER FUNCTION public.fn_eb_direct_worker_offline(text,text,text) OWNER TO eb_direct_binding_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_worker_offline(text,text,text) FROM PUBLIC,v5_app_runtime,v5_worker,v5_proof;
GRANT EXECUTE ON FUNCTION public.fn_eb_direct_worker_offline(text,text,text) TO eb_direct_proof_worker;

CREATE FUNCTION public.fn_eb_direct_worker_ready(p_namespace text, p_locator text)
RETURNS boolean LANGUAGE sql SECURITY DEFINER SET search_path=pg_catalog,public AS $$
    SELECT EXISTS (SELECT 1 FROM public.enterprise_direct_worker_heartbeat
      WHERE namespace=p_namespace AND locator_sha256=p_locator
        AND seen_at > clock_timestamp() - interval '90 seconds')
$$;
ALTER FUNCTION public.fn_eb_direct_worker_ready(text,text) OWNER TO eb_direct_binding_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_worker_ready(text,text) FROM PUBLIC,v5_worker,v5_proof,eb_direct_proof_worker;
GRANT EXECUTE ON FUNCTION public.fn_eb_direct_worker_ready(text,text) TO v5_app_runtime;

-- Filter before LIMIT so other namespaces cannot starve this worker.
CREATE FUNCTION public.fn_eb_direct_scan_candidates_for_location(p_namespace text, p_locator text, p_limit integer)
RETURNS TABLE(asset_id bigint) LANGUAGE sql SECURITY DEFINER SET search_path=pg_catalog,public AS $$
    SELECT l.asset_id FROM public.enterprise_asset_direct_link l
    JOIN public.attach_pending p ON p.object_key=l.object_key
    JOIN public.attachment a ON a.id=p.confirmed_attachment_id AND a.path=p.object_key
    LEFT JOIN public.enterprise_direct_scan_proof s ON s.asset_id=l.asset_id
    LEFT JOIN public.enterprise_direct_scan_retry r ON r.asset_id=l.asset_id
    WHERE l.binding_state='reserved' AND p.direct_state='CONFIRMED'
      AND a.storage_namespace=p_namespace AND a.storage_locator_sha256=p_locator
      AND (s.asset_id IS NULL OR s.expires_at<=clock_timestamp())
      AND (r.asset_id IS NULL OR r.retry_after<=clock_timestamp())
    ORDER BY l.asset_id LIMIT greatest(0,least(coalesce(p_limit,0),50))
$$;
ALTER FUNCTION public.fn_eb_direct_scan_candidates_for_location(text,text,integer) OWNER TO eb_direct_binding_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_scan_candidates_for_location(text,text,integer) FROM PUBLIC,v5_app_runtime,v5_worker,v5_proof;
GRANT EXECUTE ON FUNCTION public.fn_eb_direct_scan_candidates_for_location(text,text,integer) TO eb_direct_proof_worker;

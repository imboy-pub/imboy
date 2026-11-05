DO $$ BEGIN
    IF EXISTS (SELECT 1 FROM public.enterprise_direct_scan_proof)
       OR EXISTS (SELECT 1 FROM public.enterprise_direct_scan_retry)
       OR EXISTS (SELECT 1 FROM public.enterprise_asset_direct_link WHERE binding_state='bound') THEN
        RAISE EXCEPTION 'cannot remove enterprise proof with live evidence or bindings' USING ERRCODE='23510';
    END IF;
END $$;
REVOKE EXECUTE ON FUNCTION public.fn_eb_direct_bind(bigint,bigint,bigint,jsonb,jsonb,text,text,bigint) FROM v5_app_runtime;
DO $$
DECLARE src text; part text := $part$    PERFORM 1 FROM public.enterprise_direct_scan_proof s
      WHERE s.asset_id=p_asset AND s.object_key=v_key AND s.winner_fence=p_fence
        AND s.receipt_sha256=p_receipt_sha AND s.ruleset_sha256=p_ruleset_sha
        AND s.expires_at=to_timestamp(p_receipt_expiry) AND s.expires_at>clock_timestamp() FOR SHARE;
    IF NOT FOUND THEN RAISE EXCEPTION 'enterprise direct independent proof required' USING ERRCODE='23510'; END IF;
$part$;
BEGIN
    SELECT pg_get_functiondef('public.fn_eb_direct_bind(bigint,bigint,bigint,jsonb,jsonb,text,text,bigint)'::regprocedure) INTO src;
    IF position(part IN src)=0 THEN RAISE EXCEPTION 'binding proof guard shape changed'; END IF;
    EXECUTE replace(src,part,'');
END $$;
DROP FUNCTION public.fn_eb_direct_proof_view(bigint,bigint,bigint);
DROP FUNCTION public.fn_eb_direct_scan_failed(bigint);
DROP FUNCTION public.fn_eb_direct_scan_candidates(integer);
DROP FUNCTION public.fn_eb_direct_record_scan(bigint,jsonb,text,text,bigint);
DROP FUNCTION public.fn_eb_direct_scan_projection(bigint);
DROP TABLE public.enterprise_direct_scan_proof;
DROP TABLE public.enterprise_direct_scan_retry;
REVOKE USAGE ON SCHEMA public FROM eb_direct_proof_worker;
-- Keep the global NOLOGIN role: it may be used by another database.

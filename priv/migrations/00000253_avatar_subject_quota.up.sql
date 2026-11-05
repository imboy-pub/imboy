-- Quota belongs to the full application subject; lock it for the whole registration transaction.
CREATE FUNCTION public.fn_dv1_quota_check_app(p_app_id text, p_org_id text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_live integer;
BEGIN
    IF p_app_id IS NULL OR p_app_id = '' OR p_org_id IS NULL OR p_org_id = '' THEN
        RAISE EXCEPTION 'invalid application subject' USING ERRCODE = '22023';
    END IF;
    PERFORM pg_advisory_xact_lock(hashtextextended(
        'dv1-app-quota:' || length(p_org_id)::text || ':' || p_org_id || ':' || p_app_id, 253));
    SELECT count(*) INTO v_live FROM public.attach_pending
      WHERE creator_user_id = 0
        AND upload_registration->'subject'->>'kind' = 'application'
        AND upload_registration->'subject'->>'org_id' = p_org_id
        AND upload_registration->'subject'->>'principal' = p_app_id
        AND direct_state IN ('RESERVED', 'DECLARED', 'GRANTED', 'FINALIZING')
        AND (session_deadline IS NULL OR session_deadline > now());
    IF v_live >= 4 THEN
        RAISE EXCEPTION 'dv1 session quota exceeded (%)', v_live USING ERRCODE = '23514';
    END IF;
END;
$$;
REVOKE ALL ON FUNCTION public.fn_dv1_quota_check_app(text,text) FROM PUBLIC;
DO $quota_update$
DECLARE v_def text;
BEGIN
    SELECT pg_get_functiondef('public.fn_dv1_register_application(text,text,text,text,text,uuid,text,text,text,text,text,text)'::regprocedure) INTO v_def;
    IF position('PERFORM fn_dv1_quota_check_app(p_app_id);' IN v_def) = 0 THEN
        RAISE EXCEPTION 'unexpected application register quota guard';
    END IF;
    EXECUTE replace(v_def, 'PERFORM fn_dv1_quota_check_app(p_app_id);',
                    'PERFORM fn_dv1_quota_check_app(p_app_id, p_org_id);');
END;
$quota_update$;

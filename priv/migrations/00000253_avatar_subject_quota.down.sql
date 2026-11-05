DO $quota_update$
DECLARE v_def text;
BEGIN
    SELECT pg_get_functiondef('public.fn_dv1_register_application(text,text,text,text,text,uuid,text,text,text,text,text,text)'::regprocedure) INTO v_def;
    IF position('PERFORM fn_dv1_quota_check_app(p_app_id, p_org_id);' IN v_def) = 0 THEN
        RAISE EXCEPTION 'unexpected application register quota guard';
    END IF;
    EXECUTE replace(v_def, 'PERFORM fn_dv1_quota_check_app(p_app_id, p_org_id);',
                    'PERFORM fn_dv1_quota_check_app(p_app_id);');
END;
$quota_update$;
DROP FUNCTION public.fn_dv1_quota_check_app(text,text);

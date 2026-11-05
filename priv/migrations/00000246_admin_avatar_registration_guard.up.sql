-- Modify only the application branch while preserving installed subject guards.
DO $avatar_guard$
DECLARE v_def text; v_before text := $before$                IF coalesce(v_subject->>'principal','') !~ '^[1-9][0-9]*$'
                   OR coalesce(v_subject->>'org_id','') !~ '^[1-9][0-9]*$'
                   OR v_subject->>'principal' IS DISTINCT FROM v_decl->>'application_id'
                   OR v_subject->>'org_id' IS DISTINCT FROM v_decl->>'org_id' THEN$before$;
        v_after text := $after$                IF NOT COALESCE((
                    (coalesce(v_subject->>'principal','') ~ '^[1-9][0-9]*$'
                     AND coalesce(v_subject->>'org_id','') ~ '^[1-9][0-9]*$')
                    OR (v_subject->>'org_id' = '0' AND v_p.scope = 'public'
                        AND (coalesce(v_subject->>'principal','') ~ '^[1-9][0-9]*$'
                             OR coalesce(v_subject->>'principal','') ~* '^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$')
                        AND left(v_p.object_key, length('app' || (v_subject->>'principal') || '/avatar/'))
                            = 'app' || (v_subject->>'principal') || '/avatar/')
                ), false) OR v_subject->>'principal' IS DISTINCT FROM v_decl->>'application_id'
                  OR v_subject->>'org_id' IS DISTINCT FROM v_decl->>'org_id' THEN$after$;
BEGIN
    SELECT pg_get_functiondef('public.fn_v5_pending_session_guard()'::regprocedure) INTO v_def;
    IF position(v_before IN v_def) = 0 THEN
        RAISE EXCEPTION 'unexpected pending session guard definition';
    END IF;
    EXECUTE replace(v_def, v_before, v_after);
END
$avatar_guard$;

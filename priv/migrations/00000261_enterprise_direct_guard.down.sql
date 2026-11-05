SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';
DO $$
DECLARE
    v_definition text;
    v_branch text := $branch$ELSIF v_subject->>'kind' = 'enterprise' AND v_p.creator_user_id = 0 THEN
                IF NOT EXISTS (
                    SELECT 1 FROM public.enterprise_asset_direct_link l
                    JOIN public.enterprise_asset e ON e.id = l.asset_id
                    WHERE l.object_key = v_p.object_key
                      AND l.asset_id::text = v_decl->>'asset_id'
                      AND l.organization_id::text = v_subject->>'org_id'
                      AND l.workspace_id::text = v_decl->>'workspace_id'
                      AND l.conversation_id::text = v_decl->>'conversation_id'
                      AND l.subject->>'kind' = v_subject->>'actor_kind'
                      AND l.subject->>'principal' = v_subject->>'principal'
                      AND l.subject->>'org_id' = v_subject->>'org_id'
                      AND l.subject->>'session_id' IS NOT DISTINCT FROM v_subject->>'session_id'
                      AND e.organization_id = l.organization_id
                      AND e.workspace_id = l.workspace_id
                      AND e.conversation_id = l.conversation_id
                      AND e.object_key = l.object_key
                      AND e.upload_registration->>'protocol' = 'direct-v1'
                      AND e.upload_registration->'subject' = l.subject
                      AND e.status = 'pending_confirm' AND e.lifecycle_state = 'live'
                      AND public.fn_eb_direct_bound_scope_lock(l.organization_id, l.workspace_id,
                          l.conversation_id, l.asset_id, l.object_key, l.subject)
                ) THEN
                    RAISE EXCEPTION 'enterprise direct subject mismatch' USING ERRCODE = '23510';
                END IF;
                v_ref := nullif(v_decl->>'scope_ref', '');
            $branch$;
BEGIN
    IF EXISTS (SELECT 1 FROM public.enterprise_asset_direct_link) THEN
        RAISE EXCEPTION 'enterprise direct reservations require reconciliation'
            USING ERRCODE = '23510';
    END IF;
    SELECT pg_get_functiondef('public.fn_v5_pending_session_guard()'::regprocedure) INTO v_definition;
    IF v_definition IS NULL OR position(v_branch IN v_definition) = 0
       OR length(v_definition) - length(replace(v_definition, v_branch, '')) <> length(v_branch) THEN
        RAISE EXCEPTION 'cannot safely restore pending session guard' USING ERRCODE = '23510';
    END IF;
    EXECUTE replace(v_definition, v_branch, '');
END;
$$;

DO $$
DECLARE
    v_definition text;
    v_prelock text := $prelock$PERFORM public.fn_eb_direct_workspace_prelock(p_key);
    $prelock$;
BEGIN
    SELECT pg_get_functiondef('public.fn_dv1_winner_pin(text,integer,text,text)'::regprocedure) INTO v_definition;
    IF v_definition IS NULL OR position(v_prelock IN v_definition) = 0
       OR length(v_definition) - length(replace(v_definition, v_prelock, '')) <> length(v_prelock) THEN
        RAISE EXCEPTION 'cannot safely restore direct winner pin' USING ERRCODE = '23510';
    END IF;
    EXECUTE replace(v_definition, v_prelock, '');
END;
$$;

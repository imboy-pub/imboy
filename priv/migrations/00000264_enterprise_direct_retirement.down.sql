SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';
DO $$ BEGIN
    IF EXISTS (SELECT 1 FROM public.enterprise_direct_retire_guard) THEN
        RAISE EXCEPTION 'enterprise retirement transaction active' USING ERRCODE='23510';
    END IF;
    IF to_regclass('public.enterprise_direct_purge_wait') IS NOT NULL THEN
        IF EXISTS (SELECT 1 FROM public.enterprise_direct_purge_wait) THEN
            RAISE EXCEPTION 'enterprise direct purge deferrals require reconciliation' USING ERRCODE='23510';
        END IF;
    END IF;
    IF to_regclass('public.enterprise_direct_retirement_audit') IS NOT NULL THEN
        IF EXISTS (SELECT 1 FROM public.enterprise_direct_retirement_audit) THEN
            RAISE EXCEPTION 'enterprise direct retirement audit requires preservation' USING ERRCODE='23510';
        END IF;
    END IF;
END $$;
DROP FUNCTION public.fn_eb_direct_retire(bigint,bigint,bigint);
DROP FUNCTION IF EXISTS public.fn_eb_direct_purge_candidates(bigint,bigint,bigint[],bigint[]);
DROP FUNCTION IF EXISTS public.fn_eb_direct_refresh_gc(bigint,bigint,bigint);
DROP FUNCTION IF EXISTS public.fn_eb_direct_purge_deferred(bigint,bigint,text,bigint);
DROP FUNCTION IF EXISTS public.fn_eb_direct_purge_defer(bigint,bigint,text,bigint);
DROP FUNCTION IF EXISTS public.fn_eb_direct_purge_clear(bigint,bigint,text,bigint);
CREATE OR REPLACE FUNCTION public.fn_eb_direct_activation_guard() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
BEGIN
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
DROP TABLE public.enterprise_direct_retire_guard;
DROP TABLE IF EXISTS public.enterprise_direct_purge_wait;
DROP TABLE IF EXISTS public.enterprise_direct_retirement_audit;
REVOKE EXECUTE ON FUNCTION public.fn_v5_lock_ordinary_upload(text),
    public.fn_v5_intent_ordinary_delete(text,text) FROM eb_direct_retire_owner;
REVOKE SELECT ON public.workspace, public.enterprise_asset, public.enterprise_asset_direct_link,
    public.attachment, public.attach_pending, public.attach_direct_attempt,
    public.enterprise_retention_hold FROM eb_direct_retire_owner;
REVOKE SELECT ON public.enterprise_message FROM eb_direct_retire_owner;
REVOKE UPDATE (id) ON public.workspace FROM eb_direct_retire_owner;
REVOKE UPDATE (status, lifecycle_state) ON public.enterprise_asset FROM eb_direct_retire_owner;
REVOKE UPDATE (id) ON public.attachment FROM eb_direct_retire_owner;
REVOKE UPDATE (asset_id) ON public.enterprise_asset_direct_link FROM eb_direct_retire_owner;
REVOKE UPDATE (direct_state, session_state) ON public.attach_pending FROM eb_direct_retire_owner;
REVOKE UPDATE (attempt_no) ON public.attach_direct_attempt FROM eb_direct_retire_owner;
REVOKE DELETE ON public.enterprise_asset_direct_link FROM eb_direct_retire_owner;
REVOKE USAGE ON SCHEMA public FROM eb_direct_retire_owner;

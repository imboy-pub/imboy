SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';
DO $$ BEGIN
    IF EXISTS (SELECT 1 FROM public.enterprise_asset_direct_link) THEN
        RAISE EXCEPTION 'enterprise direct links require reconciliation' USING ERRCODE='23510';
    END IF;
END $$;
DROP TRIGGER eb_direct_legacy_queue_guard ON public.enterprise_asset_delete_queue;
DROP FUNCTION public.fn_eb_direct_legacy_queue_guard();
DROP TRIGGER eb_direct_gc_guard ON public.attachment;
DROP FUNCTION public.fn_eb_direct_gc_guard();
DROP TRIGGER eb_direct_activation_guard ON public.enterprise_asset;
DROP FUNCTION public.fn_eb_direct_activation_guard();
DROP FUNCTION public.fn_eb_direct_bind(bigint,bigint,bigint,jsonb,jsonb,text,text,bigint);
REVOKE EXECUTE ON FUNCTION public.fn_eb_direct_workspace_prelock(text),
    public.fn_eb_direct_scope_lock(bigint,bigint,bigint,jsonb), public.fn_v5_lock_ordinary_upload(text)
    FROM eb_direct_binding_owner;
REVOKE SELECT ON public.enterprise_asset, public.enterprise_asset_direct_link,
    public.attach_pending, public.attachment, public.attach_direct_attempt FROM eb_direct_binding_owner;
REVOKE UPDATE (status, upload_content, version, updated_at) ON public.enterprise_asset FROM eb_direct_binding_owner;
REVOKE UPDATE (binding_state, winner_fence, scan_receipt_sha256, scan_ruleset_digest)
    ON public.enterprise_asset_direct_link FROM eb_direct_binding_owner;
REVOKE UPDATE (attempt_no) ON public.attach_direct_attempt FROM eb_direct_binding_owner;
REVOKE USAGE ON SCHEMA public FROM eb_direct_binding_owner;
ALTER TABLE public.enterprise_asset_direct_link
    DROP CONSTRAINT ck_eb_direct_binding_state,
    DROP COLUMN binding_state,
    DROP COLUMN winner_fence,
    DROP COLUMN scan_receipt_sha256,
    DROP COLUMN scan_ruleset_digest;

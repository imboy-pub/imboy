SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';
DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM public.enterprise_asset_direct_link) THEN
        RAISE EXCEPTION 'enterprise direct reservations require reconciliation'
            USING ERRCODE = '23510';
    END IF;
END;
$$;
DROP FUNCTION public.fn_eb_direct_reserve(bigint, bigint, bigint, jsonb);
DROP FUNCTION public.fn_eb_direct_workspace_prelock(text);
DROP FUNCTION public.fn_eb_direct_bound_scope_lock(bigint, bigint, bigint, bigint, text, jsonb);
DROP FUNCTION public.fn_eb_direct_scope_lock(bigint, bigint, bigint, jsonb);
DROP TABLE public.enterprise_asset_direct_link;
REVOKE SELECT ON public.enterprise_conversation, public.customer_service_session,
    public.customer_service_seat, public.customer_service_visit_token,
    public.customer_service_widget_installation, public.organization_member,
    public.organization_business_identity, public.organization_business_identity_assignment,
    public.workspace, public.enterprise_asset FROM eb_direct_scope_owner;
REVOKE UPDATE (id) ON public.enterprise_conversation, public.workspace, public.enterprise_asset FROM eb_direct_scope_owner;
REVOKE UPDATE (status) ON public.customer_service_session, public.organization_member,
    public.organization_business_identity, public.organization_business_identity_assignment FROM eb_direct_scope_owner;
REVOKE UPDATE (enabled) ON public.customer_service_seat FROM eb_direct_scope_owner;
REVOKE UPDATE (revoked_at) ON public.customer_service_visit_token FROM eb_direct_scope_owner;
REVOKE UPDATE (status) ON public.customer_service_widget_installation FROM eb_direct_scope_owner;
REVOKE USAGE ON SCHEMA public FROM eb_direct_scope_owner;

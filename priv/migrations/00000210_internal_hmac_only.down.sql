-- Dropped authorization fields cannot be reconstructed for existing applications/tickets.
-- Only an empty business schema permits a lossless downgrade.
SET LOCAL lock_timeout='5s';
LOCK TABLE public.enterprise_application, public.enterprise_application_credential,
    public.enterprise_oa_login_ticket IN ACCESS EXCLUSIVE MODE;
DO $$ BEGIN
    IF EXISTS (SELECT 1 FROM public.enterprise_application)
       OR EXISTS (SELECT 1 FROM public.enterprise_oa_login_ticket)
       OR EXISTS (SELECT 1 FROM public.enterprise_application_credential WHERE secret_digest IS NULL) THEN
        RAISE EXCEPTION 'HMAC-only data exists; rollback requires an explicit credential and authorization recovery plan';
    END IF;
END $$;
ALTER TABLE public.enterprise_application_credential
    ALTER COLUMN secret_digest SET NOT NULL;
ALTER TABLE public.enterprise_application
    ADD COLUMN allowed_scopes jsonb NOT NULL DEFAULT '[]'::jsonb,
    ADD CONSTRAINT ck_ea_scopes_array CHECK (jsonb_typeof(allowed_scopes) = 'array');
ALTER TABLE public.enterprise_oa_login_ticket
    ADD COLUMN issuer_authentication text NOT NULL DEFAULT 'bearer'
    CHECK (issuer_authentication IN ('bearer','hmac'));

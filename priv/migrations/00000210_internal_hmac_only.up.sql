-- OA Internal API has one HMAC authentication protocol.
-- Keep historical digest values as inert data; new credentials need no digest.
SET LOCAL lock_timeout='5s';
LOCK TABLE public.enterprise_application, public.enterprise_application_credential,
    public.enterprise_oa_login_ticket IN ACCESS EXCLUSIVE MODE;
ALTER TABLE public.enterprise_application_credential
    ALTER COLUMN secret_digest DROP NOT NULL;
ALTER TABLE public.enterprise_application DROP COLUMN allowed_scopes;
ALTER TABLE public.enterprise_oa_login_ticket DROP COLUMN issuer_authentication;

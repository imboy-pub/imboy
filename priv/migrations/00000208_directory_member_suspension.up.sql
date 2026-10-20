SET lock_timeout='5s';
-- Provenance for a directory source's temporary suspension. A different local
-- membership revision can never be restored by an old OA request.
ALTER TABLE public.enterprise_directory_membership_source
    ADD COLUMN suspension_member_version bigint,
    ADD CONSTRAINT directory_suspension_version_positive
        CHECK (suspension_member_version IS NULL OR suspension_member_version > 0);

ALTER TABLE public.enterprise_application_grant_scope DROP CONSTRAINT ck_eags_scope_fixed;
ALTER TABLE public.enterprise_application_grant_scope ADD CONSTRAINT ck_eags_scope_fixed CHECK (scope IN ('application:read','identities:read','identities:write','members:write','groups:read','groups:write','workspaces:read','projects:read','channels:read','files:write','messages:send','messages:send_as_human','friend_requests:create','webhooks:manage','sso:exchange','customer_service:read','customer_service:write','workspaces:write','channels:write','oa:login:issue'));

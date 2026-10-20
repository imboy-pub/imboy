DO $$ BEGIN
    IF EXISTS (SELECT 1 FROM public.enterprise_application_grant_scope WHERE scope='members:write')
       OR EXISTS (SELECT 1 FROM public.enterprise_application WHERE allowed_scopes ? 'members:write')
       OR EXISTS (SELECT 1 FROM public.enterprise_directory_membership_source
               WHERE suspension_member_version IS NOT NULL) THEN
        RAISE EXCEPTION 'Resolve directory-owned suspensions before rollback';
    END IF;
END $$;
ALTER TABLE public.enterprise_directory_membership_source
    DROP CONSTRAINT directory_suspension_version_positive,
    DROP COLUMN suspension_member_version;

ALTER TABLE public.enterprise_application_grant_scope DROP CONSTRAINT ck_eags_scope_fixed;
ALTER TABLE public.enterprise_application_grant_scope ADD CONSTRAINT ck_eags_scope_fixed CHECK (scope IN ('application:read','identities:read','identities:write','groups:read','groups:write','workspaces:read','projects:read','channels:read','files:write','messages:send','messages:send_as_human','friend_requests:create','webhooks:manage','sso:exchange','customer_service:read','customer_service:write','workspaces:write','channels:write','oa:login:issue'));

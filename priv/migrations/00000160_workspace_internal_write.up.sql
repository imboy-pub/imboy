-- Explicit Workspace write permission and concurrency versions; no auto grants.
SET lock_timeout = '5s';
SET statement_timeout = '15min';
ALTER TABLE enterprise_application_grant_scope DROP CONSTRAINT ck_eags_scope_fixed;
ALTER TABLE enterprise_application_grant_scope ADD CONSTRAINT ck_eags_scope_fixed CHECK (scope = ANY (ARRAY['application:read'::text,'identities:read'::text,'identities:write'::text,'groups:read'::text,'groups:write'::text,'workspaces:read'::text,'projects:read'::text,'channels:read'::text,'files:write'::text,'messages:send'::text,'messages:send_as_human'::text,'friend_requests:create'::text,'webhooks:manage'::text,'sso:exchange'::text,'customer_service:read'::text,'customer_service:write'::text,'workspaces:write'::text]));
ALTER TABLE workspace ADD COLUMN version bigint NOT NULL DEFAULT 1 CHECK(version > 0);
CREATE FUNCTION workspace_bump_version() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
 NEW.version := OLD.version + 1;
 RETURN NEW;
END $$;
CREATE TRIGGER workspace_bump_version BEFORE UPDATE ON workspace FOR EACH ROW EXECUTE FUNCTION workspace_bump_version();

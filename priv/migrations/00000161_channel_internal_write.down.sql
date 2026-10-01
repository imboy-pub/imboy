-- Refuse to discard grants or version evidence implicitly.
SET lock_timeout = '5s';
SET statement_timeout = '15min';
LOCK TABLE enterprise_application_grant_scope, enterprise_application, channel IN ACCESS EXCLUSIVE MODE;
DO $$ BEGIN
 IF EXISTS(SELECT 1 FROM enterprise_application_grant_scope WHERE scope='channels:write')
 OR EXISTS(SELECT 1 FROM enterprise_application WHERE allowed_scopes ? 'channels:write')
 OR EXISTS(SELECT 1 FROM channel WHERE version > 1) THEN
  RAISE EXCEPTION 'Channel write grants or versions require an explicit rollback plan';
 END IF;
END $$;
DROP TRIGGER channel_bump_version ON channel;
DROP FUNCTION channel_bump_version();
ALTER TABLE channel DROP COLUMN version;
ALTER TABLE enterprise_application_grant_scope DROP CONSTRAINT ck_eags_scope_fixed;
ALTER TABLE enterprise_application_grant_scope ADD CONSTRAINT ck_eags_scope_fixed CHECK (scope = ANY (ARRAY['application:read'::text,'identities:read'::text,'identities:write'::text,'groups:read'::text,'groups:write'::text,'workspaces:read'::text,'projects:read'::text,'channels:read'::text,'files:write'::text,'messages:send'::text,'messages:send_as_human'::text,'friend_requests:create'::text,'webhooks:manage'::text,'sso:exchange'::text,'customer_service:read'::text,'customer_service:write'::text,'workspaces:write'::text]));

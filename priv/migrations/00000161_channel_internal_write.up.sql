-- Explicit channel write grants and governance versions; no auto grants.
SET lock_timeout = '5s';
SET statement_timeout = '15min';
ALTER TABLE enterprise_application_grant_scope DROP CONSTRAINT ck_eags_scope_fixed;
ALTER TABLE enterprise_application_grant_scope ADD CONSTRAINT ck_eags_scope_fixed CHECK (scope = ANY (ARRAY['application:read'::text,'identities:read'::text,'identities:write'::text,'groups:read'::text,'groups:write'::text,'workspaces:read'::text,'projects:read'::text,'channels:read'::text,'files:write'::text,'messages:send'::text,'messages:send_as_human'::text,'friend_requests:create'::text,'webhooks:manage'::text,'sso:exchange'::text,'customer_service:read'::text,'customer_service:write'::text,'workspaces:write'::text,'channels:write'::text]));
ALTER TABLE channel ADD COLUMN version bigint NOT NULL DEFAULT 1 CHECK(version > 0);
CREATE FUNCTION channel_bump_version() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
 IF ROW(NEW.name,NEW.description,NEW.avatar,NEW.custom_id,NEW.tags,NEW.visibility,NEW.access_type,NEW.join_policy,NEW.status,NEW.scope,NEW.workspace_id,NEW.creator_uid,NEW.is_verified) IS DISTINCT FROM ROW(OLD.name,OLD.description,OLD.avatar,OLD.custom_id,OLD.tags,OLD.visibility,OLD.access_type,OLD.join_policy,OLD.status,OLD.scope,OLD.workspace_id,OLD.creator_uid,OLD.is_verified) THEN
  NEW.version := OLD.version + 1;
 ELSE
  NEW.version := OLD.version;
 END IF;
 RETURN NEW;
END $$;
CREATE TRIGGER channel_bump_version BEFORE UPDATE ON channel FOR EACH ROW EXECUTE FUNCTION channel_bump_version();

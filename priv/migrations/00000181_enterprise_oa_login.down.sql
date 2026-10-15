SET lock_timeout='5s';
LOCK TABLE enterprise_oa_login_ticket,enterprise_application_grant_scope,enterprise_application IN ACCESS EXCLUSIVE MODE;
DO $$ BEGIN
 IF EXISTS(SELECT 1 FROM enterprise_oa_login_ticket) OR
 EXISTS(SELECT 1 FROM enterprise_application_grant_scope WHERE scope='oa:login:issue') OR
 EXISTS(SELECT 1 FROM enterprise_application WHERE allowed_scopes ? 'oa:login:issue') THEN
 RAISE EXCEPTION 'OA login records or permissions prevent lossless rollback' USING ERRCODE='23514';
 END IF;
END $$;
DROP TABLE enterprise_oa_login_ticket;
ALTER TABLE enterprise_application_grant_scope DROP CONSTRAINT ck_eags_scope_fixed;
ALTER TABLE enterprise_application_grant_scope ADD CONSTRAINT ck_eags_scope_fixed CHECK (scope IN ('application:read','identities:read','identities:write','groups:read','groups:write','workspaces:read','projects:read','channels:read','files:write','messages:send','messages:send_as_human','friend_requests:create','webhooks:manage','sso:exchange','customer_service:read','customer_service:write','workspaces:write','channels:write'));
ALTER TABLE enterprise_application_grant_scope DROP CONSTRAINT fk_eags_resource_target;
ALTER TABLE enterprise_application_grant_scope DROP COLUMN resource_grant_id;
ALTER TABLE enterprise_application_grant_scope ADD COLUMN resource_grant_id bigint GENERATED ALWAYS AS (CASE WHEN scope IN ('application:read','webhooks:manage','sso:exchange') THEN NULL ELSE grant_id END) STORED;
ALTER TABLE enterprise_application_grant_scope ADD CONSTRAINT fk_eags_resource_target FOREIGN KEY(resource_grant_id,resource_target_required) REFERENCES enterprise_application_grant(id,has_resource_target) ON DELETE RESTRICT;

-- Refuse rollback while new permissions or usage exist; never delete them.
-- 新权限或用量仍存在时拒绝回退，禁止静默删除。
SET lock_timeout = '5s';
SET statement_timeout = '15min';
-- Hold permissions/usage stable through the guard and constraint restoration.
LOCK TABLE enterprise_application_grant_scope, enterprise_application, enterprise_application_usage IN ACCESS EXCLUSIVE MODE;
DO $$ BEGIN
 IF EXISTS (SELECT 1 FROM enterprise_application_grant_scope WHERE scope IN ('customer_service:read','customer_service:write'))
 OR EXISTS (SELECT 1 FROM enterprise_application WHERE allowed_scopes ?| ARRAY['customer_service:read','customer_service:write'])
 OR EXISTS (SELECT 1 FROM enterprise_application_usage WHERE metric = 'seat.read') THEN
  RAISE EXCEPTION 'Seat Internal API permissions or usage require an explicit rollback plan';
 END IF;
END $$;
ALTER TABLE enterprise_application_grant_scope DROP CONSTRAINT IF EXISTS ck_eags_scope_fixed;
ALTER TABLE enterprise_application_grant_scope ADD CONSTRAINT ck_eags_scope_fixed CHECK (scope = ANY (ARRAY['application:read'::text,'identities:read'::text,'identities:write'::text,'groups:read'::text,'groups:write'::text,'workspaces:read'::text,'projects:read'::text,'channels:read'::text,'files:write'::text,'messages:send'::text,'messages:send_as_human'::text,'friend_requests:create'::text,'webhooks:manage'::text,'sso:exchange'::text]));
ALTER TABLE enterprise_application_usage DROP CONSTRAINT IF EXISTS ck_eau_metric;
ALTER TABLE enterprise_application_usage ADD CONSTRAINT ck_eau_metric CHECK (metric = ANY (ARRAY['identity.bound'::text,'identity.revoked'::text,'directory.page'::text,'file.confirmed'::text,'message.accepted'::text,'message.failed'::text]));
COMMENT ON COLUMN customer_service_event.actor_kind IS '执行主体类别快照：seat | visitor | tenant_admin | platform_admin | system';

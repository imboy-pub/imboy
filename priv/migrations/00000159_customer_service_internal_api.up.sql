-- Seat Internal API: explicit scopes only; no automatic permission grants.
-- 坐席接口登记独立读写权限及聚合用量，不自动授予权限。
SET lock_timeout = '5s';
SET statement_timeout = '15min';
ALTER TABLE enterprise_application_grant_scope DROP CONSTRAINT IF EXISTS ck_eags_scope_fixed;
ALTER TABLE enterprise_application_grant_scope ADD CONSTRAINT ck_eags_scope_fixed CHECK (scope = ANY (ARRAY['application:read'::text,'identities:read'::text,'identities:write'::text,'groups:read'::text,'groups:write'::text,'workspaces:read'::text,'projects:read'::text,'channels:read'::text,'files:write'::text,'messages:send'::text,'messages:send_as_human'::text,'friend_requests:create'::text,'webhooks:manage'::text,'sso:exchange'::text,'customer_service:read'::text,'customer_service:write'::text]));
ALTER TABLE enterprise_application_usage DROP CONSTRAINT IF EXISTS ck_eau_metric;
ALTER TABLE enterprise_application_usage ADD CONSTRAINT ck_eau_metric CHECK (metric = ANY (ARRAY['identity.bound'::text,'identity.revoked'::text,'directory.page'::text,'file.confirmed'::text,'message.accepted'::text,'message.failed'::text,'seat.read'::text]));
COMMENT ON COLUMN customer_service_event.actor_kind IS '执行主体类别快照：seat | visitor | tenant_admin | platform_admin | system | application';

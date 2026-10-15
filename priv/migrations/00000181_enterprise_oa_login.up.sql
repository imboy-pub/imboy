-- OA -> IMBoy, deliberately separate from outbound SSO codes. No implicit grants.
SET lock_timeout='5s';
ALTER TABLE enterprise_application_grant_scope DROP CONSTRAINT ck_eags_scope_fixed;
ALTER TABLE enterprise_application_grant_scope ADD CONSTRAINT ck_eags_scope_fixed CHECK (scope IN ('application:read','identities:read','identities:write','groups:read','groups:write','workspaces:read','projects:read','channels:read','files:write','messages:send','messages:send_as_human','friend_requests:create','webhooks:manage','sso:exchange','customer_service:read','customer_service:write','workspaces:write','channels:write','oa:login:issue'));
ALTER TABLE enterprise_application_grant_scope DROP CONSTRAINT fk_eags_resource_target;
ALTER TABLE enterprise_application_grant_scope DROP COLUMN resource_grant_id;
ALTER TABLE enterprise_application_grant_scope ADD COLUMN resource_grant_id bigint GENERATED ALWAYS AS (CASE WHEN scope IN ('application:read','webhooks:manage','sso:exchange','oa:login:issue') THEN NULL ELSE grant_id END) STORED;
ALTER TABLE enterprise_application_grant_scope ADD CONSTRAINT fk_eags_resource_target FOREIGN KEY(resource_grant_id,resource_target_required) REFERENCES enterprise_application_grant(id,has_resource_target) ON DELETE RESTRICT;

CREATE TABLE enterprise_oa_login_ticket (
 digest bytea PRIMARY KEY CHECK(octet_length(digest)=32),
 application_id bigint NOT NULL REFERENCES enterprise_application(id),
 credential_id bigint NOT NULL REFERENCES enterprise_application_credential(id),
 external_user_id text NOT NULL CHECK(length(external_user_id) BETWEEN 1 AND 256),
 intent text NOT NULL CHECK(intent IN ('login','link')),
 challenge text NOT NULL CHECK(length(challenge)=43),
 stage text NOT NULL DEFAULT 'issued' CHECK(stage IN ('issued','pending','consumed')),
 device_id varchar(40),
 expires_at timestamptz NOT NULL DEFAULT (clock_timestamp()+interval '60 seconds'),
 created_at timestamptz NOT NULL DEFAULT clock_timestamp(),
 CHECK(stage <> 'pending' OR device_id IS NOT NULL)
);

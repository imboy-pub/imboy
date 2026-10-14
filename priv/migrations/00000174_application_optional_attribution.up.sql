-- ADR §8/8.1: optional application attribution and target-free self Grants.
-- erlang_migrate supplies the transaction; do not add BEGIN/COMMIT here.
-- No data backfill or implicit resource authorization. Migration 173 remains
-- responsible for Grant -> AppId independently of the resource organization.
SET lock_timeout = '5s';
SET statement_timeout = '15min';

ALTER TABLE enterprise_application
    ALTER COLUMN organization_id DROP NOT NULL;
ALTER TABLE enterprise_application_credential
    ALTER COLUMN organization_id DROP NOT NULL,
    ADD CONSTRAINT fk_eac_application_id FOREIGN KEY (application_id)
        REFERENCES enterprise_application(id) ON DELETE RESTRICT;
ALTER TABLE enterprise_oa_sso_code
    ALTER COLUMN organization_id DROP NOT NULL,
    ADD CONSTRAINT fk_eosc_application_id FOREIGN KEY (application_id)
        REFERENCES enterprise_application(id) ON DELETE RESTRICT;

-- Keep existing composite attribution FKs: non-null attribution must still
-- match the application. The independent FKs above prevent MATCH SIMPLE's
-- null escape from accepting an application_id that does not exist.
ALTER TABLE enterprise_application_grant
    ALTER COLUMN organization_id DROP NOT NULL,
    ADD CONSTRAINT ck_eag_self_without_workspace CHECK (
        organization_id IS NOT NULL OR workspace_scope_kind = 'none'
    ),
    ADD COLUMN has_resource_target boolean
        GENERATED ALWAYS AS (organization_id IS NOT NULL) STORED,
    ADD CONSTRAINT uq_eag_id_resource_target UNIQUE (id, has_resource_target);

-- Existing grant_workspace.org NOT NULL + composite Grant kind FK ensure
-- that a null-target/none Grant cannot carry any Workspace rows.
-- NULL targets still have a real idempotency namespace; ordinary UNIQUE
-- (organization_id, application_id, idempotency_key) does not cover NULL.
CREATE UNIQUE INDEX uq_eag_self_app_idempotency
    ON enterprise_application_grant (application_id, idempotency_key)
    WHERE organization_id IS NULL;

-- A non-self scope must reference the (GrantId, true) parent key. A self
-- scope needs no resource target, but its existing fk_eags_grant still checks
-- the GrantId. Both columns are GENERATED so callers cannot spoof them.
-- This FK also rejects changing a resource-bearing Grant's target to NULL,
-- including concurrent parent/scope changes, without cross-table triggers.
ALTER TABLE enterprise_application_grant_scope
    ADD COLUMN resource_grant_id bigint GENERATED ALWAYS AS (
        CASE WHEN scope IN ('application:read', 'webhooks:manage', 'sso:exchange')
             THEN NULL ELSE grant_id END
    ) STORED,
    ADD COLUMN resource_target_required boolean GENERATED ALWAYS AS (true) STORED,
    ADD CONSTRAINT fk_eags_resource_target
        FOREIGN KEY (resource_grant_id, resource_target_required)
        REFERENCES enterprise_application_grant (id, has_resource_target)
        ON DELETE RESTRICT;

ALTER TABLE enterprise_audit_event
    ALTER COLUMN organization_id DROP NOT NULL,
    ADD CONSTRAINT ck_eae_identity_requires_org CHECK (
        business_identity_id IS NULL OR organization_id IS NOT NULL
    );
-- Existing business-identity composite FK and append-only trigger stay intact.

COMMENT ON COLUMN enterprise_application.organization_id IS
    'Optional business attribution; grants alone authorize resource targets (ADR §8)';
COMMENT ON COLUMN enterprise_application_credential.organization_id IS
    'Optional application attribution; AppId FK always applies and non-null attribution must match';
COMMENT ON COLUMN enterprise_oa_sso_code.organization_id IS
    'Optional application attribution; not part of SSO authorization or code binding';
COMMENT ON COLUMN enterprise_application_grant.organization_id IS
    'Explicit resource target; NULL only permits application-self scopes, never a wildcard';
COMMENT ON COLUMN enterprise_application_grant.workspace_scope_kind IS
    'none with target=whole target organization; none without target=self only; explicit=listed target Workspaces';
COMMENT ON COLUMN enterprise_application_grant.has_resource_target IS
    'Generated FK discriminator for resource scopes; not an additional authorization grant';
COMMENT ON COLUMN enterprise_application_grant_scope.resource_grant_id IS
    'Generated Grant reference for non-self scopes; requires a non-null resource target';
COMMENT ON COLUMN enterprise_application_grant_scope.resource_target_required IS
    'Generated constant true for resource-target FK; cannot be supplied by callers';
COMMENT ON COLUMN enterprise_audit_event.organization_id IS
    'Optional organization for instance/application-self events; business identity requires its organization';

-- Deferred: internal idempotency, Webhook, external identity, usage and origin
-- tables retain their current organization contracts. Their runtime paths must
-- not claim support for null attribution until separately migrated and tested.

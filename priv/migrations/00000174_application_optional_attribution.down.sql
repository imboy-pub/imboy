-- Refuse any rollback that would require inventing attribution or deleting
-- new-form records (including immutable audit history). erlang_migrate wraps
-- this file in one transaction. Hold write-excluding locks through the check
-- and DDL so concurrent inserts cannot invalidate the rollback precondition.
SET lock_timeout = '5s';
SET statement_timeout = '15min';

LOCK TABLE enterprise_application, enterprise_application_credential,
    enterprise_oa_sso_code, enterprise_application_grant,
    enterprise_application_grant_scope, enterprise_audit_event
    IN ACCESS EXCLUSIVE MODE;

DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM enterprise_application WHERE organization_id IS NULL)
       OR EXISTS (SELECT 1 FROM enterprise_application_credential WHERE organization_id IS NULL)
       OR EXISTS (SELECT 1 FROM enterprise_oa_sso_code WHERE organization_id IS NULL)
       OR EXISTS (SELECT 1 FROM enterprise_application_grant WHERE organization_id IS NULL)
       OR EXISTS (SELECT 1 FROM enterprise_audit_event WHERE organization_id IS NULL) THEN
        RAISE EXCEPTION 'cannot roll back 174 while null-attribution or application-self records exist; restore a compatible backup instead'
            USING ERRCODE = '23514';
    END IF;
END $$;

ALTER TABLE enterprise_audit_event
    ALTER COLUMN organization_id SET NOT NULL,
    DROP CONSTRAINT ck_eae_identity_requires_org;
ALTER TABLE enterprise_application_grant_scope
    DROP CONSTRAINT fk_eags_resource_target,
    DROP COLUMN resource_grant_id,
    DROP COLUMN resource_target_required;
DROP INDEX uq_eag_self_app_idempotency;
ALTER TABLE enterprise_application_grant
    ALTER COLUMN organization_id SET NOT NULL,
    DROP CONSTRAINT ck_eag_self_without_workspace,
    DROP CONSTRAINT uq_eag_id_resource_target,
    DROP COLUMN has_resource_target;
ALTER TABLE enterprise_oa_sso_code
    ALTER COLUMN organization_id SET NOT NULL,
    DROP CONSTRAINT fk_eosc_application_id;
ALTER TABLE enterprise_application_credential
    ALTER COLUMN organization_id SET NOT NULL,
    DROP CONSTRAINT fk_eac_application_id;
ALTER TABLE enterprise_application
    ALTER COLUMN organization_id SET NOT NULL;

COMMENT ON COLUMN enterprise_application.organization_id IS
    '业务归属（business attribution）；非授权边界、非租户（2026-10-12 定位修订）';
COMMENT ON COLUMN enterprise_application_credential.organization_id IS NULL;
COMMENT ON COLUMN enterprise_oa_sso_code.organization_id IS NULL;
COMMENT ON COLUMN enterprise_application_grant.organization_id IS
    'Explicit resource organization targeted by this Grant; not application attribution or an instance-wide wildcard';
COMMENT ON COLUMN enterprise_application_grant.workspace_scope_kind IS
    'none=org 全域（零行 grant_workspace）| explicit=显式 workspace 授权（至少一行，行数语义由 Grant command 同一事务校验）';
COMMENT ON COLUMN enterprise_audit_event.organization_id IS NULL;

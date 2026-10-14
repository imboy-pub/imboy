SET lock_timeout = '5s';
SET statement_timeout = '15min';
LOCK TABLE enterprise_directory_organization_source, enterprise_directory_department_source,
    enterprise_directory_membership_source, enterprise_directory_department_member_source,
    enterprise_application_grant, enterprise_internal_idempotency IN ACCESS EXCLUSIVE MODE;
DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM enterprise_directory_organization_source)
       OR EXISTS (SELECT 1 FROM enterprise_directory_department_source)
       OR EXISTS (SELECT 1 FROM enterprise_directory_membership_source)
       OR EXISTS (SELECT 1 FROM enterprise_directory_department_member_source)
       OR EXISTS (SELECT 1 FROM enterprise_application_grant
                  WHERE provisioning_source = 'directory_bootstrap')
       OR EXISTS (SELECT 1 FROM enterprise_internal_idempotency i
                  LEFT JOIN enterprise_application a ON a.id = i.application_id
                  WHERE a.id IS NULL OR i.organization_id IS DISTINCT FROM a.organization_id)
    THEN
        RAISE EXCEPTION 'cannot roll back 177 without losing directory source data'
            USING ERRCODE = '23514';
    END IF;
END $$;
DROP TABLE enterprise_directory_department_member_source;
DROP TABLE enterprise_directory_membership_source;
DROP TABLE enterprise_directory_department_source;
DROP TABLE enterprise_directory_organization_source;
DROP FUNCTION fn_enterprise_directory_source_version_guard();
ALTER TABLE organization_department DROP CONSTRAINT uq_directory_department_org_id;
ALTER TABLE enterprise_application_grant DROP CONSTRAINT ck_eag_provisioning_source;
ALTER TABLE enterprise_application_grant ADD CONSTRAINT ck_eag_provisioning_source
    CHECK (provisioning_source IN ('manual', 'application_default'));
ALTER TABLE enterprise_internal_idempotency DROP CONSTRAINT fk_eii_application;
ALTER TABLE enterprise_internal_idempotency ADD CONSTRAINT fk_eii_application
    FOREIGN KEY (organization_id, application_id)
    REFERENCES enterprise_application(organization_id, id) ON DELETE RESTRICT;

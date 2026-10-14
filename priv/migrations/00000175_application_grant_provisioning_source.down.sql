-- Refuse to discard persisted automatic-provisioning provenance.
SET lock_timeout = '5s';
SET statement_timeout = '15min';
LOCK TABLE enterprise_application_grant IN ACCESS EXCLUSIVE MODE;
DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM enterprise_application_grant
               WHERE provisioning_source = 'application_default') THEN
        RAISE EXCEPTION 'cannot roll back 175 while automatic Grant provenance exists'
            USING ERRCODE = '23514';
    END IF;
END $$;
ALTER TABLE enterprise_application_grant
    DROP CONSTRAINT ck_eag_provisioning_source,
    DROP COLUMN provisioning_source;

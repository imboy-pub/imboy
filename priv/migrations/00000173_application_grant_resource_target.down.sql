-- Refuse rollback when it would discard authorized cross-organization Grants.
DO $$
BEGIN
    IF EXISTS (
        SELECT 1 FROM enterprise_application_grant g
        JOIN enterprise_application a ON a.id = g.application_id
        WHERE g.organization_id IS DISTINCT FROM a.organization_id
    ) THEN
        RAISE EXCEPTION 'cannot restore application attribution FK while cross-organization Grants exist'
            USING ERRCODE = '23514';
    END IF;
END $$;

ALTER TABLE enterprise_application_grant
    DROP CONSTRAINT fk_eag_application,
    ADD CONSTRAINT fk_eag_application FOREIGN KEY (organization_id, application_id)
        REFERENCES enterprise_application(organization_id, id) ON DELETE RESTRICT;

COMMENT ON COLUMN enterprise_application_grant.organization_id IS
    'Organization of the application and granted resources';

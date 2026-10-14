-- Grant.organization_id is the explicitly granted resource organization,
-- independent of the application's optional business attribution (ADR section 8).
-- Keep Grant -> organization and Grant workspace -> target workspace integrity.
SET lock_timeout = '5s';
SET statement_timeout = '15min';

ALTER TABLE enterprise_application_grant
    DROP CONSTRAINT fk_eag_application,
    ADD CONSTRAINT fk_eag_application FOREIGN KEY (application_id)
        REFERENCES enterprise_application(id) ON DELETE RESTRICT;

COMMENT ON COLUMN enterprise_application_grant.organization_id IS
    'Explicit resource organization targeted by this Grant; not application attribution or an instance-wide wildcard';

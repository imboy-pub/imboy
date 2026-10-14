-- Grant provenance is persisted, never inferred from an idempotency key.
-- Existing rows remain manual. erlang_migrate supplies the transaction.
SET lock_timeout = '5s';
SET statement_timeout = '15min';

ALTER TABLE enterprise_application_grant
    ADD COLUMN provisioning_source text NOT NULL DEFAULT 'manual',
    ADD CONSTRAINT ck_eag_provisioning_source CHECK (
        provisioning_source IN ('manual', 'application_default')
    );
COMMENT ON COLUMN enterprise_application_grant.provisioning_source IS
    'manual=explicit existing grant; application_default=server provisioned during application creation';

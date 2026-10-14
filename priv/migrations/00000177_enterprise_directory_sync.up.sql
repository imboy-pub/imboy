SET lock_timeout = '5s';
SET statement_timeout = '15min';
ALTER TABLE enterprise_internal_idempotency DROP CONSTRAINT fk_eii_application;
ALTER TABLE enterprise_internal_idempotency ADD CONSTRAINT fk_eii_application
    FOREIGN KEY (application_id) REFERENCES enterprise_application(id) ON DELETE RESTRICT;

ALTER TABLE enterprise_application_grant DROP CONSTRAINT ck_eag_provisioning_source;
ALTER TABLE enterprise_application_grant ADD CONSTRAINT ck_eag_provisioning_source
    CHECK (provisioning_source IN ('manual', 'application_default', 'directory_bootstrap'));

CREATE TABLE enterprise_directory_organization_source (
    application_id bigint NOT NULL REFERENCES enterprise_application(id) ON DELETE RESTRICT,
    external_organization_id text NOT NULL CHECK (length(external_organization_id) BETWEEN 1 AND 256),
    organization_id bigint UNIQUE REFERENCES organization(id) ON DELETE RESTRICT,
    confirmed_owner_user_id bigint REFERENCES "user"(id) ON DELETE SET NULL,
    enabled boolean NOT NULL DEFAULT false,
    configuration_version bigint NOT NULL DEFAULT 1 CHECK (configuration_version > 0),
    authorization_expires_at timestamptz NOT NULL,
    configured_by_adm_user_id bigint REFERENCES adm_user(id) ON DELETE RESTRICT,
    bootstrap_grant_id bigint REFERENCES enterprise_application_grant(id) ON DELETE RESTRICT,
    source_version bigint NOT NULL DEFAULT 0 CHECK (source_version >= 0),
    payload_digest text CHECK (payload_digest ~ '^[0-9a-f]{64}$'),
    PRIMARY KEY (application_id, external_organization_id),
    UNIQUE (application_id, organization_id),
    CONSTRAINT ck_directory_initial_owner_required
        CHECK (organization_id IS NOT NULL OR confirmed_owner_user_id IS NOT NULL),
    CHECK ((source_version = 0 AND payload_digest IS NULL) OR
           (source_version > 0 AND payload_digest IS NOT NULL))
);

ALTER TABLE organization_department ADD CONSTRAINT uq_directory_department_org_id
    UNIQUE (organization_id, id);

CREATE TABLE enterprise_directory_department_source (
    application_id bigint NOT NULL,
    organization_id bigint NOT NULL,
    external_department_id text NOT NULL CHECK (length(external_department_id) BETWEEN 1 AND 256),
    department_id bigint NOT NULL UNIQUE,
    source_version bigint NOT NULL CHECK (source_version > 0),
    payload_digest text NOT NULL CHECK (payload_digest ~ '^[0-9a-f]{64}$'),
    PRIMARY KEY (application_id, organization_id, external_department_id),
    UNIQUE (application_id, organization_id, department_id),
    FOREIGN KEY (application_id, organization_id)
        REFERENCES enterprise_directory_organization_source(application_id, organization_id) ON DELETE RESTRICT,
    FOREIGN KEY (organization_id, department_id)
        REFERENCES organization_department(organization_id, id) ON DELETE RESTRICT
);

CREATE TABLE enterprise_directory_membership_source (
    application_id bigint NOT NULL,
    organization_id bigint NOT NULL,
    external_user_id text NOT NULL,
    managed boolean NOT NULL DEFAULT false,
    offboarding_member_updated_at timestamptz,
    department_external_ids jsonb NOT NULL CHECK (jsonb_typeof(department_external_ids) = 'array'),
    projected_user_id bigint REFERENCES "user"(id) ON DELETE SET NULL,
    source_version bigint NOT NULL CHECK (source_version > 0),
    payload_digest text NOT NULL CHECK (payload_digest ~ '^[0-9a-f]{64}$'),
    status text NOT NULL CHECK (status IN ('active', 'removed')),
    CHECK (offboarding_member_updated_at IS NULL OR
           (managed AND status='removed')),
    PRIMARY KEY (application_id, organization_id, external_user_id),
    FOREIGN KEY (application_id, organization_id)
        REFERENCES enterprise_directory_organization_source(application_id, organization_id) ON DELETE RESTRICT,
    FOREIGN KEY (application_id, external_user_id)
        REFERENCES enterprise_source_person(application_id, external_user_id) ON DELETE RESTRICT
);

CREATE INDEX i_directory_membership_person
    ON enterprise_directory_membership_source(application_id, external_user_id, status);

COMMENT ON COLUMN enterprise_directory_membership_source.offboarding_member_updated_at IS
    'Exact organization_member.updated_at after this source actually removed the projected member; null is not removal attribution';

CREATE TABLE enterprise_directory_department_member_source (
    application_id bigint NOT NULL,
    organization_id bigint NOT NULL,
    department_id bigint NOT NULL,
    user_id bigint NOT NULL,
    PRIMARY KEY (application_id, department_id, user_id),
    FOREIGN KEY (application_id, organization_id, department_id)
        REFERENCES enterprise_directory_department_source(application_id, organization_id, department_id)
        ON DELETE RESTRICT,
    FOREIGN KEY (organization_id, user_id)
        REFERENCES organization_member(organization_id, user_id) ON DELETE CASCADE,
    FOREIGN KEY (department_id, user_id)
        REFERENCES organization_department_member(department_id, user_id) ON DELETE CASCADE
);

CREATE FUNCTION fn_enterprise_directory_source_version_guard() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
    IF NEW.application_id IS DISTINCT FROM OLD.application_id THEN
        RAISE EXCEPTION 'directory application is immutable' USING ERRCODE = '23514';
    END IF;
    IF TG_TABLE_NAME = 'enterprise_directory_organization_source' THEN
        IF NEW.external_organization_id IS DISTINCT FROM OLD.external_organization_id OR
           (OLD.organization_id IS NOT NULL AND NEW.organization_id IS DISTINCT FROM OLD.organization_id) OR
           (OLD.bootstrap_grant_id IS NOT NULL AND NEW.bootstrap_grant_id IS DISTINCT FROM OLD.bootstrap_grant_id) THEN
            RAISE EXCEPTION 'directory organization mapping is immutable' USING ERRCODE = '23514';
        END IF;
    ELSIF TG_TABLE_NAME = 'enterprise_directory_department_source' THEN
        IF (NEW.organization_id, NEW.external_department_id, NEW.department_id) IS DISTINCT FROM
           (OLD.organization_id, OLD.external_department_id, OLD.department_id) THEN
            RAISE EXCEPTION 'directory department mapping is immutable' USING ERRCODE = '23514';
        END IF;
    ELSE
        IF (NEW.organization_id, NEW.external_user_id) IS DISTINCT FROM
           (OLD.organization_id, OLD.external_user_id) THEN
            RAISE EXCEPTION 'directory membership key is immutable' USING ERRCODE = '23514';
        END IF;
    END IF;
    IF NEW.source_version < OLD.source_version OR
       (NEW.source_version = OLD.source_version AND NEW.payload_digest IS DISTINCT FROM OLD.payload_digest) THEN
        RAISE EXCEPTION 'directory source version conflict' USING ERRCODE = '23514';
    END IF;
    RETURN NEW;
END $$;

CREATE TRIGGER trg_directory_organization_version BEFORE UPDATE ON enterprise_directory_organization_source
    FOR EACH ROW EXECUTE FUNCTION fn_enterprise_directory_source_version_guard();
CREATE TRIGGER trg_directory_department_version BEFORE UPDATE ON enterprise_directory_department_source
    FOR EACH ROW EXECUTE FUNCTION fn_enterprise_directory_source_version_guard();
CREATE TRIGGER trg_directory_membership_version BEFORE UPDATE ON enterprise_directory_membership_source
    FOR EACH ROW EXECUTE FUNCTION fn_enterprise_directory_source_version_guard();

-- Source personnel exist before IMBoy account linking and independently of organizations.
SET lock_timeout = '5s';
SET statement_timeout = '15min';
LOCK TABLE "user", enterprise_external_identity IN ACCESS EXCLUSIVE MODE;

CREATE TABLE enterprise_source_person (
    application_id bigint NOT NULL REFERENCES enterprise_application(id) ON DELETE RESTRICT,
    external_user_id text NOT NULL CHECK (length(external_user_id) BETWEEN 1 AND 256),
    user_id bigint REFERENCES "user"(id) ON DELETE SET NULL,
    binding_retired boolean NOT NULL DEFAULT false,
    display_name text NOT NULL DEFAULT '' CHECK (length(display_name) <= 128),
    status text NOT NULL DEFAULT 'active' CHECK (status IN ('active', 'removed')),
    source_version bigint NOT NULL DEFAULT 0 CHECK (source_version >= 0),
    created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (application_id, external_user_id),
    UNIQUE (application_id, user_id),
    UNIQUE (application_id, external_user_id, user_id),
    CHECK (NOT binding_retired OR (user_id IS NULL AND status = 'removed'))
);

-- Unique constraints reject inconsistent history; never guess which account wins.
INSERT INTO enterprise_source_person
    (application_id, external_user_id, user_id, status, created_at, updated_at)
SELECT application_id, external_user_id, user_id, 'active', min(created_at),
       max(COALESCE(updated_at, created_at))
FROM enterprise_external_identity
GROUP BY application_id, external_user_id, user_id;

-- Organization mappings may coexist, but the canonical source UID is unique.
ALTER TABLE enterprise_external_identity DROP CONSTRAINT uq_eei_app_external_global;
ALTER TABLE enterprise_external_identity DROP CONSTRAINT fk_eei_application;
ALTER TABLE enterprise_external_identity ADD CONSTRAINT fk_eei_application
    FOREIGN KEY (application_id) REFERENCES enterprise_application(id) ON DELETE RESTRICT;

CREATE FUNCTION fn_enterprise_source_person_guard() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
    IF TG_OP = 'UPDATE' THEN
        IF (NEW.application_id, NEW.external_user_id) IS DISTINCT FROM
           (OLD.application_id, OLD.external_user_id) THEN
            RAISE EXCEPTION 'source identity key is immutable' USING ERRCODE = '23514';
        END IF;
        IF OLD.binding_retired AND (NOT NEW.binding_retired OR NEW.user_id IS NOT NULL
            OR NEW.status IS DISTINCT FROM OLD.status
            OR NEW.display_name IS DISTINCT FROM OLD.display_name
            OR NEW.source_version IS DISTINCT FROM OLD.source_version) THEN
            RAISE EXCEPTION 'deleted account binding cannot be reused' USING ERRCODE = '23514';
        END IF;
        IF OLD.user_id IS NOT NULL AND NEW.user_id IS DISTINCT FROM OLD.user_id THEN
            -- Only the user FK's deletion action may retire the association.
            IF NEW.user_id IS NULL AND NOT EXISTS (SELECT 1 FROM "user" WHERE id = OLD.user_id) THEN
                NEW.binding_retired := true;
                NEW.status := 'removed';
                NEW.updated_at := CURRENT_TIMESTAMP;
            ELSE
                RAISE EXCEPTION 'source identity cannot be rebound' USING ERRCODE = '23514';
            END IF;
        END IF;
        IF NEW.source_version < OLD.source_version OR
           (NEW.source_version = OLD.source_version AND
            (NEW.display_name IS DISTINCT FROM OLD.display_name OR
             NEW.status IS DISTINCT FROM OLD.status) AND NOT NEW.binding_retired) THEN
            RAISE EXCEPTION 'source version must advance for imported changes' USING ERRCODE = '23514';
        END IF;
    END IF;
    IF TG_OP = 'UPDATE' AND NEW.user_id IS NOT NULL AND NEW.status = 'removed' AND
       OLD.user_id IS NULL THEN
        RAISE EXCEPTION 'removed source identity cannot be claimed' USING ERRCODE = '23514';
    END IF;
    IF NEW.user_id IS NOT NULL AND
       (TG_OP = 'INSERT' OR OLD.user_id IS NULL) THEN
        PERFORM 1 FROM "user" WHERE id = NEW.user_id AND account_type = 0 AND status = 1
            FOR SHARE;
        IF NOT FOUND THEN
            RAISE EXCEPTION 'source identity requires an active Human account' USING ERRCODE = '23514';
        END IF;
    END IF;
    RETURN NEW;
END $$;
CREATE TRIGGER trg_enterprise_source_person_guard
    BEFORE INSERT OR UPDATE ON enterprise_source_person
    FOR EACH ROW EXECUTE FUNCTION fn_enterprise_source_person_guard();

-- Keep existing callers and non-null legacy reads intact, with one canonical UID.
CREATE FUNCTION fn_enterprise_external_identity_source() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
    IF TG_OP = 'INSERT' AND EXISTS (
        SELECT 1 FROM enterprise_external_identity
        WHERE organization_id IS NOT DISTINCT FROM NEW.organization_id
            AND application_id = NEW.application_id AND external_user_id = NEW.external_user_id
            AND user_id <> NEW.user_id
    ) THEN
        RETURN NEW;
    END IF;
    INSERT INTO enterprise_source_person
        (application_id, external_user_id, user_id, status, created_at, updated_at)
    SELECT NEW.application_id, NEW.external_user_id, NEW.user_id, 'active',
        NEW.created_at, COALESCE(NEW.updated_at, NEW.created_at)
    WHERE NOT EXISTS (SELECT 1 FROM enterprise_source_person
        WHERE application_id = NEW.application_id AND external_user_id = NEW.external_user_id)
    ON CONFLICT (application_id, external_user_id) DO NOTHING;
    UPDATE enterprise_source_person AS p SET user_id = NEW.user_id
    WHERE p.application_id = NEW.application_id AND p.external_user_id = NEW.external_user_id
        AND (p.user_id IS NULL OR p.user_id = NEW.user_id)
        AND NOT p.binding_retired
        AND (p.status = 'active' OR NEW.status = 'removed' AND p.user_id = NEW.user_id);
    IF NOT FOUND THEN
        RAISE EXCEPTION 'source identity cannot be claimed or restored'
            USING ERRCODE = '23514', CONSTRAINT = 'ck_eei_source_identity_conflict';
    END IF;
    RETURN NEW;
END $$;
CREATE TRIGGER trg_enterprise_external_identity_source
    BEFORE INSERT OR UPDATE ON enterprise_external_identity
    FOR EACH ROW EXECUTE FUNCTION fn_enterprise_external_identity_source();
ALTER TABLE enterprise_external_identity ADD CONSTRAINT fk_eei_source_person
    FOREIGN KEY (application_id, external_user_id, user_id)
    REFERENCES enterprise_source_person(application_id, external_user_id, user_id)
    ON UPDATE RESTRICT ON DELETE RESTRICT;

CREATE FUNCTION fn_enterprise_source_person_user_delete() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
    DELETE FROM enterprise_external_identity WHERE user_id = OLD.id;
    RETURN OLD;
END $$;
CREATE TRIGGER trg_enterprise_source_person_user_delete
    BEFORE DELETE ON "user"
    FOR EACH ROW EXECUTE FUNCTION fn_enterprise_source_person_user_delete();

COMMENT ON TABLE enterprise_source_person IS
    'OA source personnel and immutable account association; null user_id is unlinked, retired bindings cannot be reused';

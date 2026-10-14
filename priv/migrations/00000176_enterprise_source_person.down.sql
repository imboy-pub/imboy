-- Rollback must not discard pending personnel, imported fields or retired identities.
SET lock_timeout = '5s';
SET statement_timeout = '15min';
LOCK TABLE "user", enterprise_external_identity, enterprise_source_person IN ACCESS EXCLUSIVE MODE;
DO $$
BEGIN
    IF EXISTS (
        SELECT 1 FROM enterprise_source_person p
        LEFT JOIN (
            SELECT application_id, external_user_id, user_id,
                min(created_at) AS created_at, max(COALESCE(updated_at, created_at)) AS updated_at
            FROM enterprise_external_identity GROUP BY application_id, external_user_id, user_id
        ) e USING (application_id, external_user_id, user_id)
        WHERE e.user_id IS NULL OR p.binding_retired OR p.source_version <> 0
           OR p.display_name <> '' OR p.status <> 'active'
           OR p.created_at IS DISTINCT FROM e.created_at
           OR p.updated_at IS DISTINCT FROM e.updated_at
    ) OR EXISTS (
        SELECT 1 FROM enterprise_external_identity
        GROUP BY application_id, external_user_id HAVING count(*) > 1
    ) OR EXISTS (
        SELECT 1 FROM enterprise_external_identity e JOIN enterprise_application a ON a.id=e.application_id
        WHERE e.organization_id IS DISTINCT FROM a.organization_id
    ) THEN
        RAISE EXCEPTION 'cannot roll back 176 without losing source personnel data'
            USING ERRCODE = '23514';
    END IF;
END $$;
DROP TRIGGER trg_enterprise_source_person_user_delete ON "user";
DROP FUNCTION fn_enterprise_source_person_user_delete();
ALTER TABLE enterprise_external_identity DROP CONSTRAINT fk_eei_source_person;
ALTER TABLE enterprise_external_identity DROP CONSTRAINT fk_eei_application;
ALTER TABLE enterprise_external_identity ADD CONSTRAINT fk_eei_application
    FOREIGN KEY (organization_id, application_id)
    REFERENCES enterprise_application(organization_id, id) ON DELETE RESTRICT;
ALTER TABLE enterprise_external_identity ADD CONSTRAINT uq_eei_app_external_global
    UNIQUE (application_id, external_user_id);
DROP TRIGGER trg_enterprise_external_identity_source ON enterprise_external_identity;
DROP FUNCTION fn_enterprise_external_identity_source();
DROP TABLE enterprise_source_person;
DROP FUNCTION fn_enterprise_source_person_guard();

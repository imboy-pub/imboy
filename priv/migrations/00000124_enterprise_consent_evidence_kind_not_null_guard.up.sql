-- Tighten migration 122's CHECK: PostgreSQL accepts a CHECK whose expression is NULL,
-- so consent_at IS NOT NULL with a NULL evidence kind must be rejected explicitly.
-- Existing invalid rows are never silently backfilled into synthetic evidence.

SET lock_timeout = '5s';
SET statement_timeout = '15min';

DO $$
BEGIN
    IF EXISTS (
        SELECT 1
          FROM enterprise_conversation
         WHERE (consent_at IS NULL) <> (consent_evidence_kind IS NULL)
            OR consent_evidence_kind IS DISTINCT FROM 'synthetic'
               AND consent_evidence_kind IS NOT NULL
    ) THEN
        RAISE EXCEPTION 'enterprise_conversation contains invalid consent evidence rows'
            USING ERRCODE = '23514';
    END IF;
END
$$;

ALTER TABLE enterprise_conversation
    DROP CONSTRAINT IF EXISTS ck_ec_consent_evidence_kind;
ALTER TABLE enterprise_conversation
    ADD CONSTRAINT ck_ec_consent_evidence_kind CHECK (
        (consent_at IS NULL AND consent_evidence_kind IS NULL)
        OR (
            consent_at IS NOT NULL
            AND consent_evidence_kind IS NOT NULL
            AND consent_evidence_kind = 'synthetic'
        )
    );

COMMENT ON CONSTRAINT ck_ec_consent_evidence_kind ON enterprise_conversation IS
    '无 consent 时 evidence kind 必须 NULL；有 consent 时 evidence kind 必须非空且恰为 synthetic';

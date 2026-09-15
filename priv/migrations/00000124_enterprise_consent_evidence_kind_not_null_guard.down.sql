-- Restore migration 122's original constraint shape. The column and data remain intact.

SET lock_timeout = '5s';
SET statement_timeout = '15min';

ALTER TABLE enterprise_conversation
    DROP CONSTRAINT IF EXISTS ck_ec_consent_evidence_kind;
ALTER TABLE enterprise_conversation
    ADD CONSTRAINT ck_ec_consent_evidence_kind CHECK (
        (consent_at IS NULL AND consent_evidence_kind IS NULL)
        OR (consent_at IS NOT NULL AND consent_evidence_kind = 'synthetic')
    );

COMMENT ON CONSTRAINT ck_ec_consent_evidence_kind ON enterprise_conversation IS
    '同意证据类别 CHECK：非空取值集合恰为 {synthetic}；无 consent 时不得写证据';

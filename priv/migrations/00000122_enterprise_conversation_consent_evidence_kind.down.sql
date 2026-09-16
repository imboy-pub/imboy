-- 迁移 00000122 回滚：移除同意证据类别列及其 CHECK。
-- 迁移契约：禁止 BEGIN/COMMIT（erlang_migrate 外层单事务包裹）。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

ALTER TABLE enterprise_conversation
    DROP CONSTRAINT IF EXISTS ck_ec_consent_evidence_kind;
ALTER TABLE enterprise_conversation
    DROP COLUMN IF EXISTS consent_evidence_kind;

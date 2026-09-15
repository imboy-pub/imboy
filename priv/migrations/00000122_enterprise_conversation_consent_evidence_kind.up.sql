-- 迁移 00000122: 企业会话的「同意证据类别」列（EB-03R §2.4 M2 / R1）。
-- 计划契约：EB-03R M2-a..M2-d（可空、无 DEFAULT、非空取值集合恰为 {synthetic}、
--   不得回填历史行、报告上限继续是 synthetic_state_machine_only）。
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 语义（Q4 裁决）：
--   无 consent（consent_at IS NULL）           → 该列必须 IS NULL
--   有 consent 且 V1 可接受证据                → 该列 = 'synthetic'
--   'real' / 'verified_real' / 其他任何取值    → DB 拒绝（CHECK 非空分支只有 'synthetic'）
--
-- 为什么不能有 DEFAULT：若默认 'synthetic'，「无 consent」会被伪装成「有证据」，
-- 使 consent gate 的证据面出现假阳。故列可空、且**不设默认值**。
-- 为什么不能回填：历史行的证据来源无法证明，自动回填等于伪造证据。
-- 本迁移只 ADD COLUMN（既有行该列为 NULL），不含任何对既有行的 UPDATE。
--
-- 边界声明：'synthetic' 只表示「V1 本地状态机可接受的合成证据」，**不构成**
-- 真实同意、真实告知或任何生产合规结论。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

ALTER TABLE enterprise_conversation
    ADD COLUMN IF NOT EXISTS consent_evidence_kind text;

ALTER TABLE enterprise_conversation
    DROP CONSTRAINT IF EXISTS ck_ec_consent_evidence_kind;
ALTER TABLE enterprise_conversation
    ADD CONSTRAINT ck_ec_consent_evidence_kind CHECK (
        (consent_at IS NULL AND consent_evidence_kind IS NULL)
        OR (consent_at IS NOT NULL AND consent_evidence_kind = 'synthetic')
    );

COMMENT ON COLUMN enterprise_conversation.consent_evidence_kind IS
    '同意证据类别：可空、无 DEFAULT。无 consent（consent_at IS NULL）时必须 NULL；有 consent 时恰为 ''synthetic''（V1 本地状态机可接受的合成证据）；''real''/''verified_real''/其他取值一律被 ck_ec_consent_evidence_kind 拒绝';
COMMENT ON CONSTRAINT ck_ec_consent_evidence_kind ON enterprise_conversation IS
    '同意证据类别 CHECK：非空取值集合恰为 {synthetic}；无 consent 时不得写证据（可空 + 无 DEFAULT ⇒ 无 consent 不可能伪装成有证据）';

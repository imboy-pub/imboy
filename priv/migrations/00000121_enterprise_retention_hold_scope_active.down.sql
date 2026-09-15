-- 迁移 00000121 回滚：恢复 00000117 的无条件 hold → message 复合引用。
-- 迁移契约：禁止 BEGIN/COMMIT（erlang_migrate 外层单事务包裹）。
--
-- 回滚语义：先删派生列（连带其 FK 与索引），再加回 00000117 的
-- `fk_erh_message`（逐字同形）。回滚要求所有 hold 的 scope_message_id 仍能
-- 匹配到现存消息——若期间已按 M1 语义删过被 released hold 引用的消息，
-- 加回 FK 会失败（fail-closed：宁可拒绝回滚，也不静默丢引用）。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

ALTER TABLE enterprise_retention_hold DROP CONSTRAINT IF EXISTS fk_erh_active_message;
DROP INDEX IF EXISTS i_erh_active_scope_message;
ALTER TABLE enterprise_retention_hold DROP COLUMN IF EXISTS active_scope_message_id;

ALTER TABLE enterprise_retention_hold
    DROP CONSTRAINT IF EXISTS fk_erh_message;
ALTER TABLE enterprise_retention_hold
    ADD CONSTRAINT fk_erh_message
    FOREIGN KEY (organization_id, workspace_id, scope_message_id)
    REFERENCES enterprise_message (organization_id, workspace_id, id) ON DELETE RESTRICT;

COMMENT ON COLUMN enterprise_retention_hold.scope_message_id IS
    '作用域消息 id（原 resource id，审计可追）：append-only 不可改；非 message 作用域时为 NULL。release 后该列逐字保留（不置 NULL）';

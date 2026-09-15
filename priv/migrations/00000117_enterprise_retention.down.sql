-- 迁移 00000117 回滚：企业保留策略与保留 Hold。
-- 顺序：先删 enterprise_message 的 purge 触发器/函数 → hold 触发器/函数 → hold → policy
--       → policy 守卫函数。
-- 迁移契约：禁止 BEGIN/COMMIT（erlang_migrate 外层单事务包裹）。

DROP TRIGGER IF EXISTS trg_enterprise_message_purge_guard ON enterprise_message;
DROP FUNCTION IF EXISTS fn_enterprise_message_purge_guard();

DROP TRIGGER IF EXISTS trg_enterprise_retention_hold_append_only ON enterprise_retention_hold;
DROP FUNCTION IF EXISTS fn_enterprise_retention_hold_append_only();
DROP TABLE IF EXISTS enterprise_retention_hold;

DROP TRIGGER IF EXISTS trg_enterprise_retention_policy_guard ON enterprise_retention_policy;
DROP FUNCTION IF EXISTS fn_enterprise_retention_policy_guard();
DROP TRIGGER IF EXISTS trg_enterprise_retention_policy_immutable ON enterprise_retention_policy;
DROP FUNCTION IF EXISTS fn_enterprise_retention_policy_immutable();
DROP TABLE IF EXISTS enterprise_retention_policy;

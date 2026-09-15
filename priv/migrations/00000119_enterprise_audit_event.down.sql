-- 迁移 00000119 回滚：企业审计事件。
-- 迁移契约：禁止 BEGIN/COMMIT（erlang_migrate 外层单事务包裹）。

DROP TRIGGER IF EXISTS trg_enterprise_audit_event_append_only ON enterprise_audit_event;
DROP FUNCTION IF EXISTS fn_enterprise_audit_event_append_only();
DROP TABLE IF EXISTS enterprise_audit_event;

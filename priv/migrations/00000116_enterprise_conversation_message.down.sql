-- 迁移 00000116 回滚：企业会话与企业消息。
-- 顺序：先删触发器/函数 → delivery → message → conversation → 最后移除 workspace 的追加 UNIQUE。
-- 迁移契约：禁止 BEGIN/COMMIT（erlang_migrate 外层单事务包裹）。

DROP TRIGGER IF EXISTS trg_enterprise_message_retention_guard ON enterprise_message;
DROP FUNCTION IF EXISTS fn_enterprise_message_retention_guard();
DROP TRIGGER IF EXISTS trg_enterprise_message_actor_required ON enterprise_message;
DROP FUNCTION IF EXISTS fn_enterprise_message_actor_required();

DROP TABLE IF EXISTS enterprise_message_delivery;
DROP TABLE IF EXISTS enterprise_message;
DROP TABLE IF EXISTS enterprise_conversation;

ALTER TABLE workspace DROP CONSTRAINT IF EXISTS uq_workspace_organization_id_id;

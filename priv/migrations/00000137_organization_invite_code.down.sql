-- 00000137_organization_invite_code.down.sql
-- 组织邀请码回滚：删除 organization_invite_code 表（新能力整体回滚，邀请码
-- 数据即丢失，符合预期）。schema_migrations 版本登记由 erlang_migrate 自行
-- 管理，本文件不得触碰。禁止 BEGIN/COMMIT（erlang_migrate 外层单事务包裹）。

DROP TABLE IF EXISTS organization_invite_code;

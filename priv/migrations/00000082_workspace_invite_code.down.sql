-- 00000082_workspace_invite_code.down.sql
-- 工作区团队码回滚：删除 workspace_invite 表（新能力整体回滚，团队码数据
-- 即丢失，符合预期）。schema_migrations 版本登记由 erlang_migrate 自行管理，
-- 本文件不得触碰。禁止 BEGIN/COMMIT（erlang_migrate 外层单事务包裹）。

DROP TABLE IF EXISTS workspace_invite;

-- 迁移 00000128 down: 安全回滚 Organization Invitation（与 up 严格逆序）。
-- 本迁移为纯 expand：只删除 00000128 引入的表与索引，不触碰 00000125 及更早对象。
-- 保留 invitation rows 语义（计划 ROLLBACK 节）仅适用于「停止新入口」的功能回滚；
-- 结构回滚（本文件）在 rehearsal 中使用，行数据随表删除。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

DROP INDEX IF EXISTS i_organization_invitation_org_created;
DROP INDEX IF EXISTS i_organization_invitation_target_status;
DROP INDEX IF EXISTS uq_organization_invitation_single_pending;

DROP TABLE IF EXISTS organization_invitation;

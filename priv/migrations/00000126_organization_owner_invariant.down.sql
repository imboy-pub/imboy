-- 迁移 00000126 down: 回滚 Owner Invariant
-- 恢复 fk_organization_owner 为 00000095 的 ON DELETE CASCADE 原状，
-- 并移除 active-owner partial unique index。
-- 注意：回滚即重新打开「删 owner 用户级联删组织」与「双 owner 行」两个历史
-- 风险窗口（ORG-09 P02/P11 现状），仅用于合同切换前的回滚演练（ORG-11）。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

DROP INDEX IF EXISTS uq_organization_member_single_active_owner;

ALTER TABLE organization DROP CONSTRAINT IF EXISTS fk_organization_owner;
ALTER TABLE organization ADD CONSTRAINT fk_organization_owner
    FOREIGN KEY (owner_id) REFERENCES "user"(id) ON DELETE CASCADE;

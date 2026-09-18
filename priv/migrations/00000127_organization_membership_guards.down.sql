-- 迁移 00000127 down: 回滚 Membership Guards
-- 移除两侧 invariant constraint trigger 及其函数，并把
-- trg_organization_primary_owner_member_guard 恢复为 00000113 的
-- 原 BEFORE 即时触发器形态（原文重建）。
-- 注意：回滚后 transfer「先降旧 → 再改 owner_id」顺序会被即时 guard 误拒，
-- 仅用于合同切换前的回滚演练（ORG-11），不与 00000127 up 后的 transfer 代码混用。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

DROP TRIGGER IF EXISTS trg_organization_member_owner_invariant ON organization_member;
DROP FUNCTION IF EXISTS fn_organization_member_owner_invariant();

DROP TRIGGER IF EXISTS trg_organization_owner_invariant ON organization;
DROP FUNCTION IF EXISTS fn_organization_owner_invariant();

-- 恢复 00000113 原 guard 触发器（BEFORE 即时；函数 fn_organization_primary_owner_member_guard 未动）。
DROP TRIGGER IF EXISTS trg_organization_primary_owner_member_guard ON organization_member;
CREATE TRIGGER trg_organization_primary_owner_member_guard
    BEFORE UPDATE OF role, status OR DELETE ON organization_member
    FOR EACH ROW EXECUTE FUNCTION fn_organization_primary_owner_member_guard();

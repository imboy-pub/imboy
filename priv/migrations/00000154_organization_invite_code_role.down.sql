-- 00000154_organization_invite_code_role.down.sql
-- 安全回滚：存在 admin 角色码时拒绝回滚（fail-closed 预检）。
-- admin 码是"凭码即得管理员"的活凭证，静默丢列会让已发出的码失去
-- 角色语义（回退后一律按 member 加入）——应显式确认后人工回滚。

DO $$
DECLARE
    v_count bigint;
BEGIN
    SELECT COUNT(*) INTO v_count FROM organization_invite_code WHERE role = 'admin';
    IF v_count > 0 THEN
        RAISE EXCEPTION 'organization_invite_code has % admin-code rows; confirm explicitly before dropping role column', v_count;
    END IF;
END $$;

ALTER TABLE organization_invite_code DROP CONSTRAINT IF EXISTS ck_organization_invite_code_role;
ALTER TABLE organization_invite_code DROP COLUMN IF EXISTS role;

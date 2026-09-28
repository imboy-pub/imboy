-- 00000155_organization_status_pending.down.sql
-- 安全回滚：存在 pending/rejected 组织时拒绝回缩枚举（fail-closed 预检）。
-- pending/rejected 行是未走完/未通过的注册审核，静默删除约束会失去
-- 状态合法性裁决——应先完成审核（approve/reject）或显式确认后人工回滚。

DO $$
DECLARE
    v_count bigint;
BEGIN
    SELECT COUNT(*) INTO v_count FROM organization WHERE status IN ('pending', 'rejected');
    IF v_count > 0 THEN
        RAISE EXCEPTION 'organization has % pending/rejected rows; settle review or confirm explicitly before shrinking status enum', v_count;
    END IF;
END $$;

ALTER TABLE organization DROP CONSTRAINT IF EXISTS ck_organization_status;
ALTER TABLE organization ADD CONSTRAINT ck_organization_status
    CHECK (status = ANY (ARRAY['active'::text, 'archived'::text]));

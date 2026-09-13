-- 迁移 00000113 回滚：只有仍可由 organization.owner_id 重建的数据时才允许删除。

DO $$
BEGIN
    IF EXISTS (
        SELECT 1
          FROM organization_member om
          LEFT JOIN organization o
            ON o.id = om.organization_id
           AND o.owner_id = om.user_id
         WHERE o.id IS NULL
            OR om.role <> 'owner'
            OR om.status <> 'active'
    ) THEN
        RAISE EXCEPTION
            'organization_member 已含 owner_id 之外的治理数据，拒绝回滚以避免成员关系丢失';
    END IF;
END;
$$;

DROP TRIGGER IF EXISTS trg_organization_owner_member_sync ON organization;
DROP FUNCTION IF EXISTS fn_organization_owner_member_sync();
DROP TRIGGER IF EXISTS trg_organization_primary_owner_member_guard ON organization_member;
DROP FUNCTION IF EXISTS fn_organization_primary_owner_member_guard();
DROP TABLE IF EXISTS organization_member;

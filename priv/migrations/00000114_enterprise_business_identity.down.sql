-- 迁移 00000114 回滚：企业业务身份与经办关系。
-- 顺序：先还原 00000113 的 organization_member.status 约束（存在 suspended 行则 fail-closed 拒绝）
--       → 再删除 offboarding 守卫 → assignment → identity。
-- 迁移契约：禁止 BEGIN/COMMIT（erlang_migrate 外层单事务包裹）。

DO $$
BEGIN
    IF EXISTS (
        SELECT 1 FROM organization_member
         WHERE status NOT IN ('active', 'removed')
    ) THEN
        RAISE EXCEPTION
            'organization_member 仍存在 suspended 等 00000113 未定义的状态，拒绝回滚以避免状态语义丢失'
            USING ERRCODE = '23514';
    END IF;
END;
$$;

ALTER TABLE organization_member DROP CONSTRAINT IF EXISTS ck_organization_member_status;
ALTER TABLE organization_member ADD CONSTRAINT ck_organization_member_status
    CHECK (status = ANY (ARRAY['active'::text, 'removed'::text]));

COMMENT ON COLUMN organization_member.status IS
    '状态: active 在册 | removed 已移除；不联动 workspace_member';

DROP TRIGGER IF EXISTS trg_organization_member_offboarding_guard ON organization_member;
DROP FUNCTION IF EXISTS fn_organization_member_offboarding_guard();

DROP TABLE IF EXISTS organization_business_identity_assignment;

DROP TRIGGER IF EXISTS trg_organization_business_identity_function_immutable
    ON organization_business_identity;
DROP FUNCTION IF EXISTS fn_organization_business_identity_function_immutable();
DROP TABLE IF EXISTS organization_business_identity;

-- 迁移 00000113: Organization 多成员治理关系。
-- Organization 负责资源归属；Workspace 成员关系保持独立，允许跨 Organization 协作。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

CREATE TABLE IF NOT EXISTS organization_member (
    organization_id bigint NOT NULL,
    user_id         bigint NOT NULL,
    role            text DEFAULT 'member' NOT NULL,
    invited_by      bigint,
    joined_at       timestamp with time zone,
    status          text DEFAULT 'active' NOT NULL,
    created_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_organization_member PRIMARY KEY (organization_id, user_id),
    CONSTRAINT ck_organization_member_role
        CHECK (role = ANY (ARRAY['owner'::text, 'admin'::text, 'member'::text])),
    CONSTRAINT ck_organization_member_status
        CHECK (status = ANY (ARRAY['active'::text, 'removed'::text]))
);

ALTER TABLE organization_member DROP CONSTRAINT IF EXISTS fk_organization_member_organization;
ALTER TABLE organization_member ADD CONSTRAINT fk_organization_member_organization
    FOREIGN KEY (organization_id) REFERENCES organization(id) ON DELETE CASCADE;

ALTER TABLE organization_member DROP CONSTRAINT IF EXISTS fk_organization_member_user;
ALTER TABLE organization_member ADD CONSTRAINT fk_organization_member_user
    FOREIGN KEY (user_id) REFERENCES "user"(id) ON DELETE CASCADE;

ALTER TABLE organization_member DROP CONSTRAINT IF EXISTS fk_organization_member_invited_by;
ALTER TABLE organization_member ADD CONSTRAINT fk_organization_member_invited_by
    FOREIGN KEY (invited_by) REFERENCES "user"(id) ON DELETE SET NULL;

CREATE INDEX IF NOT EXISTS i_organization_member_uid_org_status
    ON organization_member USING btree (user_id, organization_id, status);
CREATE INDEX IF NOT EXISTS i_organization_member_org_role_status
    ON organization_member USING btree (organization_id, role, status);

-- 兼容既有 organization.owner_id：主 Owner 必须同时是一名 active Organization Member。
INSERT INTO organization_member (
    organization_id, user_id, role, joined_at, status, created_at, updated_at
)
SELECT id, owner_id, 'owner', created_at, 'active', created_at, CURRENT_TIMESTAMP
  FROM organization
ON CONFLICT (organization_id, user_id) DO UPDATE
   SET role = 'owner', status = 'active', updated_at = CURRENT_TIMESTAMP;

-- organization.owner_id 暂时保留为主 Owner 兼容锚点；新增或转移时同步成员行。
CREATE OR REPLACE FUNCTION fn_organization_owner_member_sync() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    INSERT INTO organization_member (
        organization_id, user_id, role, joined_at, status, created_at, updated_at
    ) VALUES (
        NEW.id, NEW.owner_id, 'owner', CURRENT_TIMESTAMP, 'active',
        CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
    )
    ON CONFLICT (organization_id, user_id) DO UPDATE
       SET role = 'owner', status = 'active', updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_organization_owner_member_sync ON organization;
CREATE TRIGGER trg_organization_owner_member_sync
    AFTER INSERT OR UPDATE OF owner_id ON organization
    FOR EACH ROW EXECUTE FUNCTION fn_organization_owner_member_sync();

-- 主 Owner 的成员行不能被单独移除或降级；先转移 organization.owner_id，
-- 再处理旧 Owner 的普通成员关系。
CREATE OR REPLACE FUNCTION fn_organization_primary_owner_member_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF EXISTS (
        SELECT 1 FROM organization
         WHERE id = OLD.organization_id AND owner_id = OLD.user_id
    ) AND (
        TG_OP = 'DELETE' OR NEW.role <> 'owner' OR NEW.status <> 'active'
    ) THEN
        RAISE EXCEPTION
            'organization % 的主 Owner % 不能被移除或降级，请先转移 organization.owner_id',
            OLD.organization_id, OLD.user_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_organization_primary_owner_member_guard';
    END IF;
    IF TG_OP = 'DELETE' THEN
        RETURN OLD;
    END IF;
    RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_organization_primary_owner_member_guard ON organization_member;
CREATE TRIGGER trg_organization_primary_owner_member_guard
    BEFORE UPDATE OF role, status OR DELETE ON organization_member
    FOR EACH ROW EXECUTE FUNCTION fn_organization_primary_owner_member_guard();

COMMENT ON TABLE organization_member IS
    'Organization 治理成员；独立于 workspace_member，成员无需加入 Organization 即可受邀进入其 Workspace';
COMMENT ON COLUMN organization_member.role IS
    '组织角色: owner 主治理者 | admin 管理员 | member 普通组织成员';
COMMENT ON COLUMN organization_member.status IS
    '状态: active 在册 | removed 已移除；不联动 workspace_member';

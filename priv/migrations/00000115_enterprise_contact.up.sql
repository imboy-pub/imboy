-- 迁移 00000115: 企业客户与客户资料（Enterprise Contact）。
-- 计划契约：§3 EB-D04（企业客户不是个人好友）、§4.1（enterprise_contact /
--   enterprise_contact_identity / enterprise_contact_assignment / enterprise_note）。
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策：
--   * 客户关系 owner 恒为 Organization；imboy_user_id 只是可选关联（ON DELETE SET NULL），
--     不写 user_friend、不在个人好友链上建边（EB-D04）。
--   * profile_cipher / body_cipher 是企业托管密文，明文一律不入库；key_version 与密文成对出现，
--     缺 key_version 的密文一律拒绝（fail-closed，EB-D05 密钥版本原则）。
--   * enterprise_contact_identity 只存 channel、组织域 HMAC subject 与必要掩码；
--     subject_hmac 强制 64 位小写 hex，禁止裸 SHA / 可字典反查的哈希。
--   * enterprise_note 只允许软删除：status='deleted' 与 deleted_at IS NOT NULL 必须同时成立。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 1: enterprise_contact
-- ============================================================
CREATE TABLE IF NOT EXISTS enterprise_contact (
    id                              bigint                   NOT NULL,  -- TSID
    organization_id                 bigint                   NOT NULL,
    imboy_user_id                   bigint,                            -- 可选关联个人账号；不是关系 owner
    status                          text                     DEFAULT 'active' NOT NULL,
    display_name                    text,
    profile_cipher                  text,
    profile_key_version             integer,
    created_by_business_identity_id bigint,
    version                         integer                  DEFAULT 1 NOT NULL,
    created_at                      timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at                      timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_enterprise_contact PRIMARY KEY (id),
    CONSTRAINT uq_enterprise_contact_org_id UNIQUE (organization_id, id),
    CONSTRAINT ck_enterprise_contact_status CHECK (status = ANY (ARRAY['active'::text, 'archived'::text])),
    CONSTRAINT ck_enterprise_contact_profile_key CHECK (
        profile_cipher IS NULL OR profile_key_version IS NOT NULL),
    CONSTRAINT ck_enterprise_contact_version CHECK (version >= 1),
    CONSTRAINT fk_enterprise_contact_organization FOREIGN KEY (organization_id)
        REFERENCES organization(id) ON DELETE RESTRICT,
    CONSTRAINT fk_enterprise_contact_imboy_user FOREIGN KEY (imboy_user_id)
        REFERENCES "user"(id) ON DELETE SET NULL,
    CONSTRAINT fk_enterprise_contact_created_by_identity
        FOREIGN KEY (organization_id, created_by_business_identity_id)
        REFERENCES organization_business_identity (organization_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE enterprise_contact IS
    '企业客户/外部联系人记录：关系 owner 为 Organization，可关联 IMBoy user 但不属于任何个人好友链（EB-D04）';
COMMENT ON COLUMN enterprise_contact.organization_id IS '资源 owner 租户；不可由普通业务操作改变';
COMMENT ON COLUMN enterprise_contact.imboy_user_id IS '可选关联的 IMBoy 个人账号；仅 SET NULL，不级联、不合并个人关系';
COMMENT ON COLUMN enterprise_contact.status IS '状态: active 在用 | archived 已归档（历史保留）';
COMMENT ON COLUMN enterprise_contact.display_name IS '企业侧展示名（明文仅限非敏感称呼；敏感资料一律走 profile_cipher）';
COMMENT ON COLUMN enterprise_contact.profile_cipher IS '客户资料的企业托管密文；明文不入库，缺 profile_key_version 时一律拒绝';
COMMENT ON COLUMN enterprise_contact.profile_key_version IS 'profile_cipher 对应的企业托管密钥版本';
COMMENT ON COLUMN enterprise_contact.created_by_business_identity_id IS '创建该客户的业务身份（审计）；不改变 Org 归属';

CREATE INDEX IF NOT EXISTS i_enterprise_contact_org_status ON enterprise_contact
    USING btree (organization_id, status);

-- ============================================================
-- Phase 2: enterprise_contact_identity
-- ============================================================
CREATE TABLE IF NOT EXISTS enterprise_contact_identity (
    id              bigint                   NOT NULL,  -- TSID
    organization_id bigint                   NOT NULL,
    contact_id      bigint                   NOT NULL,
    channel         text                     NOT NULL,
    subject_hmac    text                     NOT NULL,  -- 组织域 HMAC，64 hex
    subject_mask    text,
    created_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_enterprise_contact_identity PRIMARY KEY (id),
    CONSTRAINT uq_eci_org_channel_hmac UNIQUE (organization_id, channel, subject_hmac),
    CONSTRAINT ck_eci_channel CHECK (
        channel = ANY (ARRAY['imboy'::text, 'wechat'::text, 'phone'::text, 'email'::text, 'other'::text])),
    CONSTRAINT ck_eci_subject_hmac CHECK (subject_hmac ~ '^[0-9a-f]{64}$'),
    CONSTRAINT fk_eci_contact FOREIGN KEY (organization_id, contact_id)
        REFERENCES enterprise_contact (organization_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE enterprise_contact_identity IS
    '客户渠道标识：只存 channel、组织域 HMAC subject 与必要掩码，不存可字典反查的裸哈希与明文';
COMMENT ON COLUMN enterprise_contact_identity.subject_hmac IS
    '组织域 HMAC-SHA256 的 64 位小写 hex；同一 Org 内 (channel, subject_hmac) 唯一，用于幂等去重';
COMMENT ON COLUMN enterprise_contact_identity.subject_mask IS '展示用掩码（如 wx***1），不得还原为完整 subject';

-- ============================================================
-- Phase 3: enterprise_contact_assignment
-- ============================================================
CREATE TABLE IF NOT EXISTS enterprise_contact_assignment (
    id                   bigint                   NOT NULL,  -- TSID
    organization_id      bigint                   NOT NULL,
    contact_id           bigint                   NOT NULL,
    business_identity_id bigint                   NOT NULL,
    role                 text                     NOT NULL,
    status               text                     DEFAULT 'active' NOT NULL,
    assigned_at          timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    ended_at             timestamp with time zone,
    assigned_by          bigint,
    created_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_enterprise_contact_assignment PRIMARY KEY (id),
    CONSTRAINT ck_eca_role CHECK (role = ANY (ARRAY['primary'::text, 'collaborator'::text])),
    CONSTRAINT ck_eca_status CHECK (status = ANY (ARRAY['active'::text, 'ended'::text])),
    CONSTRAINT ck_eca_ended_at_consistency CHECK (
        (status = 'active' AND ended_at IS NULL)
        OR (status = 'ended' AND ended_at IS NOT NULL)),
    CONSTRAINT ck_eca_time_range CHECK (ended_at IS NULL OR ended_at >= assigned_at),
    CONSTRAINT fk_eca_contact FOREIGN KEY (organization_id, contact_id)
        REFERENCES enterprise_contact (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_eca_identity FOREIGN KEY (organization_id, business_identity_id)
        REFERENCES organization_business_identity (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_eca_assigned_by FOREIGN KEY (assigned_by) REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE enterprise_contact_assignment IS
    '客户经办分配：把 contact 交给某个 business identity（primary 主办 / collaborator 协办），保留 active→ended 历史';
COMMENT ON COLUMN enterprise_contact_assignment.role IS '分配角色: primary 主办（同一 contact 同时最多一个 active）| collaborator 协办';
COMMENT ON COLUMN enterprise_contact_assignment.business_identity_id IS '承接该客户的业务身份；与 contact 必须同 Org（复合 FK）';
COMMENT ON COLUMN enterprise_contact_assignment.assigned_by IS '操作人（审计快照）；user 删除后置 NULL，不级联企业数据';

CREATE UNIQUE INDEX IF NOT EXISTS uq_eca_primary_contact
    ON enterprise_contact_assignment (organization_id, contact_id)
    WHERE role = 'primary' AND status = 'active';

CREATE UNIQUE INDEX IF NOT EXISTS uq_eca_active_contact_identity
    ON enterprise_contact_assignment (organization_id, contact_id, business_identity_id)
    WHERE status = 'active';

CREATE INDEX IF NOT EXISTS i_eca_org_identity ON enterprise_contact_assignment
    USING btree (organization_id, business_identity_id, status);

-- ============================================================
-- Phase 4: enterprise_note
-- ============================================================
CREATE TABLE IF NOT EXISTS enterprise_note (
    id                   bigint                   NOT NULL,  -- TSID
    organization_id      bigint                   NOT NULL,
    contact_id           bigint                   NOT NULL,
    business_identity_id bigint,
    actor_user_id        bigint,
    body_cipher          text,
    body_key_version     integer,
    status               text                     DEFAULT 'active' NOT NULL,
    deleted_at           timestamp with time zone,
    created_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_enterprise_note PRIMARY KEY (id),
    CONSTRAINT ck_enterprise_note_status CHECK (status = ANY (ARRAY['active'::text, 'deleted'::text])),
    CONSTRAINT ck_enterprise_note_deleted_at CHECK (
        (status = 'active' AND deleted_at IS NULL)
        OR (status = 'deleted' AND deleted_at IS NOT NULL)),
    CONSTRAINT ck_enterprise_note_body_key CHECK (
        body_cipher IS NULL OR body_key_version IS NOT NULL),
    CONSTRAINT fk_enterprise_note_contact FOREIGN KEY (organization_id, contact_id)
        REFERENCES enterprise_contact (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_enterprise_note_identity FOREIGN KEY (organization_id, business_identity_id)
        REFERENCES organization_business_identity (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_enterprise_note_actor FOREIGN KEY (actor_user_id)
        REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE enterprise_note IS
    '客户跟进记录（企业托管密文正文）：只允许软删除，删除必须同时写 status=deleted 与 deleted_at（审计保留）';
COMMENT ON COLUMN enterprise_note.body_cipher IS '正文企业托管密文；明文不入库，缺 body_key_version 时一律拒绝';
COMMENT ON COLUMN enterprise_note.status IS '状态: active 有效 | deleted 已软删除（deleted_at 必须非空，禁止物理 DELETE 语义）';
COMMENT ON COLUMN enterprise_note.actor_user_id IS '写该记录的 user（审计）；user 删除后置 NULL，不级联企业数据';

CREATE INDEX IF NOT EXISTS i_enterprise_note_org_contact ON enterprise_note
    USING btree (organization_id, contact_id, status);

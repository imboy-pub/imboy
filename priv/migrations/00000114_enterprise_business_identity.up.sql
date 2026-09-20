-- 迁移 00000114: 企业业务身份与经办关系（Organization Business Identity）。
-- 计划契约：
--   §3 EB-D02（稳定业务身份）、EB-D03（Owner/assignee/actor 三分）、EB-D07（离职状态机）、
--   §4.1（organization_business_identity / organization_business_identity_assignment）。
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策：
--   * function_key 是业务身份类型（V1 冻结 sales|customer_service），不是权限、不是 governance role
--     （EB-D11）。创建后不可变：变更职能必须新建 identity，历史经办关系不被改写。
--   * 复合 FK (organization_id, business_identity_id, function_key) 让数据库保证 assignment 的
--     function_key 与 identity 一致，partial unique 才能以 (organization_id, user_id, function_key)
--     表达「同一员工同一职能最多一个 active 经办」的基数（EB-D02 V1 基数冻结）。
--   * assignment.user_id 用 ON DELETE SET NULL；配合 CHECK (status <> 'active' OR user_id IS NOT NULL)
--     使「删除仍持有 active 经办关系的 user」直接被数据库拒绝（fail-closed，保护企业数据 owner）。
--     这是 §4.3「删除/停用 A 不改变企业业务行数」的数据库侧保证，禁止改为 CASCADE。
--   * organization_member.status 兼容扩展为 active|suspended|removed（EB-D07）：
--     suspended 立即让企业授权失败但不删个人账号。历史值 active/removed 的语义、名称、
--     行数据一律不变，也不修改 00000113 迁移文件本身。
--   * 新增 offboarding 守卫要求「直接 removed」先完成交接；它与 113 的
--     trg_organization_primary_owner_member_guard 同为 BEFORE 触发器并互不冲突。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 1: organization_business_identity
-- ============================================================
CREATE TABLE IF NOT EXISTS organization_business_identity (
    id                 bigint                   NOT NULL,  -- TSID
    organization_id    bigint                   NOT NULL,
    function_key       text                     NOT NULL,  -- V1: sales | customer_service
    display_name       text                     NOT NULL,
    status             text                     DEFAULT 'active' NOT NULL,
    version            integer                  DEFAULT 1 NOT NULL,
    created_by_user_id bigint,
    created_at         timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at         timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_organization_business_identity PRIMARY KEY (id),
    CONSTRAINT uq_obi_org_id UNIQUE (organization_id, id),
    CONSTRAINT uq_obi_org_id_function UNIQUE (organization_id, id, function_key),
    CONSTRAINT uq_obi_org_function_name UNIQUE (organization_id, function_key, display_name),
    CONSTRAINT ck_obi_function_key CHECK (function_key = ANY (ARRAY['sales'::text, 'customer_service'::text])),
    CONSTRAINT ck_obi_status CHECK (status = ANY (ARRAY['active'::text, 'retired'::text])),
    CONSTRAINT ck_obi_version CHECK (version >= 1),
    CONSTRAINT ck_obi_display_name CHECK (display_name <> ''),
    CONSTRAINT fk_obi_organization FOREIGN KEY (organization_id)
        REFERENCES organization(id) ON DELETE RESTRICT,
    CONSTRAINT fk_obi_created_by FOREIGN KEY (created_by_user_id)
        REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE organization_business_identity IS
    '企业业务身份（稳定经办主体，归 Organization）：承担 sales/customer_service 职能，不可登录、不发 JWT、不拥有个人好友';
COMMENT ON COLUMN organization_business_identity.id IS '主键 TSID；handover 不改变该 ID（资源 owner/ID 不变）';
COMMENT ON COLUMN organization_business_identity.organization_id IS '资源 owner 租户；不可由普通业务操作改变，删除机构一律 RESTRICT';
COMMENT ON COLUMN organization_business_identity.function_key IS '业务职能类型 V1 冻结: sales | customer_service；创建后不可变（EB-D11：不是权限、不是 governance role）';
COMMENT ON COLUMN organization_business_identity.display_name IS '组织内展示名；同一 Org+职能内唯一';
COMMENT ON COLUMN organization_business_identity.status IS '状态: active 可用 | retired 已退役（退役不等于删除，历史关系保留）';
COMMENT ON COLUMN organization_business_identity.version IS '乐观锁版本，CAS 从 1 起';
COMMENT ON COLUMN organization_business_identity.created_by_user_id IS '创建人（审计快照）；user 删除后置 NULL，不级联企业数据';

CREATE INDEX IF NOT EXISTS i_obi_org_status ON organization_business_identity
    USING btree (organization_id, status);

-- function_key 不可变（改职能=新建 identity）
CREATE OR REPLACE FUNCTION fn_organization_business_identity_function_immutable() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.function_key IS DISTINCT FROM OLD.function_key THEN
        RAISE EXCEPTION
            'organization_business_identity % 的 function_key 创建后不可变（%->%），请新建 identity',
            OLD.id, OLD.function_key, NEW.function_key
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_organization_business_identity_function_immutable';
    END IF;
    RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_organization_business_identity_function_immutable
    ON organization_business_identity;
CREATE TRIGGER trg_organization_business_identity_function_immutable
    BEFORE UPDATE OF function_key ON organization_business_identity
    FOR EACH ROW EXECUTE FUNCTION fn_organization_business_identity_function_immutable();

-- ============================================================
-- Phase 2: organization_business_identity_assignment
-- ============================================================
CREATE TABLE IF NOT EXISTS organization_business_identity_assignment (
    id                   bigint                   NOT NULL,  -- TSID
    organization_id      bigint                   NOT NULL,
    business_identity_id bigint                   NOT NULL,
    function_key         text                     NOT NULL,
    user_id              bigint,                            -- assignee（可空=尚未绑定）
    status               text                     DEFAULT 'active' NOT NULL,
    assigned_at          timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    ended_at             timestamp with time zone,
    assigned_by          bigint,
    end_reason           text,
    version              integer                  DEFAULT 1 NOT NULL,
    created_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_obia PRIMARY KEY (id),
    CONSTRAINT fk_obia_identity_function
        FOREIGN KEY (organization_id, business_identity_id, function_key)
        REFERENCES organization_business_identity (organization_id, id, function_key)
        ON DELETE RESTRICT,
    CONSTRAINT fk_obia_user FOREIGN KEY (user_id) REFERENCES "user"(id) ON DELETE SET NULL,
    CONSTRAINT fk_obia_assigned_by FOREIGN KEY (assigned_by) REFERENCES "user"(id) ON DELETE SET NULL,
    CONSTRAINT ck_obia_status CHECK (status = ANY (ARRAY['active'::text, 'ended'::text])),
    CONSTRAINT ck_obia_active_requires_user CHECK (status <> 'active' OR user_id IS NOT NULL),
    CONSTRAINT ck_obia_ended_at_consistency CHECK (
        (status = 'active' AND ended_at IS NULL)
        OR (status = 'ended' AND ended_at IS NOT NULL)),
    CONSTRAINT ck_obia_time_range CHECK (ended_at IS NULL OR ended_at >= assigned_at),
    CONSTRAINT ck_obia_version CHECK (version >= 1)
);

COMMENT ON TABLE organization_business_identity_assignment IS
    '业务身份经办关系（identity <- user 的 assignee 绑定）；user 仅作 assignee，不是资源 owner；active 关系存在时禁止直接 removed（EB-D07）';
COMMENT ON COLUMN organization_business_identity_assignment.organization_id IS '资源 owner 租户；与 identity 同 Org（由复合 FK 保证）';
COMMENT ON COLUMN organization_business_identity_assignment.function_key IS '必须与 identity 的 function_key 一致（复合 FK 强制），使 active 唯一性可按 (Org,user,function) 表达';
COMMENT ON COLUMN organization_business_identity_assignment.user_id IS '经办人（assignee）；ON DELETE SET NULL + ck_obia_active_requires_user = 删除 active 经办人时数据库直接拒绝，不级联企业数据';
COMMENT ON COLUMN organization_business_identity_assignment.ended_at IS '经办结束时间；status=ended 时必须非空，status=active 时必须为空';
COMMENT ON COLUMN organization_business_identity_assignment.assigned_by IS '操作人（审计快照）';
COMMENT ON COLUMN organization_business_identity_assignment.end_reason IS '结束原因（合成/自由文本，审计用）';

-- 同一 identity 同时最多一个 active assignment（V1 基数冻结）
CREATE UNIQUE INDEX IF NOT EXISTS uq_obia_active_identity
    ON organization_business_identity_assignment (organization_id, business_identity_id)
    WHERE status = 'active';

-- 同一 Org/user/function_key 最多一个 active assignment（允许一人同时 sales + customer_service）
CREATE UNIQUE INDEX IF NOT EXISTS uq_obia_active_user_function
    ON organization_business_identity_assignment (organization_id, user_id, function_key)
    WHERE status = 'active' AND user_id IS NOT NULL;

CREATE INDEX IF NOT EXISTS i_obia_org_user ON organization_business_identity_assignment
    USING btree (organization_id, user_id);

-- ============================================================
-- Phase 3: organization_member.status 兼容扩展 + 直接移除守卫
-- ============================================================
-- 历史值 active/removed 语义不变，仅追加 suspended（EB-D07：suspended 立即撤权，不删个人账号）。
ALTER TABLE organization_member DROP CONSTRAINT IF EXISTS ck_organization_member_status;
ALTER TABLE organization_member ADD CONSTRAINT ck_organization_member_status
    CHECK (status = ANY (ARRAY['active'::text, 'suspended'::text, 'removed'::text]));

COMMENT ON COLUMN organization_member.status IS
    '状态: active 在册 | suspended 已暂停（企业业务授权立即失败，个人账号不受影响，是可恢复的撤权第一步）| removed 已移除；不联动 workspace_member';

-- 直接移除守卫：仍持有 active 企业经办关系时禁止 removed / DELETE，必须先完成 offboarding 交接。
CREATE OR REPLACE FUNCTION fn_organization_member_offboarding_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF (TG_OP = 'DELETE' AND OLD.status <> 'removed')
       OR (TG_OP = 'UPDATE' AND NEW.status = 'removed') THEN
        IF EXISTS (
            SELECT 1
              FROM organization_business_identity_assignment a
             WHERE a.organization_id = OLD.organization_id
               AND a.user_id = OLD.user_id
               AND a.status = 'active'
        ) THEN
            RAISE EXCEPTION
                'organization_member(%,%) 仍持有 active 企业经办关系，直接移除被拒绝：offboarding_required',
                OLD.organization_id, OLD.user_id
                USING ERRCODE = '23514',
                      CONSTRAINT = 'trg_organization_member_offboarding_guard';
        END IF;
    END IF;
    IF TG_OP = 'DELETE' THEN
        RETURN OLD;
    END IF;
    RETURN NEW;
END;
$$;

COMMENT ON FUNCTION fn_organization_member_offboarding_guard() IS
    '职务离场守卫：存在 active organization_business_identity_assignment 时，organization_member 的 removed 变更或 DELETE 一律 23514（offboarding_required）；suspended 不拦截';

DROP TRIGGER IF EXISTS trg_organization_member_offboarding_guard ON organization_member;
CREATE TRIGGER trg_organization_member_offboarding_guard
    BEFORE UPDATE OF status OR DELETE ON organization_member
    FOR EACH ROW EXECUTE FUNCTION fn_organization_member_offboarding_guard();

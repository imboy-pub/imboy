-- 迁移 00000136: Enterprise Internal API 基础五表（EPGZ-01 / plan-gz §5）。
-- 计划契约：plan-gz §5 数据与 migration 方案（enterprise_application /
--   enterprise_application_credential / enterprise_external_identity /
--   enterprise_internal_idempotency / enterprise_oa_sso_code）、§4.1 认证
--   （DB 只存 SHA-256 digest + 非敏感 prefix，不存明文）、§4.2 固定 scopes。
-- 任务契约：EPGZ-01（GZ 唯一 migration owner；编号由 A0 在 BASE-00 原子预留）。
-- 迁移契约：up=可重复执行，down=完整对称回滚。禁止 BEGIN/COMMIT——erlang_migrate
--   外层单事务包裹。
--
-- 设计决策：
--   * 关联表全部携带 (organization_id, id) 复合唯一约束（uq_*_org_id），
--     供子表复合 FK 引用（00000114 obia / 00000116 ec / 00000133 ag 同构）；
--     子表对 application 的引用一律复合 FK (organization_id, application_id)
--     -> enterprise_application(organization_id, id)，跨 Org 引用 23503 拒绝。
--   * credential 只存 secret_digest（SHA-256 hex，长度 64 CHECK）与全局唯一
--     credential_prefix（认证定位键，不含 secret 明文）；明文只在创建/轮换
--     响应出现一次（plan-gz §4.1）。status active|revoked 与 revoked_at 一致性
--     CHECK 沿用 00000133 agent_grant 口径；expired 不入库，由 status+expires_at
--     实时判定（同 00000133 设计）。
--   * mapping 双向唯一：(organization_id, application_id, external_user_id) 与
--     (organization_id, application_id, user_id) 各一个 UNIQUE；status
--     active|removed，重绑走 upsert 覆盖（113 organization_member 同构）。
--     目标必须是「active Human member」：FK (organization_id, user_id) ->
--     organization_member 保证在册；BEFORE 触发器补足 member active +
--     user.account_type=0（Human）+ user.status=1（正常）语义（fail-closed 23514）。
--     仅校验 status='active' 的行——removed 行允许保留历史引用（成员随后离场
--     不应连坐阻断 unbind 历史行）。
--   * idempotency 主键 (organization_id, application_id, idempotency_key)
--     （plan §5），带 expires_at NOT NULL + 清理索引；response_code/resource_id
--     首写可空，claim 后由上层回填（repo 层 UPDATE）。
--   * sso code 只存 code_digest（SHA-256 hex，全局唯一即消费定位键），绑定
--     org/app/user/redirect_uri/nonce_digest；consumed_at 单次消费 CAS 语义
--     由 UPDATE ... WHERE consumed_at IS NULL 实现（应用层），DB 侧 CHECK
--     保证 digest 形态。60 秒有效期由 expires_at 表达，不另建状态列。
--   * FK 删除行为镜像仓内惯例：organization 一律 RESTRICT（fail-closed，
--     机构走 archived 软删除不触发物理 DELETE）；principal_user_id 审计可空
--     SET NULL（00000114 created_by_user_id 同构）；其余业务 FK RESTRICT，
--     不使用 CASCADE 抹企业数据。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 1: enterprise_application
-- ============================================================
CREATE TABLE IF NOT EXISTS enterprise_application (
    id                 bigint                   NOT NULL,  -- TSID
    organization_id    bigint                   NOT NULL,
    principal_user_id  bigint,                             -- 可信 service-principal user（可空；Application 自身是独立实体）
    application_key    text                     NOT NULL,
    name               text                     NOT NULL,
    status             text                     DEFAULT 'active' NOT NULL,
    allowed_scopes     jsonb                    DEFAULT '[]'::jsonb NOT NULL,
    created_at         timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at         timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_enterprise_application PRIMARY KEY (id),
    CONSTRAINT uq_ea_org_id UNIQUE (organization_id, id),
    CONSTRAINT uq_ea_org_key UNIQUE (organization_id, application_key),
    CONSTRAINT ck_ea_status CHECK (status = ANY (ARRAY['active'::text, 'disabled'::text])),
    CONSTRAINT ck_ea_key_nonempty CHECK (application_key <> '' AND length(application_key) <= 128),
    CONSTRAINT ck_ea_name_nonempty CHECK (name <> ''),
    CONSTRAINT ck_ea_scopes_array CHECK (jsonb_typeof(allowed_scopes) = 'array'),
    CONSTRAINT fk_ea_organization FOREIGN KEY (organization_id)
        REFERENCES organization(id) ON DELETE RESTRICT,
    CONSTRAINT fk_ea_principal_user FOREIGN KEY (principal_user_id)
        REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE  enterprise_application IS
    '企业集成 Application（/api/internal/v1 调用主体，归 Organization；调用方只能是客户 OA 后端）';
COMMENT ON COLUMN enterprise_application.id IS '主键 TSID';
COMMENT ON COLUMN enterprise_application.organization_id IS '资源 owner 租户；删除机构一律 RESTRICT（fail-closed）';
COMMENT ON COLUMN enterprise_application.principal_user_id IS
    '内部绑定的可信 service-principal user（可空审计锚点）；业务判定只认 Application 自身，不依赖该 user 的账号类型字面值（plan-gz §4.3）';
COMMENT ON COLUMN enterprise_application.application_key IS 'Org 内唯一的稳定标识（URL/审计/凭证组合用）';
COMMENT ON COLUMN enterprise_application.name IS '展示名（客户 OA 后端应用名）';
COMMENT ON COLUMN enterprise_application.status IS '生命周期: active 可用 | disabled 已停用（停用即拒绝全部 internal API）';
COMMENT ON COLUMN enterprise_application.allowed_scopes IS
    '固定 scopes 白名单 jsonb 数组（plan-gz §4.2 十值；成员校验在应用层，不支持 wildcard）';

CREATE INDEX IF NOT EXISTS i_ea_org_status ON enterprise_application
    USING btree (organization_id, status);

-- ============================================================
-- Phase 2: enterprise_application_credential
-- ============================================================
CREATE TABLE IF NOT EXISTS enterprise_application_credential (
    id                bigint                   NOT NULL,  -- TSID（credential_id）
    organization_id   bigint                   NOT NULL,
    application_id    bigint                   NOT NULL,
    credential_prefix text                     NOT NULL,
    secret_digest     text                     NOT NULL,
    status            text                     DEFAULT 'active' NOT NULL,
    expires_at        timestamp with time zone,
    last_used_at      timestamp with time zone,
    created_at        timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    revoked_at        timestamp with time zone,
    CONSTRAINT pk_enterprise_application_credential PRIMARY KEY (id),
    CONSTRAINT uq_eac_org_id UNIQUE (organization_id, id),
    CONSTRAINT uq_eac_prefix UNIQUE (credential_prefix),
    CONSTRAINT ck_eac_status CHECK (status = ANY (ARRAY['active'::text, 'revoked'::text])),
    CONSTRAINT ck_eac_prefix_nonempty CHECK (credential_prefix <> '' AND length(credential_prefix) <= 128),
    CONSTRAINT ck_eac_secret_digest CHECK (length(secret_digest) = 64),
    CONSTRAINT ck_eac_status_revoked_match CHECK (
        (status = 'revoked') = (revoked_at IS NOT NULL)
    ),
    CONSTRAINT fk_eac_organization FOREIGN KEY (organization_id)
        REFERENCES organization(id) ON DELETE RESTRICT,
    CONSTRAINT fk_eac_application FOREIGN KEY (organization_id, application_id)
        REFERENCES enterprise_application (organization_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE  enterprise_application_credential IS
    'Application 凭证（形态 ib_int_<credential_id>.<secret>；DB 只存 SHA-256 digest 与非敏感 prefix，明文不落库）';
COMMENT ON COLUMN enterprise_application_credential.credential_prefix IS
    '认证定位键（credential 的非 secret 部分；全局唯一）；按 prefix 定位后 secret 走 constant-time digest 比对';
COMMENT ON COLUMN enterprise_application_credential.secret_digest IS 'secret 的 SHA-256 hex（64 字符）；明文只在创建/轮换响应出现一次';
COMMENT ON COLUMN enterprise_application_credential.status IS '生命周期: active 可用 | revoked 已吊销';
COMMENT ON COLUMN enterprise_application_credential.expires_at IS '过期时间（可空=不过期）；到期由 status+expires_at 实时判定，不落 expired 状态值';
COMMENT ON COLUMN enterprise_application_credential.last_used_at IS '最近认证时间（审计/轮换策略用）';

CREATE INDEX IF NOT EXISTS i_eac_org_app_status ON enterprise_application_credential
    USING btree (organization_id, application_id, status);

-- ============================================================
-- Phase 3: enterprise_external_identity
-- ============================================================
CREATE TABLE IF NOT EXISTS enterprise_external_identity (
    id                bigint                   NOT NULL,  -- TSID
    organization_id   bigint                   NOT NULL,
    application_id    bigint                   NOT NULL,
    external_user_id  text                     NOT NULL,
    user_id           bigint                   NOT NULL,
    status            text                     DEFAULT 'active' NOT NULL,
    created_at        timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at        timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_enterprise_external_identity PRIMARY KEY (id),
    CONSTRAINT uq_eei_org_id UNIQUE (organization_id, id),
    CONSTRAINT uq_eei_org_app_external UNIQUE (organization_id, application_id, external_user_id),
    CONSTRAINT uq_eei_org_app_user UNIQUE (organization_id, application_id, user_id),
    CONSTRAINT ck_eei_status CHECK (status = ANY (ARRAY['active'::text, 'removed'::text])),
    CONSTRAINT ck_eei_external_nonempty CHECK (external_user_id <> '' AND length(external_user_id) <= 256),
    CONSTRAINT fk_eei_organization FOREIGN KEY (organization_id)
        REFERENCES organization(id) ON DELETE RESTRICT,
    CONSTRAINT fk_eei_application FOREIGN KEY (organization_id, application_id)
        REFERENCES enterprise_application (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_eei_member FOREIGN KEY (organization_id, user_id)
        REFERENCES organization_member (organization_id, user_id) ON DELETE RESTRICT
);

COMMENT ON TABLE  enterprise_external_identity IS
    '外部身份映射（客户 OA 的 external_user_id <-> IMBoy active Human member；sender_user_id 的唯一授权来源）';
COMMENT ON COLUMN enterprise_external_identity.external_user_id IS '客户 OA 侧员工标识（同 Application 内唯一）';
COMMENT ON COLUMN enterprise_external_identity.user_id IS
    '映射目标：必须是同 Org active Human member（FK 保证在册 + 触发器校验 member active / account_type=0 / user.status=1）';
COMMENT ON COLUMN enterprise_external_identity.status IS
    '状态: active 生效 | removed 已解除（重绑走 upsert 覆盖，不自动恢复）';

-- 「active Human member」运行时守卫：仅对 status='active' 的行校验；
-- removed 行保留历史引用不校验（成员离场不连坐历史映射行）。
CREATE OR REPLACE FUNCTION fn_enterprise_external_identity_member_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_member_status text;
    v_account_type  smallint;
    v_user_status   smallint;
BEGIN
    IF NEW.status = 'active' THEN
        SELECT om.status, u.account_type, u.status
          INTO v_member_status, v_account_type, v_user_status
          FROM organization_member om
          JOIN "user" u ON u.id = om.user_id
         WHERE om.organization_id = NEW.organization_id
           AND om.user_id = NEW.user_id;

        IF v_member_status IS NULL THEN
            RAISE EXCEPTION
                'enterprise_external_identity 目标 (%，%) 不是 organization_member，不能绑定 active 映射',
                NEW.organization_id, NEW.user_id
                USING ERRCODE = '23514',
                      CONSTRAINT = 'trg_enterprise_external_identity_member_guard';
        END IF;
        IF v_member_status <> 'active' THEN
            RAISE EXCEPTION
                'enterprise_external_identity 目标 member (%) 状态为 %，仅 active 成员可绑定映射',
                NEW.user_id, v_member_status
                USING ERRCODE = '23514',
                      CONSTRAINT = 'trg_enterprise_external_identity_member_guard';
        END IF;
        IF v_account_type <> 0 THEN
            RAISE EXCEPTION
                'enterprise_external_identity 目标 (%) account_type=%，仅 Human(account_type=0) 可绑定映射',
                NEW.user_id, v_account_type
                USING ERRCODE = '23514',
                      CONSTRAINT = 'trg_enterprise_external_identity_member_guard';
        END IF;
        IF v_user_status <> 1 THEN
            RAISE EXCEPTION
                'enterprise_external_identity 目标 (%) user.status=%，仅正常(1)账号可绑定映射',
                NEW.user_id, v_user_status
                USING ERRCODE = '23514',
                      CONSTRAINT = 'trg_enterprise_external_identity_member_guard';
        END IF;
    END IF;
    RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_enterprise_external_identity_member_guard
    ON enterprise_external_identity;
CREATE TRIGGER trg_enterprise_external_identity_member_guard
    BEFORE INSERT OR UPDATE OF organization_id, application_id, user_id, status
    ON enterprise_external_identity
    FOR EACH ROW EXECUTE FUNCTION fn_enterprise_external_identity_member_guard();

COMMENT ON FUNCTION fn_enterprise_external_identity_member_guard() IS
    'active 映射目标守卫：同 Org active organization_member + Human(account_type=0) + user.status=1，否则 23514（plan-gz §5「目标必须是 active Human member」）';

-- ============================================================
-- Phase 4: enterprise_internal_idempotency
-- ============================================================
-- 主键即 (organization_id, application_id, idempotency_key)（plan §5）；
-- 无代理 id，无 (org,id) 复合唯一（无子表引用它）。
CREATE TABLE IF NOT EXISTS enterprise_internal_idempotency (
    organization_id bigint                   NOT NULL,
    application_id  bigint                   NOT NULL,
    idempotency_key text                     NOT NULL,
    request_digest  text                     NOT NULL,
    resource_type   text                     NOT NULL,
    resource_id     bigint,
    response_code   integer,
    expires_at      timestamp with time zone NOT NULL,
    created_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_enterprise_internal_idempotency PRIMARY KEY (organization_id, application_id, idempotency_key),
    CONSTRAINT ck_eii_key_nonempty CHECK (idempotency_key <> '' AND length(idempotency_key) <= 256),
    CONSTRAINT ck_eii_resource_type CHECK (resource_type <> '' AND length(resource_type) <= 64),
    CONSTRAINT ck_eii_request_digest CHECK (length(request_digest) = 64),
    CONSTRAINT fk_eii_organization FOREIGN KEY (organization_id)
        REFERENCES organization(id) ON DELETE RESTRICT,
    CONSTRAINT fk_eii_application FOREIGN KEY (organization_id, application_id)
        REFERENCES enterprise_application (organization_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE  enterprise_internal_idempotency IS
    '/api/internal/v1 幂等记录（所有 mutation 必带 Idempotency-Key；同 key 同 body 重放原结果，同 key 异 body 409 idempotency_conflict）';
COMMENT ON COLUMN enterprise_internal_idempotency.request_digest IS '请求体 SHA-256 hex（64 字符）；重放比对值';
COMMENT ON COLUMN enterprise_internal_idempotency.resource_type IS '幂等资源类型（如 message.direct / group.member）';
COMMENT ON COLUMN enterprise_internal_idempotency.resource_id IS '已创建资源 TSID（claim 前可空）';
COMMENT ON COLUMN enterprise_internal_idempotency.response_code IS '已存储响应码（claim 前可空）';
COMMENT ON COLUMN enterprise_internal_idempotency.expires_at IS '幂等窗口截止；过期后同 key 视为新请求';

CREATE INDEX IF NOT EXISTS i_eii_expires ON enterprise_internal_idempotency
    USING btree (expires_at);

-- ============================================================
-- Phase 5: enterprise_oa_sso_code
-- ============================================================
CREATE TABLE IF NOT EXISTS enterprise_oa_sso_code (
    id                bigint                   NOT NULL,  -- TSID
    organization_id   bigint                   NOT NULL,
    application_id    bigint                   NOT NULL,
    user_id           bigint                   NOT NULL,
    code_digest       text                     NOT NULL,
    redirect_uri      text                     NOT NULL,
    nonce_digest      text                     NOT NULL,
    expires_at        timestamp with time zone NOT NULL,
    consumed_at       timestamp with time zone,
    created_at        timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_enterprise_oa_sso_code PRIMARY KEY (id),
    CONSTRAINT uq_eosc_org_id UNIQUE (organization_id, id),
    CONSTRAINT uq_eosc_code_digest UNIQUE (code_digest),
    CONSTRAINT ck_eosc_code_digest CHECK (length(code_digest) = 64),
    CONSTRAINT ck_eosc_nonce_digest CHECK (length(nonce_digest) = 64),
    CONSTRAINT ck_eosc_redirect_https CHECK (redirect_uri LIKE 'https://%'),
    CONSTRAINT ck_eosc_consumed_after_created CHECK (consumed_at IS NULL OR consumed_at >= created_at),
    CONSTRAINT fk_eosc_organization FOREIGN KEY (organization_id)
        REFERENCES organization(id) ON DELETE RESTRICT,
    CONSTRAINT fk_eosc_application FOREIGN KEY (organization_id, application_id)
        REFERENCES enterprise_application (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_eosc_user FOREIGN KEY (user_id)
        REFERENCES "user"(id) ON DELETE RESTRICT
);

COMMENT ON TABLE  enterprise_oa_sso_code IS
    'OA 一次性 SSO code（60 秒；只存 code_digest；exchange 原子单次消费，重放拒绝）';
COMMENT ON COLUMN enterprise_oa_sso_code.user_id IS '发起 SSO 的 Human（/api/v1/oa/sso/code 以 Human JWT 签发）';
COMMENT ON COLUMN enterprise_oa_sso_code.code_digest IS 'opaque code 的 SHA-256 hex（64 字符）；全局唯一即消费定位键';
COMMENT ON COLUMN enterprise_oa_sso_code.redirect_uri IS '绑定的客户 exact redirect URI（exchange 必须匹配）';
COMMENT ON COLUMN enterprise_oa_sso_code.nonce_digest IS 'state/nonce 的 SHA-256 hex（防重放绑定）';
COMMENT ON COLUMN enterprise_oa_sso_code.consumed_at IS '消费时间（NULL=未消费；UPDATE ... WHERE consumed_at IS NULL 实现单次消费 CAS）';

CREATE INDEX IF NOT EXISTS i_eosc_org_app_user ON enterprise_oa_sso_code
    USING btree (organization_id, application_id, user_id);

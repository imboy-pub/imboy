-- 迁移 00000131: Widget 持久化基础（Customer Service Widget Foundation）。
-- 计划契约：POST-V4.1 run §12.4（A1：Widget installation / bootstrap / nonce
--   新迁移与 PG adapter）、§12.7 CSB-01（CSB-01-A01..A05）。
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策：
--   * bootstrap token 复用 customer_service_visit_token（CSB-01：「能复用
--     visit token 时不复制」）。visit_token 已具备与 125 同口径的
--     (organization_id, contact_id) 复合 FK（fk_csvt_contact）、token_digest
--     唯一（uq_csvt_org_digest）、expires_at / revoked_at 语义；本迁移只
--     **追加**三个可空列——widget_installation_id（复合 FK 指回 installation，
--     同 Org 口径）、anonymous_subject_hmac、last_seen_at。既有运营侧 visit
--     token 行零语义变化（三列为 NULL 即非 Widget 令牌），未放宽任何既有
--     不变量，因此不新建独立 bootstrap 表。
--   * 明文 secret / signing key / JTI 绝不落库：identity_key 只存 sha256 摘要
--     （key_digest），nonce 只存 jti_digest；public_widget_id 是公开标识
--     （wgt_pub_ 前缀由应用层生成），DB 只做 NOT NULL + 全局 UNIQUE + 非空，
--     不承载任何 secret。
--   * allowed_origins / branding 存**原文** jsonb：同源 sibling origin 规范化
--     与 branding 键白名单校验都在应用层，DB 不复制规则。
--   * JTI 重放防护的 DB 裁决点：uq_cswn_install_jti 复合唯一
--     (organization_id, widget_installation_id, jti_digest)——并发同 jti 恰好
--     一个 INSERT 成功，其余 23505。expires_at 上的 i_cswn_expiry 是定界清理
--     worker 的唯一入口（worker 本身不在本卡）。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 1: customer_service_widget_installation（Widget 安装 = Org 的公开接入面）
-- ============================================================
CREATE TABLE IF NOT EXISTS customer_service_widget_installation (
    id                 bigint                   NOT NULL,  -- TSID
    organization_id    bigint                   NOT NULL,
    public_widget_id   text                     NOT NULL,  -- 公开标识；非 secret
    display_name       text                     NOT NULL,
    allowed_origins    jsonb                    DEFAULT '[]'::jsonb NOT NULL,  -- 原文；规范化在应用层
    branding           jsonb                    DEFAULT '{}'::jsonb NOT NULL,  -- 键白名单在应用层
    consent_version    text                     NOT NULL,
    status             text                     DEFAULT 'active' NOT NULL,
    revoked_at         timestamp with time zone,
    created_by_user_id bigint,
    version            integer                  DEFAULT 1 NOT NULL,
    created_at         timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at         timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_customer_service_widget_installation PRIMARY KEY (id),
    CONSTRAINT uq_cswi_org_id UNIQUE (organization_id, id),
    CONSTRAINT uq_cswi_public_widget_id UNIQUE (public_widget_id),
    CONSTRAINT ck_cswi_public_widget_id CHECK (public_widget_id <> ''),
    CONSTRAINT ck_cswi_display_name CHECK (display_name <> ''),
    CONSTRAINT ck_cswi_consent_version CHECK (consent_version <> ''),
    CONSTRAINT ck_cswi_status CHECK (status = ANY (ARRAY['active'::text, 'revoked'::text])),
    CONSTRAINT ck_cswi_revoked_consistency CHECK (
        (status = 'active' AND revoked_at IS NULL)
        OR (status = 'revoked' AND revoked_at IS NOT NULL)),
    CONSTRAINT ck_cswi_version CHECK (version >= 1),
    CONSTRAINT fk_cswi_organization FOREIGN KEY (organization_id)
        REFERENCES organization(id) ON DELETE RESTRICT,
    CONSTRAINT fk_cswi_created_by FOREIGN KEY (created_by_user_id)
        REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE customer_service_widget_installation IS
    'Widget 安装（Org 的公开接入面）：public_widget_id 可公开分发但只在本 Org 解析；
    明文 secret 不在本表（密钥在 widget_identity_key，只存 digest）';
COMMENT ON COLUMN customer_service_widget_installation.public_widget_id IS
    '公开 Widget 标识（wgt_pub_ 前缀由应用层生成）：可嵌入客户页面，不携带任何 secret；
    换取任何权限都必须携带 Org 作用域（CSB-01-A02）';
COMMENT ON COLUMN customer_service_widget_installation.allowed_origins IS
    '允许嵌入的 origin 原文 jsonb 数组；同源 sibling origin 规范化由应用层做（DB 存原文）';
COMMENT ON COLUMN customer_service_widget_installation.branding IS
    '品牌配置 jsonb；键白名单校验由应用层做，DB 不解释内容';
COMMENT ON COLUMN customer_service_widget_installation.consent_version IS
    'Widget 侧访客同意文案版本标识（应用层生成；非空）';

CREATE INDEX IF NOT EXISTS i_cswi_org_status ON customer_service_widget_installation
    USING btree (organization_id, status);

-- ============================================================
-- Phase 2: customer_service_widget_identity_key（signing key；只存摘要）
-- ============================================================
CREATE TABLE IF NOT EXISTS customer_service_widget_identity_key (
    id              bigint                   NOT NULL,  -- TSID
    organization_id bigint                   NOT NULL,
    installation_id bigint                   NOT NULL,
    key_digest      text                     NOT NULL,  -- sha256 hex；明文 signing key 绝不落库
    key_version     integer                  NOT NULL,
    display_hint    text,                               -- 掩码提示（如明文后 4 位）
    status          text                     DEFAULT 'active' NOT NULL,
    expires_at      timestamp with time zone NOT NULL,
    revoked_at      timestamp with time zone,
    created_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_customer_service_widget_identity_key PRIMARY KEY (id),
    CONSTRAINT uq_cswk_org_id UNIQUE (organization_id, id),
    CONSTRAINT uq_cswk_install_version UNIQUE (organization_id, installation_id, key_version),
    CONSTRAINT ck_cswk_digest CHECK (key_digest <> ''),
    CONSTRAINT ck_cswk_key_version CHECK (key_version >= 1),
    CONSTRAINT ck_cswk_status CHECK (status = ANY (ARRAY['active'::text, 'revoked'::text])),
    CONSTRAINT ck_cswk_expiry CHECK (expires_at > created_at),
    CONSTRAINT ck_cswk_revoked_consistency CHECK (
        (status = 'active' AND revoked_at IS NULL)
        OR (status = 'revoked' AND revoked_at IS NOT NULL)),
    CONSTRAINT fk_cswk_installation FOREIGN KEY (organization_id, installation_id)
        REFERENCES customer_service_widget_installation (organization_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE customer_service_widget_identity_key IS
    'Widget 身份签名密钥（安装级，key_version 轮换）：只存 sha256 摘要；
    明文 signing key 只在创建时返回一次（EB-D10 同口径）';
COMMENT ON COLUMN customer_service_widget_identity_key.key_version IS
    '密钥版本：同一 installation 内单调轮换（复合唯一约束防并发重放同版本）';

-- ============================================================
-- Phase 3: bootstrap token 复用 customer_service_visit_token（只追加列）
-- ============================================================
ALTER TABLE customer_service_visit_token
    ADD COLUMN IF NOT EXISTS widget_installation_id bigint,
    ADD COLUMN IF NOT EXISTS anonymous_subject_hmac text,
    ADD COLUMN IF NOT EXISTS last_seen_at timestamp with time zone;

ALTER TABLE customer_service_visit_token
    DROP CONSTRAINT IF EXISTS fk_csvt_widget_installation;
ALTER TABLE customer_service_visit_token
    ADD CONSTRAINT fk_csvt_widget_installation
    FOREIGN KEY (organization_id, widget_installation_id)
    REFERENCES customer_service_widget_installation (organization_id, id)
    ON DELETE RESTRICT;

COMMENT ON COLUMN customer_service_visit_token.widget_installation_id IS
    'Widget 令牌的安装绑定（NULL=运营侧签发的 visit token）：复合 FK 同 Org 口径，
    Widget bootstrap 令牌必须绑定本 Org 的 installation';
COMMENT ON COLUMN customer_service_visit_token.anonymous_subject_hmac IS
    '匿名主体 HMAC（应用层计算）：不存裸会员标识 / Cookie / 支付信息，只存 HMAC';
COMMENT ON COLUMN customer_service_visit_token.last_seen_at IS
    'Widget 令牌最近一次使用时间（心跳/活跃）；运营侧令牌为 NULL';

-- Widget 令牌按安装维度检索/吊销的索引（定界）。
CREATE INDEX IF NOT EXISTS i_csvt_widget_install ON customer_service_visit_token
    USING btree (organization_id, widget_installation_id)
    WHERE widget_installation_id IS NOT NULL;

-- ============================================================
-- Phase 4: customer_service_widget_nonce（JTI 重放防护；DB 唯一裁决）
-- ============================================================
CREATE TABLE IF NOT EXISTS customer_service_widget_nonce (
    id              bigint                   NOT NULL,  -- TSID
    organization_id bigint                   NOT NULL,
    installation_id bigint                   NOT NULL,
    jti_digest      text                     NOT NULL,  -- sha256 hex；明文 JTI 绝不落库
    expires_at      timestamp with time zone NOT NULL,
    created_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_customer_service_widget_nonce PRIMARY KEY (id),
    CONSTRAINT uq_cswn_org_id UNIQUE (organization_id, id),
    CONSTRAINT uq_cswn_install_jti UNIQUE (organization_id, installation_id, jti_digest),
    CONSTRAINT ck_cswn_jti CHECK (jti_digest <> ''),
    CONSTRAINT ck_cswn_expiry CHECK (expires_at > created_at),
    CONSTRAINT fk_cswn_installation FOREIGN KEY (organization_id, installation_id)
        REFERENCES customer_service_widget_installation (organization_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE customer_service_widget_nonce IS
    'Widget 请求 JTI 防重放（append-only）：并发同 jti 恰好一个 INSERT 成功（23505=重放）；
    行只按 expires_at 定界清理';

-- 定界清理索引：清理 worker 只按 expires_at 扫描（本卡不含 worker）。
CREATE INDEX IF NOT EXISTS i_cswn_expiry ON customer_service_widget_nonce
    USING btree (expires_at);

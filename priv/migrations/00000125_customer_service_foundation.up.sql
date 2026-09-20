-- 迁移 00000125: 客服基础五表（Customer Service Foundation）。
-- 计划契约：
--   §4.2（customer_service_seat / shop_key / visit_token / session / event）、
--   §3 EB-D02/EB-D03/EB-D05/EB-D10、§5.2、CS-01-A01..A05。
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策：
--   * seat 的 PK 就是 business_identity_id（§4.2 冻结）：seat 是 identity 的运营属性
--     （enabled / max_concurrent），不含 owner user——seat 的当前用户来自 active
--     identity assignment，不落在 seat 表上（A04 rebind 连续性的表结构前提）。
--   * A01 的数据库级保证：seat.function_key 固定 'customer_service'（CHECK），
--     并以复合 FK (organization_id, business_identity_id, function_key) 指向
--     organization_business_identity (organization_id, id, function_key)——
--     引用 sales identity 的 INSERT 直接被外键拒绝，绕过应用也进不来。
--   * 三分（EB-D03）：owner 恒为 organization_id；business_identity_id 是经办；
--     user 列全部只是审计快照（ON DELETE SET NULL）。禁止 CASCADE 到 user。
--   * session 绑定 (organization_id, workspace_id, conversation_id, contact_id)，
--     且 (org, conversation, contact) 复合 FK 保证 session 的 contact 与
--     enterprise_conversation 的 contact 一致；不存任何消息副本（真源是
--     enterprise_message，客服消息只经 enterprise_business_facade 写入，A03）。
--   * A02 的数据库级基础：session.status + version 行级 CAS（应用在单事务内
--     FOR UPDATE 锁 seat 行后按 (status,version) 条件 UPDATE，恰好一个并发成功）；
--     max_concurrent 上限由应用在同一事务里以 active 计数裁决（seat 行锁串行化）。
--   * 访客凭证只存 digest（sha256 hex）：明文 shop key / visit token 只在创建时
--     返回一次；吊销/过期由列状态表达（EB-D10：visit key 不是登录凭证）。
--   * event 是客服域 append-only 状态审计（镜像 119 的守卫）；业务消息/附件的
--     审计仍以 enterprise_audit_event 为真源，本表不复制其内容。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 1: customer_service_seat（客服坐席 = identity 的运营属性）
-- ============================================================
CREATE TABLE IF NOT EXISTS customer_service_seat (
    organization_id      bigint                   NOT NULL,
    business_identity_id bigint                   NOT NULL,  -- PK 即 identity；TSID 全局唯一
    function_key         text                     DEFAULT 'customer_service' NOT NULL,
    enabled              boolean                  DEFAULT true NOT NULL,
    max_concurrent       integer                  DEFAULT 1 NOT NULL,
    version              integer                  DEFAULT 1 NOT NULL,
    created_by_user_id   bigint,
    created_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_customer_service_seat PRIMARY KEY (business_identity_id),
    CONSTRAINT uq_css_org_identity UNIQUE (organization_id, business_identity_id),
    CONSTRAINT ck_css_function_key CHECK (function_key = 'customer_service'),
    CONSTRAINT ck_css_max_concurrent CHECK (max_concurrent >= 1),
    CONSTRAINT ck_css_version CHECK (version >= 1),
    CONSTRAINT fk_css_identity_function
        FOREIGN KEY (organization_id, business_identity_id, function_key)
        REFERENCES organization_business_identity (organization_id, id, function_key)
        ON DELETE RESTRICT,
    CONSTRAINT fk_css_created_by FOREIGN KEY (created_by_user_id)
        REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE customer_service_seat IS
    '客服坐席（identity 的运营属性，归 Organization）：PK=business_identity_id；function 必须 customer_service；不含 owner user（A01/A04）';
COMMENT ON COLUMN customer_service_seat.business_identity_id IS
    '坐席对应的稳定业务身份；当前用户由 active identity assignment 决定，identity rebind 后 seat 原地继续可用';
COMMENT ON COLUMN customer_service_seat.function_key IS
    '固定 customer_service（CHECK + 复合 FK 双重保证引用的一定是客服 identity，A01）';
COMMENT ON COLUMN customer_service_seat.enabled IS
    '坐席开关：false 时新 claim 立即被拒（suspend seat），既有 active 会话不自动迁移';
COMMENT ON COLUMN customer_service_seat.max_concurrent IS
    '该坐席允许的最大 active 会话数；claim 时在 seat 行锁内按 active 计数裁决（A02）';

CREATE INDEX IF NOT EXISTS i_css_org_enabled ON customer_service_seat
    USING btree (organization_id, enabled);

-- ============================================================
-- Phase 2: customer_service_shop_key（门店接入密钥；digest 存储）
-- ============================================================
CREATE TABLE IF NOT EXISTS customer_service_shop_key (
    id                 bigint                   NOT NULL,  -- TSID
    organization_id    bigint                   NOT NULL,
    key_digest         text                     NOT NULL,  -- sha256 hex；明文只返回一次
    display_hint       text,                               -- 掩码提示（如明文后 4 位）
    status             text                     DEFAULT 'active' NOT NULL,
    revoked_at         timestamp with time zone,
    created_by_user_id bigint,
    version            integer                  DEFAULT 1 NOT NULL,
    created_at         timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at         timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_customer_service_shop_key PRIMARY KEY (id),
    CONSTRAINT uq_cssk_org_digest UNIQUE (organization_id, key_digest),
    CONSTRAINT ck_cssk_status CHECK (status = ANY (ARRAY['active'::text, 'revoked'::text])),
    CONSTRAINT ck_cssk_digest CHECK (key_digest <> ''),
    CONSTRAINT ck_cssk_revoked_consistency CHECK (
        (status = 'active' AND revoked_at IS NULL)
        OR (status = 'revoked' AND revoked_at IS NOT NULL)),
    CONSTRAINT ck_cssk_version CHECK (version >= 1),
    CONSTRAINT fk_cssk_organization FOREIGN KEY (organization_id)
        REFERENCES organization(id) ON DELETE RESTRICT,
    CONSTRAINT fk_cssk_created_by FOREIGN KEY (created_by_user_id)
        REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE customer_service_shop_key IS
    '门店接入密钥（Org 级）：只存 sha256 digest；明文仅创建响应返回一次，可吊销（EB-D10 cs_shop_key）';
COMMENT ON COLUMN customer_service_shop_key.key_digest IS
    '明文密钥的 sha256 hex；数据库永不保存明文，丢明文只能吊销重发';

-- ============================================================
-- Phase 3: customer_service_visit_token（访客令牌；绑定 Org+contact）
-- ============================================================
CREATE TABLE IF NOT EXISTS customer_service_visit_token (
    id                             bigint                   NOT NULL,  -- TSID
    organization_id                bigint                   NOT NULL,
    contact_id                     bigint                   NOT NULL,
    token_digest                   text                     NOT NULL,  -- sha256 hex；明文只返回一次
    display_hint                   text,
    expires_at                     timestamp with time zone NOT NULL,
    revoked_at                     timestamp with time zone,
    created_by_business_identity_id bigint,
    created_by_user_id             bigint,
    version                        integer                  DEFAULT 1 NOT NULL,
    created_at                     timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at                     timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_customer_service_visit_token PRIMARY KEY (id),
    CONSTRAINT uq_csvt_org_id UNIQUE (organization_id, id),
    CONSTRAINT uq_csvt_org_digest UNIQUE (organization_id, token_digest),
    CONSTRAINT ck_csvt_digest CHECK (token_digest <> ''),
    CONSTRAINT ck_csvt_expiry CHECK (expires_at > created_at),
    CONSTRAINT ck_csvt_version CHECK (version >= 1),
    CONSTRAINT fk_csvt_contact FOREIGN KEY (organization_id, contact_id)
        REFERENCES enterprise_contact (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_csvt_created_by_identity FOREIGN KEY (organization_id, created_by_business_identity_id)
        REFERENCES organization_business_identity (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_csvt_created_by FOREIGN KEY (created_by_user_id)
        REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE customer_service_visit_token IS
    '访客令牌（EB-D10 cs_visit）：digest 有效 + 未过期 + 未吊销才可对绑定的 Org/contact 发消息/查自己的会话；不是 member/seat 凭证（A05）';
COMMENT ON COLUMN customer_service_visit_token.expires_at IS
    '过期时间；过期后 digest 即使未吊销也立即失效';

CREATE INDEX IF NOT EXISTS i_csvt_org_contact ON customer_service_visit_token
    USING btree (organization_id, contact_id);

-- ============================================================
-- Phase 4: customer_service_session（客服会话；不存消息副本）
-- ============================================================
-- session 的 (org, conversation, contact) 一致性需要一个可被复合 FK 引用的
-- enterprise_conversation 唯一索引；116 未提供 (organization_id, id, contact_id)。
-- 这是纯追加索引（不改任何既有行/约束），使数据库拒绝 contact 与会话不一致的 session。
CREATE UNIQUE INDEX IF NOT EXISTS uq_ec_org_id_contact
    ON enterprise_conversation (organization_id, id, contact_id);

CREATE TABLE IF NOT EXISTS customer_service_session (
    id                   bigint                   NOT NULL,  -- TSID
    organization_id      bigint                   NOT NULL,
    workspace_id         bigint                   NOT NULL,  -- 铁律 6：租户操作范围显式贯穿
    contact_id           bigint                   NOT NULL,
    conversation_id      bigint                   NOT NULL,  -- 企业会话真源；消息只经 facade 写入
    business_identity_id bigint,                             -- 当前服务坐席 identity；queued 时为空
    visit_token_id       bigint,                             -- 开会话的访客令牌（审计；可空）
    status               text                     DEFAULT 'queued' NOT NULL,
    rating               integer,                            -- 1..5；仅 closed 会话可评
    rating_at            timestamp with time zone,
    queued_at            timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    claimed_at           timestamp with time zone,
    closed_at            timestamp with time zone,
    close_reason         text,
    created_by_user_id   bigint,
    version              integer                  DEFAULT 1 NOT NULL,  -- CAS 乐观锁
    created_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_customer_service_session PRIMARY KEY (id),
    CONSTRAINT uq_csss_org_id UNIQUE (organization_id, id),
    CONSTRAINT ck_csss_status CHECK (status = ANY (ARRAY['queued'::text, 'active'::text, 'closed'::text])),
    CONSTRAINT ck_csss_active_requires_identity CHECK (status <> 'active' OR business_identity_id IS NOT NULL),
    CONSTRAINT ck_csss_queued_no_identity CHECK (status <> 'queued' OR business_identity_id IS NULL),
    CONSTRAINT ck_csss_closed_time CHECK (status <> 'closed' OR closed_at IS NOT NULL),
    CONSTRAINT ck_csss_rating_range CHECK (rating IS NULL OR rating BETWEEN 1 AND 5),
    CONSTRAINT ck_csss_rating_needs_closed CHECK (rating IS NULL OR status = 'closed'),
    CONSTRAINT ck_csss_rating_time CHECK (
        (rating IS NULL AND rating_at IS NULL) OR (rating IS NOT NULL AND rating_at IS NOT NULL)),
    CONSTRAINT ck_csss_version CHECK (version >= 1),
    CONSTRAINT fk_csss_workspace FOREIGN KEY (organization_id, workspace_id)
        REFERENCES workspace (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_csss_contact FOREIGN KEY (organization_id, contact_id)
        REFERENCES enterprise_contact (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_csss_conversation FOREIGN KEY (organization_id, conversation_id, contact_id)
        REFERENCES enterprise_conversation (organization_id, id, contact_id) ON DELETE RESTRICT,
    CONSTRAINT fk_csss_seat FOREIGN KEY (organization_id, business_identity_id)
        REFERENCES customer_service_seat (organization_id, business_identity_id) ON DELETE RESTRICT,
    CONSTRAINT fk_csss_visit_token FOREIGN KEY (organization_id, visit_token_id)
        REFERENCES customer_service_visit_token (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_csss_created_by FOREIGN KEY (created_by_user_id)
        REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE customer_service_session IS
    '客服会话（queued/active/closed/version）：绑定企业会话真源；不存任何消息副本，客服消息只经 enterprise_business_facade 写 enterprise_message（A03）';
COMMENT ON COLUMN customer_service_session.business_identity_id IS
    '当前服务坐席的业务身份（不是 user_id）：identity rebind 换人后本行与历史零迁移仍然连续（A04）';
COMMENT ON COLUMN customer_service_session.version IS
    'CAS 乐观锁：claim/transfer/close 按 (status, version) 条件 UPDATE，并发恰好一个成功（A02）';
COMMENT ON COLUMN customer_service_session.rating IS
    '满意度评分 1..5；只允许写在 closed 会话上，写评分同时落 customer_service_event 审计';

CREATE INDEX IF NOT EXISTS i_csss_org_status ON customer_service_session
    USING btree (organization_id, status);
-- 同一会话同一时刻至多一个开放客服 session（§4.2 部分唯一索引；表级 UNIQUE
-- 约束不支持 WHERE，必须用独立部分唯一索引表达）
CREATE UNIQUE INDEX IF NOT EXISTS uq_csss_org_conv_open ON customer_service_session
    USING btree (organization_id, conversation_id)
    WHERE (status <> 'closed');
CREATE INDEX IF NOT EXISTS i_csss_org_identity_active ON customer_service_session
    USING btree (organization_id, business_identity_id)
    WHERE (status = 'active');
CREATE INDEX IF NOT EXISTS i_csss_org_contact ON customer_service_session
    USING btree (organization_id, contact_id);

-- ============================================================
-- Phase 5: customer_service_event（客服状态审计；append-only）
-- ============================================================
CREATE TABLE IF NOT EXISTS customer_service_event (
    id                   bigint                   NOT NULL,  -- TSID
    organization_id      bigint                   NOT NULL,
    session_id           bigint,
    business_identity_id bigint,
    actor_user_id        bigint,
    actor_kind           text,                               -- seat | visitor | tenant_admin | platform_admin | system
    action               text                     NOT NULL,  -- seat.created / session.claimed / ...
    detail               jsonb                    DEFAULT '{}'::jsonb NOT NULL,
    created_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_customer_service_event PRIMARY KEY (id),
    CONSTRAINT uq_cse_org_id UNIQUE (organization_id, id),
    CONSTRAINT ck_cse_action CHECK (action <> ''),
    CONSTRAINT fk_cse_organization FOREIGN KEY (organization_id)
        REFERENCES organization(id) ON DELETE RESTRICT,
    CONSTRAINT fk_cse_session FOREIGN KEY (organization_id, session_id)
        REFERENCES customer_service_session (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_cse_identity FOREIGN KEY (organization_id, business_identity_id)
        REFERENCES organization_business_identity (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_cse_actor FOREIGN KEY (actor_user_id)
        REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE customer_service_event IS
    '客服域状态审计（append-only）：seat/session/key/token 的运营状态变化；业务消息/附件审计仍以 enterprise_audit_event 为真源';
COMMENT ON COLUMN customer_service_event.actor_kind IS
    '执行主体类别快照：seat | visitor | tenant_admin | platform_admin | system';

CREATE INDEX IF NOT EXISTS i_cse_org_session ON customer_service_event
    USING btree (organization_id, session_id);
CREATE INDEX IF NOT EXISTS i_cse_org_created ON customer_service_event
    USING btree (organization_id, created_at DESC);

-- append-only 守卫（镜像 119）：DELETE 一律拒绝；UPDATE 仅允许 user 删除时
-- actor_user_id 由 FK 置 NULL，其余任何列改写一律 23514。
CREATE OR REPLACE FUNCTION fn_customer_service_event_append_only() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION
            'customer_service_event 是 append-only 审计真源，禁止 DELETE'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_customer_service_event_append_only';
    END IF;

    IF (NEW.id, NEW.organization_id, NEW.session_id, NEW.business_identity_id,
        NEW.actor_kind, NEW.action, NEW.detail, NEW.created_at)
       IS DISTINCT FROM
       (OLD.id, OLD.organization_id, OLD.session_id, OLD.business_identity_id,
        OLD.actor_kind, OLD.action, OLD.detail, OLD.created_at)
       OR NEW.actor_user_id IS NOT NULL THEN
        RAISE EXCEPTION
            'customer_service_event 是 append-only 审计真源，禁止 UPDATE（仅允许 user 删除时 actor_user_id 置 NULL）'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_customer_service_event_append_only';
    END IF;

    RETURN NEW;
END;
$$;

COMMENT ON FUNCTION fn_customer_service_event_append_only() IS
    '客服审计 append-only 守卫：DELETE 一律 23514；UPDATE 一律 23514（仅 FK 置 NULL actor_user_id 例外）';

DROP TRIGGER IF EXISTS trg_customer_service_event_append_only ON customer_service_event;
CREATE TRIGGER trg_customer_service_event_append_only
    BEFORE UPDATE OR DELETE ON customer_service_event
    FOR EACH ROW EXECUTE FUNCTION fn_customer_service_event_append_only();

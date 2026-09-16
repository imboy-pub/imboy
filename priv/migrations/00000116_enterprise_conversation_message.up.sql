-- 迁移 00000116: 企业会话与企业消息（Enterprise Conversation & Message）。
-- 计划契约：§3 EB-D05（企业会话和消息）、§4.1（enterprise_conversation / enterprise_message /
--   enterprise_message_delivery）、§4.3（会话/消息必须显式 Workspace 且与资源 OrgId 一致）。
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策：
--   * 跨 Org Workspace 在 DB 层被拒绝的唯一实现点：workspace 上追加 UNIQUE (organization_id, id)
--     （纯追加，不改 00000076 语义），再由 conversation/message 的复合 FK
--     (organization_id, workspace_id) → workspace(organization_id, id) 强制同 Org。
--     organization_id IS NULL 的 workspace 因复合 FK 匹配不到而天然被拒绝。
--   * sender 使用两个显式 nullable 复合 FK（sender_contact_id / sender_business_identity_id）
--     + XOR CHECK，而不是无法建 FK 的多态 sender_id（EB-D05）。
--   * 「出站必须有 actor」用 BEFORE INSERT 触发器而非 CHECK：CHECK 会在删除 user 触发
--     FK SET NULL 时炸掉，破坏「删除 user 不级联企业数据」（A03）。（入站无 actor 用 CHECK，
--     因为入站 message 本来就不写 actor_user_id，SET NULL 对它无副作用。）
--   * retain_until 快照只能后移：策略延长允许、前移（提前删除）由触发器 fail-closed 拒绝。
--   * delivery 只拥有投递状态，可独立压缩/清理，不拥有消息内容，也不得 DELETE/UPDATE canonical message。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 1: workspace 追加复合 FK 引用目标（纯追加，不改历史迁移）
-- ============================================================
ALTER TABLE workspace DROP CONSTRAINT IF EXISTS uq_workspace_organization_id_id;
ALTER TABLE workspace ADD CONSTRAINT uq_workspace_organization_id_id UNIQUE (organization_id, id);

COMMENT ON CONSTRAINT uq_workspace_organization_id_id ON workspace IS
    '企业复合 FK 引用目标：使 enterprise_conversation/message 能以 (organization_id, workspace_id) 强制「Workspace 属于同一 Org」；organization_id IS NULL 的历史 Workspace 因匹配不到而被拒绝';

-- ============================================================
-- Phase 2: enterprise_conversation
-- ============================================================
CREATE TABLE IF NOT EXISTS enterprise_conversation (
    id                   bigint                   NOT NULL,  -- TSID
    organization_id      bigint                   NOT NULL,
    workspace_id         bigint                   NOT NULL,
    contact_id           bigint                   NOT NULL,
    business_identity_id bigint,
    status               text                     DEFAULT 'active' NOT NULL,
    version              integer                  DEFAULT 1 NOT NULL,
    notice_version       text,
    consent_at           timestamp with time zone,
    consent_subject      text,
    created_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_enterprise_conversation PRIMARY KEY (id),
    CONSTRAINT uq_ec_org_id UNIQUE (organization_id, id),
    CONSTRAINT uq_ec_org_ws_id UNIQUE (organization_id, workspace_id, id),
    CONSTRAINT ck_ec_status CHECK (status = ANY (ARRAY['active'::text, 'closed'::text])),
    CONSTRAINT ck_ec_consent CHECK (consent_at IS NULL OR notice_version IS NOT NULL),
    CONSTRAINT ck_ec_version CHECK (version >= 1),
    CONSTRAINT fk_ec_workspace FOREIGN KEY (organization_id, workspace_id)
        REFERENCES workspace (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_ec_contact FOREIGN KEY (organization_id, contact_id)
        REFERENCES enterprise_contact (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_ec_identity FOREIGN KEY (organization_id, business_identity_id)
        REFERENCES organization_business_identity (organization_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE enterprise_conversation IS
    '企业会话（归 Org）：显式绑定同 Org 的 Workspace，并关联 client 与当前经办 identity；handover 只改当前 identity，不改历史 message';
COMMENT ON COLUMN enterprise_conversation.workspace_id IS '所属 Workspace；V1 由服务端解析默认 Workspace，API 不接受调用方任意指定/切换；跨 Org 由复合 FK 拒绝';
COMMENT ON COLUMN enterprise_conversation.status IS '状态: active 进行中 | closed 已关闭（关闭不删除内容）';
COMMENT ON COLUMN enterprise_conversation.notice_version IS '首次会话的告知文本版本；未同意不得持久化内容（真实告知文本由人工 Gate 决定）';
COMMENT ON COLUMN enterprise_conversation.consent_at IS '同意时间；非空时 notice_version 必须非空';
COMMENT ON COLUMN enterprise_conversation.consent_subject IS '同意主体标识（合成/受控值；不得存放真实客户标识）';

CREATE INDEX IF NOT EXISTS i_ec_org_status ON enterprise_conversation
    USING btree (organization_id, workspace_id, status);

-- ============================================================
-- Phase 3: enterprise_message
-- ============================================================
CREATE TABLE IF NOT EXISTS enterprise_message (
    id                          bigint                   NOT NULL,  -- TSID
    organization_id             bigint                   NOT NULL,
    workspace_id                bigint                   NOT NULL,
    conversation_id             bigint                   NOT NULL,
    sender_type                 text                     NOT NULL,
    sender_contact_id           bigint,
    sender_business_identity_id bigint,
    actor_user_id               bigint,
    client_msg_id               text                     NOT NULL,
    body_cipher                 text,
    key_version                 integer,
    aad_hash                    text,
    content_hash                text,
    policy_id                   bigint,
    policy_version              integer,
    retention_days              integer                  NOT NULL,
    retain_until                timestamp with time zone NOT NULL,
    visibility                  text                     DEFAULT 'visible' NOT NULL,
    version                     integer                  DEFAULT 1 NOT NULL,
    created_at                  timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at                  timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_enterprise_message PRIMARY KEY (id),
    CONSTRAINT uq_em_org_ws_id UNIQUE (organization_id, workspace_id, id),
    CONSTRAINT uq_em_org_ws_conversation_id UNIQUE (organization_id, workspace_id, conversation_id, id),
    CONSTRAINT uq_em_org_conversation_client_msg UNIQUE (organization_id, conversation_id, client_msg_id),
    CONSTRAINT ck_em_sender_type CHECK (sender_type = ANY (ARRAY['contact'::text, 'business_identity'::text])),
    CONSTRAINT ck_em_sender_xor CHECK (
        (sender_type = 'contact'
         AND sender_contact_id IS NOT NULL
         AND sender_business_identity_id IS NULL)
        OR (sender_type = 'business_identity'
            AND sender_contact_id IS NULL
            AND sender_business_identity_id IS NOT NULL)),
    CONSTRAINT ck_em_inbound_no_actor CHECK (sender_type <> 'contact' OR actor_user_id IS NULL),
    CONSTRAINT ck_em_visibility CHECK (visibility = ANY (ARRAY['visible'::text, 'hidden'::text])),
    CONSTRAINT ck_em_retention_days CHECK (retention_days > 0),
    CONSTRAINT ck_em_version CHECK (version >= 1),
    CONSTRAINT fk_em_conversation FOREIGN KEY (organization_id, workspace_id, conversation_id)
        REFERENCES enterprise_conversation (organization_id, workspace_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_em_sender_contact FOREIGN KEY (organization_id, sender_contact_id)
        REFERENCES enterprise_contact (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_em_sender_identity FOREIGN KEY (organization_id, sender_business_identity_id)
        REFERENCES organization_business_identity (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_em_actor FOREIGN KEY (actor_user_id) REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE enterprise_message IS
    '企业消息（canonical 真源，归 Org/Workspace/Conversation）：不入个人 msg_c2c / ACK 清理链；hide/撤回只追加 tombstone 与 audit 并改 visibility';
COMMENT ON COLUMN enterprise_message.sender_type IS '发送方类型: contact 客户入站 | business_identity 员工出站；与两个 sender FK 构成 XOR';
COMMENT ON COLUMN enterprise_message.sender_contact_id IS '客户入站时的 contact（复合 FK 保证同 Org）；出站必须为 NULL';
COMMENT ON COLUMN enterprise_message.sender_business_identity_id IS '员工出站的 business identity（复合 FK 保证同 Org）；入站必须为 NULL';
COMMENT ON COLUMN enterprise_message.actor_user_id IS '执行该次发送的 user（仅审计/展示，不参与资源 owner 判定）；user 删除后置 NULL，因此「出站必须有 actor」由 BEFORE INSERT 触发器强制而非 CHECK';
COMMENT ON COLUMN enterprise_message.key_version IS 'body_cipher 对应的企业托管密钥版本；AAD 至少绑定 OrgId/WorkspaceId/ConversationId/MessageId';
COMMENT ON COLUMN enterprise_message.aad_hash IS 'AAD 绑定摘要（合成/哈希值），用于证明 AAD 不符时 fail-closed';
COMMENT ON COLUMN enterprise_message.policy_id IS '接受时固化的 retention policy 版本 ID（快照，不随未来策略回填）';
COMMENT ON COLUMN enterprise_message.retain_until IS '保留截止时间快照；只能后移（延长），前移由触发器拒绝；到期且无 active hold 才允许 bounded purge';
COMMENT ON COLUMN enterprise_message.visibility IS '可见性: visible 可见 | hidden 客户端隐藏/撤回 tombstone（不改变行存在性）';

CREATE INDEX IF NOT EXISTS i_em_org_conversation_retain ON enterprise_message
    USING btree (organization_id, workspace_id, conversation_id, retain_until);
CREATE INDEX IF NOT EXISTS i_em_org_retain_until ON enterprise_message
    USING btree (organization_id, workspace_id, retain_until);

-- 出站消息必须记录 actor（写入时强制；不用 CHECK，避免 user 删除触发 SET NULL 时炸掉）
CREATE OR REPLACE FUNCTION fn_enterprise_message_actor_required() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.sender_type = 'business_identity' AND NEW.actor_user_id IS NULL THEN
        RAISE EXCEPTION
            'enterprise_message % 为出站消息（business_identity），必须在写入时记录 actor_user_id',
            NEW.id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_message_actor_required';
    END IF;
    RETURN NEW;
END;
$$;

COMMENT ON FUNCTION fn_enterprise_message_actor_required() IS
    '出站消息 actor 强制：sender_type=business_identity 且 actor_user_id IS NULL 时 23514。刻意用触发器而非 CHECK，使删除 user 引发的 SET NULL 仍可成功（A03 不级联企业数据）';

DROP TRIGGER IF EXISTS trg_enterprise_message_actor_required ON enterprise_message;
CREATE TRIGGER trg_enterprise_message_actor_required
    BEFORE INSERT ON enterprise_message
    FOR EACH ROW EXECUTE FUNCTION fn_enterprise_message_actor_required();

-- retain_until 快照只能后移
CREATE OR REPLACE FUNCTION fn_enterprise_message_retention_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.retain_until < OLD.retain_until THEN
        RAISE EXCEPTION
            'enterprise_message % 的 retain_until 只能后移：% -> % 被拒绝（禁止提前删除）',
            OLD.id, OLD.retain_until, NEW.retain_until
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_message_retention_guard';
    END IF;
    RETURN NEW;
END;
$$;

COMMENT ON FUNCTION fn_enterprise_message_retention_guard() IS
    '保留期快照守卫：retain_until 前移（缩短）一律 23514；只允许经审计延长（EB-D12）';

DROP TRIGGER IF EXISTS trg_enterprise_message_retention_guard ON enterprise_message;
CREATE TRIGGER trg_enterprise_message_retention_guard
    BEFORE UPDATE OF retain_until ON enterprise_message
    FOR EACH ROW EXECUTE FUNCTION fn_enterprise_message_retention_guard();

-- ============================================================
-- Phase 4: enterprise_message_delivery
-- ============================================================
CREATE TABLE IF NOT EXISTS enterprise_message_delivery (
    id              bigint                   NOT NULL,  -- TSID
    organization_id bigint                   NOT NULL,
    workspace_id    bigint                   NOT NULL,
    message_id      bigint                   NOT NULL,
    recipient_ref   text                     NOT NULL,  -- 'contact:<id>' | 'identity:<id>'
    device_id       text,
    status          text                     DEFAULT 'pending' NOT NULL,
    acked_at        timestamp with time zone,
    version         integer                  DEFAULT 1 NOT NULL,
    created_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_enterprise_message_delivery PRIMARY KEY (id),
    CONSTRAINT uq_emd_org_message_recipient_device
        UNIQUE (organization_id, message_id, recipient_ref, device_id),
    CONSTRAINT ck_emd_recipient_ref CHECK (recipient_ref ~ '^(contact|identity):[0-9]+$'),
    CONSTRAINT ck_emd_status CHECK (status = ANY (ARRAY['pending'::text, 'delivered'::text, 'failed'::text])),
    CONSTRAINT ck_emd_version CHECK (version >= 1),
    CONSTRAINT fk_emd_message FOREIGN KEY (organization_id, workspace_id, message_id)
        REFERENCES enterprise_message (organization_id, workspace_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE enterprise_message_delivery IS
    '客户端投递 ACK（归 Org/Workspace/Message）：只拥有投递状态，可独立压缩/清理，不得 DELETE/UPDATE canonical message';
COMMENT ON COLUMN enterprise_message_delivery.recipient_ref IS '接收方引用: contact:<id> 或 identity:<id>（不存 user 级 owner）';
COMMENT ON COLUMN enterprise_message_delivery.status IS '投递状态: pending 待投递 | delivered 已投递 | failed 投递失败';
COMMENT ON COLUMN enterprise_message_delivery.acked_at IS 'ACK 时间（幂等键见 uq_emd_org_message_recipient_device）';

CREATE INDEX IF NOT EXISTS i_emd_org_message ON enterprise_message_delivery
    USING btree (organization_id, workspace_id, message_id, status);

-- 迁移 00000140: 完整版 internal API 治理增量（FULL-02 / plan-full §3.1 + §5）。
--
-- 编号契约：139 由 FULL-01 占用；137/138 由活跃 foreign run 持有（本 run 刻意
--   避让，见 stage-full/FULL-00.md §2/§3）。本迁移是本 phase 唯一 DDL。
--
-- ⚠️ 命名纪律（本迁移实测教训）：PG 的函数/触发器名同一 schema 内**全局唯一**
--   （触发器名按表唯一，但函数名全局唯一）。仓内既有 enterprise_business 域
--   （00000117/00000118/00000121）已占用 `fn_enterprise_asset_retention_guard`、
--   `fn_enterprise_asset_purge_guard`、`fn_enterprise_retention_*`、
--   `trg_enterprise_asset_retention_guard` 等名字。首次起草本迁移时用
--   `fn_enterprise_asset_retention_guard` 命名，`CREATE OR REPLACE` **静默改写
--   了 eb 域 enterprise_asset 表的守卫函数体**（跨域破坏，真库实测被抓）。
--   故本迁移全部新对象统一加 **efapi**（enterprise full api）前缀，与本域既存
--   对象、eb 域对象、136/139 对象均不可能相撞。
--
-- 内容（全部为「在广州表上增量增加，不重复建表」的新增对象）：
--   1. enterprise_application 内容策略列（allowed_mime_types / max_file_size_bytes）
--      + 元素级守卫触发器 —— plan-full §3.1「企业附件 …内容策略」。
--   2. enterprise_attachment_retention —— **企业附件**（attachment 行 scope='enterprise'）
--      的留存/法务 hold/purge 状态机 + 声明式不变量（hold 生效中禁止 purge、
--      retention 窗口未满禁止 purge、purged 终态不可回退、retention 只可延长、
--      治理行禁止物理删除）—— plan-full §3.1「retention/hold/purge 不变量」。
--      不复用 eb 域 enterprise_retention_hold/policy 的理由（非「同义表」）：
--      eb 的两张表以 (organization_id, workspace_id) 为键、主体是
--      enterprise_conversation / enterprise_message（FK 与 scope_type CHECK
--      都钉死在 eb 子域），**无法表达附件级 hold**；扩它等于改另一个 feature 的
--      scope 语义与全部 eb 用例。本表主体是 attachment（Application 域附件），
--      键与生命周期完全不同，属并列而非同义。
--   3. enterprise_application_usage —— **只存聚合计量**（org/app/metric/周期/计数），
--      列集封闭，无正文、无 PII —— plan-full §5。
--   4. enterprise_group_origin —— 企业群的 Application 归属（Application
--      membership / 群生命周期 status active|archived 单向）+ 声明式边界
--      （跨 Org 复合 FK）—— plan-full §3.1「企业群生命周期、成员角色、
--      Application membership」。
--   5. enterprise_message_origin —— 企业托管消息的**真实 Application origin**
--      一等账本：application_id 永不 NULL，human sender 必带 sender_user_id，
--      non_e2ee 恒真（企业托管固定非 E2EE），行禁止物理删除 —— plan-full §3.1
--      「存储必须同时保留真实 Application origin（不得只留 Human 痕迹）」。
--
-- 迁移契约：up=可重复执行（IF NOT EXISTS / DROP TRIGGER IF EXISTS + CREATE /
--   CREATE OR REPLACE），down=完整对称回滚（只 DROP 本迁移新建对象/列）。
--   禁止 BEGIN/COMMIT —— erlang_migrate 外层单事务包裹。
--   无 backfill（新增列带默认值/可空，无需 expand->contract）。
--
-- 设计决策：
--   * 全部 FK 一律 RESTRICT（fail-closed；不 CASCADE 抹企业治理数据），
--     与 00000136 同款。子表对 application 的引用一律复合 FK
--     (organization_id, application_id) -> enterprise_application(organization_id, id)
--     ——跨 Org 引用 23503 拒绝（IDOR 的 DB 层兜底）。
--   * 「只增不减」纪律：附件留存/purge 账本、群归属、消息 origin 三类治理行
--     一律 BEFORE DELETE 触发器 23514 禁止物理删除——与 00000139 Grant 同款，
--     保证治理证据不可被抹除、归档不可被回退成更宽的状态。
--   * 不变量尽量用**声明式** CHECK/触发器（而非仅应用层判定）：负例可在
--     DB 层独立复现（真库套件绕过应用层直写即可证明）。
--   * 新表均登记 docs/compliance/data-disposition.yml（exclusions：组织治理
--     资产/聚合计量/无正文 PII），否则 data_disposition_tests 覆盖面用例红。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- 1) enterprise_application：内容策略列 + 元素级守卫
-- ============================================================

ALTER TABLE enterprise_application
    ADD COLUMN IF NOT EXISTS allowed_mime_types text[] DEFAULT '{}' NOT NULL;

ALTER TABLE enterprise_application
    ADD COLUMN IF NOT EXISTS max_file_size_bytes bigint;

DO $$
BEGIN
    ALTER TABLE enterprise_application
        ADD CONSTRAINT ck_ea_max_file_size
        CHECK (max_file_size_bytes IS NULL OR max_file_size_bytes > 0);
EXCEPTION
    WHEN duplicate_object THEN NULL;
END $$;

COMMENT ON COLUMN enterprise_application.allowed_mime_types IS
    '企业附件 MIME 允许清单（元素级校验由触发器 trg_efapi_application_mime_guard 守卫）；空数组 = 沿用全局 elib_oss 白名单（不放大：confirm 侧仍以 HEAD 真实值复核）';
COMMENT ON COLUMN enterprise_application.max_file_size_bytes IS
    '企业附件单文件上限（字节）；NULL = 沿用全局 elib_oss:max_file_size()。只可收紧全局上限，不可放大（presign/confirm 取 min(全局, 本值)）';

-- MIME 元素级守卫（镜像 00000136 的 trg_enterprise_application_redirect_guard 写法）：
--   元素非空 · 形如 type/subtype（小写字母数字 + 有限符号）· 禁 '*'（通配不是
--   合法元素，fail-closed）· 单元素 <=128 · <=32 个 · 无重复。
-- 非法元素一律 23514，绝不静默丢弃（静默丢弃会把「配了但没生效」变成隐身放行）。
CREATE OR REPLACE FUNCTION fn_efapi_application_mime_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_mime text;
    v_seen text[] := ARRAY[]::text[];
BEGIN
    IF NEW.allowed_mime_types IS NULL THEN
        RAISE EXCEPTION 'allowed_mime_types 不可为 NULL；空清单请用空数组 {}'
            USING ERRCODE = '23514';
    END IF;
    IF array_length(NEW.allowed_mime_types, 1) IS NULL THEN
        RETURN NEW;
    END IF;
    IF array_length(NEW.allowed_mime_types, 1) > 32 THEN
        RAISE EXCEPTION 'allowed_mime_types 最多 32 个元素（当前 %）',
            array_length(NEW.allowed_mime_types, 1)
            USING ERRCODE = '23514';
    END IF;
    FOREACH v_mime IN ARRAY NEW.allowed_mime_types LOOP
        IF v_mime IS NULL OR v_mime = '' OR length(v_mime) > 128 THEN
            RAISE EXCEPTION 'allowed_mime_types 元素非空且长度 <=128' USING ERRCODE = '23514';
        END IF;
        IF position('*' in v_mime) > 0 THEN
            RAISE EXCEPTION 'allowed_mime_types 不接受通配元素（%）', v_mime
                USING ERRCODE = '23514';
        END IF;
        IF v_mime !~ '^[a-z0-9][a-z0-9!#$&^_.+-]*/[a-z0-9][a-z0-9!#$&^_.+-]*$' THEN
            RAISE EXCEPTION 'allowed_mime_types 元素须为 type/subtype 形态（%）', v_mime
                USING ERRCODE = '23514';
        END IF;
        IF v_mime = ANY (v_seen) THEN
            RAISE EXCEPTION 'allowed_mime_types 元素重复（%）', v_mime USING ERRCODE = '23514';
        END IF;
        v_seen := v_seen || v_mime;
    END LOOP;
    RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_efapi_application_mime_guard ON enterprise_application;
CREATE TRIGGER trg_efapi_application_mime_guard
    BEFORE INSERT OR UPDATE OF allowed_mime_types ON enterprise_application
    FOR EACH ROW EXECUTE FUNCTION fn_efapi_application_mime_guard();

-- ============================================================
-- 2) enterprise_attachment_retention：附件留存 / 法务 hold / purge 状态机
-- ============================================================

CREATE TABLE IF NOT EXISTS enterprise_attachment_retention (
    attachment_id      bigint                   NOT NULL,
    organization_id    bigint                   NOT NULL,
    application_id     bigint                   NOT NULL,
    retention_until    timestamp with time zone NOT NULL,
    hold_state         text                     DEFAULT 'none' NOT NULL,
    hold_reason        text,
    hold_set_at        timestamp with time zone,
    purge_state        text                     DEFAULT 'live' NOT NULL,
    purged_at          timestamp with time zone,
    created_at         timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at         timestamp with time zone,
    CONSTRAINT pk_ear PRIMARY KEY (attachment_id),
    CONSTRAINT ck_ear_hold_state CHECK (hold_state = ANY (ARRAY['none'::text, 'held'::text])),
    CONSTRAINT ck_ear_hold_consistency CHECK ((hold_state = 'held') = (hold_set_at IS NOT NULL)),
    CONSTRAINT ck_ear_hold_reason CHECK (
        (hold_state = 'held' AND hold_reason IS NOT NULL AND hold_reason <> '')
        OR (hold_state = 'none' AND hold_reason IS NULL)
    ),
    CONSTRAINT ck_ear_purge_state CHECK (purge_state = ANY (ARRAY['live'::text, 'purged'::text])),
    CONSTRAINT ck_ear_purge_consistency CHECK ((purge_state = 'purged') = (purged_at IS NOT NULL)),
    CONSTRAINT fk_ear_attachment FOREIGN KEY (attachment_id)
        REFERENCES attachment(id) ON DELETE RESTRICT,
    CONSTRAINT fk_ear_application FOREIGN KEY (organization_id, application_id)
        REFERENCES enterprise_application(organization_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE enterprise_attachment_retention IS
    '企业附件（attachment scope=enterprise）留存/法务 hold/purge 账本（每附件一行；行禁止物理删除，purge 走 purge_state=purged 保留证据）';
COMMENT ON COLUMN enterprise_attachment_retention.retention_until IS '留存到期时间；只可延长（缩短被 23514 拒绝），未到期禁止 purge';
COMMENT ON COLUMN enterprise_attachment_retention.hold_state IS '法务 hold: none 无 | held 扣留中（held 期间禁止 purge，DB 层强制）';
COMMENT ON COLUMN enterprise_attachment_retention.purge_state IS 'purge 状态: live | purged（终态，不可回退）';

CREATE INDEX IF NOT EXISTS i_ear_org_app ON enterprise_attachment_retention
    USING btree (organization_id, application_id);
CREATE INDEX IF NOT EXISTS i_ear_purgeable ON enterprise_attachment_retention
    USING btree (retention_until) WHERE (purge_state = 'live' AND hold_state = 'none');

-- 状态机守卫（BEFORE UPDATE / BEFORE DELETE）：
--   ① purge 仅在 hold_state='none' 且 CURRENT_TIMESTAMP >= retention_until 时允许；
--   ② purged 是终态（不可 purged -> live）；
--   ③ retention_until 只可延长（缩短会提前释放数据，属降级）；
--   ④ 归属键 (attachment_id, organization_id, application_id) 不可变更；
--   ⑤ 治理行禁止物理删除（append-only 证据）。
CREATE OR REPLACE FUNCTION fn_efapi_attachment_retention_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION 'enterprise_attachment_retention 行禁止物理删除（purge 走 purge_state=purged）'
            USING ERRCODE = '23514';
    END IF;

    IF OLD.purge_state = 'purged' AND NEW.purge_state <> 'purged' THEN
        RAISE EXCEPTION 'purged 为终态，不可回退（% -> %）', OLD.purge_state, NEW.purge_state
            USING ERRCODE = '23514';
    END IF;

    IF OLD.purge_state <> 'purged' AND NEW.purge_state = 'purged' THEN
        IF OLD.hold_state = 'held' THEN
            RAISE EXCEPTION '法务 hold 生效中禁止 purge（attachment_id=%）', OLD.attachment_id
                USING ERRCODE = '23514';
        END IF;
        IF CURRENT_TIMESTAMP < OLD.retention_until THEN
            RAISE EXCEPTION 'retention 窗口未满禁止 purge（attachment_id=% until=%）',
                OLD.attachment_id, OLD.retention_until
                USING ERRCODE = '23514';
        END IF;
    END IF;

    IF NEW.retention_until < OLD.retention_until THEN
        RAISE EXCEPTION 'retention_until 只可延长（% -> %）', OLD.retention_until, NEW.retention_until
            USING ERRCODE = '23514';
    END IF;

    IF NEW.attachment_id <> OLD.attachment_id
        OR NEW.organization_id <> OLD.organization_id
        OR NEW.application_id <> OLD.application_id THEN
        RAISE EXCEPTION '治理行的 (attachment_id, organization_id, application_id) 不可变更'
            USING ERRCODE = '23514';
    END IF;

    RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_efapi_attachment_retention_guard ON enterprise_attachment_retention;
CREATE TRIGGER trg_efapi_attachment_retention_guard
    BEFORE UPDATE OR DELETE ON enterprise_attachment_retention
    FOR EACH ROW EXECUTE FUNCTION fn_efapi_attachment_retention_guard();

-- 读取面：可 purge 集合（hold 与 retention 在**每次查询**求值，不物化）。
CREATE OR REPLACE VIEW v_efapi_attachment_purgeable AS
    SELECT ar.attachment_id, ar.organization_id, ar.application_id, ar.retention_until
    FROM enterprise_attachment_retention ar
    WHERE ar.purge_state = 'live'
      AND ar.hold_state = 'none'
      AND CURRENT_TIMESTAMP >= ar.retention_until;

-- ============================================================
-- 3) enterprise_application_usage：只存聚合计量
-- ============================================================

CREATE TABLE IF NOT EXISTS enterprise_application_usage (
    organization_id  bigint                   NOT NULL,
    application_id   bigint                   NOT NULL,
    metric           text                     NOT NULL,
    period_start     date                     NOT NULL,
    counter          bigint                   DEFAULT 0 NOT NULL,
    updated_at       timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_eau PRIMARY KEY (organization_id, application_id, metric, period_start),
    CONSTRAINT ck_eau_metric CHECK (metric = ANY (ARRAY[
        'identity.bound'::text,
        'identity.revoked'::text,
        'directory.page'::text,
        'file.confirmed'::text,
        'message.accepted'::text,
        'message.failed'::text
    ])),
    CONSTRAINT ck_eau_counter CHECK (counter >= 0),
    CONSTRAINT fk_eau_application FOREIGN KEY (organization_id, application_id)
        REFERENCES enterprise_application(organization_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE enterprise_application_usage IS
    '企业 Application 用量**聚合计量**（org/app/固定 metric 名/周期/计数）；刻意不含正文、会话内容、external_user_id 等任何 PII 列——列集封闭，仅计数与固定枚举键';
COMMENT ON COLUMN enterprise_application_usage.metric IS '固定计量名枚举（无自由文本，防止把正文塞进 metric）';
COMMENT ON COLUMN enterprise_application_usage.period_start IS '计量周期起点（按月 date_trunc）';

-- 聚合计量禁止物理删除（只增不减的计量证据；计数值本身允许 +1 更新）。
CREATE OR REPLACE FUNCTION fn_efapi_usage_no_delete() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    RAISE EXCEPTION 'enterprise_application_usage 行禁止物理删除（聚合计量只增不减）'
        USING ERRCODE = '23514';
END;
$$;

DROP TRIGGER IF EXISTS trg_efapi_usage_no_delete ON enterprise_application_usage;
CREATE TRIGGER trg_efapi_usage_no_delete
    BEFORE DELETE ON enterprise_application_usage
    FOR EACH ROW EXECUTE FUNCTION fn_efapi_usage_no_delete();

-- ============================================================
-- 4) enterprise_group_origin：企业群的 Application 归属
-- ============================================================

CREATE TABLE IF NOT EXISTS enterprise_group_origin (
    group_id         bigint                   NOT NULL,
    organization_id  bigint                   NOT NULL,
    application_id   bigint                   NOT NULL,
    workspace_id     bigint                   NOT NULL,
    status           text                     DEFAULT 'active' NOT NULL,
    archived_at      timestamp with time zone,
    created_at       timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at       timestamp with time zone,
    CONSTRAINT pk_ego PRIMARY KEY (group_id),
    CONSTRAINT ck_ego_status CHECK (status = ANY (ARRAY['active'::text, 'archived'::text])),
    CONSTRAINT ck_ego_archived_consistency CHECK ((status = 'archived') = (archived_at IS NOT NULL)),
    CONSTRAINT fk_ego_group FOREIGN KEY (group_id)
        REFERENCES "group"(id) ON DELETE RESTRICT,
    CONSTRAINT fk_ego_workspace FOREIGN KEY (workspace_id)
        REFERENCES workspace(id) ON DELETE RESTRICT,
    CONSTRAINT fk_ego_application FOREIGN KEY (organization_id, application_id)
        REFERENCES enterprise_application(organization_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE enterprise_group_origin IS
    '企业群的 Application 归属（Application membership）：哪一份 Application 在哪个 Org/Workspace 建立该企业群；行禁止物理删除，归档走 status=archived（单向）';
COMMENT ON COLUMN enterprise_group_origin.status IS '生命周期: active | archived（archived 单向不可回退）';
COMMENT ON COLUMN enterprise_group_origin.archived_at IS '归档时间（与 status=archived 一致性由 CHECK 钉死）';

-- 归属守卫：归档单向（archived -> active 拒）；归属键不可改；行禁止删除。
CREATE OR REPLACE FUNCTION fn_efapi_group_origin_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION 'enterprise_group_origin 行禁止物理删除（归档走 status=archived）'
            USING ERRCODE = '23514';
    END IF;
    IF OLD.status = 'archived' AND NEW.status <> 'archived' THEN
        RAISE EXCEPTION '企业群归档为单向操作（archived 不可回退）' USING ERRCODE = '23514';
    END IF;
    IF NEW.group_id <> OLD.group_id
        OR NEW.organization_id <> OLD.organization_id
        OR NEW.application_id <> OLD.application_id
        OR NEW.workspace_id <> OLD.workspace_id THEN
        RAISE EXCEPTION '企业群归属键 (group_id, organization_id, application_id, workspace_id) 不可变更'
            USING ERRCODE = '23514';
    END IF;
    RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_efapi_group_origin_guard ON enterprise_group_origin;
CREATE TRIGGER trg_efapi_group_origin_guard
    BEFORE UPDATE OR DELETE ON enterprise_group_origin
    FOR EACH ROW EXECUTE FUNCTION fn_efapi_group_origin_guard();

CREATE INDEX IF NOT EXISTS i_ego_org_app_status ON enterprise_group_origin
    USING btree (organization_id, application_id, status);

-- ============================================================
-- 5) enterprise_message_origin：企业托管消息的真实 Application origin
-- ============================================================

CREATE TABLE IF NOT EXISTS enterprise_message_origin (
    id                 bigint                   NOT NULL,
    conversation_kind  text                     NOT NULL,
    organization_id    bigint                   NOT NULL,
    application_id     bigint                   NOT NULL,
    sender_kind        text                     NOT NULL,
    sender_user_id     bigint,
    non_e2ee           boolean                  DEFAULT true NOT NULL,
    msg_row_id         bigint                   NOT NULL,
    msg_id             text                     NOT NULL,
    created_at         timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_emo PRIMARY KEY (id),
    CONSTRAINT ck_emo_conversation CHECK (conversation_kind = ANY (ARRAY['direct'::text, 'group'::text])),
    CONSTRAINT ck_emo_sender_kind CHECK (sender_kind = ANY (ARRAY['application'::text, 'human'::text])),
    CONSTRAINT ck_emo_human_sender CHECK ((sender_kind = 'human') = (sender_user_id IS NOT NULL)),
    CONSTRAINT ck_emo_sender_positive CHECK (sender_user_id IS NULL OR sender_user_id > 0),
    CONSTRAINT ck_emo_non_e2ee CHECK (non_e2ee),
    CONSTRAINT ck_emo_msg_id CHECK (msg_id <> ''),
    CONSTRAINT ck_emo_msg_row CHECK (msg_row_id > 0),
    CONSTRAINT fk_emo_application FOREIGN KEY (organization_id, application_id)
        REFERENCES enterprise_application(organization_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE enterprise_message_origin IS
    '企业托管消息 origin 账本（plan-full §3.1）：application_id 永不 NULL（真实 Application origin），sender_kind=human 时 sender_user_id 必填（Human 痕迹与 Application 痕迹同时存在，二者缺一即 23514）；non_e2ee 恒真；行禁止物理删除';
COMMENT ON COLUMN enterprise_message_origin.application_id IS '真实调用方 Application（审计 actor）；不允许只留 Human 痕迹';
COMMENT ON COLUMN enterprise_message_origin.non_e2ee IS '企业托管消息固定非 E2EE（CHECK 恒真，不可写入 false）';

CREATE OR REPLACE FUNCTION fn_efapi_message_origin_no_delete() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    RAISE EXCEPTION 'enterprise_message_origin 行禁止物理删除（origin 证据只增不减）'
        USING ERRCODE = '23514';
END;
$$;

DROP TRIGGER IF EXISTS trg_efapi_message_origin_no_delete ON enterprise_message_origin;
CREATE TRIGGER trg_efapi_message_origin_no_delete
    BEFORE DELETE ON enterprise_message_origin
    FOR EACH ROW EXECUTE FUNCTION fn_efapi_message_origin_no_delete();

CREATE INDEX IF NOT EXISTS i_emo_org_app ON enterprise_message_origin
    USING btree (organization_id, application_id, created_at);
CREATE INDEX IF NOT EXISTS i_emo_msg ON enterprise_message_origin
    USING btree (conversation_kind, msg_row_id);

-- =====================================================================
-- 00000180_customer_service_ai_agent.up.sql — 智能客服 AI Agent（受控自动应答，v4）
--
-- 设计真源：docs/architecture/2026-10-14-customer-service-ai-agent-v4.md
-- 前身：v1（PROPOSED）/ v2（CONDITIONAL_GO）/ v3 ——均已被 v4 取代。
--
-- 关键纪律（改本文件前必须先读）：
--   * 安全默认：auto_reply_mode 默认 'disabled' ⇒ 本迁移本身不产生任何 AI 流量。
--     开启是显式运营动作，不是迁移副作用。
--   * 不建消息副本：消息真源仍是 enterprise_message。本文件只存「AI 为什么这么
--     决定」与「哪条 AI 消息真的发出去了」这两类事实。
--   * 事务边界：LLM / embedding 的慢 I/O 绝不在 DB 事务内（本项目已有实证缺陷：
--     moya 回课把 provider HTTP 放事务内 → 20~30s 长事务，未修）。
--     本文件不含任何需要长事务的设计：作业由「持久事件 + 游标」在事务外派生。
--   * 数值阈值不得凭空设：检索分（ts_rank / RRF）不是概率，不做阈值裁决；
--     model_reported_confidence 是**辅助信号**，在黄金集标定前不允许单独放行。
--   * 预算限额（H.3）：limit_tokens / limit_cost_micro 无默认值、可空。
--     limit IS NULL = 未配置 = 准入 fail-closed 拒绝（不是无限额）；
--     -1「不限」语义废除（G4：全仓零生产计量，任何数值阈值都不得凭空设定）。
--   * 迁移编号纪律（v4 R1/R3）：编号 180 = main 最大号 179 + 1。前身 173 号
--     是本 run 的 git 未跟踪文件（173 已被 application_grant_resource_target
--     占用），按「新建迁移」重编号为 180，不是原地改写已存在迁移。
--
-- 迁移内不 SET lock_timeout / statement_timeout（v4 R4：全仓 178 个 up 无此
-- 先例，且 runner 的 BEGIN…COMMIT 内 SET 属会话级泄漏，提交后仍残留于连接）。
--
-- No BEGIN/COMMIT: migration runner owns the transaction.
-- =====================================================================

-- ---------------------------------------------------------------------
-- 1. AI 坐席配置（org 级）
--    与 v1 的差异：auto_reply_enabled(boolean) → auto_reply_mode(四态)；
--    faq_min_score(数值阈值) → faq_require_hit(布尔证据门)；
--    新增 config_version（全局/组织级开关的栅栏版本，见 v3 §4）。
-- ---------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS customer_service_ai_agent (
    organization_id      bigint                   NOT NULL,
    id                   bigint                   NOT NULL CHECK (id > 0),
    name                 text                     NOT NULL DEFAULT '',
    provider             text                     NOT NULL DEFAULT 'bigmodel',
    model                text                     NOT NULL DEFAULT '',
    system_prompt        text                     NOT NULL DEFAULT '',
    greeting             text                     NOT NULL DEFAULT '',
    -- 四态：disabled 完全不跑 / shadow 完整跑但不投递 / canary 仅白名单 / enabled 全量
    auto_reply_mode      text                     NOT NULL DEFAULT 'disabled'
        CONSTRAINT ck_csaia_mode CHECK (auto_reply_mode IN
            ('disabled', 'shadow', 'canary', 'enabled')),
    -- 配置栅栏版本：mode / 策略 / prompt 变更时自增。投递前必须校验，防止
    -- 关闭 AI（或改策略）后，在途的旧生成结果仍然提交。
    config_version       integer                  NOT NULL DEFAULT 1
        CONSTRAINT ck_csaia_config_version CHECK (config_version > 0),
    -- AI 发话所用的坐席身份。⚠ 见 v3 §3：既有 sender_shape 要求
    -- identity_id 逐字等于会话绑定的 business_identity_id，故 AI 必须有
    -- 自己的 seat 行（建议 presence 恒 offline，永不被派单）。
    seat_identity_id     bigint,
    max_unresolved_turns integer                  NOT NULL DEFAULT 2
        CONSTRAINT ck_csaia_unresolved CHECK (max_unresolved_turns >= 0),
    -- 证据门：无 FAQ 命中即不自动作答（替代 v1 的数值阈值）
    faq_require_hit      boolean                  NOT NULL DEFAULT true,
    -- 并发与寿命上限（防止单租户耗尽 worker；见 v3 §8）
    max_inflight         integer                  NOT NULL DEFAULT 4
        CONSTRAINT ck_csaia_inflight CHECK (max_inflight > 0),
    job_timeout_ms       integer                  NOT NULL DEFAULT 60000
        CONSTRAINT ck_csaia_job_timeout CHECK (job_timeout_ms > 0),
    max_job_age_ms       integer                  NOT NULL DEFAULT 300000
        CONSTRAINT ck_csaia_max_age CHECK (max_job_age_ms > 0),
    version              integer                  NOT NULL DEFAULT 1
        CONSTRAINT ck_csaia_version CHECK (version > 0),
    created_at           timestamptz              NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at           timestamptz              NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_customer_service_ai_agent PRIMARY KEY (organization_id, id),
    CONSTRAINT fk_csaia_organization FOREIGN KEY (organization_id)
        REFERENCES organization (id) ON DELETE RESTRICT,
    CONSTRAINT fk_csaia_seat FOREIGN KEY (organization_id, seat_identity_id)
        REFERENCES organization_business_identity (organization_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE customer_service_ai_agent IS
    '客服 AI 坐席配置（org 级）。auto_reply_mode 默认 disabled ⇒ 迁移不产生 AI 流量';
COMMENT ON COLUMN customer_service_ai_agent.config_version IS
    '配置栅栏版本：mode/策略/prompt 变更自增。投递事务必须校验，拦截在途旧结果';

-- ---------------------------------------------------------------------
-- 2. canary 白名单
--    「canary」若无白名单载体，就只是一句承诺（v3 §7.2 要求机制而非口号）。
-- ---------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS customer_service_ai_canary (
    organization_id bigint                   NOT NULL,
    scope_kind      text                     NOT NULL
        CONSTRAINT ck_csaic_scope CHECK (scope_kind IN ('workspace', 'session')),
    scope_id        bigint                   NOT NULL CHECK (scope_id > 0),
    note            text                     NOT NULL DEFAULT '',
    created_at      timestamptz              NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_customer_service_ai_canary PRIMARY KEY (organization_id, scope_kind, scope_id),
    CONSTRAINT fk_csaic_organization FOREIGN KEY (organization_id)
        REFERENCES organization (id) ON DELETE RESTRICT
);

COMMENT ON TABLE customer_service_ai_canary IS
    'canary 允许投递的范围白名单：workspace 或 session 粒度。空集 = canary 不投任何会话';

-- ---------------------------------------------------------------------
-- 3. FAQ 知识库（pg_jieba 词法检索）
--    与 v1 的差异：
--      * 不再硬编码 embedding vector(1024)：维度未 benchmark 前不得冻结 schema。
--        向量列 + HNSW 索引由**后续独立迁移**在选型确定后添加。
--      * enabled(boolean) → status(四态)，承载「谁负责 / 何时生效 / 过期后停用」。
--      * 新增适用范围 scope_workspace_ids（同 org 不同商城规则不同）。
-- ---------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS customer_service_faq (
    organization_id     bigint                   NOT NULL,
    id                  bigint                   NOT NULL CHECK (id > 0),
    question            text                     NOT NULL
        CONSTRAINT ck_csfaq_question CHECK (question <> ''),
    answer              text                     NOT NULL DEFAULT '',
    intent              text                     NOT NULL DEFAULT 'other',
    category            text                     NOT NULL DEFAULT '',
    keywords            text[]                   NOT NULL DEFAULT '{}',
    -- 运营生命周期：draft 草稿 / published 生效 / paused 暂停 / retired 退役
    status              text                     NOT NULL DEFAULT 'draft'
        CONSTRAINT ck_csfaq_status CHECK (status IN
            ('draft', 'published', 'paused', 'retired')),
    -- 知识资产责任人（不是 CRUD 操作者）
    owner_user_id       bigint,
    -- 适用范围：空数组 = 本 org 全部 workspace
    scope_workspace_ids bigint[]                 NOT NULL DEFAULT '{}',
    -- 有效期：NULL = 长期有效。过期后检索必须排除（不是「找到但业务已失效」）
    valid_from          timestamptz,
    valid_until         timestamptz,
    -- 向量元数据（列本身由后续迁移添加；此处先记元数据，用于识别「陈旧 embedding」）
    embedding_model     text,
    embedding_version   text,
    embedding_dim       integer
        CONSTRAINT ck_csfaq_dim CHECK (embedding_dim IS NULL OR embedding_dim > 0),
    content_hash        text,
    embedded_at         timestamptz,
    search_tsv          tsvector,
    hit_count           bigint                   NOT NULL DEFAULT 0
        CONSTRAINT ck_csfaq_hits CHECK (hit_count >= 0),
    version             integer                  NOT NULL DEFAULT 1
        CONSTRAINT ck_csfaq_version CHECK (version > 0),
    created_at          timestamptz              NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at          timestamptz              NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_customer_service_faq PRIMARY KEY (organization_id, id),
    CONSTRAINT ck_csfaq_validity CHECK (valid_until IS NULL OR valid_from IS NULL
        OR valid_until > valid_from),
    CONSTRAINT fk_csfaq_organization FOREIGN KEY (organization_id)
        REFERENCES organization (id) ON DELETE RESTRICT
);

COMMENT ON TABLE customer_service_faq IS
    'FAQ 知识库（运营资产）。status/owner/scope/valid_until 决定「何时可被引用」；'
    '检索必须同时满足 status=published 且未过期且适用当前 workspace';
COMMENT ON COLUMN customer_service_faq.content_hash IS
    'question/keywords/answer 的规范化哈希：与 embedding 元数据比对以识别陈旧向量'
    '（改文本未重算 embedding ⇒ 静默降召回、不报错）';

-- tsv 由触发器维护（PG 侧分词，避免应用侧漏算）
CREATE OR REPLACE FUNCTION fn_csfaq_build_tsv() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    NEW.search_tsv := to_tsvector('jiebacfg',
        coalesce(NEW.question, '') || ' ' ||
        coalesce(array_to_string(NEW.keywords, ' '), '') || ' ' ||
        coalesce(NEW.answer, ''));
    RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_csfaq_build_tsv ON customer_service_faq;
CREATE TRIGGER trg_csfaq_build_tsv
    BEFORE INSERT OR UPDATE OF question, keywords, answer ON customer_service_faq
    FOR EACH ROW EXECUTE FUNCTION fn_csfaq_build_tsv();

CREATE INDEX IF NOT EXISTS idx_csfaq_search_tsv ON customer_service_faq
    USING gin (search_tsv);
CREATE INDEX IF NOT EXISTS idx_csfaq_org_status ON customer_service_faq
    USING btree (organization_id, status);

-- ---------------------------------------------------------------------
-- 4. 会话 AI 状态
--    与 v1 的差异：新增 generation_epoch（投递栅栏）、context_watermark
--    （「这条回复在回答哪一段输入」）、以及四态结果标签（outcome）。
--    ⚠ outcome 只记录**判定依据明确**的结果，不得用模型自述替代。
-- ---------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS customer_service_ai_session (
    organization_id       bigint                   NOT NULL,
    workspace_id          bigint                   NOT NULL,
    session_id            bigint                   NOT NULL,
    contact_id            bigint,
    conversation_id       bigint,
    current_intent        text                     NOT NULL DEFAULT '',
    intent_confidence     numeric(4, 3)
        CONSTRAINT ck_csais_conf CHECK (intent_confidence IS NULL
            OR (intent_confidence >= 0 AND intent_confidence <= 1)),
    ai_turns              integer                  NOT NULL DEFAULT 0
        CONSTRAINT ck_csais_turns CHECK (ai_turns >= 0),
    unresolved_turns      integer                  NOT NULL DEFAULT 0
        CONSTRAINT ck_csais_unres CHECK (unresolved_turns >= 0),
    -- 人工优先的最终栅栏：人工接管 / 交接 / 会话关闭 / 总开关或策略变更 → 自增
    generation_epoch      integer                  NOT NULL DEFAULT 1
        CONSTRAINT ck_csais_epoch CHECK (generation_epoch > 0),
    -- 已消费的输入水位（事件 id 或 inbound message id），用于会话级顺序与去重
    context_watermark     bigint                   NOT NULL DEFAULT 0
        CONSTRAINT ck_csais_watermark CHECK (context_watermark >= 0),
    -- 交接事实（粘性）
    handoff               boolean                  NOT NULL DEFAULT false,
    handoff_reason        text                     NOT NULL DEFAULT '',
    handoff_at            timestamptz,
    handoff_seat_identity_id bigint,
    -- 结果标签：模型「认为可回答」与「客户问题真的解决了」是两件事
    outcome               text                     NOT NULL DEFAULT ''
        CONSTRAINT ck_csais_outcome CHECK (outcome IN
            ('', 'confirmed_resolved', 'assumed_resolved', 'unresolved_reopened', 'abandoned')),
    outcome_source        text                     NOT NULL DEFAULT ''
        CONSTRAINT ck_csais_outcome_src CHECK (outcome_source IN
            ('', 'customer_feedback', 'followup', 'human_takeover', 'human_review', 'rating')),
    outcome_at            timestamptz,
    version               integer                  NOT NULL DEFAULT 1
        CONSTRAINT ck_csais_version CHECK (version > 0),
    created_at            timestamptz              NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at            timestamptz              NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_customer_service_ai_session PRIMARY KEY (organization_id, workspace_id, session_id),
    CONSTRAINT ck_csais_outcome_pair CHECK ((outcome = '') = (outcome_source = '')),
    CONSTRAINT fk_csais_organization FOREIGN KEY (organization_id)
        REFERENCES organization (id) ON DELETE RESTRICT,
    CONSTRAINT fk_csais_session FOREIGN KEY (organization_id, session_id)
        REFERENCES customer_service_session (organization_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE customer_service_ai_session IS
    '会话级 AI 状态。generation_epoch 是「人工永远优先」的最终栅栏载体';
COMMENT ON COLUMN customer_service_ai_session.outcome IS
    '会话结果标签：confirmed_resolved 客户确认 / assumed_resolved 推定（后续无追问）'
    '/ unresolved_reopened 未解决或重启 / abandoned 放弃。必须带 outcome_source';

-- ---------------------------------------------------------------------
-- 5. AI 逐轮决策审计（append-only，但保留受控清理通道）
--    与 v1 的差异：
--      * resolved → answerable（生成时模型只能判断「这题我答得了」，不能
--        判断「客户问题解决了」）
--      * confidence → model_reported_confidence（辅助信号，非唯一裁决依据）
--      * 新增 cited_faq_ids / cited_faq_versions（引用可追溯）
--      * 新增 cancel_reason / job_id / context_watermark
--      * detail 加 DB 级密钥泄漏守卫
--      * append-only 加受控 purge 逃生口 —— 解决「append-only 与保留策略冲突」
--        （既有唯一先例：enterprise_audit_event.actor_user_id 的 ON DELETE SET NULL）
-- ---------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS customer_service_ai_turn (
    organization_id            bigint                   NOT NULL,
    workspace_id               bigint,
    session_id                 bigint                   NOT NULL,
    job_id                     bigint,
    seq                        integer                  NOT NULL CHECK (seq > 0),
    direction                  text                     NOT NULL
        CONSTRAINT ck_csait_dir CHECK (direction IN ('inbound', 'outbound')),
    intent                     text,
    -- 模型自述置信度。⚠ 命名即契约：这是「模型说自己多有把握」，
    -- 不是「该类回答真的有这么高的正确率」。标定前不得单独据此放行。
    model_reported_confidence  numeric(4, 3)
        CONSTRAINT ck_csait_conf CHECK (model_reported_confidence IS NULL
            OR (model_reported_confidence >= 0 AND model_reported_confidence <= 1)),
    -- 生成阶段可用字段：模型认为「有资料可答」
    answerable                 boolean,
    -- 引用可追溯：answer 引用了哪些 FAQ（及当时的版本）
    cited_faq_ids              bigint[]                 NOT NULL DEFAULT '{}',
    cited_faq_versions         integer[]                NOT NULL DEFAULT '{}',
    context_watermark          bigint                   NOT NULL DEFAULT 0
        CONSTRAINT ck_csait_watermark CHECK (context_watermark >= 0),
    action                     text                     NOT NULL DEFAULT ''
        CONSTRAINT ck_csait_action CHECK (action IN
            ('', 'auto_reply', 'handoff', 'no_match', 'disabled', 'skipped', 'cancelled')),
    cancel_reason              text                     NOT NULL DEFAULT '',
    faq_ids                    bigint[]                 NOT NULL DEFAULT '{}',
    reply_text                 text,
    model                      text                     NOT NULL DEFAULT '',
    provider                   text                     NOT NULL DEFAULT '',
    latency_ms                 integer
        CONSTRAINT ck_csait_latency CHECK (latency_ms IS NULL OR latency_ms >= 0),
    prompt_tokens              integer
        CONSTRAINT ck_csait_pt CHECK (prompt_tokens IS NULL OR prompt_tokens >= 0),
    completion_tokens          integer
        CONSTRAINT ck_csait_ct CHECK (completion_tokens IS NULL OR completion_tokens >= 0),
    -- 账本字段：不得出现密钥/凭据（DB 级守卫，不只靠测试）
    detail                     jsonb                    NOT NULL DEFAULT '{}'::jsonb
        CONSTRAINT ck_csait_no_secret CHECK (NOT (detail ?| ARRAY[
            'api_key', 'apikey', 'secret', 'secret_key', 'token', 'access_token',
            'refresh_token', 'password', 'authorization', 'cookie', 'private_key',
            'client_secret', 'session_key'])),
    created_at                 timestamptz              NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_customer_service_ai_turn PRIMARY KEY (organization_id, session_id, seq),
    CONSTRAINT fk_csait_organization FOREIGN KEY (organization_id)
        REFERENCES organization (id) ON DELETE RESTRICT,
    CONSTRAINT fk_csait_session FOREIGN KEY (organization_id, session_id)
        REFERENCES customer_service_session (organization_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE customer_service_ai_turn IS
    'AI 逐轮决策审计（append-only）。删除需显式 SET LOCAL imboy.ai_trace_purge=on，'
    '用于与保留/删除策略衔接（不是默认开放通道）';
COMMENT ON COLUMN customer_service_ai_turn.answerable IS
    '生成阶段模型自述「有资料可答」。≠ 客户问题已解决（后者见 ai_session.outcome）';
COMMENT ON COLUMN customer_service_ai_turn.model_reported_confidence IS
    '模型自报置信度（辅助信号）。未经黄金集标定前，不得作为唯一放行依据';

-- append-only 守卫：UPDATE 一律拒；DELETE 仅允许带 purge 令牌的受控清理。
-- （与 eb_pg_purge 的 SET LOCAL imboy.enterprise_purge='on' 同构；这是约定级
--  守卫，不是数据库权限边界 —— 拥有连接写权限者仍可设置该 GUC。）
CREATE OR REPLACE FUNCTION fn_customer_service_ai_turn_append_only() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_OP = 'DELETE' THEN
        IF current_setting('imboy.ai_trace_purge', true) = 'on' THEN
            RETURN OLD;
        END IF;
        RAISE EXCEPTION
            'customer_service_ai_turn is append-only; DELETE requires imboy.ai_trace_purge=on'
            USING ERRCODE = '23514';
    END IF;
    RAISE EXCEPTION 'customer_service_ai_turn is append-only; UPDATE is forbidden'
        USING ERRCODE = '23514';
END $$;

DROP TRIGGER IF EXISTS trg_csait_append_only ON customer_service_ai_turn;
CREATE TRIGGER trg_csait_append_only
    BEFORE UPDATE OR DELETE ON customer_service_ai_turn
    FOR EACH ROW EXECUTE FUNCTION fn_customer_service_ai_turn_append_only();

CREATE INDEX IF NOT EXISTS i_csait_org_session ON customer_service_ai_turn
    USING btree (organization_id, session_id, seq DESC);

-- ---------------------------------------------------------------------
-- 6. AI 投递事实（新增）
--    「模型决定要答」与「这句话真的发出去了」是两个事实。
--    有 turn 无 delivery 是合法且必须可表达的：no_match / provider 超时 /
--    限流转人工 / 最终栅栏 CAS 失败 / 会话已关闭 / 人工已接管。
--    幂等键绑定「逻辑回复」而非可能随重试变化的 turn：
--      uq_csaid_reply(organization_id, session_id, job_id)
-- ---------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS customer_service_ai_delivery (
    organization_id  bigint                   NOT NULL,
    workspace_id     bigint                   NOT NULL,
    session_id       bigint                   NOT NULL,
    job_id           bigint                   NOT NULL,
    turn_id          bigint,
    message_id       bigint                   NOT NULL,
    conversation_id  bigint                   NOT NULL,
    -- 来源类别：P0 只有 ai_product（产品内置受控自动应答）；
    -- agent_operator 为 V3.1 §11 的 Agent operator 预留（需 Grant/Run/Effect 齐备）
    origin_kind      text                     NOT NULL DEFAULT 'ai_product'
        CONSTRAINT ck_csaid_origin CHECK (origin_kind IN ('ai_product', 'agent_operator')),
    agent_run_id     bigint,
    effect_id        bigint,
    grant_id         bigint,
    seat_identity_id bigint                   NOT NULL,
    provider         text                     NOT NULL DEFAULT '',
    model            text                     NOT NULL DEFAULT '',
    -- grounding 快照：引用过的 FAQ id/version/excerpt + 引用时的内容哈希。
    -- 存快照而非运行时回读，避免 FAQ 被改写后「旧承诺无法复现」。
    grounding        jsonb                    NOT NULL DEFAULT '{}'::jsonb,
    generation_epoch integer                  NOT NULL CHECK (generation_epoch > 0),
    created_at       timestamptz              NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_customer_service_ai_delivery PRIMARY KEY (organization_id, session_id, message_id),
    -- 一条逻辑作业最多一个已提交的对外效果（跨重试稳定）
    CONSTRAINT uq_csaid_reply UNIQUE (organization_id, session_id, job_id),
    CONSTRAINT ck_csaid_operator CHECK (origin_kind <> 'agent_operator'
        OR (agent_run_id IS NOT NULL AND grant_id IS NOT NULL)),
    CONSTRAINT fk_csaid_organization FOREIGN KEY (organization_id)
        REFERENCES organization (id) ON DELETE RESTRICT,
    CONSTRAINT fk_csaid_session FOREIGN KEY (organization_id, session_id)
        REFERENCES customer_service_session (organization_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE customer_service_ai_delivery IS
    'AI 消息投递事实：message_id 指向 enterprise_message 真源（不建副本）。'
    'AI 消息的来源可辨识性由此表 + 界面徽标共同保证';

-- ---------------------------------------------------------------------
-- 7. AI 作业（新增）—— 带**执行权栅栏**的 durable job
--    ⚠ 全仓既有 SKIP LOCKED 队列（≥8 处）都只有时间比较 + status，
--      没有递增栅栏 ⇒ 租约过期后旧 worker 复活仍可提交。
--      本表刻意照抄唯一带真栅栏的实现 agent_run（version 单调 + lease_owner
--      + CAS WHERE version=$expected），而不是照抄 bot_delivery。
-- ---------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS customer_service_ai_job (
    id                 bigint                   NOT NULL CHECK (id > 0),
    organization_id    bigint                   NOT NULL,
    workspace_id       bigint                   NOT NULL,
    session_id         bigint                   NOT NULL,
    conversation_id    bigint                   NOT NULL,
    inbound_message_id bigint                   NOT NULL,
    -- 派生时的会话栅栏快照；投递前需与当前值比对
    generation_epoch   integer                  NOT NULL CHECK (generation_epoch > 0),
    context_watermark  bigint                   NOT NULL DEFAULT 0
        CONSTRAINT ck_csaij_watermark CHECK (context_watermark >= 0),
    status             text                     NOT NULL DEFAULT 'pending'
        CONSTRAINT ck_csaij_status CHECK (status IN
            ('pending', 'running', 'retry', 'completed', 'cancelled', 'failed', 'dead')),
    attempt            integer                  NOT NULL DEFAULT 0 CHECK (attempt >= 0),
    -- 执行权栅栏：每次认领 version+1；续租/完成/失败/投递都必须带
    -- WHERE version=$expected，旧 worker 的期望值必然失配 ⇒ cas_conflict
    version            integer                  NOT NULL DEFAULT 1 CHECK (version > 0),
    lease_owner        text,
    lease_until        timestamptz,
    next_retry_at      timestamptz              NOT NULL DEFAULT CURRENT_TIMESTAMP,
    -- 任务寿命上限：过期任务不得「排队很久后突然回复」
    deadline_at        timestamptz              NOT NULL,
    last_error_code    text                     NOT NULL DEFAULT '',
    enqueued_at        timestamptz              NOT NULL DEFAULT CURRENT_TIMESTAMP,
    completed_at       timestamptz,
    CONSTRAINT pk_customer_service_ai_job PRIMARY KEY (organization_id, id),
    -- 一条用户消息最多一个逻辑 AI 作业（固化进 DB，不靠应用层自觉）
    CONSTRAINT uq_csaij_org_inbound UNIQUE (organization_id, workspace_id, inbound_message_id),
    CONSTRAINT fk_csaij_organization FOREIGN KEY (organization_id)
        REFERENCES organization (id) ON DELETE RESTRICT,
    CONSTRAINT fk_csaij_session FOREIGN KEY (organization_id, session_id)
        REFERENCES customer_service_session (organization_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE customer_service_ai_job IS
    'AI 作业（durable）。version 是执行权栅栏：拦截「租约过期后旧 worker 复活提交」';

CREATE INDEX IF NOT EXISTS i_csaij_claimable ON customer_service_ai_job
    USING btree (status, next_retry_at) WHERE status IN ('pending', 'retry');
CREATE INDEX IF NOT EXISTS i_csaij_session ON customer_service_ai_job
    USING btree (organization_id, session_id, enqueued_at DESC);

-- ---------------------------------------------------------------------
-- 8. 作业派生游标 + 消费幂等（新增）
--    ⚠ 刻意**不**把 ai_job 建单放进 canonical persist_hook：
--      persist_hook 失败会回滚整笔消息事务 ⇒ AI 专属表/逻辑故障会让客户
--      连普通消息都发不出去。改为在事务外 tail customer_service_event
--      （append-only，与消息同事务提交），用本表做可重放水位。
--      仓库现状：无服务端消费者 offset/consumer group ⇒ 需自建（即本表）。
--    v4（A 组，R5/R6/R7）：`last_event_id` 是应用层 TSID，发号时刻 ≠ 提交
--      顺序——探针 A1 实证迟提交事件被「id 单调推进」永久跳过。故派发依据
--      改为 xmin-horizon 闸门（last_xmin_horizon），id 仅作观测与 reconcile
--      起点；配合 8b 的 consume_seen 幂等表与低频 reconcile sweep 兜底。
-- ---------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS customer_service_ai_consume_cursor (
    organization_id bigint                   NOT NULL,
    workspace_id    bigint                   NOT NULL,
    consumer        text                     NOT NULL DEFAULT 'ai_job_derive',
    last_event_id   bigint                   NOT NULL DEFAULT 0
        CONSTRAINT ck_csaicc_cursor CHECK (last_event_id >= 0),
    -- 上一次消费使用的 pg_snapshot_xmin 闸门；下次增量下界。
    -- 倒退（xid 回卷 / VACUUM FREEZE）不改写本值，记 horizon_regressed 事件
    -- 并触发 reconcile sweep（A.4）。
    last_xmin_horizon bigint                 NOT NULL DEFAULT 0
        CONSTRAINT ck_csaicc_horizon CHECK (last_xmin_horizon >= 0),
    last_consumed_at timestamptz             NOT NULL DEFAULT now(),
    updated_at      timestamptz              NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_customer_service_ai_consume_cursor
        PRIMARY KEY (organization_id, workspace_id, consumer),
    CONSTRAINT fk_csaicc_organization FOREIGN KEY (organization_id)
        REFERENCES organization (id) ON DELETE RESTRICT
);

COMMENT ON TABLE customer_service_ai_consume_cursor IS
    'AI 作业派生的可重放水位（事务外 tail customer_service_event 用）。'
    '派发依据是 last_xmin_horizon 闸门而非 last_event_id（TSID 非提交序，探针 A1）';
COMMENT ON COLUMN customer_service_ai_consume_cursor.last_consumed_at IS
    '上次推进时间。消费者长期离线告警：now() - last_consumed_at > 阈值（A.4）';

-- 8b. 消费幂等表（A.2.2 逐字）：同一 (consumer, event_id) 至多一次副作用。
--     保留窗口默认 7 天，由消费事务顺带清退（consumed_at 索引）。
--     不挂 organization FK：合同 02-A.2.2 冻结 DDL 无此约束，且本表高频
--     写入 + 批量清退，避免每行额外 FK 开销。
CREATE TABLE IF NOT EXISTS customer_service_ai_consume_seen (
    organization_id bigint      NOT NULL,
    workspace_id    bigint      NOT NULL,
    consumer        text        NOT NULL,
    event_id        bigint      NOT NULL,
    consumed_at     timestamptz NOT NULL DEFAULT now(),
    CONSTRAINT pk_csaics PRIMARY KEY (organization_id, workspace_id, consumer, event_id)
);

CREATE INDEX IF NOT EXISTS idx_csaics_prune ON customer_service_ai_consume_seen
    USING btree (consumed_at);

COMMENT ON TABLE customer_service_ai_consume_seen IS
    '事件消费幂等账（I2）：INSERT ON CONFLICT DO NOTHING 影响行数=0 即已消费。'
    '7 天保留窗口必须 > BACKSCAN_EVENT_COUNT 覆盖的时间跨度（启动校验，A.6）';

-- ---------------------------------------------------------------------
-- 9. 成本预算（新增）—— 调用前**预占**，完成后按实际结算
--    ⚠ 全仓现状：无任何生产 token/cost 计量（estimated_cost 等只存在于
--      scripts/mock_llm_server.py）；agent_rate_limiter 是 ETS 单节点固定窗口、
--      无租户维度、无日级硬顶；billing_meter 明确禁止拦截热路径。
--      ⇒ 「预算准入」必须新建，不能宣称复用。
--    预留额度用**条件 UPDATE 返回 0 行**判定失败，而不是 CHECK 约束：
--    CHECK 违规会 abort 整个事务，使负例之后的所有断言变成 25P02 假红。
-- ---------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS customer_service_ai_budget (
    organization_id     bigint                   NOT NULL,
    -- 'YYYY-MM-DD'（日）或 'YYYY-MM'（月）
    period              text                     NOT NULL CHECK (period <> ''),
    scope_kind          text                     NOT NULL
        CONSTRAINT ck_csaib_scope CHECK (scope_kind IN ('session', 'org', 'global')),
    -- session → session_id；org → organization_id；global → 0
    scope_id            bigint                   NOT NULL CHECK (scope_id >= 0),
    -- H.3（G4）：限额无默认值。NULL = 未配置 ⇒ 准入谓词
    -- (reserved + used + est <= limit) 求值为 NULL ⇒ 条件 UPDATE 0 行 ⇒
    -- fail-closed 拒绝。-1「不限」语义废除（全仓零生产计量，数值不得凭空设）。
    limit_tokens        bigint
        CONSTRAINT ck_csaib_lt CHECK (limit_tokens IS NULL OR limit_tokens >= 0),
    reserved_tokens     bigint                   NOT NULL DEFAULT 0 CHECK (reserved_tokens >= 0),
    used_tokens         bigint                   NOT NULL DEFAULT 0 CHECK (used_tokens >= 0),
    limit_cost_micro    bigint
        CONSTRAINT ck_csaib_lc CHECK (limit_cost_micro IS NULL OR limit_cost_micro >= 0),
    reserved_cost_micro bigint                   NOT NULL DEFAULT 0 CHECK (reserved_cost_micro >= 0),
    used_cost_micro     bigint                   NOT NULL DEFAULT 0 CHECK (used_cost_micro >= 0),
    version             integer                  NOT NULL DEFAULT 1 CHECK (version > 0),
    updated_at          timestamptz              NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_customer_service_ai_budget
        PRIMARY KEY (organization_id, period, scope_kind, scope_id),
    CONSTRAINT fk_csaib_organization FOREIGN KEY (organization_id)
        REFERENCES organization (id) ON DELETE RESTRICT
);

COMMENT ON TABLE customer_service_ai_budget IS
    'AI 成本预算累计视图（三级：session/org/global；R11：不是账本）。'
    'limit IS NULL = 未配置 = 准入 fail-closed 拒绝（不是无限额）；'
    '调用级真账本见 customer_service_ai_budget_attempt';
COMMENT ON COLUMN customer_service_ai_budget.reserved_tokens IS
    '预占额度。并发调用不得穿透上限：准入由单条条件 UPDATE 原子完成'
    '（reserved+used+est <= limit），失败返回 0 行，不抛异常';

-- 9b. 预算调用级账本（E.2.2 逐字，R11 对治）：累计表无法识别重复结算
--     （探针 E4 实证 used 520→1040）。每次迁移都是状态 CAS
--     (state + version)，影响 0 行 ⇒ 已迁移，不重复改 budget。
CREATE TABLE IF NOT EXISTS customer_service_ai_budget_attempt (
    attempt_id       text        PRIMARY KEY,
    -- 冻结生成规则（E.2.2）：'<job_id>:<attempt>:<layer_kind>'，稳定可重放
    organization_id  bigint      NOT NULL,
    session_id       bigint      NOT NULL,
    job_id           bigint      NOT NULL,
    layer_kind       text        NOT NULL,
    scope_id         bigint      NOT NULL,
    period           text        NOT NULL,
    reserved_tokens  bigint      NOT NULL,
    settled_tokens   bigint      NOT NULL DEFAULT 0,
    state            text        NOT NULL,
    version          bigint      NOT NULL DEFAULT 1,
    created_at       timestamptz NOT NULL DEFAULT now(),
    updated_at       timestamptz NOT NULL DEFAULT now(),
    CONSTRAINT ck_csaiba_state CHECK (state IN ('reserved','settled','released','uncertain')),
    CONSTRAINT ck_csaiba_settled CHECK (settled_tokens >= 0 AND settled_tokens <= reserved_tokens)
);

CREATE INDEX IF NOT EXISTS idx_csaiba_reconcile ON customer_service_ai_budget_attempt
    USING btree (state, created_at);

COMMENT ON TABLE customer_service_ai_budget_attempt IS
    '预算调用级账本（E 组）。生命周期 reserved→settled/released/uncertain，'
    'uncertain 保守占额度由 reconcile（5min）收口；attempt_id 为幂等键';

-- ---------------------------------------------------------------------
-- 11. AI trace 受控清理（F 组三层防御，R13/R14 对治）
--     L1 权限层：REVOKE DELETE/UPDATE/TRUNCATE（普通角色 42501）
--     L2 触发器层：fn_customer_service_ai_turn_append_only（23514）
--     L3 身份层：SECURITY DEFINER + session_user 白名单 —— 探针 F5 实证
--               普通「角色」可自行 SET LOCAL GUC 后 DELETE 成功，GUC 单独
--               不构成权限边界；session_user 不可被 SET ROLE 伪造。
--     残余风险 K1（显式登记不掩盖）：应用连接是超级用户 ⇒ 可
--     SET SESSION AUTHORIZATION / DISABLE TRIGGER，DB 层无法防御自身；
--     生产改用非超级应用角色列为独立 P1。P0-7 验收显式断言该绕过存在。
-- ---------------------------------------------------------------------
DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_catalog.pg_roles WHERE rolname = 'cs_ai_purge') THEN
        CREATE ROLE cs_ai_purge LOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE;
    END IF;
END
$$;

CREATE TABLE IF NOT EXISTS cs_ai_purge_principal (
    principal text     PRIMARY KEY,
    enabled   boolean  NOT NULL DEFAULT false
);

COMMENT ON TABLE cs_ai_purge_principal IS
    'purge 身份白名单（F.3 L3）。principal = 允许执行 cs_ai_purge_trace 的'
    ' session_user；enabled=false 即时摘除';

-- 播种（F.3.2 决策）：迁移自建角色名自播种（值来自本迁移，非硬编码用户名）；
-- purge worker 以该角色独立非超级凭据连接（K1b：与 imboy_user 分离）。
INSERT INTO cs_ai_purge_principal (principal, enabled)
VALUES ('cs_ai_purge', true)
ON CONFLICT (principal) DO NOTHING;

-- 03-F.3.1 冻结 DDL 的两处实现性修正（合同伪代码不可逐字落地）：
--   1. elib_tsid:next() 是 Erlang 表达式，SQL 内不可执行 ⇒ 审计行 id 由
--      调用方生成 TSID 后经 p_event_id 传入（与应用层事件同源发号）；
--   2. customer_service_event.workspace_id 在迁移 135 后 NOT NULL ⇒ 按
--      135 既有回填语义（org 默认 workspace，兜底最小 active workspace）
--      函数内求值，不能用 NULL 常量。
CREATE OR REPLACE FUNCTION cs_ai_purge_trace(
    p_org_id      bigint,
    p_before_ts   timestamptz,
    p_reason      text,
    p_request_id  text,
    p_event_id    bigint
) RETURNS bigint
LANGUAGE plpgsql
SECURITY DEFINER
SET search_path = pg_catalog, public
AS $$
DECLARE
    v_allowed   boolean;
    v_count     bigint;
    v_workspace bigint;
BEGIN
    -- L3 身份层：SET ROLE 只改 current_user，不改 session_user（F-3b 关键断言）
    SELECT EXISTS (
        SELECT 1 FROM public.cs_ai_purge_principal p
         WHERE p.principal = session_user AND p.enabled
    ) INTO v_allowed;
    IF NOT v_allowed THEN
        RAISE EXCEPTION 'purge_not_authorized' USING ERRCODE = '42501';
    END IF;

    -- 二次确认（defense in depth：把误执行从单点失误变成两点失误；
    -- 单独存在时不是权限边界——探针 F5）
    IF current_setting('imboy.ai_trace_purge', true) IS DISTINCT FROM 'on' THEN
        RAISE EXCEPTION 'purge_confirmation_missing' USING ERRCODE = '23514';
    END IF;

    DELETE FROM public.customer_service_ai_turn
     WHERE organization_id = p_org_id AND created_at < p_before_ts;
    GET DIAGNOSTICS v_count = ROW_COUNT;

    SELECT d.workspace_id INTO v_workspace
      FROM public.organization_default_workspace d
     WHERE d.organization_id = p_org_id;
    IF v_workspace IS NULL THEN
        SELECT min(w.id) INTO v_workspace
          FROM public.workspace w
         WHERE w.organization_id = p_org_id AND w.status = 'active';
    END IF;

    INSERT INTO public.customer_service_event
        (id, organization_id, workspace_id, session_id, business_identity_id,
         actor_user_id, actor_kind, action, detail, created_at)
    VALUES (p_event_id, p_org_id, v_workspace, NULL, NULL,
            NULL, 'system', 'ai.trace_purged',
            jsonb_build_object('count', v_count,
                               'before_ts', p_before_ts,
                               'reason', p_reason,
                               'request_id', p_request_id,
                               'principal', session_user),
            now());

    RETURN v_count;
END $$;

REVOKE ALL ON FUNCTION cs_ai_purge_trace(bigint, timestamptz, text, text, bigint)
    FROM PUBLIC;
GRANT EXECUTE ON FUNCTION cs_ai_purge_trace(bigint, timestamptz, text, text, bigint)
    TO cs_ai_purge;

-- L1 权限层：普通角色无 DELETE/UPDATE/TRUNCATE（表 owner 不受影响——K1）
REVOKE DELETE, UPDATE, TRUNCATE ON customer_service_ai_turn FROM PUBLIC;

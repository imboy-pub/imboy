-- 迁移 00000153_customer_service_seat_console.up.sql: 工作台坐席控制台嵌入
-- （Customer Service Seat Console）持久化基础。
-- 计划契约：seat-console-embed run SC-BE（工作台坐席控制台嵌入面 /seat/:id）。
-- 迁移契约：up=可重复执行（IF NOT EXISTS 幂等），down=安全回滚（fail-closed）。
-- 禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策（镜像 00000132 customer_service_widget_installation 的表级口径）：
--   * public_seat_console_id 是公开标识（嵌入 iframe src 的全局反查键，非
--     secret）：DB 只做 NOT NULL + 全局 UNIQUE + 非空；任何权限换取都经
--     application 层的 active 门，DB 不承载 secret。
--   * allowed_origins 存**原文** jsonb（归一化/去重/上限校验都在应用层，
--     DB 不复制规则）；CSP frame-ancestors 由 handler 按应用层归一结果输出。
--   * (organization_id, workspace_id) 复合 FK 与 125 的 fk_csss_workspace
--     同口径：workspace 必须属于同一 Org，跨租户绑定时 INSERT/UPDATE 直接
--     23503 拒绝。
--   * 同一 (Org, Workspace) 至多一个 active 控制台：部分唯一索引
--     uq_cssc_org_ws_active（WHERE status='active'）——吊销后可重建
--     （revoked 行保留以审计），活跃槽位唯一（23505 → 应用层 conflict）。
--   * status / revoked_at 一致性 CHECK 与 132 同款：(status='revoked') =
--     (revoked_at IS NOT NULL)。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

CREATE TABLE IF NOT EXISTS customer_service_seat_console (
    id                     bigint                   NOT NULL,  -- TSID
    organization_id        bigint                   NOT NULL,
    workspace_id           bigint                   NOT NULL,
    public_seat_console_id text                     NOT NULL,  -- 公开标识；非 secret
    allowed_origins        jsonb                    DEFAULT '[]'::jsonb NOT NULL,  -- 原文；归一化在应用层
    status                 text                     DEFAULT 'active' NOT NULL,
    revoked_at             timestamp with time zone,
    created_by_user_id     bigint,
    version                integer                  DEFAULT 1 NOT NULL,
    created_at             timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at             timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_customer_service_seat_console PRIMARY KEY (id),
    CONSTRAINT uq_cssc_org_id UNIQUE (organization_id, id),
    CONSTRAINT uq_cssc_public_seat_console_id UNIQUE (public_seat_console_id),
    CONSTRAINT ck_cssc_public_seat_console_id CHECK (public_seat_console_id <> ''),
    CONSTRAINT ck_cssc_status CHECK (status = ANY (ARRAY['active'::text, 'revoked'::text])),
    CONSTRAINT ck_cssc_revoked_consistency CHECK (
        (status = 'active' AND revoked_at IS NULL)
        OR (status = 'revoked' AND revoked_at IS NOT NULL)),
    CONSTRAINT ck_cssc_version CHECK (version >= 1),
    CONSTRAINT fk_cssc_organization FOREIGN KEY (organization_id)
        REFERENCES organization(id) ON DELETE RESTRICT,
    CONSTRAINT fk_cssc_workspace FOREIGN KEY (organization_id, workspace_id)
        REFERENCES workspace (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_cssc_created_by FOREIGN KEY (created_by_user_id)
        REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE customer_service_seat_console IS
    '客服工作台坐席控制台嵌入（Org+Workspace 的公开接入面）：public_seat_console_id
    可公开分发（iframe src 全局反查键），嵌入策略由应用层 allowed_origins 归一后
    以 CSP frame-ancestors 输出；本表不承载任何 secret';
COMMENT ON COLUMN customer_service_seat_console.public_seat_console_id IS
    '公开控制台标识（TSID 十进制 string，应用层生成）：/seat/:id 的全局反查键；
    换取任何嵌入授权都必须命中 active 行（吊销即失效，无存在性枚举）';
COMMENT ON COLUMN customer_service_seat_console.allowed_origins IS
    '允许嵌入控制台的 origin 原文 jsonb 数组；scheme+host+port 归一化、去重与
    上限校验由应用层做（DB 存原文）';
COMMENT ON COLUMN customer_service_seat_console.status IS
    'active|revoked：revoked 是 kill switch（行保留以审计，嵌入面立即失效）';

-- 活跃槽位唯一：同一 (Org, Workspace) 至多一个 active 控制台（吊销后可重建）。
CREATE UNIQUE INDEX IF NOT EXISTS uq_cssc_org_ws_active ON customer_service_seat_console
    (organization_id, workspace_id)
    WHERE status = 'active';

-- 列表/反查的常规索引（Org+Workspace+status 前缀与列表谓词对齐）。
CREATE INDEX IF NOT EXISTS i_cssc_org_ws_status ON customer_service_seat_console
    USING btree (organization_id, workspace_id, status);

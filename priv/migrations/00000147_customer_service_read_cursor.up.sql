-- 迁移 00000147: 客服已读游标表（CS-BE-04 durable read cursor / unread）。
--
-- 冻结决策（CS-DEC-02 / 计划 v1.1 CS-BE-04）：
--   * read cursor 绑定 (organization, session, business_identity)——**不是全局
--     user**：转接（transfer）换绑 identity 后，受让人从 transfer 边界开始计
--     新未读；前经办人的游标留在原地，互不污染。
--   * ACK 单调前进、不可回退：应用写路径以「新值 > 旧值」为条件的 upsert
--     （ON CONFLICT ... DO UPDATE ... WHERE last_read_message_id < EXCLUDED.*），
--     重复 / 乱序后到的旧 ACK 是零行 no-op（幂等），updated_at 不被刷新。
--   * 未读数不建冗余计数表：由 cursor 与 enterprise_message 事实**现算**
--     （count(visible AND sender_type='contact' AND id > cursor)）——本表只有
--     游标一列事实，无任何计数列，杜绝双写漂移。
--   * transfer 边界在同一写事务内落库（cs_pg_session:transfer_session 的
--     CAS UPDATE 成功后 upsert 受让人游标 = 该时刻会话内最大 message id，
--     无消息为 0）——transfer 前的历史消息对受让人默认 0 unread。
--   * SSE 推送只是「刷新提示」：本表只由显式 ACK 与 transfer 边界写入，
--     没有任何事件/推送侧的隐式推进路径。
--
-- 命名纪律（FULL-02 / 00000146 同款）：不新建函数/触发器，只建表/约束/索引/
-- 注释；幂等用 IF NOT EXISTS / DROP IF EXISTS；游标取值非负。
--
-- 禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

CREATE TABLE IF NOT EXISTS customer_service_read_cursor (
    id                   bigint                   NOT NULL,  -- TSID
    organization_id      bigint                   NOT NULL,
    workspace_id         bigint                   NOT NULL,  -- 铁律 6：租户操作范围显式贯穿
    session_id           bigint                   NOT NULL,
    business_identity_id bigint                   NOT NULL,  -- 绑定经办 identity（不是 user）
    last_read_message_id bigint                   NOT NULL,  -- 已读游标：0=尚无已读事实
    created_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_customer_service_read_cursor PRIMARY KEY (id),
    CONSTRAINT uq_csrc_org_session_identity
        UNIQUE (organization_id, session_id, business_identity_id),
    CONSTRAINT ck_csrc_cursor_nonneg CHECK (last_read_message_id >= 0),
    CONSTRAINT fk_csrc_session FOREIGN KEY (organization_id, session_id)
        REFERENCES customer_service_session (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_csrc_seat FOREIGN KEY (organization_id, business_identity_id)
        REFERENCES customer_service_seat (organization_id, business_identity_id)
        ON DELETE RESTRICT
);

COMMENT ON TABLE customer_service_read_cursor IS
    '客服已读游标（CS-BE-04，绑定 org+session+经办 identity）：ACK 单调前进不可回退；未读数由本表与 enterprise_message 事实现算，无冗余计数列';
COMMENT ON COLUMN customer_service_read_cursor.business_identity_id IS
    '游标归属的经办业务身份（assignment/identity 绑定）：同一会话不同经办各有独立游标；transfer 换绑后受让人从 transfer 边界起计新未读（CS-DEC-02）';
COMMENT ON COLUMN customer_service_read_cursor.last_read_message_id IS
    '已读到的最后一条 enterprise_message id（0=尚无已读事实）：ACK 写路径按「新值>旧值」单调 upsert，重复/乱序旧 ACK 为零行 no-op';

CREATE INDEX IF NOT EXISTS i_csrc_org_identity ON customer_service_read_cursor
    USING btree (organization_id, business_identity_id);

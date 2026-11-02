-- V3-23 企业频道（S3）消息通道：origin 账本 conversation_kind 扩 'channel'
-- + 企业频道消息表。
--
-- 前向迁移（编号预约制，不改历史 SQL；00000140 的 ck_emo_conversation
-- 原文：CHECK (conversation_kind = ANY (ARRAY['direct','group']))）：
--   1) ck_emo_conversation 扩取值 'channel'——INT-50 企业频道代发与
--      人共用的 origin 账本必须能登记 channel 会话类别；
--   2) 新表 enterprise_channel_message：频道消息无群成员语义（SPEC §5 B1
--      倾向独立表），不走 msg_c2g；企业托管恒非 E2EE（e2ee 恒 NULL，
--      无 sender_did——服务端主体无设备）。

SET lock_timeout = '5s';

-- 1) origin 账本会话类别扩展（CHECK 重建，前向只增取值）
ALTER TABLE enterprise_message_origin
    DROP CONSTRAINT ck_emo_conversation;
ALTER TABLE enterprise_message_origin
    ADD CONSTRAINT ck_emo_conversation
    CHECK (conversation_kind = ANY (ARRAY['direct'::text, 'group'::text, 'channel'::text]));

-- 2) 企业频道消息表（行语义与 msg_c2c/msg_c2g 同族：msg_id 为业务消息 ID）
CREATE TABLE IF NOT EXISTS enterprise_channel_message (
    id              bigint                   NOT NULL,
    channel_id      bigint                   NOT NULL,
    organization_id bigint                   NOT NULL,
    from_id         bigint                   NOT NULL,
    msg_id          text                     NOT NULL,
    msg_type        character varying(50)    NOT NULL,
    payload         jsonb                    NOT NULL,
    e2ee            boolean,
    server_ts       timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_enterprise_channel_message PRIMARY KEY (id),
    CONSTRAINT uq_ecm_msg_id UNIQUE (msg_id),
    CONSTRAINT ck_ecm_channel CHECK (channel_id > 0),
    CONSTRAINT ck_ecm_org CHECK (organization_id > 0),
    CONSTRAINT ck_ecm_from CHECK (from_id > 0),
    CONSTRAINT ck_ecm_msg_id_shape CHECK (msg_id <> ''),
    CONSTRAINT ck_ecm_non_e2ee CHECK (e2ee IS NULL)
);

CREATE INDEX IF NOT EXISTS i_ecm_channel_created
    ON enterprise_channel_message USING btree (channel_id, created_at DESC, id DESC);
CREATE INDEX IF NOT EXISTS i_ecm_org_channel
    ON enterprise_channel_message USING btree (organization_id, channel_id);

COMMENT ON TABLE enterprise_channel_message IS
    '企业频道消息（V3-23 INT-50）：企业 Application 代发与指定员工发送共用通道；'
    'scope 边界（org/workspace/channel status）由 logic 层 fail-closed 校验，'
    '本表不触个人 E2EE 路径（e2ee 恒 NULL）';

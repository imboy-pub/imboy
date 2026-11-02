-- V3-23 企业频道消息通道回滚：删除频道消息表并还原 origin CHECK。
SET lock_timeout = '5s';

DROP TABLE IF EXISTS enterprise_channel_message;

ALTER TABLE enterprise_message_origin
    DROP CONSTRAINT ck_emo_conversation;
ALTER TABLE enterprise_message_origin
    ADD CONSTRAINT ck_emo_conversation
    CHECK (conversation_kind = ANY (ARRAY['direct'::text, 'group'::text]));

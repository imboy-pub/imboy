-- Table: public.msg_c2g_timeline

-- DROP TABLE IF EXISTS public."msg_c2g_timeline";

CREATE TABLE IF NOT EXISTS public."msg_c2g_timeline"
(
    msg_id varchar(40) NOT NULL,
    to_uid bigint NOT NULL,
    to_gid bigint NOT NULL,
    client_ack smallint NOT NULL DEFAULT 0, -- 0 客户端未确认  1 客户端已确认
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.msg_c2g_timeline OWNER to imboy_user;

COMMENT ON TABLE public.msg_c2g_timeline IS '群聊消息时间线';
COMMENT ON COLUMN public.msg_c2g_timeline.msg_id IS '消息唯一标识';

COMMENT ON COLUMN public.msg_c2g_timeline.to_uid IS '消息接收人user_id';
COMMENT ON COLUMN public.msg_c2g_timeline.to_gid IS '消息接收群 group_id';
COMMENT ON COLUMN public.msg_c2g_timeline.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';

-- index
CREATE INDEX uk_c2g_timeline_MsgId ON public.msg_c2g_timeline (msg_id);
CREATE UNIQUE INDEX uk_c2g_timeline_ToUid_MsgId ON public.msg_c2g_timeline (to_uid, msg_id);
CREATE INDEX idx_c2g_timeline_ToUid_ClientAck ON public.msg_c2g_timeline (to_uid, client_ack);

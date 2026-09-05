-- 迁移 00087: report_ticket 一等举报目标与结构化证据（合规计划 R-01）
-- Migration 00087: first-class report targets and structured evidence (R-01).
--
-- 目标：举报指向真实 UGC 对象（含 C2C/C2G/频道消息），消息 ID 不再写入
-- description 自由文本伪装成 user 举报。旧行 target_sub_type='' /
-- scope=0 / author=0 / evidence='{}'，对 moment/group/channel/user 兼容不变。
--   * target_sub_type : 消息表面 c2c | c2g | channel（非消息举报为空串）
--   * target_scope_id : c2c=对话对端 uid；c2g=群 id；channel=频道 id
--   * target_author_id: 被举报消息发送者 uid
--   * evidence        : 白名单字段 JSONB（内容摘录/哈希/客户端 msg_id/
--                       E2EE 同意标记/举报时点目标状态等服务端核验元数据）
-- Do not edit manually.

ALTER TABLE public.report_ticket ADD COLUMN IF NOT EXISTS target_sub_type character varying(16) DEFAULT ''::character varying NOT NULL;
ALTER TABLE public.report_ticket ADD COLUMN IF NOT EXISTS target_scope_id bigint DEFAULT 0 NOT NULL;
ALTER TABLE public.report_ticket ADD COLUMN IF NOT EXISTS target_author_id bigint DEFAULT 0 NOT NULL;
ALTER TABLE public.report_ticket ADD COLUMN IF NOT EXISTS evidence jsonb DEFAULT '{}'::jsonb NOT NULL;

-- target_type 白名单扩展 'message'（重建 CHECK，旧值集合保持合法）
ALTER TABLE public.report_ticket DROP CONSTRAINT IF EXISTS chk_report_ticket_target_type;
ALTER TABLE public.report_ticket ADD CONSTRAINT chk_report_ticket_target_type CHECK (
    (target_type)::text = ANY ((ARRAY['moment'::character varying, 'group'::character varying, 'channel'::character varying, 'user'::character varying, 'message'::character varying])::text[])
);

-- target_sub_type 白名单（空串=非消息举报）
ALTER TABLE public.report_ticket DROP CONSTRAINT IF EXISTS chk_report_ticket_target_sub_type;
ALTER TABLE public.report_ticket ADD CONSTRAINT chk_report_ticket_target_sub_type CHECK (
    (target_sub_type)::text = ANY ((ARRAY[''::character varying, 'c2c'::character varying, 'c2g'::character varying, 'channel'::character varying])::text[])
);

-- message 举报必须携带子类型与 scope/author（服务端创建前核验，CHECK 为底线防御）
ALTER TABLE public.report_ticket DROP CONSTRAINT IF EXISTS chk_report_ticket_message_shape;
ALTER TABLE public.report_ticket ADD CONSTRAINT chk_report_ticket_message_shape CHECK (
    (target_type)::text <> 'message'::text
    OR (
        (target_sub_type)::text = ANY ((ARRAY['c2c'::character varying, 'c2g'::character varying, 'channel'::character varying])::text[])
        AND target_scope_id > 0
        AND target_author_id > 0
    )
);

CREATE INDEX IF NOT EXISTS idx_report_ticket_author ON public.report_ticket USING btree (target_author_id) WHERE (target_author_id > 0);
CREATE INDEX IF NOT EXISTS idx_report_ticket_sub_scope ON public.report_ticket USING btree (target_sub_type, target_scope_id) WHERE (target_sub_type <> ''::character varying);

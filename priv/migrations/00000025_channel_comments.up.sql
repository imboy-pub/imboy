-- 频道消息评论表 / Channel message comments
-- 对标公众号/知识星球评论：支持多级回复、点赞计数
CREATE TABLE IF NOT EXISTS public.channel_comment (
    id bigint NOT NULL,
    channel_id bigint NOT NULL,
    message_id bigint NOT NULL,
    user_id bigint NOT NULL,
    user_name character varying(200) DEFAULT ''::character varying NOT NULL,
    user_avatar character varying(320) DEFAULT ''::character varying NOT NULL,
    content text NOT NULL,
    parent_id bigint DEFAULT 0 NOT NULL,
    reply_to_uid bigint DEFAULT 0 NOT NULL,
    reply_to_name character varying(200) DEFAULT ''::character varying NOT NULL,
    like_count bigint DEFAULT 0 NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

-- 主键
ALTER TABLE ONLY public.channel_comment
    ADD CONSTRAINT channel_comment_pkey PRIMARY KEY (id);

-- 索引：按消息查评论（消息详情页评论列表）
CREATE INDEX idx_channel_comment_message_id
    ON public.channel_comment USING btree (message_id, status, created_at);

-- 索引：按频道查评论（频道评论总览）
CREATE INDEX idx_channel_comment_channel_id
    ON public.channel_comment USING btree (channel_id, status, created_at);

-- 索引：按用户查评论（我的评论）
CREATE INDEX idx_channel_comment_user_id
    ON public.channel_comment USING btree (user_id, created_at);

-- 父评论引用（自引用，软引用不建 FK 以适配 TSID）
CREATE INDEX idx_channel_comment_parent_id
    ON public.channel_comment USING btree (parent_id);

COMMENT ON TABLE public.channel_comment IS '频道消息评论';
COMMENT ON COLUMN public.channel_comment.parent_id IS '父评论ID，0=顶级评论';
COMMENT ON COLUMN public.channel_comment.reply_to_uid IS '被回复用户ID（二级回复），0=无';
COMMENT ON COLUMN public.channel_comment.status IS '1=正常, 0=已删除';

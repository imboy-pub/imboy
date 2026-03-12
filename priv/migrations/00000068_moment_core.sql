-- ================================================================
-- Moment Core - 数据库迁移
-- 版本: 00000068
-- 日期: 2026-02-26
-- 说明: 朋友圈（Moment）核心表结构（首版）
-- ================================================================

-- ================================================================
-- 1) 动态主表
-- ================================================================
CREATE TABLE IF NOT EXISTS public.moment_post (
    id BIGSERIAL PRIMARY KEY,
    author_uid BIGINT NOT NULL,
    content TEXT NOT NULL DEFAULT '',
    media JSONB NOT NULL DEFAULT '[]',          -- 图片/视频元数据数组
    visibility SMALLINT NOT NULL DEFAULT 1,     -- 0公开 1仅好友 2仅自己 3部分可见 4不给谁看
    allow_comment BOOLEAN NOT NULL DEFAULT true,
    like_count INTEGER NOT NULL DEFAULT 0,
    comment_count INTEGER NOT NULL DEFAULT 0,
    status SMALLINT NOT NULL DEFAULT 1,         -- -1删除 0禁用 1正常
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    CONSTRAINT fk_moment_post_author FOREIGN KEY (author_uid)
        REFERENCES public.user(id) ON DELETE CASCADE,
    CONSTRAINT chk_moment_post_visibility CHECK (visibility IN (0, 1, 2, 3, 4))
);

CREATE INDEX IF NOT EXISTS idx_moment_post_author_created
    ON public.moment_post(author_uid, created_at DESC);

CREATE INDEX IF NOT EXISTS idx_moment_post_status_created
    ON public.moment_post(status, created_at DESC);

CREATE INDEX IF NOT EXISTS idx_moment_post_visibility_status
    ON public.moment_post(visibility, status, created_at DESC);

COMMENT ON TABLE public.moment_post IS '朋友圈动态主表';
COMMENT ON COLUMN public.moment_post.author_uid IS '动态发布者用户ID';
COMMENT ON COLUMN public.moment_post.content IS '动态文本内容';
COMMENT ON COLUMN public.moment_post.media IS '媒体数组(JSONB): image/video 元数据';
COMMENT ON COLUMN public.moment_post.visibility IS '可见性: 0公开 1仅好友 2仅自己 3部分可见 4不给谁看';
COMMENT ON COLUMN public.moment_post.allow_comment IS '是否允许评论';
COMMENT ON COLUMN public.moment_post.like_count IS '点赞数量缓存';
COMMENT ON COLUMN public.moment_post.comment_count IS '评论数量缓存';
COMMENT ON COLUMN public.moment_post.status IS '状态: -1删除 0禁用 1正常';

-- ================================================================
-- 2) 动态可见性 ACL 表
-- ================================================================
CREATE TABLE IF NOT EXISTS public.moment_post_acl (
    id BIGSERIAL PRIMARY KEY,
    post_id BIGINT NOT NULL,
    uid BIGINT NOT NULL,
    acl_type SMALLINT NOT NULL,                 -- 1 allow(部分可见) 2 deny(不给谁看)
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    CONSTRAINT fk_moment_post_acl_post FOREIGN KEY (post_id)
        REFERENCES public.moment_post(id) ON DELETE CASCADE,
    CONSTRAINT fk_moment_post_acl_uid FOREIGN KEY (uid)
        REFERENCES public.user(id) ON DELETE CASCADE,
    CONSTRAINT chk_moment_post_acl_type CHECK (acl_type IN (1, 2)),
    CONSTRAINT uk_moment_post_acl UNIQUE (post_id, uid, acl_type)
);

CREATE INDEX IF NOT EXISTS idx_moment_post_acl_post
    ON public.moment_post_acl(post_id, acl_type);

CREATE INDEX IF NOT EXISTS idx_moment_post_acl_uid
    ON public.moment_post_acl(uid, acl_type);

COMMENT ON TABLE public.moment_post_acl IS '动态可见性ACL表(allow/deny)';
COMMENT ON COLUMN public.moment_post_acl.acl_type IS '1=allow 2=deny';

-- ================================================================
-- 3) 时间线表（收件箱）
-- ================================================================
CREATE TABLE IF NOT EXISTS public.moment_timeline (
    id BIGSERIAL PRIMARY KEY,
    recipient_uid BIGINT NOT NULL,              -- 时间线所属用户
    post_id BIGINT NOT NULL,
    author_uid BIGINT NOT NULL,
    rank_at TIMESTAMPTZ NOT NULL,               -- 排序时间（默认动态创建时间）
    status SMALLINT NOT NULL DEFAULT 1,         -- 0隐藏 1可见
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    CONSTRAINT fk_moment_timeline_post FOREIGN KEY (post_id)
        REFERENCES public.moment_post(id) ON DELETE CASCADE,
    CONSTRAINT fk_moment_timeline_recipient FOREIGN KEY (recipient_uid)
        REFERENCES public.user(id) ON DELETE CASCADE,
    CONSTRAINT fk_moment_timeline_author FOREIGN KEY (author_uid)
        REFERENCES public.user(id) ON DELETE CASCADE,
    CONSTRAINT uk_moment_timeline_uid_post UNIQUE (recipient_uid, post_id)
);

CREATE INDEX IF NOT EXISTS idx_moment_timeline_uid_rank
    ON public.moment_timeline(recipient_uid, rank_at DESC, id DESC);

CREATE INDEX IF NOT EXISTS idx_moment_timeline_author
    ON public.moment_timeline(author_uid, rank_at DESC);

COMMENT ON TABLE public.moment_timeline IS '朋友圈时间线收件箱';
COMMENT ON COLUMN public.moment_timeline.rank_at IS '时间线排序时间';

-- ================================================================
-- 4) 点赞表
-- ================================================================
CREATE TABLE IF NOT EXISTS public.moment_like (
    id BIGSERIAL PRIMARY KEY,
    post_id BIGINT NOT NULL,
    user_id BIGINT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    CONSTRAINT fk_moment_like_post FOREIGN KEY (post_id)
        REFERENCES public.moment_post(id) ON DELETE CASCADE,
    CONSTRAINT fk_moment_like_user FOREIGN KEY (user_id)
        REFERENCES public.user(id) ON DELETE CASCADE,
    CONSTRAINT uk_moment_like_post_user UNIQUE (post_id, user_id)
);

CREATE INDEX IF NOT EXISTS idx_moment_like_post_created
    ON public.moment_like(post_id, created_at DESC);

CREATE INDEX IF NOT EXISTS idx_moment_like_user_created
    ON public.moment_like(user_id, created_at DESC);

COMMENT ON TABLE public.moment_like IS '朋友圈点赞关系表';

-- ================================================================
-- 5) 评论表
-- ================================================================
CREATE TABLE IF NOT EXISTS public.moment_comment (
    id BIGSERIAL PRIMARY KEY,
    post_id BIGINT NOT NULL,
    user_id BIGINT NOT NULL,
    reply_to_uid BIGINT,                        -- 可空，回复目标用户
    content VARCHAR(500) NOT NULL,
    status SMALLINT NOT NULL DEFAULT 1,         -- 0删除 1正常
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    CONSTRAINT fk_moment_comment_post FOREIGN KEY (post_id)
        REFERENCES public.moment_post(id) ON DELETE CASCADE,
    CONSTRAINT fk_moment_comment_user FOREIGN KEY (user_id)
        REFERENCES public.user(id) ON DELETE CASCADE,
    CONSTRAINT fk_moment_comment_reply_to_uid FOREIGN KEY (reply_to_uid)
        REFERENCES public.user(id) ON DELETE SET NULL
);

CREATE INDEX IF NOT EXISTS idx_moment_comment_post_created
    ON public.moment_comment(post_id, created_at DESC);

CREATE INDEX IF NOT EXISTS idx_moment_comment_user_created
    ON public.moment_comment(user_id, created_at DESC);

COMMENT ON TABLE public.moment_comment IS '朋友圈评论表';

-- ================================================================
-- 6) 举报表（Admin 首版必须）
-- ================================================================
CREATE TABLE IF NOT EXISTS public.moment_report (
    id BIGSERIAL PRIMARY KEY,
    post_id BIGINT NOT NULL,
    reporter_uid BIGINT NOT NULL,
    reason VARCHAR(64) NOT NULL DEFAULT '',
    description VARCHAR(500) NOT NULL DEFAULT '',
    status SMALLINT NOT NULL DEFAULT 0,         -- 0待处理 1已驳回 2已确认违规
    handled_by BIGINT,
    handled_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    CONSTRAINT fk_moment_report_post FOREIGN KEY (post_id)
        REFERENCES public.moment_post(id) ON DELETE CASCADE,
    CONSTRAINT fk_moment_report_reporter FOREIGN KEY (reporter_uid)
        REFERENCES public.user(id) ON DELETE CASCADE,
    CONSTRAINT fk_moment_report_handler FOREIGN KEY (handled_by)
        REFERENCES public.user(id) ON DELETE SET NULL,
    CONSTRAINT chk_moment_report_status CHECK (status IN (0, 1, 2)),
    CONSTRAINT uk_moment_report_post_reporter UNIQUE (post_id, reporter_uid)
);

CREATE INDEX IF NOT EXISTS idx_moment_report_status_created
    ON public.moment_report(status, created_at DESC);

CREATE INDEX IF NOT EXISTS idx_moment_report_post
    ON public.moment_report(post_id, created_at DESC);

COMMENT ON TABLE public.moment_report IS '朋友圈举报记录表';
COMMENT ON COLUMN public.moment_report.status IS '0待处理 1已驳回 2已确认违规';

-- ================================================================
-- 7) 触发器函数：自动更新 updated_at
-- ================================================================
CREATE OR REPLACE FUNCTION fn_moment_touch_updated_at()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS tr_moment_post_updated_at ON public.moment_post;
CREATE TRIGGER tr_moment_post_updated_at
    BEFORE UPDATE ON public.moment_post
    FOR EACH ROW
    EXECUTE FUNCTION fn_moment_touch_updated_at();

DROP TRIGGER IF EXISTS tr_moment_comment_updated_at ON public.moment_comment;
CREATE TRIGGER tr_moment_comment_updated_at
    BEFORE UPDATE ON public.moment_comment
    FOR EACH ROW
    EXECUTE FUNCTION fn_moment_touch_updated_at();

DROP TRIGGER IF EXISTS tr_moment_report_updated_at ON public.moment_report;
CREATE TRIGGER tr_moment_report_updated_at
    BEFORE UPDATE ON public.moment_report
    FOR EACH ROW
    EXECUTE FUNCTION fn_moment_touch_updated_at();

-- ================================================================
-- 8) 验证查询（可选）
-- ================================================================
-- SELECT tablename FROM pg_tables WHERE schemaname='public' AND tablename LIKE 'moment_%' ORDER BY tablename;


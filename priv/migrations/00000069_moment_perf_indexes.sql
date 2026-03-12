-- ================================================================
-- Moment Performance Index Tuning
-- 版本: 00000069
-- 日期: 2026-02-26
-- 说明: 针对 Moment Feed/用户页/评论/举报/时间线查询的索引校准
-- ================================================================

-- Feed 主查询: status + id 游标倒序
CREATE INDEX IF NOT EXISTS idx_moment_post_active_id_desc
    ON public.moment_post(id DESC)
    WHERE status = 1;

-- 用户动态查询: author_uid + status + id 游标倒序
CREATE INDEX IF NOT EXISTS idx_moment_post_author_active_id_desc
    ON public.moment_post(author_uid, id DESC)
    WHERE status = 1;

-- 评论分页查询: post_id + status + id 游标倒序
CREATE INDEX IF NOT EXISTS idx_moment_comment_post_active_id_desc
    ON public.moment_comment(post_id, id DESC)
    WHERE status = 1;

-- 举报待处理分页: status=0 + id 倒序
CREATE INDEX IF NOT EXISTS idx_moment_report_pending_id_desc
    ON public.moment_report(id DESC)
    WHERE status = 0;

-- 时间线分页: recipient_uid + status + id 游标倒序
CREATE INDEX IF NOT EXISTS idx_moment_timeline_recipient_active_id_desc
    ON public.moment_timeline(recipient_uid, id DESC)
    WHERE status = 1;

-- 点赞列表: post_id + id 倒序
CREATE INDEX IF NOT EXISTS idx_moment_like_post_id_desc
    ON public.moment_like(post_id, id DESC);

-- 为外键列添加缺失的索引，优化 JOIN 和级联删除性能
-- 2026-04-06

CREATE INDEX IF NOT EXISTS i_user_friend_from_user_id
    ON public.user_friend (from_user_id);

CREATE INDEX IF NOT EXISTS i_user_friend_category_id
    ON public.user_friend (category_id)
    WHERE category_id > 0;

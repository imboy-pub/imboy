-- 迁移 233 down: 回滚频道查看更新边界水位（W08）
--
-- 仅回退结构性增量（四列 + 索引）；按 plan §8.3 回退原则，回退只关闭
-- 新入口/协议且保留数据语义——此处无数据删除（列值随列一起删除）。

DROP INDEX IF EXISTS public.i_channel_message_channel_update_seq;

ALTER TABLE public.channel_subscription
    DROP COLUMN IF EXISTS activation_version;

ALTER TABLE public.channel_subscription
    DROP COLUMN IF EXISTS view_seq;

ALTER TABLE public.channel_message
    DROP COLUMN IF EXISTS update_seq;

ALTER TABLE public.channel
    DROP COLUMN IF EXISTS publish_seq;

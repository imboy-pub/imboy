-- ============================================================
-- 合并迁移回滚 00000003: channel（原 000004_channel）
-- ============================================================


DROP TABLE IF EXISTS public."channel_subscription" CASCADE;
DROP TABLE IF EXISTS public."channel_stats_daily" CASCADE;
DROP TABLE IF EXISTS public."channel_reaction" CASCADE;
DROP TABLE IF EXISTS public."channel_price" CASCADE;
DROP TABLE IF EXISTS public."channel_order" CASCADE;
DROP TABLE IF EXISTS public."channel_message_view" CASCADE;
DROP TABLE IF EXISTS public."channel_message" CASCADE;
DROP TABLE IF EXISTS public."channel_invitation" CASCADE;
DROP TABLE IF EXISTS public."channel_admin" CASCADE;
DROP TABLE IF EXISTS public."channel" CASCADE;

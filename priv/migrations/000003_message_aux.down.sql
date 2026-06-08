-- ============================================================
-- 合并迁移回滚 000003: message_aux
-- ============================================================


DROP TABLE IF EXISTS public."msg_store_seq" CASCADE;
DROP TABLE IF EXISTS public."msg_store" CASCADE;
DROP TABLE IF EXISTS public."msg_reaction" CASCADE;
DROP TABLE IF EXISTS public."msg_mention" CASCADE;
DROP TABLE IF EXISTS public."msg_forward" CASCADE;
DROP TABLE IF EXISTS public."msg_read" CASCADE;
DROP TABLE IF EXISTS public."msg_topic" CASCADE;
DROP TABLE IF EXISTS public."msg_c2g_timeline" CASCADE;

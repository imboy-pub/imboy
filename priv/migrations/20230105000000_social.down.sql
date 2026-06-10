-- ============================================================
-- 合并迁移回滚 000005: social
-- ============================================================


DROP TABLE IF EXISTS public."wallet_transaction" CASCADE;
DROP TABLE IF EXISTS public."wallet" CASCADE;
DROP TABLE IF EXISTS public."plugin_audit_log" CASCADE;
DROP TABLE IF EXISTS public."report_action_log" CASCADE;
DROP TABLE IF EXISTS public."report_ticket" CASCADE;
DROP TABLE IF EXISTS public."feedback_reply" CASCADE;
DROP TABLE IF EXISTS public."feedback" CASCADE;
DROP TABLE IF EXISTS public."compliance_key" CASCADE;
DROP TABLE IF EXISTS public."e2ee_trusted_contacts" CASCADE;
DROP TABLE IF EXISTS public."e2ee_transfer_sessions" CASCADE;
DROP TABLE IF EXISTS public."e2ee_social_shards" CASCADE;
DROP TABLE IF EXISTS public."e2ee_shard_transmission_log" CASCADE;
DROP TABLE IF EXISTS public."e2ee_local_backups" CASCADE;
DROP TABLE IF EXISTS public."e2ee_key_shares" CASCADE;
DROP TABLE IF EXISTS public."live_room" CASCADE;
DROP TABLE IF EXISTS public."moment_timeline" CASCADE;
DROP TABLE IF EXISTS public."moment_report" CASCADE;
DROP TABLE IF EXISTS public."moment_like" CASCADE;
DROP TABLE IF EXISTS public."moment_comment" CASCADE;
DROP TABLE IF EXISTS public."moment_post_acl" CASCADE;
DROP TABLE IF EXISTS public."moment_post" CASCADE;

DROP TABLE IF EXISTS public."app_upgrade_log" CASCADE;
DROP INDEX IF EXISTS idx_app_upgrade_log_created_at;
DROP INDEX IF EXISTS idx_app_upgrade_log_event;
DROP INDEX IF EXISTS idx_app_upgrade_log_client_vsn;

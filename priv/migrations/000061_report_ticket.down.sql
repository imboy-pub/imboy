DROP TABLE IF EXISTS public."report_ticket" CASCADE;
DROP TABLE IF EXISTS public."report_action_log" CASCADE;
DROP INDEX IF EXISTS idx_report_ticket_status_created;
DROP INDEX IF EXISTS idx_report_ticket_target_status;
DROP INDEX IF EXISTS idx_report_ticket_reporter_created;
DROP INDEX IF EXISTS idx_report_action_report_created;
DROP INDEX IF EXISTS idx_report_action_operator_created;
DROP FUNCTION IF EXISTS public.fn_report_ticket_touch_updated_at;
DROP TRIGGER IF EXISTS tr_report_ticket_updated_at ON public."report_ticket";

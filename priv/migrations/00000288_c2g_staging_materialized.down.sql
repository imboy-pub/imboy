-- Before application rollback, stop all producers writing true. Drain marked
-- staging rows with the new workers and stop them before starting legacy workers.
-- Retain failed marked rows until explicitly recovered; they may be retried.
DO $rollback$
BEGIN
    LOCK TABLE public.msg_store_staging IN ACCESS EXCLUSIVE MODE;
    IF EXISTS (
        SELECT 1 FROM public.msg_store_staging
        WHERE c2g_materialized AND (processed_at IS NULL OR error_msg IS NOT NULL)
    ) THEN
        RAISE EXCEPTION 'C2G materialized staging rows still require the new worker';
    END IF;
    ALTER TABLE public.msg_store_staging DROP COLUMN c2g_materialized;
END
$rollback$;

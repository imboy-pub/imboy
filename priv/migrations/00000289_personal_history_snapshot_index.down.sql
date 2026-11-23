SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

DROP INDEX IF EXISTS public.idx_pha_snapshot_account_id;

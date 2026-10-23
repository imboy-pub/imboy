-- Paid-request history must not be silently discarded by a rollback.
DO $$ BEGIN RAISE EXCEPTION 'provider task rollback requires explicit archival and review'; END $$;

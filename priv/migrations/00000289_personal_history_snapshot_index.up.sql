-- Account snapshot pagination uses immutable archive IDs and includes tombstones.
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

CREATE INDEX idx_pha_snapshot_account_id
    ON public.personal_history_archive (account_id, domain, generation, id);

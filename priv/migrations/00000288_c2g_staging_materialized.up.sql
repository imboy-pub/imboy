-- Apply the schema first. Existing producers may continue writing the default false.
-- Stop and verify all legacy staging workers have stopped before enabling any
-- producer that writes true. Legacy workers would replay the formal rows.
ALTER TABLE public.msg_store_staging
    ADD COLUMN c2g_materialized boolean NOT NULL DEFAULT false,
    ADD CONSTRAINT msg_store_staging_c2g_materialized_check
        CHECK (NOT c2g_materialized OR type = 'c2g') NOT VALID;

-- Create the durable inbox before the TSID guard scans the schema on first boot.
-- Existing deployments retain their staging rows; runtime ensure_table_exists
-- remains idempotent and uses the same columns and indexes.
CREATE TABLE IF NOT EXISTS public.msg_store_staging (
    id BIGINT PRIMARY KEY,
    type VARCHAR(10) NOT NULL,
    msg_id VARCHAR(50) NOT NULL,
    msg_type VARCHAR(50),
    action VARCHAR(50),
    e2ee JSONB,
    sender_did VARCHAR(128),
    conv_seq BIGINT,
    payload JSONB NOT NULL,
    from_id BIGINT NOT NULL,
    to_id BIGINT,
    to_id_list BIGINT[],
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    server_ts TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    retry_count INTEGER NOT NULL DEFAULT 0,
    processed_at TIMESTAMPTZ,
    available_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    error_msg TEXT,
    CONSTRAINT msg_store_staging_type_msg_id_key UNIQUE (type, msg_id)
);
CREATE INDEX IF NOT EXISTS msg_store_staging_processed_at_idx
    ON public.msg_store_staging (processed_at) WHERE processed_at IS NOT NULL;
CREATE INDEX IF NOT EXISTS msg_store_staging_available_at_idx
    ON public.msg_store_staging (available_at) WHERE processed_at IS NULL;
CREATE INDEX IF NOT EXISTS msg_store_staging_created_at_idx
    ON public.msg_store_staging (created_at);

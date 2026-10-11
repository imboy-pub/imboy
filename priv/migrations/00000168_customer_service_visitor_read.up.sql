-- Visitor first-read metadata; never copy canonical message content.
-- No BEGIN/COMMIT: migration runner owns the transaction.
SET lock_timeout = '5s';
SET statement_timeout = '15min';
CREATE UNIQUE INDEX IF NOT EXISTS uq_csss_visitor_read_scope
    ON customer_service_session (organization_id, workspace_id, id, contact_id, conversation_id);
CREATE TABLE IF NOT EXISTS customer_service_visitor_message_read (
    organization_id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    session_id bigint NOT NULL,
    contact_id bigint NOT NULL,
    conversation_id bigint NOT NULL,
    message_id bigint NOT NULL CHECK (message_id > 0),
    read_at timestamptz NOT NULL,
    PRIMARY KEY (organization_id, workspace_id, session_id, contact_id, message_id),
    FOREIGN KEY (organization_id, workspace_id, session_id, contact_id, conversation_id)
        REFERENCES customer_service_session (organization_id, workspace_id, id, contact_id, conversation_id)
        ON DELETE CASCADE,
    FOREIGN KEY (organization_id, workspace_id, conversation_id, message_id)
        REFERENCES enterprise_message (organization_id, workspace_id, conversation_id, id)
        ON DELETE CASCADE
);
CREATE INDEX IF NOT EXISTS idx_csvmr_canonical_message
    ON customer_service_visitor_message_read (organization_id, workspace_id, conversation_id, message_id);
CREATE TABLE IF NOT EXISTS customer_service_visitor_read_cursor (
    organization_id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    session_id bigint NOT NULL,
    contact_id bigint NOT NULL,
    conversation_id bigint NOT NULL,
    highest_confirmed_id bigint NOT NULL CHECK (highest_confirmed_id > 0),
    last_advanced_at timestamptz NOT NULL,
    PRIMARY KEY (organization_id, workspace_id, session_id, contact_id),
    FOREIGN KEY (organization_id, workspace_id, session_id, contact_id, conversation_id)
        REFERENCES customer_service_session (organization_id, workspace_id, id, contact_id, conversation_id)
        ON DELETE CASCADE
);
COMMENT ON TABLE customer_service_visitor_message_read IS 'Per-message first visitor read receipt; no inferred lower-ID reads, no body or token';
COMMENT ON TABLE customer_service_visitor_read_cursor IS 'Monotonic auxiliary maximum only; never proof of lower-message visibility';

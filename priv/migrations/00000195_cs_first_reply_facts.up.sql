-- Legacy sessions stay unknown; only new application opens opt into measurement v1.
SET LOCAL lock_timeout = '5s';
ALTER TABLE customer_service_session
    ADD COLUMN response_measurement_version smallint NOT NULL DEFAULT 0,
    ADD COLUMN first_visitor_message_id bigint,
    ADD COLUMN first_visitor_message_at timestamptz,
    ADD COLUMN first_human_reply_id bigint,
    ADD COLUMN first_human_reply_at timestamptz,
    ADD COLUMN first_ai_reply_id bigint,
    ADD COLUMN first_ai_reply_at timestamptz,
    ADD CONSTRAINT ck_cs_response_measurement_version CHECK (response_measurement_version IN (0,1)),
    ADD CONSTRAINT ck_cs_first_visitor_pair CHECK
        ((first_visitor_message_id IS NULL) = (first_visitor_message_at IS NULL)),
    ADD CONSTRAINT ck_cs_first_human_pair CHECK
        ((first_human_reply_id IS NULL) = (first_human_reply_at IS NULL)),
    ADD CONSTRAINT ck_cs_first_ai_pair CHECK
        ((first_ai_reply_id IS NULL) = (first_ai_reply_at IS NULL)),
    ADD CONSTRAINT ck_cs_first_human_elapsed CHECK
        (first_human_reply_at IS NULL OR (first_visitor_message_at IS NOT NULL
            AND first_human_reply_at >= first_visitor_message_at)),
    ADD CONSTRAINT ck_cs_first_ai_elapsed CHECK
        (first_ai_reply_at IS NULL OR (first_visitor_message_at IS NOT NULL
            AND first_ai_reply_at >= first_visitor_message_at));
-- IDs are retained metadata, deliberately without FKs that would prevent message purge.

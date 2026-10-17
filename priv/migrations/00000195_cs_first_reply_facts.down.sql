SET LOCAL lock_timeout = '5s';
ALTER TABLE customer_service_session
    DROP CONSTRAINT ck_cs_first_ai_elapsed,
    DROP CONSTRAINT ck_cs_first_human_elapsed,
    DROP CONSTRAINT ck_cs_first_ai_pair,
    DROP CONSTRAINT ck_cs_first_human_pair,
    DROP CONSTRAINT ck_cs_first_visitor_pair,
    DROP CONSTRAINT ck_cs_response_measurement_version,
    DROP COLUMN first_ai_reply_at,
    DROP COLUMN first_ai_reply_id,
    DROP COLUMN first_human_reply_at,
    DROP COLUMN first_human_reply_id,
    DROP COLUMN first_visitor_message_at,
    DROP COLUMN first_visitor_message_id,
    DROP COLUMN response_measurement_version;

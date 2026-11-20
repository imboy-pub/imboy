ALTER TABLE public.msg_store
    ADD COLUMN reply_to_msg_id VARCHAR(50),
    ADD COLUMN reply_to_from_id BIGINT,
    ADD COLUMN reply_snippet TEXT;

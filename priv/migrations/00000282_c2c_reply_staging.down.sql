ALTER TABLE public.msg_store_staging
    DROP COLUMN reply_snippet,
    DROP COLUMN reply_to_from_id,
    DROP COLUMN reply_to_msg_id;

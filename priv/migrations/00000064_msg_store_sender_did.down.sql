-- 00000064_msg_store_sender_did.down.sql
ALTER TABLE IF EXISTS public.msg_store
    DROP COLUMN IF EXISTS sender_did;

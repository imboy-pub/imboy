DROP TABLE IF EXISTS public."msg_store_seq" CASCADE;
DROP TABLE IF EXISTS public."msg_store" CASCADE;
DROP INDEX IF EXISTS uk_msg_store_msg_id_created_at;
DROP INDEX IF EXISTS i_msg_store_conv_seq;
DROP INDEX IF EXISTS i_msg_store_from_id;

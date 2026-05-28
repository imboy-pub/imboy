DROP TABLE IF EXISTS public."msg_read" CASCADE;
DROP INDEX IF EXISTS uk_msg_read_msg_to_did;
DROP INDEX IF EXISTS idx_msg_read_msg_id;
DROP INDEX IF EXISTS idx_msg_read_to_uid;
DROP INDEX IF EXISTS idx_msg_read_from_uid;
DROP INDEX IF EXISTS idx_msg_read_read_at;

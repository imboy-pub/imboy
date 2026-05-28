DROP TABLE IF EXISTS public."msg_mention" CASCADE;
DROP INDEX IF EXISTS i_msg_c2g_mentions;
DROP INDEX IF EXISTS i_msg_mention_uid_read;
DROP INDEX IF EXISTS i_msg_mention_group_uid;
DROP INDEX IF EXISTS i_msg_mention_msg_id;
ALTER TABLE public."msg_c2g" DROP COLUMN IF EXISTS "mentions";

DROP INDEX IF EXISTS i_c2c_expire_at;
DROP INDEX IF EXISTS i_c2g_expire_at;
ALTER TABLE public."msg_c2c" DROP COLUMN IF EXISTS "expire_at";
ALTER TABLE public."msg_c2g" DROP COLUMN IF EXISTS "expire_at";
ALTER TABLE public."msg_store" DROP COLUMN IF EXISTS "expire_at";

DROP INDEX IF EXISTS idx_msg_c2c_reply;
DROP INDEX IF EXISTS idx_msg_c2g_reply;
ALTER TABLE public."msg_c2c" DROP COLUMN IF EXISTS "reply_to_msg_id";
ALTER TABLE public."msg_c2c" DROP COLUMN IF EXISTS "reply_to_from_id";
ALTER TABLE public."msg_c2c" DROP COLUMN IF EXISTS "reply_snippet";
ALTER TABLE public."msg_c2g" DROP COLUMN IF EXISTS "reply_to_msg_id";
ALTER TABLE public."msg_c2g" DROP COLUMN IF EXISTS "reply_to_from_id";
ALTER TABLE public."msg_c2g" DROP COLUMN IF EXISTS "reply_snippet";

DROP INDEX IF EXISTS idx_msg_c2c_pinned;
DROP INDEX IF EXISTS idx_msg_c2g_pinned;
ALTER TABLE public."msg_c2c" DROP COLUMN IF EXISTS "pinned";
ALTER TABLE public."msg_c2g" DROP COLUMN IF EXISTS "pinned";

DROP INDEX IF EXISTS idx_msg_c2c_payload_fts;
DROP INDEX IF EXISTS idx_msg_c2g_payload_fts;

DROP TABLE IF EXISTS public."msg_c2c" CASCADE;
DROP INDEX IF EXISTS uk_c2c_MsgId_CreatedAt;
DROP INDEX IF EXISTS i_c2c_ToId;
DROP INDEX IF EXISTS i_c2c_FromId;
DROP INDEX IF EXISTS i_c2c_MsgType;
DROP INDEX IF EXISTS i_c2c_e2ee;

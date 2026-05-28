DROP TRIGGER IF EXISTS imboy_for_msg_c2g ON public."msg_c2g";

DROP FUNCTION IF EXISTS public.imboy_msg_c2g_fun;

DROP TABLE IF EXISTS public."msg_c2g" CASCADE;
DROP INDEX IF EXISTS i_c2g_ToId;
DROP INDEX IF EXISTS i_c2g_FromId;
DROP INDEX IF EXISTS i_c2g_MsgType;
DROP INDEX IF EXISTS i_c2g_e2ee;

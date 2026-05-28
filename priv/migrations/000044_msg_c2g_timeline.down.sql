DROP TABLE IF EXISTS public."msg_c2g_timeline" CASCADE;
DROP INDEX IF EXISTS uk_c2g_timeline_MsgId;
DROP INDEX IF EXISTS uk_c2g_timeline_ToUid_MsgId;
DROP INDEX IF EXISTS idx_c2g_timeline_ToUid_ClientAck;

DROP INDEX IF EXISTS uk_cvt_UserId_Type_PeerId;
DROP INDEX IF EXISTS idx_conversation_pin_conversation;
DROP INDEX IF EXISTS idx_conversation_delete_conversation;

DROP TABLE IF EXISTS public."conversation" CASCADE;
DROP INDEX IF EXISTS uk_cvt_UserId_Type_PeerId;

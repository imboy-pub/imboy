-- 工作区摘要含已送达消息 / Workspace previews include acknowledged messages.
-- The migration runner owns the transaction. No message content changes.
SET lock_timeout = '5s';
SET statement_timeout = '15min';
CREATE INDEX IF NOT EXISTS idx_c2g_timeline_workspace_preview
    ON public.msg_c2g_timeline (to_uid, to_gid, conv_seq DESC, created_at DESC)
    WHERE conv_seq IS NOT NULL;

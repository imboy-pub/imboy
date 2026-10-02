-- Read facts are scoped to a membership generation; delivery ACK is not read.
CREATE TABLE public.workspace_group_read_cursor (
    generation_id bigint PRIMARY KEY REFERENCES public.group_member_generation(id) ON DELETE CASCADE,
    read_seq bigint NOT NULL CHECK (read_seq >= 1),
    updated_at timestamptz NOT NULL DEFAULT now()
);
CREATE INDEX idx_c2g_timeline_workspace_unread
    ON public.msg_c2g_timeline (to_uid, to_gid, conv_seq)
    WHERE conv_seq IS NOT NULL;

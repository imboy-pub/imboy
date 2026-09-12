DROP INDEX IF EXISTS public.idx_attachment_group_anchor_pending;
DROP INDEX IF EXISTS public.idx_gmg_group_text_user_open;

ALTER TABLE public.attachment
    DROP CONSTRAINT IF EXISTS ck_attachment_group_anchor_kind,
    DROP CONSTRAINT IF EXISTS ck_attachment_anchor_conv_seq,
    DROP COLUMN IF EXISTS group_file_id,
    DROP COLUMN IF EXISTS anchor_conv_seq,
    DROP COLUMN IF EXISTS anchor_msg_id;

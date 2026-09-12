-- E2EE-2026-012: bind group attachments to the authoritative C2G sequence.
ALTER TABLE public.attachment
    ADD COLUMN anchor_msg_id text,
    ADD COLUMN anchor_conv_seq bigint,
    ADD COLUMN group_file_id bigint;

ALTER TABLE public.attachment
    ADD CONSTRAINT ck_attachment_anchor_conv_seq
    CHECK (anchor_conv_seq IS NULL OR anchor_conv_seq >= 1);

ALTER TABLE public.attachment
    ADD CONSTRAINT ck_attachment_group_anchor_kind
    CHECK (group_file_id IS NULL OR (anchor_msg_id IS NULL AND anchor_conv_seq IS NULL));

-- Group files are shared resources rather than chat history. Preserve their
-- existing current-member ACL with an explicit link instead of pretending they
-- belong to a C2G message.
UPDATE public.attachment a
SET group_file_id = gf.id
FROM public.group_file gf
WHERE a.scope = 'group'
  AND a.scope_ref = gf.group_id::text
  AND a.creator_user_id = gf.uploader_id
  AND a.path = gf.file_id || '/' || regexp_replace(gf.file_name, '^.*/', '');

-- M1 compatibility: legacy chat attachments remain visible only to
-- grandfathered generations whose start_seq is 1. New chat uploads stay NULL
-- until their C2G staging transaction binds the real sequence.
UPDATE public.attachment
SET anchor_conv_seq = 1
WHERE scope = 'group' AND group_file_id IS NULL AND anchor_conv_seq IS NULL;

CREATE INDEX idx_attachment_group_anchor_pending
    ON public.attachment (anchor_msg_id, creator_user_id, scope_ref)
    WHERE scope = 'group' AND group_file_id IS NULL
      AND anchor_conv_seq IS NULL AND status >= 0;

-- Match attachment.scope_ref(text) without casting untrusted legacy text to
-- bigint. This keeps the open-generation lookup indexable and invalid refs
-- fail-closed instead of raising a cast error.
CREATE INDEX idx_gmg_group_text_user_open
    ON public.group_member_generation ((group_id::text), user_id)
    WHERE end_seq IS NULL;

COMMENT ON COLUMN public.attachment.anchor_msg_id
    IS 'Client-generated C2G message id declared at confirm time; immutable after insert';
COMMENT ON COLUMN public.attachment.anchor_conv_seq
    IS 'Authoritative C2G conv_seq bound in the same transaction as message staging';
COMMENT ON COLUMN public.attachment.group_file_id
    IS 'Independent group_file primary key; current active members may access it outside chat-history boundaries';

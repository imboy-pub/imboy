-- A discussion group belongs to at most one channel; unbound channels stay valid.
ALTER TABLE public.channel
    ADD COLUMN discussion_group_id bigint
        REFERENCES public."group"(id) ON DELETE RESTRICT;

CREATE UNIQUE INDEX idx_channel_discussion_group_id
    ON public.channel (discussion_group_id)
    WHERE discussion_group_id IS NOT NULL;

ALTER TABLE public.channel_message
    ADD COLUMN is_essence boolean NOT NULL DEFAULT false;

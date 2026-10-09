-- Rollback is only safe before bindings or essence marks have been written.
-- Refuse to discard business data; an operator must export and resolve it first.
-- Keep locks, checks and DDL in one statement, including under autocommit.
DO $$
BEGIN
    LOCK TABLE public.channel IN ACCESS EXCLUSIVE MODE;
    LOCK TABLE public.channel_message IN ACCESS EXCLUSIVE MODE;

    IF EXISTS (
        SELECT 1 FROM public.channel WHERE discussion_group_id IS NOT NULL
    ) OR EXISTS (
        SELECT 1 FROM public.channel_message WHERE is_essence = true
    ) THEN
        RAISE EXCEPTION 'micro_community rollback refused: discussion bindings or essence marks exist';
    END IF;

    ALTER TABLE public.channel_message DROP COLUMN is_essence;
    DROP INDEX public.idx_channel_discussion_group_id;
    ALTER TABLE public.channel DROP COLUMN discussion_group_id;
END;
$$;

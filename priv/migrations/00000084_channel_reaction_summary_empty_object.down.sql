CREATE OR REPLACE FUNCTION public.fn_update_channel_message_reaction_summary() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    UPDATE public.channel_message cm
    SET reaction_summary = (
        SELECT json_object_agg(reaction_type, cnt)
        FROM (
            SELECT reaction_type, COUNT(*) as cnt
            FROM public.channel_reaction
            WHERE message_id = COALESCE(NEW.message_id, OLD.message_id)
            GROUP BY reaction_type
        ) sub
    )
    WHERE id = COALESCE(NEW.message_id, OLD.message_id);
    RETURN COALESCE(NEW, OLD);
END;
$$;
--;

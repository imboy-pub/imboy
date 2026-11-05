ALTER TABLE public.attachment_avatar_publication ADD COLUMN bound_user_id bigint;

CREATE FUNCTION public.fn_avatar_publication_binding_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path = pg_catalog, public, pg_temp AS $$
BEGIN
    IF OLD.bound_user_id IS DISTINCT FROM NEW.bound_user_id AND
       (OLD.bound_user_id IS NOT NULL OR NEW.bound_user_id IS NULL OR NEW.state <> 'complete') THEN
        RAISE EXCEPTION 'avatar publication binding is immutable' USING ERRCODE = '23514';
    END IF;
    RETURN NEW;
END $$;
CREATE TRIGGER avatar_publication_binding_guard BEFORE UPDATE OF bound_user_id
ON public.attachment_avatar_publication FOR EACH ROW
EXECUTE FUNCTION public.fn_avatar_publication_binding_guard();

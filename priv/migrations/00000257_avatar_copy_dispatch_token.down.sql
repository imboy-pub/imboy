DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM public.attachment_avatar_publication WHERE dispatch_token IS NOT NULL OR state = 'unknown') THEN
        RAISE EXCEPTION 'avatar dispatch tokens must be settled before rollback';
    END IF;
END $$;
CREATE OR REPLACE FUNCTION public.fn_avatar_publication_transition() RETURNS trigger
LANGUAGE plpgsql SET search_path = pg_catalog, public, pg_temp AS $$
BEGIN
    IF TG_OP = 'DELETE' THEN
        IF OLD.state <> 'reserved' THEN
            RAISE EXCEPTION 'avatar publication is not releasable' USING ERRCODE = '23510';
        END IF;
        RETURN OLD;
    END IF;
    IF NEW.attachment_id IS DISTINCT FROM OLD.attachment_id
       OR NEW.object_key IS DISTINCT FROM OLD.object_key
       OR NEW.public_bucket IS DISTINCT FROM OLD.public_bucket
       OR NEW.public_key IS DISTINCT FROM OLD.public_key THEN
        RAISE EXCEPTION 'avatar publication identity is immutable' USING ERRCODE = '23514';
    END IF;
    IF NEW.state <> OLD.state AND NOT (
        (OLD.state = 'reserved' AND NEW.state = 'unknown') OR
        (OLD.state = 'unknown' AND NEW.state = 'complete')
    ) THEN
        RAISE EXCEPTION 'illegal avatar publication transition' USING ERRCODE = '23514';
    END IF;
    RETURN NEW;
END $$;
ALTER TABLE public.attachment_avatar_publication DROP COLUMN dispatch_token;

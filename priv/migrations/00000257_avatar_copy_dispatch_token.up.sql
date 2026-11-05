ALTER TABLE public.attachment_avatar_publication ADD COLUMN dispatch_token uuid;
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
    IF NEW.state = OLD.state THEN
        IF NEW.dispatch_token IS DISTINCT FROM OLD.dispatch_token THEN
            RAISE EXCEPTION 'avatar dispatch token immutable' USING ERRCODE = '23514';
        END IF;
    ELSIF OLD.state = 'reserved' AND NEW.state = 'unknown'
          AND OLD.dispatch_token IS NULL AND NEW.dispatch_token IS NOT NULL THEN
        NULL;
    ELSIF OLD.state = 'unknown' AND NEW.state = 'reserved'
          AND OLD.dispatch_token IS NOT NULL AND NEW.dispatch_token IS NULL THEN
        NULL;
    ELSIF OLD.state = 'unknown' AND NEW.state = 'complete'
          AND OLD.dispatch_token IS NOT NULL
          AND NEW.dispatch_token = OLD.dispatch_token THEN
        NULL;
    ELSE
        RAISE EXCEPTION 'illegal avatar publication transition' USING ERRCODE = '23514';
    END IF;
    RETURN NEW;
END $$;

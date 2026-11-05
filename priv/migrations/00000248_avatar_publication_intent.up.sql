-- A Copy can succeed after its caller loses the response. Keep the source live
-- through business binding; a timeout is never proof of success.
CREATE TABLE public.attachment_avatar_publication (
    attachment_id bigint PRIMARY KEY REFERENCES public.attachment(id) ON DELETE CASCADE,
    object_key text NOT NULL UNIQUE,
    public_bucket text NOT NULL CHECK (public_bucket <> ''),
    public_key text NOT NULL CHECK (public_key <> ''),
    state text NOT NULL DEFAULT 'reserved' CHECK (state IN ('reserved', 'unknown', 'complete')),
    created_at timestamptz NOT NULL DEFAULT now(),
    updated_at timestamptz NOT NULL DEFAULT now(),
    UNIQUE (public_bucket, public_key)
);

CREATE FUNCTION public.fn_avatar_publication_insert_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path = pg_catalog, public, pg_temp AS $$
DECLARE a public.attachment%ROWTYPE;
BEGIN
    SELECT * INTO a FROM public.attachment WHERE id = NEW.attachment_id FOR UPDATE;
    IF NOT FOUND OR a.path <> NEW.object_key OR a.lifecycle_state <> 'live'
       OR a.status IS DISTINCT FROM 1 OR a.storage_driver IS DISTINCT FROM 'garage'
       OR a.storage_container IS NULL OR a.storage_container = NEW.public_bucket
       OR NEW.public_key <> NEW.object_key OR NEW.state <> 'reserved' THEN
        RAISE EXCEPTION 'invalid avatar publication source or target' USING ERRCODE = '23514';
    END IF;
    RETURN NEW;
END $$;
CREATE TRIGGER avatar_publication_insert_guard BEFORE INSERT
ON public.attachment_avatar_publication FOR EACH ROW
EXECUTE FUNCTION public.fn_avatar_publication_insert_guard();

CREATE FUNCTION public.fn_avatar_publication_transition() RETURNS trigger
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
CREATE TRIGGER avatar_publication_transition BEFORE UPDATE OR DELETE
ON public.attachment_avatar_publication FOR EACH ROW
EXECUTE FUNCTION public.fn_avatar_publication_transition();

CREATE FUNCTION public.fn_avatar_publication_attachment_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path = pg_catalog, public, pg_temp AS $$
BEGIN
    IF (TG_OP = 'DELETE' OR (OLD.lifecycle_state = 'live' AND NEW.lifecycle_state <> 'live')
        OR (OLD.status = 1 AND NEW.status IS DISTINCT FROM 1))
       AND EXISTS (SELECT 1 FROM public.attachment_avatar_publication p
                   WHERE p.attachment_id = OLD.id) THEN
        RAISE EXCEPTION 'avatar publication source is held' USING ERRCODE = '23510';
    END IF;
    IF TG_OP = 'DELETE' THEN RETURN OLD; END IF;
    RETURN NEW;
END $$;
CREATE TRIGGER avatar_publication_attachment_guard BEFORE UPDATE OF lifecycle_state, status OR DELETE
ON public.attachment FOR EACH ROW
EXECUTE FUNCTION public.fn_avatar_publication_attachment_guard();

GRANT SELECT ON public.attachment_avatar_publication TO v5_fn_owner;
GRANT SELECT, INSERT, UPDATE, DELETE ON public.attachment_avatar_publication TO v5_app_runtime;

CREATE TABLE public.attachment_avatar_alias (
    attachment_id bigint PRIMARY KEY REFERENCES public.attachment(id) ON DELETE CASCADE,
    object_key text NOT NULL UNIQUE,
    alias_id uuid NOT NULL UNIQUE DEFAULT gen_random_uuid(),
    state text NOT NULL DEFAULT 'pending' CHECK (state IN ('pending','published','revoked')),
    bound_user_id bigint,
    preview_expires_at timestamptz NOT NULL DEFAULT (clock_timestamp() + interval '1 hour'),
    created_at timestamptz NOT NULL DEFAULT now()
);
CREATE FUNCTION public.fn_avatar_alias_hold() RETURNS trigger
LANGUAGE plpgsql SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
    IF (TG_OP='DELETE' OR OLD.lifecycle_state IS DISTINCT FROM NEW.lifecycle_state
        OR OLD.status IS DISTINCT FROM NEW.status)
       AND EXISTS (SELECT 1 FROM public.attachment_avatar_alias WHERE attachment_id=OLD.id
           AND (state='pending' OR (state='published' AND
               (bound_user_id IS NOT NULL OR preview_expires_at > clock_timestamp())))) THEN
        RAISE EXCEPTION 'avatar alias source is held' USING ERRCODE='23510';
    END IF;
    IF TG_OP='DELETE' THEN RETURN OLD; END IF;
    RETURN NEW;
END $$;
CREATE TRIGGER avatar_alias_hold BEFORE UPDATE OF lifecycle_state,status OR DELETE ON public.attachment
FOR EACH ROW EXECUTE FUNCTION public.fn_avatar_alias_hold();
-- The application and content worker share the trusted runtime boundary.
-- This grant does not protect against arbitrary SQL by that runtime role.
GRANT SELECT, INSERT, UPDATE ON public.attachment_avatar_alias TO v5_app_runtime;
GRANT SELECT ON public.attachment_avatar_alias TO v5_fn_owner;
CREATE FUNCTION public.fn_avatar_winner_fence(p_key text)
RETURNS TABLE(id bigint, attempt_no integer, fence text)
LANGUAGE sql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
    SELECT w.id,w.attempt_no,w.dispatch_cas_uuid::text
    FROM public.attach_direct_attempt w
    WHERE w.object_key=p_key AND w.attempt_state='WINNER' FOR UPDATE
$$;
REVOKE ALL ON FUNCTION public.fn_avatar_winner_fence(text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_avatar_winner_fence(text) TO v5_app_runtime;
CREATE FUNCTION public.fn_avatar_alias_bound_visible(p_alias uuid, p_url text)
RETURNS boolean LANGUAGE sql STABLE SECURITY DEFINER
SET search_path=pg_catalog,public,pg_temp AS $$
    SELECT EXISTS (
        SELECT 1 FROM public.attachment_avatar_alias v
        WHERE v.alias_id=p_alias AND v.state='published'
          AND ((v.bound_user_id IS NULL AND v.preview_expires_at > clock_timestamp()) OR EXISTS (
              SELECT 1 FROM public."user" u
              WHERE u.id=v.bound_user_id AND u.avatar=p_url
          ))
    )
$$;
REVOKE ALL ON FUNCTION public.fn_avatar_alias_bound_visible(uuid,text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_avatar_alias_bound_visible(uuid,text) TO v5_app_runtime;
CREATE FUNCTION public.fn_avatar_alias_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE a public.attachment%ROWTYPE;
BEGIN
    IF TG_OP='INSERT' THEN
        SELECT * INTO a FROM public.attachment WHERE id=NEW.attachment_id FOR UPDATE;
        IF NOT FOUND OR a.path IS DISTINCT FROM NEW.object_key OR a.lifecycle_state <> 'live'
           OR a.status IS DISTINCT FROM 1 OR a.storage_driver IS DISTINCT FROM 'garage'
           OR NEW.state <> 'published' OR NEW.bound_user_id IS NOT NULL THEN
            RAISE EXCEPTION 'invalid avatar alias source' USING ERRCODE='23514';
        END IF;
    ELSE
        IF (NEW.attachment_id, NEW.object_key) IS DISTINCT FROM
           (OLD.attachment_id, OLD.object_key)
           OR (NEW.alias_id IS DISTINCT FROM OLD.alias_id AND NOT
              (OLD.state='published' AND OLD.bound_user_id IS NULL
               AND OLD.preview_expires_at <= clock_timestamp() AND NEW.state='published'
               AND NEW.preview_expires_at > clock_timestamp()))
           OR (OLD.state='pending' AND NEW.state <> 'pending')
           OR (OLD.state='published' AND NEW.state NOT IN ('published','revoked'))
           OR (OLD.state='revoked' AND NEW.state <> 'revoked')
           OR (OLD.bound_user_id IS NOT NULL AND NEW.bound_user_id IS DISTINCT FROM OLD.bound_user_id) THEN
            RAISE EXCEPTION 'invalid avatar alias transition' USING ERRCODE='23514';
        END IF;
    END IF;
    RETURN NEW;
END $$;
CREATE TRIGGER avatar_alias_guard BEFORE INSERT OR UPDATE ON public.attachment_avatar_alias
FOR EACH ROW EXECUTE FUNCTION public.fn_avatar_alias_guard();

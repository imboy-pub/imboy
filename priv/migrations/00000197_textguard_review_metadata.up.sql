ALTER TABLE public.review_queue ADD COLUMN IF NOT EXISTS metadata jsonb NOT NULL DEFAULT '{}'::jsonb;
ALTER TABLE public.channel_message ADD COLUMN IF NOT EXISTS moderation_version text NOT NULL DEFAULT '';
ALTER TABLE public."user" ADD COLUMN IF NOT EXISTS moderation_versions jsonb NOT NULL DEFAULT '{}'::jsonb;

CREATE OR REPLACE FUNCTION public.textguard_profile_version() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
    field text;
BEGIN
    FOREACH field IN ARRAY ARRAY['nickname', 'sign', 'profession', 'school', 'interests'] LOOP
        IF (to_jsonb(NEW)->field) IS DISTINCT FROM (to_jsonb(OLD)->field)
            AND (NEW.moderation_versions->>field) IS NOT DISTINCT FROM (OLD.moderation_versions->>field) THEN
            NEW.moderation_versions := jsonb_set(NEW.moderation_versions, ARRAY[field], to_jsonb(replace(gen_random_uuid()::text, '-', '')), true);
        END IF;
    END LOOP;
    RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS textguard_profile_version ON public."user";
CREATE TRIGGER textguard_profile_version BEFORE UPDATE OF nickname, sign, profession, school, interests
    ON public."user" FOR EACH ROW EXECUTE FUNCTION public.textguard_profile_version();

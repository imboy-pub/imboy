ALTER TABLE public.review_queue DROP COLUMN IF EXISTS metadata;
ALTER TABLE public."user" DROP COLUMN IF EXISTS moderation_versions;
ALTER TABLE public.channel_message DROP COLUMN IF EXISTS moderation_version;
DROP TRIGGER IF EXISTS textguard_profile_version ON public."user";
DROP FUNCTION IF EXISTS public.textguard_profile_version();

DROP INDEX IF EXISTS public.uq_channel_message_request_id;
ALTER TABLE public.channel_message DROP COLUMN IF EXISTS request_id;

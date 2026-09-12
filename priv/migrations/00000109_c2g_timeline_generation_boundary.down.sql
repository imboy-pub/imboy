DROP INDEX IF EXISTS public.idx_c2g_timeline_generation_pending;

ALTER TABLE public.msg_c2g_timeline
    DROP CONSTRAINT IF EXISTS chk_msg_c2g_timeline_conv_seq_positive;

ALTER TABLE public.msg_c2g_timeline
    DROP COLUMN IF EXISTS conv_seq;

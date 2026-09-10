-- Persist the complete immutable webhook destination alongside the existing DNS pin.
ALTER TABLE public.bot_delivery
    ADD COLUMN IF NOT EXISTS webhook_url text NOT NULL DEFAULT '';

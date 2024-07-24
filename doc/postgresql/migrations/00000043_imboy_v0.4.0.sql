
ALTER TABLE public.user_device ADD COLUMN IF NOT EXISTS public_key VARCHAR(1024) DEFAULT '';

COMMENT ON COLUMN public.user_device.public_key IS '用户设备公钥';

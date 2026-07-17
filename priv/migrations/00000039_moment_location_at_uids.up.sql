-- 迁移 000039: moment_post 加 location + at_uids（朋友圈"所在位置 + @提醒谁看"数据层 E1）
--   location jsonb：所在位置 {name,lng,lat,address?}，无位置为 NULL。
--   at_uids jsonb：被 @ 提醒可见者的 uid 数组（默认 []），发帖成功后按可见性 ACL 通知。
--   参照 msg_c2g.mentions / moment_post.media 的 jsonb 加列风格。
--   Adds moment_post.location (nullable) and at_uids (default []) for the
--   "location + @mention" data layer.

ALTER TABLE public.moment_post ADD COLUMN IF NOT EXISTS location jsonb DEFAULT NULL;
ALTER TABLE public.moment_post ADD COLUMN IF NOT EXISTS at_uids jsonb DEFAULT '[]'::jsonb NOT NULL;
COMMENT ON COLUMN public.moment_post.location IS '所在位置 {name,lng,lat,address?}，无位置为 NULL / Location';
COMMENT ON COLUMN public.moment_post.at_uids IS '@提醒可见者 uid 数组（默认 []）/ Mentioned uids';

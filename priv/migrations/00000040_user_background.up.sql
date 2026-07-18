-- 迁移 000040: user 表加 background（个人资料页顶部封面背景图）
--   background text：存 Garage S3 object_key（scope=public，与 avatar 同机制），
--   无背景图为 NULL。前端 profile 页顶部 cover 与他人资料页 cover 渲染此字段。
--   类型对齐 user.avatar（text）。
--   Adds user.background (nullable) for the profile cover image.

ALTER TABLE public."user" ADD COLUMN IF NOT EXISTS background text DEFAULT NULL;
COMMENT ON COLUMN public."user".background IS '个人资料封面背景图 object_key（scope=public），无为 NULL / Profile cover';

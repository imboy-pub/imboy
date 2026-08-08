-- 迁移 000059: user 表加 profession/school/interests（个人资料扩展字段）
--   profession text：职业
--   school text：学校
--   interests text：兴趣爱好
--   三者均为自由文本，无格式校验原样落库（与 sign 同机制），默认空串。
--   Adds user.profession / user.school / user.interests for extended profile fields.

ALTER TABLE public."user" ADD COLUMN IF NOT EXISTS profession text NOT NULL DEFAULT '';
ALTER TABLE public."user" ADD COLUMN IF NOT EXISTS school text NOT NULL DEFAULT '';
ALTER TABLE public."user" ADD COLUMN IF NOT EXISTS interests text NOT NULL DEFAULT '';

COMMENT ON COLUMN public."user".profession IS '职业 / Profession';
COMMENT ON COLUMN public."user".school IS '学校 / School';
COMMENT ON COLUMN public."user".interests IS '兴趣爱好 / Interests';

-- 回滚 000059: 移除 user.profession / user.school / user.interests
-- Rollback: drop user.profession / user.school / user.interests

ALTER TABLE public."user" DROP COLUMN IF EXISTS profession;
ALTER TABLE public."user" DROP COLUMN IF EXISTS school;
ALTER TABLE public."user" DROP COLUMN IF EXISTS interests;

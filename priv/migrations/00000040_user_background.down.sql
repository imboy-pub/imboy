-- 回滚 000040: 移除 user.background
-- Rollback: drop user.background

ALTER TABLE public."user" DROP COLUMN IF EXISTS background;

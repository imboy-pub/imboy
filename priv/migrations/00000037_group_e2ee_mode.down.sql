-- 回滚群级 E2EE 开关列 / Rollback group e2ee_mode column
ALTER TABLE public."group" DROP COLUMN IF EXISTS e2ee_mode;

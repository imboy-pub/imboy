DROP TABLE IF EXISTS public."user_group_category" CASCADE;
DROP INDEX IF EXISTS idx_user_group_category_user;
DROP INDEX IF EXISTS idx_user_group_category_user_name;
ALTER TABLE public."group_member" DROP COLUMN IF EXISTS "category_id";

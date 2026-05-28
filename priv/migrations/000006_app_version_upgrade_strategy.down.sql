DROP TABLE IF EXISTS public."app_version_policy" CASCADE;
ALTER TABLE public."app_version" DROP COLUMN IF EXISTS "min_supported_vsn";
ALTER TABLE public."app_version" DROP COLUMN IF EXISTS "grayscale_percent";
ALTER TABLE public."app_version" DROP COLUMN IF EXISTS "upgrade_type";
ALTER TABLE public."app_version" DROP COLUMN IF EXISTS "changelog";
ALTER TABLE public."app_version" DROP COLUMN IF EXISTS "file_size";
ALTER TABLE public."app_version" DROP COLUMN IF EXISTS "file_hash";

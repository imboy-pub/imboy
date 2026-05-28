DROP INDEX IF EXISTS i_group_notice_pinned;
DROP INDEX IF EXISTS i_group_notice_deleted_at;
ALTER TABLE public."group_notice" DROP COLUMN IF EXISTS "pinned";
ALTER TABLE public."group_notice" DROP COLUMN IF EXISTS "deleted_at";
ALTER TABLE public."group_notice" DROP COLUMN IF EXISTS "read_count";
ALTER TABLE public."group_notice" DROP COLUMN IF EXISTS "title";

DROP TABLE IF EXISTS public."group_notice" CASCADE;
DROP INDEX IF EXISTS i_Gid_Status_ExpiredAt;

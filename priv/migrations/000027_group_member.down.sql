DROP INDEX IF EXISTS idx_group_member_mute;
ALTER TABLE public."group_member" DROP COLUMN IF EXISTS "mute_until";

DROP INDEX IF EXISTS idx_group_member_role;

ALTER TABLE public."group_member" DROP COLUMN IF EXISTS "remark";

DROP TABLE IF EXISTS public."group_member" CASCADE;
DROP INDEX IF EXISTS uk_Gid_Uid;
DROP INDEX IF EXISTS i_Uid_Gid_IsJoin;
DROP INDEX IF EXISTS i_Uid_Status;

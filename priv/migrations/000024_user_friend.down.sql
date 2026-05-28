DROP INDEX IF EXISTS i_user_friend_from_user_id;
DROP INDEX IF EXISTS i_user_friend_category_id;
DROP TABLE IF EXISTS public."user_friend" CASCADE;
DROP INDEX IF EXISTS uk_FromUID_ToUID;
DROP INDEX IF EXISTS i_Status_FromUid_Cid;
ALTER TABLE public."user_friend" DROP COLUMN IF EXISTS "last_seen_at";

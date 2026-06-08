-- ============================================================
-- 合并迁移回滚 000001: foundation
-- ============================================================


DROP TABLE IF EXISTS public."group_vote_record" CASCADE;
DROP TABLE IF EXISTS public."group_vote_option" CASCADE;
DROP TABLE IF EXISTS public."group_vote" CASCADE;
DROP TABLE IF EXISTS public."group_task_assignment" CASCADE;
DROP TABLE IF EXISTS public."group_task" CASCADE;
DROP TABLE IF EXISTS public."group_album_photo_like" CASCADE;
DROP TABLE IF EXISTS public."group_album_photo_comment" CASCADE;
DROP TABLE IF EXISTS public."group_album_photo" CASCADE;
DROP TABLE IF EXISTS public."group_album" CASCADE;
DROP TABLE IF EXISTS public."group_file" CASCADE;
DROP TABLE IF EXISTS public."group_schedule_remind" CASCADE;
DROP TABLE IF EXISTS public."group_schedule_participant" CASCADE;
DROP TABLE IF EXISTS public."group_schedule" CASCADE;
DROP TABLE IF EXISTS public."group_tag" CASCADE;
DROP TABLE IF EXISTS public."user_group_category" CASCADE;
DROP TABLE IF EXISTS public."user_group" CASCADE;
DROP TABLE IF EXISTS public."group_notice" CASCADE;
DROP TABLE IF EXISTS public."group_random_code" CASCADE;
DROP TABLE IF EXISTS public."group_log" CASCADE;
DROP TABLE IF EXISTS public."group_member" CASCADE;
DROP TABLE IF EXISTS public."group" CASCADE;
DROP TABLE IF EXISTS public."conversation_delete" CASCADE;
DROP TABLE IF EXISTS public."conversation_pin" CASCADE;
DROP TABLE IF EXISTS public."conversation" CASCADE;
DROP TABLE IF EXISTS public."user_denylist" CASCADE;
DROP TABLE IF EXISTS public."user_friend_category" CASCADE;
DROP TABLE IF EXISTS public."user_friend" CASCADE;
DROP TABLE IF EXISTS public."adm_role" CASCADE;
DROP TABLE IF EXISTS public."adm_user" CASCADE;
DROP TABLE IF EXISTS public."verification_code" CASCADE;
DROP TABLE IF EXISTS public."user_dnd_rule" CASCADE;
DROP TABLE IF EXISTS public."geo_people_nearby" CASCADE;
DROP TABLE IF EXISTS public."fts_user" CASCADE;
DROP TABLE IF EXISTS public."user_log" CASCADE;
DROP TABLE IF EXISTS public."user_tag_relation" CASCADE;
DROP TABLE IF EXISTS public."user_tag" CASCADE;
DROP TABLE IF EXISTS public."user_collect" CASCADE;
DROP TABLE IF EXISTS public."user_device" CASCADE;
DROP TABLE IF EXISTS public."user_setting" CASCADE;
DROP TABLE IF EXISTS public."user" CASCADE;
DROP TABLE IF EXISTS public."push_token" CASCADE;
DROP TABLE IF EXISTS public."attachment" CASCADE;
DROP TABLE IF EXISTS public."announcement" CASCADE;
DROP TABLE IF EXISTS public."app_upgrade_log" CASCADE;
DROP TABLE IF EXISTS public."app_version_policy" CASCADE;
DROP TABLE IF EXISTS public."app_version" CASCADE;
DROP TABLE IF EXISTS public."app_ddl" CASCADE;
DROP TABLE IF EXISTS public."system_id_segment_stats" CASCADE;
DROP TABLE IF EXISTS public."system_id_segment" CASCADE;
DROP TABLE IF EXISTS public."system_datacenter_log" CASCADE;
DROP TABLE IF EXISTS public."system_datacenter" CASCADE;
DROP TABLE IF EXISTS public."config" CASCADE;

-- 删除本项目自建函数(排除扩展自带)
DO $$
DECLARE r record;
BEGIN
  FOR r IN
    SELECT p.oid::regprocedure AS sig
    FROM pg_proc p JOIN pg_namespace n ON n.oid = p.pronamespace
    WHERE n.nspname = 'public' AND p.prokind = 'f'
      AND NOT EXISTS (SELECT 1 FROM pg_depend d WHERE d.objid = p.oid AND d.deptype = 'e')
  LOOP
    EXECUTE 'DROP FUNCTION IF EXISTS ' || r.sig || ' CASCADE';
  END LOOP;
END $$;


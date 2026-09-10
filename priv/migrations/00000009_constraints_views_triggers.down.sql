-- ============================================================
-- 合并迁移回滚 00000009: constraints_views_triggers（原 000099_constraints_views_triggers）
-- ============================================================


DROP VIEW IF EXISTS public.v_channel_invitation_stats CASCADE;
DROP VIEW IF EXISTS public.v_channel_order_stats CASCADE;
DROP VIEW IF EXISTS public.v_channel_realtime_stats CASCADE;
DROP VIEW IF EXISTS public.v_datacenters CASCADE;
DROP VIEW IF EXISTS public.v_e2ee_shard_transmission_stats CASCADE;
DROP VIEW IF EXISTS public.v_group_admins CASCADE;
DROP VIEW IF EXISTS public.v_group_senior_admins CASCADE;
DROP VIEW IF EXISTS public.v_id_segment_monitor CASCADE;
DROP VIEW IF EXISTS public.v_user_channel_reading_stats CASCADE;

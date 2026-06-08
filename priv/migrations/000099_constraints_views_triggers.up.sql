-- ============================================================
-- 合并迁移 000099: constraints_views_triggers
-- 由 70 个历史迁移基线压缩而成 (fresh-install 等价)。
-- 本文件由 erlang_migrate 整体包裹在单事务中执行。
-- ============================================================


--

COMMENT ON SCHEMA public IS 'standard public schema';


--;

--

CREATE VIEW public.v_channel_invitation_stats AS
 SELECT channel_id,
    count(*) FILTER (WHERE (status = 0)) AS pending_count,
    count(*) FILTER (WHERE (status = 1)) AS accepted_count,
    count(*) FILTER (WHERE (status = 2)) AS rejected_count,
    count(*) FILTER (WHERE (status = 3)) AS expired_count,
    count(*) AS total_count,
    max(created_at) AS last_invitation_at
   FROM public.channel_invitation
  GROUP BY channel_id;


--;

--

COMMENT ON VIEW public.v_channel_invitation_stats IS '频道邀请统计视图';


--;

--

CREATE VIEW public.v_channel_order_stats AS
 SELECT channel_id,
    count(*) FILTER (WHERE (status = 0)) AS pending_count,
    count(*) FILTER (WHERE (status = 1)) AS paid_count,
    count(*) FILTER (WHERE (status = 2)) AS refunded_count,
    COALESCE(sum(amount) FILTER (WHERE (status = 1)), (0)::numeric) AS total_revenue,
    COALESCE(sum(amount) FILTER (WHERE ((status = 1) AND (created_at > (now() - '30 days'::interval)))), (0)::numeric) AS revenue_30d,
    max(created_at) AS last_order_at
   FROM public.channel_order
  GROUP BY channel_id;


--;

--

COMMENT ON VIEW public.v_channel_order_stats IS '频道订单统计视图';


--;

--

CREATE VIEW public.v_channel_realtime_stats AS
 SELECT c.id AS channel_id,
    c.name AS channel_name,
    c.subscriber_count,
    count(DISTINCT cm.id) AS total_messages,
    COALESCE(sum(cm.view_count), (0)::numeric) AS total_views,
    COALESCE(( SELECT count(*) AS count
           FROM public.channel_reaction cr
          WHERE (cr.channel_id = c.id)), (0)::bigint) AS total_reactions,
    max(cm.created_at) AS last_message_at
   FROM (public.channel c
     LEFT JOIN public.channel_message cm ON (((c.id = cm.channel_id) AND (cm.status = 1))))
  WHERE (c.status = 1)
  GROUP BY c.id, c.name, c.subscriber_count;


--;

--

COMMENT ON VIEW public.v_channel_realtime_stats IS '频道实时统计视图';


--;

--

CREATE VIEW public.v_datacenters AS
 SELECT id,
    name,
    region,
    api_endpoint,
    is_active,
    created_at,
    updated_at,
    active_tables_count
   FROM public.get_datacenters(true) get_datacenters(id, name, region, api_endpoint, is_active, created_at, updated_at, active_tables_count);


--;

--

COMMENT ON VIEW public.v_datacenters IS '活跃机房列表视图';


--;

--

CREATE VIEW public.v_e2ee_shard_transmission_stats AS
 SELECT key_version,
    shard_id,
    uid,
    count(*) FILTER (WHERE ((action)::text = 'shard_created'::text)) AS created_count,
    count(*) FILTER (WHERE ((action)::text = 'shard_sent'::text)) AS sent_count,
    count(*) FILTER (WHERE ((action)::text = 'shard_stored'::text)) AS stored_count,
    count(*) FILTER (WHERE ((action)::text = 'shard_decrypted'::text)) AS decrypted_count,
    count(*) FILTER (WHERE ((action)::text = 'shard_recovered'::text)) AS recovered_count,
    min(created_at) AS first_transmission_at,
    max(created_at) AS last_transmission_at
   FROM public.e2ee_shard_transmission_log
  GROUP BY key_version, shard_id, uid;


--;

--

COMMENT ON VIEW public.v_e2ee_shard_transmission_stats IS 'E2EE 分片传输统计视图';


--;

--

CREATE VIEW public.v_group_admins AS
 SELECT gm.group_id,
    gm.user_id,
    gm.role,
    u.nickname,
    u.avatar,
    gm.created_at
   FROM (public.group_member gm
     LEFT JOIN public."user" u ON ((u.id = gm.user_id)))
  WHERE ((gm.role = ANY (ARRAY[3, 4, 5])) AND (gm.status = 1))
  ORDER BY gm.group_id, gm.role DESC, gm.created_at;


--;

--

CREATE VIEW public.v_group_senior_admins AS
 SELECT gm.group_id,
    gm.user_id,
    gm.role,
    u.nickname,
    u.avatar,
    gm.created_at
   FROM (public.group_member gm
     LEFT JOIN public."user" u ON ((u.id = gm.user_id)))
  WHERE ((gm.role = ANY (ARRAY[4, 5])) AND (gm.status = 1))
  ORDER BY gm.group_id, gm.role DESC, gm.created_at;


--;

--

CREATE VIEW public.v_id_segment_monitor AS
 SELECT datacenter_id,
    datacenter_name,
    datacenter_region,
    table_name,
    segment_start,
    segment_end,
    total_size,
    used_count,
    usage_percent,
    is_active,
    allocated_at,
    expired_at,
    remaining_days,
    status
   FROM public.get_datacenter_segment_status() get_datacenter_segment_status(datacenter_id, datacenter_name, datacenter_region, table_name, segment_start, segment_end, total_size, used_count, usage_percent, is_active, allocated_at, expired_at, remaining_days, status);


--;

--

COMMENT ON VIEW public.v_id_segment_monitor IS 'ID段监控视图';


--;

--

CREATE VIEW public.v_user_channel_reading_stats AS
 SELECT cs.user_id,
    cs.channel_id,
    c.name AS channel_name,
    cs.unread_count,
    cs.last_read_at,
    count(DISTINCT cmv.message_id) AS viewed_messages,
    max(cmv.viewed_at) AS last_view_at
   FROM ((public.channel_subscription cs
     JOIN public.channel c ON ((cs.channel_id = c.id)))
     LEFT JOIN public.channel_message_view cmv ON (((cs.channel_id = cmv.channel_id) AND (cs.user_id = cmv.user_id))))
  WHERE (cs.status = 1)
  GROUP BY cs.user_id, cs.channel_id, c.name, cs.unread_count, cs.last_read_at;


--;

--

COMMENT ON VIEW public.v_user_channel_reading_stats IS '用户频道阅读统计视图';


--;

--

ALTER TABLE ONLY public.channel_admin
    ADD CONSTRAINT fk_channel_admin_channel FOREIGN KEY (channel_id) REFERENCES public.channel(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.channel_admin
    ADD CONSTRAINT fk_channel_admin_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.channel_invitation
    ADD CONSTRAINT fk_channel_invitation_channel FOREIGN KEY (channel_id) REFERENCES public.channel(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.channel_message
    ADD CONSTRAINT fk_channel_message_channel FOREIGN KEY (channel_id) REFERENCES public.channel(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.channel_message_view
    ADD CONSTRAINT fk_channel_message_view_channel FOREIGN KEY (channel_id) REFERENCES public.channel(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.channel_message_view
    ADD CONSTRAINT fk_channel_message_view_message FOREIGN KEY (message_id) REFERENCES public.channel_message(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.channel_message_view
    ADD CONSTRAINT fk_channel_message_view_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.channel_order
    ADD CONSTRAINT fk_channel_order_channel FOREIGN KEY (channel_id) REFERENCES public.channel(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.channel_price
    ADD CONSTRAINT fk_channel_price_channel FOREIGN KEY (channel_id) REFERENCES public.channel(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.channel_reaction
    ADD CONSTRAINT fk_channel_reaction_channel FOREIGN KEY (channel_id) REFERENCES public.channel(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.channel_reaction
    ADD CONSTRAINT fk_channel_reaction_message FOREIGN KEY (message_id) REFERENCES public.channel_message(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.channel_reaction
    ADD CONSTRAINT fk_channel_reaction_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.channel_stats_daily
    ADD CONSTRAINT fk_channel_stats_daily_channel FOREIGN KEY (channel_id) REFERENCES public.channel(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.channel_subscription
    ADD CONSTRAINT fk_channel_subscription_channel FOREIGN KEY (channel_id) REFERENCES public.channel(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.channel_subscription
    ADD CONSTRAINT fk_channel_subscription_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.feedback_reply
    ADD CONSTRAINT fk_feedback_reply_feedback FOREIGN KEY (feedback_id) REFERENCES public.feedback(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.moment_comment
    ADD CONSTRAINT fk_moment_comment_post FOREIGN KEY (post_id) REFERENCES public.moment_post(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.moment_comment
    ADD CONSTRAINT fk_moment_comment_reply_to_uid FOREIGN KEY (reply_to_uid) REFERENCES public."user"(id) ON DELETE SET NULL;


--;

--

ALTER TABLE ONLY public.moment_comment
    ADD CONSTRAINT fk_moment_comment_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.moment_like
    ADD CONSTRAINT fk_moment_like_post FOREIGN KEY (post_id) REFERENCES public.moment_post(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.moment_like
    ADD CONSTRAINT fk_moment_like_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.moment_post_acl
    ADD CONSTRAINT fk_moment_post_acl_post FOREIGN KEY (post_id) REFERENCES public.moment_post(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.moment_post_acl
    ADD CONSTRAINT fk_moment_post_acl_uid FOREIGN KEY (uid) REFERENCES public."user"(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.moment_post
    ADD CONSTRAINT fk_moment_post_author FOREIGN KEY (author_uid) REFERENCES public."user"(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.moment_report
    ADD CONSTRAINT fk_moment_report_handler FOREIGN KEY (handled_by) REFERENCES public."user"(id) ON DELETE SET NULL;


--;

--

ALTER TABLE ONLY public.moment_report
    ADD CONSTRAINT fk_moment_report_post FOREIGN KEY (post_id) REFERENCES public.moment_post(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.moment_report
    ADD CONSTRAINT fk_moment_report_reporter FOREIGN KEY (reporter_uid) REFERENCES public."user"(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.moment_timeline
    ADD CONSTRAINT fk_moment_timeline_author FOREIGN KEY (author_uid) REFERENCES public."user"(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.moment_timeline
    ADD CONSTRAINT fk_moment_timeline_post FOREIGN KEY (post_id) REFERENCES public.moment_post(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.moment_timeline
    ADD CONSTRAINT fk_moment_timeline_recipient FOREIGN KEY (recipient_uid) REFERENCES public."user"(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.report_action_log
    ADD CONSTRAINT fk_report_action_report_id FOREIGN KEY (report_id) REFERENCES public.report_ticket(id) ON DELETE CASCADE;


--;

--

ALTER TABLE ONLY public.wallet_transaction
    ADD CONSTRAINT fk_wallet_tx_wallet FOREIGN KEY (wallet_id) REFERENCES public.wallet(id);


--;

--

ALTER TABLE ONLY public.system_id_segment
    ADD CONSTRAINT system_id_segment_datacenter_id_fkey FOREIGN KEY (datacenter_id) REFERENCES public.system_datacenter(id);


--
-- PostgreSQL database dump complete
--;

--

CREATE TRIGGER trg_config_updated_at BEFORE UPDATE ON public.config FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_app_ddl_updated_at BEFORE UPDATE ON public.app_ddl FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_app_version_updated_at BEFORE UPDATE ON public.app_version FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_announcement_updated_at BEFORE UPDATE ON public.announcement FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_attachment_updated_at BEFORE UPDATE ON public.attachment FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_push_token_updated_at BEFORE UPDATE ON public.push_token FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_user_fts AFTER INSERT OR DELETE OR UPDATE OF nickname, account, mobile, sign, region ON public."user" FOR EACH ROW EXECUTE FUNCTION public.sync_fts_user();


--;

--

CREATE TRIGGER imboy_for_user_collect AFTER INSERT OR DELETE OR UPDATE OF attach_md5 ON public.user_collect FOR EACH ROW EXECUTE FUNCTION public.imboy_user_collect_fun();


--;

--

CREATE TRIGGER trg_user_collect_updated_at BEFORE UPDATE ON public.user_collect FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_user_tag_updated_at BEFORE UPDATE ON public.user_tag FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER imboy_for_user_tag_relation AFTER INSERT OR DELETE OR UPDATE OF scene, user_id, tag_id ON public.user_tag_relation FOR EACH ROW EXECUTE FUNCTION public.imboy_user_tag_relation_fun();


--;

--

CREATE TRIGGER trg_geo_people_nearby_updated_at BEFORE UPDATE ON public.geo_people_nearby FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_adm_user_updated_at BEFORE UPDATE ON public.adm_user FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_adm_role_updated_at BEFORE UPDATE ON public.adm_role FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_user_friend_updated_at BEFORE UPDATE ON public.user_friend FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_user_friend_category_updated_at BEFORE UPDATE ON public.user_friend_category FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_user_denylist_updated_at BEFORE UPDATE ON public.user_denylist FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_group_updated_at BEFORE UPDATE ON public."group" FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_group_member_updated_at BEFORE UPDATE ON public.group_member FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_group_log_updated_at BEFORE UPDATE ON public.group_log FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_group_random_code_updated_at BEFORE UPDATE ON public.group_random_code FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_group_notice_updated_at BEFORE UPDATE ON public.group_notice FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_user_group_updated_at BEFORE UPDATE ON public.user_group FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_user_group_category_updated_at BEFORE UPDATE ON public.user_group_category FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_group_tag_updated_at BEFORE UPDATE ON public.group_tag FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_group_schedule_updated_at BEFORE UPDATE ON public.group_schedule FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_group_file_updated_at BEFORE UPDATE ON public.group_file FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_group_album_updated_at BEFORE UPDATE ON public.group_album FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_group_task_updated_at BEFORE UPDATE ON public.group_task FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_group_vote_updated_at BEFORE UPDATE ON public.group_vote FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_channel_updated_at BEFORE UPDATE ON public.channel FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER tr_channel_invitation_accept BEFORE UPDATE ON public.channel_invitation FOR EACH ROW EXECUTE FUNCTION public.fn_channel_invitation_accept();


--;

--

CREATE TRIGGER tr_channel_invitation_updated_at BEFORE UPDATE ON public.channel_invitation FOR EACH ROW EXECUTE FUNCTION public.fn_channel_subscribe_updated_at();


--;

--

CREATE TRIGGER tr_update_channel_message_view_count AFTER INSERT ON public.channel_message_view FOR EACH ROW EXECUTE FUNCTION public.fn_update_channel_message_view_count();


--;

--

CREATE TRIGGER tr_channel_order_updated_at BEFORE UPDATE ON public.channel_order FOR EACH ROW EXECUTE FUNCTION public.fn_channel_subscribe_updated_at();


--;

--

CREATE TRIGGER tr_channel_price_updated_at BEFORE UPDATE ON public.channel_price FOR EACH ROW EXECUTE FUNCTION public.fn_channel_subscribe_updated_at();


--;

--

CREATE TRIGGER tr_update_channel_message_reaction_summary AFTER INSERT OR DELETE ON public.channel_reaction FOR EACH ROW EXECUTE FUNCTION public.fn_update_channel_message_reaction_summary();


--;

--

CREATE TRIGGER tr_moment_post_updated_at BEFORE UPDATE ON public.moment_post FOR EACH ROW EXECUTE FUNCTION public.fn_moment_touch_updated_at();


--;

--

CREATE TRIGGER tr_moment_comment_updated_at BEFORE UPDATE ON public.moment_comment FOR EACH ROW EXECUTE FUNCTION public.fn_moment_touch_updated_at();


--;

--

CREATE TRIGGER tr_moment_report_updated_at BEFORE UPDATE ON public.moment_report FOR EACH ROW EXECUTE FUNCTION public.fn_moment_touch_updated_at();


--;

--

CREATE TRIGGER trg_live_room_updated_at BEFORE UPDATE ON public.live_room FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_e2ee_key_shares_updated_at BEFORE UPDATE ON public.e2ee_key_shares FOR EACH ROW EXECUTE FUNCTION public.update_e2ee_updated_at();


--;

--

CREATE TRIGGER trg_e2ee_transfer_sessions_updated_at BEFORE UPDATE ON public.e2ee_transfer_sessions FOR EACH ROW EXECUTE FUNCTION public.update_e2ee_updated_at();


--;

--

CREATE TRIGGER trg_e2ee_trusted_contacts_updated_at BEFORE UPDATE ON public.e2ee_trusted_contacts FOR EACH ROW EXECUTE FUNCTION public.update_e2ee_updated_at();


--;

--

CREATE TRIGGER trg_compliance_key_updated_at BEFORE UPDATE ON public.compliance_key FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_feedback_updated_at BEFORE UPDATE ON public.feedback FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_feedback_reply_updated_at BEFORE UPDATE ON public.feedback_reply FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER tr_report_ticket_updated_at BEFORE UPDATE ON public.report_ticket FOR EACH ROW EXECUTE FUNCTION public.fn_report_ticket_touch_updated_at();


--;

--

CREATE TRIGGER trg_report_ticket_updated_at BEFORE UPDATE ON public.report_ticket FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;

--

CREATE TRIGGER trg_wallet_updated_at BEFORE UPDATE ON public.wallet FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--;


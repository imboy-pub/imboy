-- =============================================================================
-- Migration DOWN: BIGINT -> BIGSERIAL (撤销 TSID 迁移，恢复自增序列)
--
-- 为每张表重建 BIGSERIAL 序列并设置 DEFAULT，
-- 序列起始值 = 当前表中最大 id（保证不冲突）。
-- =============================================================================

BEGIN;

-- ── 先删除依赖被修改列的视图 ──
DROP VIEW IF EXISTS v_group_admins;
DROP VIEW IF EXISTS v_group_senior_admins;
DROP VIEW IF EXISTS v_channel_realtime_stats;
DROP VIEW IF EXISTS v_user_channel_reading_stats;

-- ── user ──
CREATE SEQUENCE IF NOT EXISTS public.user_id_seq AS bigint;
SELECT setval('public.user_id_seq', COALESCE((SELECT MAX(id) FROM public."user"), 0) + 1, false);
ALTER TABLE IF EXISTS public."user" ALTER COLUMN id SET DEFAULT nextval('public.user_id_seq'::regclass);
ALTER SEQUENCE public.user_id_seq OWNED BY public."user".id;

-- ── user_device ──
CREATE SEQUENCE IF NOT EXISTS public.user_device_id_seq AS bigint;
SELECT setval('public.user_device_id_seq', COALESCE((SELECT MAX(id) FROM public.user_device), 0) + 1, false);
ALTER TABLE IF EXISTS public.user_device ALTER COLUMN id SET DEFAULT nextval('public.user_device_id_seq'::regclass);
ALTER SEQUENCE public.user_device_id_seq OWNED BY public.user_device.id;

-- ── user_collect ──
CREATE SEQUENCE IF NOT EXISTS public.user_collect_id_seq AS bigint;
SELECT setval('public.user_collect_id_seq', COALESCE((SELECT MAX(id) FROM public.user_collect), 0) + 1, false);
ALTER TABLE IF EXISTS public.user_collect ALTER COLUMN id SET DEFAULT nextval('public.user_collect_id_seq'::regclass);
ALTER SEQUENCE public.user_collect_id_seq OWNED BY public.user_collect.id;

-- ── user_denylist ──
CREATE SEQUENCE IF NOT EXISTS public.user_denylist_id_seq AS bigint;
SELECT setval('public.user_denylist_id_seq', COALESCE((SELECT MAX(id) FROM public.user_denylist), 0) + 1, false);
ALTER TABLE IF EXISTS public.user_denylist ALTER COLUMN id SET DEFAULT nextval('public.user_denylist_id_seq'::regclass);
ALTER SEQUENCE public.user_denylist_id_seq OWNED BY public.user_denylist.id;

-- ── user_tag ──
CREATE SEQUENCE IF NOT EXISTS public.user_tag_id_seq AS bigint;
SELECT setval('public.user_tag_id_seq', COALESCE((SELECT MAX(id) FROM public.user_tag), 0) + 1, false);
ALTER TABLE IF EXISTS public.user_tag ALTER COLUMN id SET DEFAULT nextval('public.user_tag_id_seq'::regclass);
ALTER SEQUENCE public.user_tag_id_seq OWNED BY public.user_tag.id;

-- ── user_tag_relation ──
CREATE SEQUENCE IF NOT EXISTS public.user_tag_relation_id_seq AS bigint;
SELECT setval('public.user_tag_relation_id_seq', COALESCE((SELECT MAX(id) FROM public.user_tag_relation), 0) + 1, false);
ALTER TABLE IF EXISTS public.user_tag_relation ALTER COLUMN id SET DEFAULT nextval('public.user_tag_relation_id_seq'::regclass);
ALTER SEQUENCE public.user_tag_relation_id_seq OWNED BY public.user_tag_relation.id;

-- ── user_friend ──
CREATE SEQUENCE IF NOT EXISTS public.user_friend_id_seq AS bigint;
SELECT setval('public.user_friend_id_seq', COALESCE((SELECT MAX(id) FROM public.user_friend), 0) + 1, false);
ALTER TABLE IF EXISTS public.user_friend ALTER COLUMN id SET DEFAULT nextval('public.user_friend_id_seq'::regclass);
ALTER SEQUENCE public.user_friend_id_seq OWNED BY public.user_friend.id;

-- ── user_friend_category ──
CREATE SEQUENCE IF NOT EXISTS public.user_friend_category_id_seq AS bigint;
SELECT setval('public.user_friend_category_id_seq', COALESCE((SELECT MAX(id) FROM public.user_friend_category), 0) + 1, false);
ALTER TABLE IF EXISTS public.user_friend_category ALTER COLUMN id SET DEFAULT nextval('public.user_friend_category_id_seq'::regclass);
ALTER SEQUENCE public.user_friend_category_id_seq OWNED BY public.user_friend_category.id;

-- ── group ──
CREATE SEQUENCE IF NOT EXISTS public.group_id_seq AS bigint;
SELECT setval('public.group_id_seq', COALESCE((SELECT MAX(id) FROM public."group"), 0) + 1, false);
ALTER TABLE IF EXISTS public."group" ALTER COLUMN id SET DEFAULT nextval('public.group_id_seq'::regclass);
ALTER SEQUENCE public.group_id_seq OWNED BY public."group".id;

-- ── group_member ──
CREATE SEQUENCE IF NOT EXISTS public.group_member_id_seq AS bigint;
SELECT setval('public.group_member_id_seq', COALESCE((SELECT MAX(id) FROM public.group_member), 0) + 1, false);
ALTER TABLE IF EXISTS public.group_member ALTER COLUMN id SET DEFAULT nextval('public.group_member_id_seq'::regclass);
ALTER SEQUENCE public.group_member_id_seq OWNED BY public.group_member.id;

-- ── group_notice ──
CREATE SEQUENCE IF NOT EXISTS public.group_notice_id_seq AS bigint;
SELECT setval('public.group_notice_id_seq', COALESCE((SELECT MAX(id) FROM public.group_notice), 0) + 1, false);
ALTER TABLE IF EXISTS public.group_notice ALTER COLUMN id SET DEFAULT nextval('public.group_notice_id_seq'::regclass);
ALTER SEQUENCE public.group_notice_id_seq OWNED BY public.group_notice.id;

-- ── group_log ──
CREATE SEQUENCE IF NOT EXISTS public.group_log_id_seq AS bigint;
SELECT setval('public.group_log_id_seq', COALESCE((SELECT MAX(id) FROM public.group_log), 0) + 1, false);
ALTER TABLE IF EXISTS public.group_log ALTER COLUMN id SET DEFAULT nextval('public.group_log_id_seq'::regclass);
ALTER SEQUENCE public.group_log_id_seq OWNED BY public.group_log.id;

-- ── group_random_code ──
CREATE SEQUENCE IF NOT EXISTS public.group_random_code_id_seq AS bigint;
SELECT setval('public.group_random_code_id_seq', COALESCE((SELECT MAX(id) FROM public.group_random_code), 0) + 1, false);
ALTER TABLE IF EXISTS public.group_random_code ALTER COLUMN id SET DEFAULT nextval('public.group_random_code_id_seq'::regclass);
ALTER SEQUENCE public.group_random_code_id_seq OWNED BY public.group_random_code.id;

-- ── group_category ──
CREATE SEQUENCE IF NOT EXISTS public.group_category_id_seq AS bigint;
SELECT setval('public.group_category_id_seq', COALESCE((SELECT MAX(id) FROM public.group_category), 0) + 1, false);
ALTER TABLE IF EXISTS public.group_category ALTER COLUMN id SET DEFAULT nextval('public.group_category_id_seq'::regclass);
ALTER SEQUENCE public.group_category_id_seq OWNED BY public.group_category.id;

-- ── user_group_category ──
CREATE SEQUENCE IF NOT EXISTS public.user_group_category_id_seq AS bigint;
SELECT setval('public.user_group_category_id_seq', COALESCE((SELECT MAX(id) FROM public.user_group_category), 0) + 1, false);
ALTER TABLE IF EXISTS public.user_group_category ALTER COLUMN id SET DEFAULT nextval('public.user_group_category_id_seq'::regclass);
ALTER SEQUENCE public.user_group_category_id_seq OWNED BY public.user_group_category.id;

-- ── group_tag ──
CREATE SEQUENCE IF NOT EXISTS public.group_tag_id_seq AS bigint;
SELECT setval('public.group_tag_id_seq', COALESCE((SELECT MAX(id) FROM public.group_tag), 0) + 1, false);
ALTER TABLE IF EXISTS public.group_tag ALTER COLUMN id SET DEFAULT nextval('public.group_tag_id_seq'::regclass);
ALTER SEQUENCE public.group_tag_id_seq OWNED BY public.group_tag.id;

-- ── group_vote ──
CREATE SEQUENCE IF NOT EXISTS public.group_vote_id_seq AS bigint;
SELECT setval('public.group_vote_id_seq', COALESCE((SELECT MAX(id) FROM public.group_vote), 0) + 1, false);
ALTER TABLE IF EXISTS public.group_vote ALTER COLUMN id SET DEFAULT nextval('public.group_vote_id_seq'::regclass);
ALTER SEQUENCE public.group_vote_id_seq OWNED BY public.group_vote.id;

-- ── group_vote_option ──
CREATE SEQUENCE IF NOT EXISTS public.group_vote_option_id_seq AS bigint;
SELECT setval('public.group_vote_option_id_seq', COALESCE((SELECT MAX(id) FROM public.group_vote_option), 0) + 1, false);
ALTER TABLE IF EXISTS public.group_vote_option ALTER COLUMN id SET DEFAULT nextval('public.group_vote_option_id_seq'::regclass);
ALTER SEQUENCE public.group_vote_option_id_seq OWNED BY public.group_vote_option.id;

-- ── group_vote_record ──
CREATE SEQUENCE IF NOT EXISTS public.group_vote_record_id_seq AS bigint;
SELECT setval('public.group_vote_record_id_seq', COALESCE((SELECT MAX(id) FROM public.group_vote_record), 0) + 1, false);
ALTER TABLE IF EXISTS public.group_vote_record ALTER COLUMN id SET DEFAULT nextval('public.group_vote_record_id_seq'::regclass);
ALTER SEQUENCE public.group_vote_record_id_seq OWNED BY public.group_vote_record.id;

-- ── group_schedule ──
CREATE SEQUENCE IF NOT EXISTS public.group_schedule_id_seq AS bigint;
SELECT setval('public.group_schedule_id_seq', COALESCE((SELECT MAX(id) FROM public.group_schedule), 0) + 1, false);
ALTER TABLE IF EXISTS public.group_schedule ALTER COLUMN id SET DEFAULT nextval('public.group_schedule_id_seq'::regclass);
ALTER SEQUENCE public.group_schedule_id_seq OWNED BY public.group_schedule.id;

-- ── group_schedule_participant ──
CREATE SEQUENCE IF NOT EXISTS public.group_schedule_participant_id_seq AS bigint;
SELECT setval('public.group_schedule_participant_id_seq', COALESCE((SELECT MAX(id) FROM public.group_schedule_participant), 0) + 1, false);
ALTER TABLE IF EXISTS public.group_schedule_participant ALTER COLUMN id SET DEFAULT nextval('public.group_schedule_participant_id_seq'::regclass);
ALTER SEQUENCE public.group_schedule_participant_id_seq OWNED BY public.group_schedule_participant.id;

-- ── group_schedule_remind ──
CREATE SEQUENCE IF NOT EXISTS public.group_schedule_remind_id_seq AS bigint;
SELECT setval('public.group_schedule_remind_id_seq', COALESCE((SELECT MAX(id) FROM public.group_schedule_remind), 0) + 1, false);
ALTER TABLE IF EXISTS public.group_schedule_remind ALTER COLUMN id SET DEFAULT nextval('public.group_schedule_remind_id_seq'::regclass);
ALTER SEQUENCE public.group_schedule_remind_id_seq OWNED BY public.group_schedule_remind.id;

-- ── group_album ──
CREATE SEQUENCE IF NOT EXISTS public.group_album_id_seq AS bigint;
SELECT setval('public.group_album_id_seq', COALESCE((SELECT MAX(id) FROM public.group_album), 0) + 1, false);
ALTER TABLE IF EXISTS public.group_album ALTER COLUMN id SET DEFAULT nextval('public.group_album_id_seq'::regclass);
ALTER SEQUENCE public.group_album_id_seq OWNED BY public.group_album.id;

-- ── group_album_photo ──
CREATE SEQUENCE IF NOT EXISTS public.group_album_photo_id_seq AS bigint;
SELECT setval('public.group_album_photo_id_seq', COALESCE((SELECT MAX(id) FROM public.group_album_photo), 0) + 1, false);
ALTER TABLE IF EXISTS public.group_album_photo ALTER COLUMN id SET DEFAULT nextval('public.group_album_photo_id_seq'::regclass);
ALTER SEQUENCE public.group_album_photo_id_seq OWNED BY public.group_album_photo.id;

-- ── group_album_photo_like ──
CREATE SEQUENCE IF NOT EXISTS public.group_album_photo_like_id_seq AS bigint;
SELECT setval('public.group_album_photo_like_id_seq', COALESCE((SELECT MAX(id) FROM public.group_album_photo_like), 0) + 1, false);
ALTER TABLE IF EXISTS public.group_album_photo_like ALTER COLUMN id SET DEFAULT nextval('public.group_album_photo_like_id_seq'::regclass);
ALTER SEQUENCE public.group_album_photo_like_id_seq OWNED BY public.group_album_photo_like.id;

-- ── group_album_photo_comment ──
CREATE SEQUENCE IF NOT EXISTS public.group_album_photo_comment_id_seq AS bigint;
SELECT setval('public.group_album_photo_comment_id_seq', COALESCE((SELECT MAX(id) FROM public.group_album_photo_comment), 0) + 1, false);
ALTER TABLE IF EXISTS public.group_album_photo_comment ALTER COLUMN id SET DEFAULT nextval('public.group_album_photo_comment_id_seq'::regclass);
ALTER SEQUENCE public.group_album_photo_comment_id_seq OWNED BY public.group_album_photo_comment.id;

-- ── group_file ──
CREATE SEQUENCE IF NOT EXISTS public.group_file_id_seq AS bigint;
SELECT setval('public.group_file_id_seq', COALESCE((SELECT MAX(id) FROM public.group_file), 0) + 1, false);
ALTER TABLE IF EXISTS public.group_file ALTER COLUMN id SET DEFAULT nextval('public.group_file_id_seq'::regclass);
ALTER SEQUENCE public.group_file_id_seq OWNED BY public.group_file.id;

-- ── group_task ──
CREATE SEQUENCE IF NOT EXISTS public.group_task_id_seq AS bigint;
SELECT setval('public.group_task_id_seq', COALESCE((SELECT MAX(id) FROM public.group_task), 0) + 1, false);
ALTER TABLE IF EXISTS public.group_task ALTER COLUMN id SET DEFAULT nextval('public.group_task_id_seq'::regclass);
ALTER SEQUENCE public.group_task_id_seq OWNED BY public.group_task.id;

-- ── group_task_assignment ──
CREATE SEQUENCE IF NOT EXISTS public.group_task_assignment_id_seq AS bigint;
SELECT setval('public.group_task_assignment_id_seq', COALESCE((SELECT MAX(id) FROM public.group_task_assignment), 0) + 1, false);
ALTER TABLE IF EXISTS public.group_task_assignment ALTER COLUMN id SET DEFAULT nextval('public.group_task_assignment_id_seq'::regclass);
ALTER SEQUENCE public.group_task_assignment_id_seq OWNED BY public.group_task_assignment.id;

-- ── msg_c2c ──
CREATE SEQUENCE IF NOT EXISTS public.msg_c2c_id_seq AS bigint;
SELECT setval('public.msg_c2c_id_seq', COALESCE((SELECT MAX(id) FROM public.msg_c2c), 0) + 1, false);
ALTER TABLE IF EXISTS public.msg_c2c ALTER COLUMN id SET DEFAULT nextval('public.msg_c2c_id_seq'::regclass);
ALTER SEQUENCE public.msg_c2c_id_seq OWNED BY public.msg_c2c.id;

-- ── msg_c2g ──
CREATE SEQUENCE IF NOT EXISTS public.msg_c2g_id_seq AS bigint;
SELECT setval('public.msg_c2g_id_seq', COALESCE((SELECT MAX(id) FROM public.msg_c2g), 0) + 1, false);
ALTER TABLE IF EXISTS public.msg_c2g ALTER COLUMN id SET DEFAULT nextval('public.msg_c2g_id_seq'::regclass);
ALTER SEQUENCE public.msg_c2g_id_seq OWNED BY public.msg_c2g.id;

-- ── msg_c2s ──
CREATE SEQUENCE IF NOT EXISTS public.msg_c2s_id_seq AS bigint;
SELECT setval('public.msg_c2s_id_seq', COALESCE((SELECT MAX(id) FROM public.msg_c2s), 0) + 1, false);
ALTER TABLE IF EXISTS public.msg_c2s ALTER COLUMN id SET DEFAULT nextval('public.msg_c2s_id_seq'::regclass);
ALTER SEQUENCE public.msg_c2s_id_seq OWNED BY public.msg_c2s.id;

-- ── msg_s2c ──
CREATE SEQUENCE IF NOT EXISTS public.msg_s2c_id_seq AS bigint;
SELECT setval('public.msg_s2c_id_seq', COALESCE((SELECT MAX(id) FROM public.msg_s2c), 0) + 1, false);
ALTER TABLE IF EXISTS public.msg_s2c ALTER COLUMN id SET DEFAULT nextval('public.msg_s2c_id_seq'::regclass);
ALTER SEQUENCE public.msg_s2c_id_seq OWNED BY public.msg_s2c.id;

-- ── msg_store ──
CREATE SEQUENCE IF NOT EXISTS public.msg_store_id_seq AS bigint;
SELECT setval('public.msg_store_id_seq', COALESCE((SELECT MAX(id) FROM public.msg_store), 0) + 1, false);
ALTER TABLE IF EXISTS public.msg_store ALTER COLUMN id SET DEFAULT nextval('public.msg_store_id_seq'::regclass);
ALTER SEQUENCE public.msg_store_id_seq OWNED BY public.msg_store.id;

-- ── msg_read ──
CREATE SEQUENCE IF NOT EXISTS public.msg_read_id_seq AS bigint;
SELECT setval('public.msg_read_id_seq', COALESCE((SELECT MAX(id) FROM public.msg_read), 0) + 1, false);
ALTER TABLE IF EXISTS public.msg_read ALTER COLUMN id SET DEFAULT nextval('public.msg_read_id_seq'::regclass);
ALTER SEQUENCE public.msg_read_id_seq OWNED BY public.msg_read.id;

-- ── msg_mention ──
CREATE SEQUENCE IF NOT EXISTS public.msg_mention_id_seq AS bigint;
SELECT setval('public.msg_mention_id_seq', COALESCE((SELECT MAX(id) FROM public.msg_mention), 0) + 1, false);
ALTER TABLE IF EXISTS public.msg_mention ALTER COLUMN id SET DEFAULT nextval('public.msg_mention_id_seq'::regclass);
ALTER SEQUENCE public.msg_mention_id_seq OWNED BY public.msg_mention.id;

-- ── msg_forward ──
CREATE SEQUENCE IF NOT EXISTS public.msg_forward_id_seq AS bigint;
SELECT setval('public.msg_forward_id_seq', COALESCE((SELECT MAX(id) FROM public.msg_forward), 0) + 1, false);
ALTER TABLE IF EXISTS public.msg_forward ALTER COLUMN id SET DEFAULT nextval('public.msg_forward_id_seq'::regclass);
ALTER SEQUENCE public.msg_forward_id_seq OWNED BY public.msg_forward.id;

-- ── msg_reaction ──
CREATE SEQUENCE IF NOT EXISTS public.msg_reaction_id_seq AS bigint;
SELECT setval('public.msg_reaction_id_seq', COALESCE((SELECT MAX(id) FROM public.msg_reaction), 0) + 1, false);
ALTER TABLE IF EXISTS public.msg_reaction ALTER COLUMN id SET DEFAULT nextval('public.msg_reaction_id_seq'::regclass);
ALTER SEQUENCE public.msg_reaction_id_seq OWNED BY public.msg_reaction.id;

-- ── msg_topic ──
CREATE SEQUENCE IF NOT EXISTS public.msg_topic_id_seq AS bigint;
SELECT setval('public.msg_topic_id_seq', COALESCE((SELECT MAX(id) FROM public.msg_topic), 0) + 1, false);
ALTER TABLE IF EXISTS public.msg_topic ALTER COLUMN id SET DEFAULT nextval('public.msg_topic_id_seq'::regclass);
ALTER SEQUENCE public.msg_topic_id_seq OWNED BY public.msg_topic.id;

-- ── conversation ──
CREATE SEQUENCE IF NOT EXISTS public.conversation_id_seq AS bigint;
SELECT setval('public.conversation_id_seq', COALESCE((SELECT MAX(id) FROM public.conversation), 0) + 1, false);
ALTER TABLE IF EXISTS public.conversation ALTER COLUMN id SET DEFAULT nextval('public.conversation_id_seq'::regclass);
ALTER SEQUENCE public.conversation_id_seq OWNED BY public.conversation.id;

-- ── conversation_pin ──
CREATE SEQUENCE IF NOT EXISTS public.conversation_pin_id_seq AS bigint;
SELECT setval('public.conversation_pin_id_seq', COALESCE((SELECT MAX(id) FROM public.conversation_pin), 0) + 1, false);
ALTER TABLE IF EXISTS public.conversation_pin ALTER COLUMN id SET DEFAULT nextval('public.conversation_pin_id_seq'::regclass);
ALTER SEQUENCE public.conversation_pin_id_seq OWNED BY public.conversation_pin.id;

-- ── conversation_delete ──
CREATE SEQUENCE IF NOT EXISTS public.conversation_delete_id_seq AS bigint;
SELECT setval('public.conversation_delete_id_seq', COALESCE((SELECT MAX(id) FROM public.conversation_delete), 0) + 1, false);
ALTER TABLE IF EXISTS public.conversation_delete ALTER COLUMN id SET DEFAULT nextval('public.conversation_delete_id_seq'::regclass);
ALTER SEQUENCE public.conversation_delete_id_seq OWNED BY public.conversation_delete.id;

-- ── attachment ──
CREATE SEQUENCE IF NOT EXISTS public.attachment_id_seq AS bigint;
SELECT setval('public.attachment_id_seq', COALESCE((SELECT MAX(id) FROM public.attachment), 0) + 1, false);
ALTER TABLE IF EXISTS public.attachment ALTER COLUMN id SET DEFAULT nextval('public.attachment_id_seq'::regclass);
ALTER SEQUENCE public.attachment_id_seq OWNED BY public.attachment.id;

-- ── moment_post ──
CREATE SEQUENCE IF NOT EXISTS public.moment_post_id_seq AS bigint;
SELECT setval('public.moment_post_id_seq', COALESCE((SELECT MAX(id) FROM public.moment_post), 0) + 1, false);
ALTER TABLE IF EXISTS public.moment_post ALTER COLUMN id SET DEFAULT nextval('public.moment_post_id_seq'::regclass);
ALTER SEQUENCE public.moment_post_id_seq OWNED BY public.moment_post.id;

-- ── moment_comment ──
CREATE SEQUENCE IF NOT EXISTS public.moment_comment_id_seq AS bigint;
SELECT setval('public.moment_comment_id_seq', COALESCE((SELECT MAX(id) FROM public.moment_comment), 0) + 1, false);
ALTER TABLE IF EXISTS public.moment_comment ALTER COLUMN id SET DEFAULT nextval('public.moment_comment_id_seq'::regclass);
ALTER SEQUENCE public.moment_comment_id_seq OWNED BY public.moment_comment.id;

-- ── moment_like ──
CREATE SEQUENCE IF NOT EXISTS public.moment_like_id_seq AS bigint;
SELECT setval('public.moment_like_id_seq', COALESCE((SELECT MAX(id) FROM public.moment_like), 0) + 1, false);
ALTER TABLE IF EXISTS public.moment_like ALTER COLUMN id SET DEFAULT nextval('public.moment_like_id_seq'::regclass);
ALTER SEQUENCE public.moment_like_id_seq OWNED BY public.moment_like.id;

-- ── moment_timeline ──
CREATE SEQUENCE IF NOT EXISTS public.moment_timeline_id_seq AS bigint;
SELECT setval('public.moment_timeline_id_seq', COALESCE((SELECT MAX(id) FROM public.moment_timeline), 0) + 1, false);
ALTER TABLE IF EXISTS public.moment_timeline ALTER COLUMN id SET DEFAULT nextval('public.moment_timeline_id_seq'::regclass);
ALTER SEQUENCE public.moment_timeline_id_seq OWNED BY public.moment_timeline.id;

-- ── moment_post_acl ──
CREATE SEQUENCE IF NOT EXISTS public.moment_post_acl_id_seq AS bigint;
SELECT setval('public.moment_post_acl_id_seq', COALESCE((SELECT MAX(id) FROM public.moment_post_acl), 0) + 1, false);
ALTER TABLE IF EXISTS public.moment_post_acl ALTER COLUMN id SET DEFAULT nextval('public.moment_post_acl_id_seq'::regclass);
ALTER SEQUENCE public.moment_post_acl_id_seq OWNED BY public.moment_post_acl.id;

-- ── moment_report ──
CREATE SEQUENCE IF NOT EXISTS public.moment_report_id_seq AS bigint;
SELECT setval('public.moment_report_id_seq', COALESCE((SELECT MAX(id) FROM public.moment_report), 0) + 1, false);
ALTER TABLE IF EXISTS public.moment_report ALTER COLUMN id SET DEFAULT nextval('public.moment_report_id_seq'::regclass);
ALTER SEQUENCE public.moment_report_id_seq OWNED BY public.moment_report.id;

-- ── channel ──
CREATE SEQUENCE IF NOT EXISTS public.channel_id_seq AS bigint;
SELECT setval('public.channel_id_seq', COALESCE((SELECT MAX(id) FROM public.channel), 0) + 1, false);
ALTER TABLE IF EXISTS public.channel ALTER COLUMN id SET DEFAULT nextval('public.channel_id_seq'::regclass);
ALTER SEQUENCE public.channel_id_seq OWNED BY public.channel.id;

-- ── channel_message ──
CREATE SEQUENCE IF NOT EXISTS public.channel_message_id_seq AS bigint;
SELECT setval('public.channel_message_id_seq', COALESCE((SELECT MAX(id) FROM public.channel_message), 0) + 1, false);
ALTER TABLE IF EXISTS public.channel_message ALTER COLUMN id SET DEFAULT nextval('public.channel_message_id_seq'::regclass);
ALTER SEQUENCE public.channel_message_id_seq OWNED BY public.channel_message.id;

-- ── channel_subscription ──
CREATE SEQUENCE IF NOT EXISTS public.channel_subscription_id_seq AS bigint;
SELECT setval('public.channel_subscription_id_seq', COALESCE((SELECT MAX(id) FROM public.channel_subscription), 0) + 1, false);
ALTER TABLE IF EXISTS public.channel_subscription ALTER COLUMN id SET DEFAULT nextval('public.channel_subscription_id_seq'::regclass);
ALTER SEQUENCE public.channel_subscription_id_seq OWNED BY public.channel_subscription.id;

-- ── channel_admin ──
CREATE SEQUENCE IF NOT EXISTS public.channel_admin_id_seq AS bigint;
SELECT setval('public.channel_admin_id_seq', COALESCE((SELECT MAX(id) FROM public.channel_admin), 0) + 1, false);
ALTER TABLE IF EXISTS public.channel_admin ALTER COLUMN id SET DEFAULT nextval('public.channel_admin_id_seq'::regclass);
ALTER SEQUENCE public.channel_admin_id_seq OWNED BY public.channel_admin.id;

-- ── channel_message_view ──
CREATE SEQUENCE IF NOT EXISTS public.channel_message_view_id_seq AS bigint;
SELECT setval('public.channel_message_view_id_seq', COALESCE((SELECT MAX(id) FROM public.channel_message_view), 0) + 1, false);
ALTER TABLE IF EXISTS public.channel_message_view ALTER COLUMN id SET DEFAULT nextval('public.channel_message_view_id_seq'::regclass);
ALTER SEQUENCE public.channel_message_view_id_seq OWNED BY public.channel_message_view.id;

-- ── channel_order ──
CREATE SEQUENCE IF NOT EXISTS public.channel_order_id_seq AS bigint;
SELECT setval('public.channel_order_id_seq', COALESCE((SELECT MAX(id) FROM public.channel_order), 0) + 1, false);
ALTER TABLE IF EXISTS public.channel_order ALTER COLUMN id SET DEFAULT nextval('public.channel_order_id_seq'::regclass);
ALTER SEQUENCE public.channel_order_id_seq OWNED BY public.channel_order.id;

-- ── channel_invitation ──
CREATE SEQUENCE IF NOT EXISTS public.channel_invitation_id_seq AS bigint;
SELECT setval('public.channel_invitation_id_seq', COALESCE((SELECT MAX(id) FROM public.channel_invitation), 0) + 1, false);
ALTER TABLE IF EXISTS public.channel_invitation ALTER COLUMN id SET DEFAULT nextval('public.channel_invitation_id_seq'::regclass);
ALTER SEQUENCE public.channel_invitation_id_seq OWNED BY public.channel_invitation.id;

-- ── channel_price ──
CREATE SEQUENCE IF NOT EXISTS public.channel_price_id_seq AS bigint;
SELECT setval('public.channel_price_id_seq', COALESCE((SELECT MAX(id) FROM public.channel_price), 0) + 1, false);
ALTER TABLE IF EXISTS public.channel_price ALTER COLUMN id SET DEFAULT nextval('public.channel_price_id_seq'::regclass);
ALTER SEQUENCE public.channel_price_id_seq OWNED BY public.channel_price.id;

-- ── channel_reaction ──
CREATE SEQUENCE IF NOT EXISTS public.channel_reaction_id_seq AS bigint;
SELECT setval('public.channel_reaction_id_seq', COALESCE((SELECT MAX(id) FROM public.channel_reaction), 0) + 1, false);
ALTER TABLE IF EXISTS public.channel_reaction ALTER COLUMN id SET DEFAULT nextval('public.channel_reaction_id_seq'::regclass);
ALTER SEQUENCE public.channel_reaction_id_seq OWNED BY public.channel_reaction.id;

-- ── channel_stats_daily ──
CREATE SEQUENCE IF NOT EXISTS public.channel_stats_daily_id_seq AS bigint;
SELECT setval('public.channel_stats_daily_id_seq', COALESCE((SELECT MAX(id) FROM public.channel_stats_daily), 0) + 1, false);
ALTER TABLE IF EXISTS public.channel_stats_daily ALTER COLUMN id SET DEFAULT nextval('public.channel_stats_daily_id_seq'::regclass);
ALTER SEQUENCE public.channel_stats_daily_id_seq OWNED BY public.channel_stats_daily.id;

-- ── feedback ──
CREATE SEQUENCE IF NOT EXISTS public.feedback_id_seq AS bigint;
SELECT setval('public.feedback_id_seq', COALESCE((SELECT MAX(id) FROM public.feedback), 0) + 1, false);
ALTER TABLE IF EXISTS public.feedback ALTER COLUMN id SET DEFAULT nextval('public.feedback_id_seq'::regclass);
ALTER SEQUENCE public.feedback_id_seq OWNED BY public.feedback.id;

-- ── feedback_reply ──
CREATE SEQUENCE IF NOT EXISTS public.feedback_reply_id_seq AS bigint;
SELECT setval('public.feedback_reply_id_seq', COALESCE((SELECT MAX(id) FROM public.feedback_reply), 0) + 1, false);
ALTER TABLE IF EXISTS public.feedback_reply ALTER COLUMN id SET DEFAULT nextval('public.feedback_reply_id_seq'::regclass);
ALTER SEQUENCE public.feedback_reply_id_seq OWNED BY public.feedback_reply.id;

-- ── e2ee_transfer_sessions ──
CREATE SEQUENCE IF NOT EXISTS public.e2ee_transfer_sessions_id_seq AS bigint;
SELECT setval('public.e2ee_transfer_sessions_id_seq', COALESCE((SELECT MAX(id) FROM public.e2ee_transfer_sessions), 0) + 1, false);
ALTER TABLE IF EXISTS public.e2ee_transfer_sessions ALTER COLUMN id SET DEFAULT nextval('public.e2ee_transfer_sessions_id_seq'::regclass);
ALTER SEQUENCE public.e2ee_transfer_sessions_id_seq OWNED BY public.e2ee_transfer_sessions.id;

-- ── e2ee_social_shards ──
CREATE SEQUENCE IF NOT EXISTS public.e2ee_social_shards_id_seq AS bigint;
SELECT setval('public.e2ee_social_shards_id_seq', COALESCE((SELECT MAX(id) FROM public.e2ee_social_shards), 0) + 1, false);
ALTER TABLE IF EXISTS public.e2ee_social_shards ALTER COLUMN id SET DEFAULT nextval('public.e2ee_social_shards_id_seq'::regclass);
ALTER SEQUENCE public.e2ee_social_shards_id_seq OWNED BY public.e2ee_social_shards.id;

-- ── e2ee_local_backups ──
CREATE SEQUENCE IF NOT EXISTS public.e2ee_local_backups_id_seq AS bigint;
SELECT setval('public.e2ee_local_backups_id_seq', COALESCE((SELECT MAX(id) FROM public.e2ee_local_backups), 0) + 1, false);
ALTER TABLE IF EXISTS public.e2ee_local_backups ALTER COLUMN id SET DEFAULT nextval('public.e2ee_local_backups_id_seq'::regclass);
ALTER SEQUENCE public.e2ee_local_backups_id_seq OWNED BY public.e2ee_local_backups.id;

-- ── e2ee_shard_transmission_log ──
CREATE SEQUENCE IF NOT EXISTS public.e2ee_shard_transmission_log_id_seq AS bigint;
SELECT setval('public.e2ee_shard_transmission_log_id_seq', COALESCE((SELECT MAX(id) FROM public.e2ee_shard_transmission_log), 0) + 1, false);
ALTER TABLE IF EXISTS public.e2ee_shard_transmission_log ALTER COLUMN id SET DEFAULT nextval('public.e2ee_shard_transmission_log_id_seq'::regclass);
ALTER SEQUENCE public.e2ee_shard_transmission_log_id_seq OWNED BY public.e2ee_shard_transmission_log.id;

-- ── e2ee_trusted_contacts ──
CREATE SEQUENCE IF NOT EXISTS public.e2ee_trusted_contacts_id_seq AS bigint;
SELECT setval('public.e2ee_trusted_contacts_id_seq', COALESCE((SELECT MAX(id) FROM public.e2ee_trusted_contacts), 0) + 1, false);
ALTER TABLE IF EXISTS public.e2ee_trusted_contacts ALTER COLUMN id SET DEFAULT nextval('public.e2ee_trusted_contacts_id_seq'::regclass);
ALTER SEQUENCE public.e2ee_trusted_contacts_id_seq OWNED BY public.e2ee_trusted_contacts.id;

-- ── e2ee_key_shares ──
CREATE SEQUENCE IF NOT EXISTS public.e2ee_key_shares_id_seq AS bigint;
SELECT setval('public.e2ee_key_shares_id_seq', COALESCE((SELECT MAX(id) FROM public.e2ee_key_shares), 0) + 1, false);
ALTER TABLE IF EXISTS public.e2ee_key_shares ALTER COLUMN id SET DEFAULT nextval('public.e2ee_key_shares_id_seq'::regclass);
ALTER SEQUENCE public.e2ee_key_shares_id_seq OWNED BY public.e2ee_key_shares.id;

-- ── report_ticket ──
CREATE SEQUENCE IF NOT EXISTS public.report_ticket_id_seq AS bigint;
SELECT setval('public.report_ticket_id_seq', COALESCE((SELECT MAX(id) FROM public.report_ticket), 0) + 1, false);
ALTER TABLE IF EXISTS public.report_ticket ALTER COLUMN id SET DEFAULT nextval('public.report_ticket_id_seq'::regclass);
ALTER SEQUENCE public.report_ticket_id_seq OWNED BY public.report_ticket.id;

-- ── report_action_log ──
CREATE SEQUENCE IF NOT EXISTS public.report_action_log_id_seq AS bigint;
SELECT setval('public.report_action_log_id_seq', COALESCE((SELECT MAX(id) FROM public.report_action_log), 0) + 1, false);
ALTER TABLE IF EXISTS public.report_action_log ALTER COLUMN id SET DEFAULT nextval('public.report_action_log_id_seq'::regclass);
ALTER SEQUENCE public.report_action_log_id_seq OWNED BY public.report_action_log.id;

-- ── wallet ──
CREATE SEQUENCE IF NOT EXISTS public.wallet_id_seq AS bigint;
SELECT setval('public.wallet_id_seq', COALESCE((SELECT MAX(id) FROM public.wallet), 0) + 1, false);
ALTER TABLE IF EXISTS public.wallet ALTER COLUMN id SET DEFAULT nextval('public.wallet_id_seq'::regclass);
ALTER SEQUENCE public.wallet_id_seq OWNED BY public.wallet.id;

-- ── wallet_transaction ──
CREATE SEQUENCE IF NOT EXISTS public.wallet_transaction_id_seq AS bigint;
SELECT setval('public.wallet_transaction_id_seq', COALESCE((SELECT MAX(id) FROM public.wallet_transaction), 0) + 1, false);
ALTER TABLE IF EXISTS public.wallet_transaction ALTER COLUMN id SET DEFAULT nextval('public.wallet_transaction_id_seq'::regclass);
ALTER SEQUENCE public.wallet_transaction_id_seq OWNED BY public.wallet_transaction.id;

-- ── live_room ──
CREATE SEQUENCE IF NOT EXISTS public.live_room_id_seq AS bigint;
SELECT setval('public.live_room_id_seq', COALESCE((SELECT MAX(id) FROM public.live_room), 0) + 1, false);
ALTER TABLE IF EXISTS public.live_room ALTER COLUMN id SET DEFAULT nextval('public.live_room_id_seq'::regclass);
ALTER SEQUENCE public.live_room_id_seq OWNED BY public.live_room.id;

-- ── push_token ──
CREATE SEQUENCE IF NOT EXISTS public.push_token_id_seq AS bigint;
SELECT setval('public.push_token_id_seq', COALESCE((SELECT MAX(id) FROM public.push_token), 0) + 1, false);
ALTER TABLE IF EXISTS public.push_token ALTER COLUMN id SET DEFAULT nextval('public.push_token_id_seq'::regclass);
ALTER SEQUENCE public.push_token_id_seq OWNED BY public.push_token.id;

-- ── announcement ──
CREATE SEQUENCE IF NOT EXISTS public.announcement_id_seq AS bigint;
SELECT setval('public.announcement_id_seq', COALESCE((SELECT MAX(id) FROM public.announcement), 0) + 1, false);
ALTER TABLE IF EXISTS public.announcement ALTER COLUMN id SET DEFAULT nextval('public.announcement_id_seq'::regclass);
ALTER SEQUENCE public.announcement_id_seq OWNED BY public.announcement.id;

-- ── compliance_key ──
CREATE SEQUENCE IF NOT EXISTS public.compliance_key_id_seq AS bigint;
SELECT setval('public.compliance_key_id_seq', COALESCE((SELECT MAX(id) FROM public.compliance_key), 0) + 1, false);
ALTER TABLE IF EXISTS public.compliance_key ALTER COLUMN id SET DEFAULT nextval('public.compliance_key_id_seq'::regclass);
ALTER SEQUENCE public.compliance_key_id_seq OWNED BY public.compliance_key.id;

-- ── adm_user ──
CREATE SEQUENCE IF NOT EXISTS public.adm_user_id_seq AS bigint;
SELECT setval('public.adm_user_id_seq', COALESCE((SELECT MAX(id) FROM public.adm_user), 0) + 1, false);
ALTER TABLE IF EXISTS public.adm_user ALTER COLUMN id SET DEFAULT nextval('public.adm_user_id_seq'::regclass);
ALTER SEQUENCE public.adm_user_id_seq OWNED BY public.adm_user.id;

-- ── adm_role ──
CREATE SEQUENCE IF NOT EXISTS public.adm_role_id_seq AS bigint;
SELECT setval('public.adm_role_id_seq', COALESCE((SELECT MAX(id) FROM public.adm_role), 0) + 1, false);
ALTER TABLE IF EXISTS public.adm_role ALTER COLUMN id SET DEFAULT nextval('public.adm_role_id_seq'::regclass);
ALTER SEQUENCE public.adm_role_id_seq OWNED BY public.adm_role.id;

-- ── app_version ──
CREATE SEQUENCE IF NOT EXISTS public.app_version_id_seq AS bigint;
SELECT setval('public.app_version_id_seq', COALESCE((SELECT MAX(id) FROM public.app_version), 0) + 1, false);
ALTER TABLE IF EXISTS public.app_version ALTER COLUMN id SET DEFAULT nextval('public.app_version_id_seq'::regclass);
ALTER SEQUENCE public.app_version_id_seq OWNED BY public.app_version.id;

-- ── app_ddl ──
CREATE SEQUENCE IF NOT EXISTS public.app_ddl_id_seq AS bigint;
SELECT setval('public.app_ddl_id_seq', COALESCE((SELECT MAX(id) FROM public.app_ddl), 0) + 1, false);
ALTER TABLE IF EXISTS public.app_ddl ALTER COLUMN id SET DEFAULT nextval('public.app_ddl_id_seq'::regclass);
ALTER SEQUENCE public.app_ddl_id_seq OWNED BY public.app_ddl.id;

-- ── user_group ──
CREATE SEQUENCE IF NOT EXISTS public.user_group_id_seq AS bigint;
SELECT setval('public.user_group_id_seq', COALESCE((SELECT MAX(id) FROM public.user_group), 0) + 1, false);
ALTER TABLE IF EXISTS public.user_group ALTER COLUMN id SET DEFAULT nextval('public.user_group_id_seq'::regclass);
ALTER SEQUENCE public.user_group_id_seq OWNED BY public.user_group.id;

-- ── system_id_segment ──
CREATE SEQUENCE IF NOT EXISTS public.system_id_segment_id_seq AS bigint;
SELECT setval('public.system_id_segment_id_seq', COALESCE((SELECT MAX(id) FROM public.system_id_segment), 0) + 1, false);
ALTER TABLE IF EXISTS public.system_id_segment ALTER COLUMN id SET DEFAULT nextval('public.system_id_segment_id_seq'::regclass);
ALTER SEQUENCE public.system_id_segment_id_seq OWNED BY public.system_id_segment.id;

-- ── system_id_segment_stats ──
CREATE SEQUENCE IF NOT EXISTS public.system_id_segment_stats_id_seq AS bigint;
SELECT setval('public.system_id_segment_stats_id_seq', COALESCE((SELECT MAX(id) FROM public.system_id_segment_stats), 0) + 1, false);
ALTER TABLE IF EXISTS public.system_id_segment_stats ALTER COLUMN id SET DEFAULT nextval('public.system_id_segment_stats_id_seq'::regclass);
ALTER SEQUENCE public.system_id_segment_stats_id_seq OWNED BY public.system_id_segment_stats.id;

-- ── system_datacenter_log ──
CREATE SEQUENCE IF NOT EXISTS public.system_datacenter_log_id_seq AS bigint;
SELECT setval('public.system_datacenter_log_id_seq', COALESCE((SELECT MAX(id) FROM public.system_datacenter_log), 0) + 1, false);
ALTER TABLE IF EXISTS public.system_datacenter_log ALTER COLUMN id SET DEFAULT nextval('public.system_datacenter_log_id_seq'::regclass);
ALTER SEQUENCE public.system_datacenter_log_id_seq OWNED BY public.system_datacenter_log.id;

-- ── 重建视图 ──
CREATE OR REPLACE VIEW v_group_admins AS
SELECT gm.group_id, gm.user_id, gm.role, u.nickname, u.avatar, gm.created_at
FROM public.group_member gm
LEFT JOIN public."user" u ON u.id = gm.user_id
WHERE gm.role IN (3, 4, 5) AND gm.status = 1
ORDER BY gm.group_id, gm.role DESC, gm.created_at ASC;

CREATE OR REPLACE VIEW v_group_senior_admins AS
SELECT gm.group_id, gm.user_id, gm.role, u.nickname, u.avatar, gm.created_at
FROM public.group_member gm
LEFT JOIN public."user" u ON u.id = gm.user_id
WHERE gm.role IN (4, 5) AND gm.status = 1
ORDER BY gm.group_id, gm.role DESC, gm.created_at ASC;

CREATE OR REPLACE VIEW v_channel_realtime_stats AS
SELECT
    c.id as channel_id, c.name as channel_name, c.subscriber_count,
    COUNT(DISTINCT cm.id) as total_messages,
    COALESCE(SUM(cm.view_count), 0) as total_views,
    COALESCE((SELECT COUNT(*) FROM public.channel_reaction cr WHERE cr.channel_id = c.id), 0) as total_reactions,
    MAX(cm.created_at) as last_message_at
FROM public.channel c
LEFT JOIN public.channel_message cm ON c.id = cm.channel_id AND cm.status = 1
WHERE c.status = 1
GROUP BY c.id, c.name, c.subscriber_count;

CREATE OR REPLACE VIEW v_user_channel_reading_stats AS
SELECT
    cs.user_id, cs.channel_id, c.name as channel_name,
    cs.unread_count, cs.last_read_at,
    COUNT(DISTINCT cmv.message_id) as viewed_messages,
    MAX(cmv.viewed_at) as last_view_at
FROM public.channel_subscription cs
JOIN public.channel c ON cs.channel_id = c.id
LEFT JOIN public.channel_message_view cmv ON cs.channel_id = cmv.channel_id AND cs.user_id = cmv.user_id
WHERE cs.status = 1
GROUP BY cs.user_id, cs.channel_id, c.name, cs.unread_count, cs.last_read_at;

COMMIT;
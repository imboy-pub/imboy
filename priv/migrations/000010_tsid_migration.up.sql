-- =============================================================================
-- Migration: BIGSERIAL -> BIGINT (TSID pre-generated IDs)
--
-- 将所有使用 BIGSERIAL 自增主键的表迁移为 BIGINT NOT NULL，
-- 由应用层通过 elib_tsid:generate(TableName) 预生成分布式唯一 ID。
--
-- 注意：此迁移不改变现有数据，仅移除序列依赖。
-- =============================================================================

BEGIN;

-- ── 先删除依赖被修改列的视图 ──
-- v_group_admins / v_group_senior_admins 依赖 user.id, group_member 列
-- v_channel_realtime_stats 依赖 channel.id, channel_message.id
-- v_user_channel_reading_stats 依赖 channel.id
DROP VIEW IF EXISTS v_group_admins;
DROP VIEW IF EXISTS v_group_senior_admins;
DROP VIEW IF EXISTS v_channel_realtime_stats;
DROP VIEW IF EXISTS v_user_channel_reading_stats;

-- ── 用户相关 ──
ALTER TABLE IF EXISTS public."user" ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public."user" ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.user_id_seq;

-- user_setting 没有 id 列（PK 是 user_id），跳过

ALTER TABLE IF EXISTS public.user_device ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.user_device ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.user_device_id_seq;

ALTER TABLE IF EXISTS public.user_collect ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.user_collect ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.user_collect_id_seq;

ALTER TABLE IF EXISTS public.user_denylist ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.user_denylist ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.user_denylist_id_seq;

ALTER TABLE IF EXISTS public.user_tag ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.user_tag ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.user_tag_id_seq;

ALTER TABLE IF EXISTS public.user_tag_relation ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.user_tag_relation ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.user_tag_relation_id_seq;

-- user_log 没有 id 列，跳过

-- ── 好友相关 ──
ALTER TABLE IF EXISTS public.user_friend ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.user_friend ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.user_friend_id_seq;

ALTER TABLE IF EXISTS public.user_friend_category ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.user_friend_category ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.user_friend_category_id_seq;

-- ── 群组相关 ──
ALTER TABLE IF EXISTS public."group" ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public."group" ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.group_id_seq;

ALTER TABLE IF EXISTS public.group_member ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.group_member ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.group_member_id_seq;

-- 重建视图（ALTER 后列类型已变更）
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

ALTER TABLE IF EXISTS public.group_notice ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.group_notice ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.group_notice_id_seq;

ALTER TABLE IF EXISTS public.group_log ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.group_log ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.group_log_id_seq;

ALTER TABLE IF EXISTS public.group_random_code ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.group_random_code ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.group_random_code_id_seq;

ALTER TABLE IF EXISTS public.group_category ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.group_category ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.group_category_id_seq;

-- 注意: user_group_category 是旧表名，在某些迁移中可能存在
ALTER TABLE IF EXISTS public.user_group_category ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.user_group_category ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.user_group_category_id_seq;

ALTER TABLE IF EXISTS public.group_tag ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.group_tag ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.group_tag_id_seq;

ALTER TABLE IF EXISTS public.group_vote ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.group_vote ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.group_vote_id_seq;

ALTER TABLE IF EXISTS public.group_vote_option ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.group_vote_option ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.group_vote_option_id_seq;

ALTER TABLE IF EXISTS public.group_vote_record ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.group_vote_record ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.group_vote_record_id_seq;

ALTER TABLE IF EXISTS public.group_schedule ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.group_schedule ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.group_schedule_id_seq;

ALTER TABLE IF EXISTS public.group_schedule_participant ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.group_schedule_participant ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.group_schedule_participant_id_seq;

ALTER TABLE IF EXISTS public.group_schedule_remind ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.group_schedule_remind ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.group_schedule_remind_id_seq;

ALTER TABLE IF EXISTS public.group_album ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.group_album ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.group_album_id_seq;

ALTER TABLE IF EXISTS public.group_album_photo ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.group_album_photo ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.group_album_photo_id_seq;

ALTER TABLE IF EXISTS public.group_album_photo_like ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.group_album_photo_like ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.group_album_photo_like_id_seq;

ALTER TABLE IF EXISTS public.group_album_photo_comment ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.group_album_photo_comment ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.group_album_photo_comment_id_seq;

ALTER TABLE IF EXISTS public.group_file ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.group_file ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.group_file_id_seq;

ALTER TABLE IF EXISTS public.group_task ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.group_task ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.group_task_id_seq;

ALTER TABLE IF EXISTS public.group_task_assignment ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.group_task_assignment ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.group_task_assignment_id_seq;

-- ── 消息相关（hypertable 需先移除压缩策略和压缩配置）──

-- 移除压缩策略
SELECT remove_compression_policy('msg_c2c', if_exists => true);
SELECT remove_compression_policy('msg_c2g', if_exists => true);
SELECT remove_compression_policy('msg_c2s', if_exists => true);
SELECT remove_compression_policy('msg_s2c', if_exists => true);
SELECT remove_compression_policy('msg_store', if_exists => true);

-- 禁用压缩以允许 ALTER COLUMN
ALTER TABLE IF EXISTS public.msg_c2c SET (timescaledb.compress = false);
ALTER TABLE IF EXISTS public.msg_c2g SET (timescaledb.compress = false);
ALTER TABLE IF EXISTS public.msg_c2s SET (timescaledb.compress = false);
ALTER TABLE IF EXISTS public.msg_s2c SET (timescaledb.compress = false);
ALTER TABLE IF EXISTS public.msg_store SET (timescaledb.compress = false);

-- ALTER hypertable 的 id 列
ALTER TABLE IF EXISTS public.msg_c2c ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.msg_c2c ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.msg_c2c_id_seq;

ALTER TABLE IF EXISTS public.msg_c2g ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.msg_c2g ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.msg_c2g_id_seq;

ALTER TABLE IF EXISTS public.msg_c2s ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.msg_c2s ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.msg_c2s_id_seq;

ALTER TABLE IF EXISTS public.msg_s2c ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.msg_s2c ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.msg_s2c_id_seq;

ALTER TABLE IF EXISTS public.msg_store ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.msg_store ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.msg_store_id_seq;

-- 恢复压缩配置
ALTER TABLE IF EXISTS msg_c2c SET (timescaledb.compress, timescaledb.compress_orderby = 'created_at DESC', timescaledb.compress_segmentby = 'to_id');
SELECT add_compression_policy('msg_c2c', INTERVAL '3 days', if_not_exists => true);

ALTER TABLE IF EXISTS msg_c2g SET (timescaledb.compress, timescaledb.compress_orderby = 'created_at DESC', timescaledb.compress_segmentby = 'to_id');
SELECT add_compression_policy('msg_c2g', INTERVAL '3 days', if_not_exists => true);

ALTER TABLE IF EXISTS msg_c2s SET (timescaledb.compress, timescaledb.compress_orderby = 'created_at DESC', timescaledb.compress_segmentby = 'to_id');
SELECT add_compression_policy('msg_c2s', INTERVAL '3 days', if_not_exists => true);

ALTER TABLE IF EXISTS msg_s2c SET (timescaledb.compress, timescaledb.compress_orderby = 'created_at DESC', timescaledb.compress_segmentby = 'to_id');
SELECT add_compression_policy('msg_s2c', INTERVAL '3 days', if_not_exists => true);

ALTER TABLE IF EXISTS msg_store SET (timescaledb.compress, timescaledb.compress_orderby = 'created_at DESC', timescaledb.compress_segmentby = 'conv_key');
SELECT add_compression_policy('msg_store', INTERVAL '3 days', if_not_exists => true);

ALTER TABLE IF EXISTS public.msg_read ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.msg_read ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.msg_read_id_seq;

ALTER TABLE IF EXISTS public.msg_mention ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.msg_mention ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.msg_mention_id_seq;

ALTER TABLE IF EXISTS public.msg_forward ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.msg_forward ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.msg_forward_id_seq;

ALTER TABLE IF EXISTS public.msg_reaction ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.msg_reaction ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.msg_reaction_id_seq;

ALTER TABLE IF EXISTS public.msg_topic ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.msg_topic ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.msg_topic_id_seq;

-- ── 会话相关 ──
ALTER TABLE IF EXISTS public.conversation ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.conversation ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.conversation_id_seq;

ALTER TABLE IF EXISTS public.conversation_pin ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.conversation_pin ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.conversation_pin_id_seq;

ALTER TABLE IF EXISTS public.conversation_delete ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.conversation_delete ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.conversation_delete_id_seq;

-- ── 附件 / 媒体 ──
ALTER TABLE IF EXISTS public.attachment ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.attachment ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.attachment_id_seq;

-- ── 朋友圈 ──
ALTER TABLE IF EXISTS public.moment_post ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.moment_post ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.moment_post_id_seq;

ALTER TABLE IF EXISTS public.moment_comment ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.moment_comment ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.moment_comment_id_seq;

ALTER TABLE IF EXISTS public.moment_like ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.moment_like ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.moment_like_id_seq;

ALTER TABLE IF EXISTS public.moment_timeline ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.moment_timeline ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.moment_timeline_id_seq;

ALTER TABLE IF EXISTS public.moment_post_acl ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.moment_post_acl ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.moment_post_acl_id_seq;

ALTER TABLE IF EXISTS public.moment_report ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.moment_report ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.moment_report_id_seq;

-- ── 频道 ──
ALTER TABLE IF EXISTS public.channel ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.channel ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.channel_id_seq;

ALTER TABLE IF EXISTS public.channel_message ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.channel_message ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.channel_message_id_seq;

ALTER TABLE IF EXISTS public.channel_subscription ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.channel_subscription ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.channel_subscription_id_seq;

ALTER TABLE IF EXISTS public.channel_admin ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.channel_admin ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.channel_admin_id_seq;

ALTER TABLE IF EXISTS public.channel_message_view ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.channel_message_view ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.channel_message_view_id_seq;

ALTER TABLE IF EXISTS public.channel_order ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.channel_order ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.channel_order_id_seq;

ALTER TABLE IF EXISTS public.channel_invitation ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.channel_invitation ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.channel_invitation_id_seq;

ALTER TABLE IF EXISTS public.channel_price ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.channel_price ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.channel_price_id_seq;

ALTER TABLE IF EXISTS public.channel_reaction ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.channel_reaction ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.channel_reaction_id_seq;

ALTER TABLE IF EXISTS public.channel_stats_daily ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.channel_stats_daily ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.channel_stats_daily_id_seq;

-- ── 反馈 ──
ALTER TABLE IF EXISTS public.feedback ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.feedback ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.feedback_id_seq;

ALTER TABLE IF EXISTS public.feedback_reply ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.feedback_reply ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.feedback_reply_id_seq;

-- ── E2EE 恢复 ──
ALTER TABLE IF EXISTS public.e2ee_transfer_sessions ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.e2ee_transfer_sessions ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.e2ee_transfer_sessions_id_seq;

ALTER TABLE IF EXISTS public.e2ee_social_shards ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.e2ee_social_shards ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.e2ee_social_shards_id_seq;

ALTER TABLE IF EXISTS public.e2ee_local_backups ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.e2ee_local_backups ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.e2ee_local_backups_id_seq;

ALTER TABLE IF EXISTS public.e2ee_shard_transmission_log ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.e2ee_shard_transmission_log ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.e2ee_shard_transmission_log_id_seq;

ALTER TABLE IF EXISTS public.e2ee_trusted_contacts ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.e2ee_trusted_contacts ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.e2ee_trusted_contacts_id_seq;

ALTER TABLE IF EXISTS public.e2ee_key_shares ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.e2ee_key_shares ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.e2ee_key_shares_id_seq;

-- ── 举报 ──
ALTER TABLE IF EXISTS public.report_ticket ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.report_ticket ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.report_ticket_id_seq;

ALTER TABLE IF EXISTS public.report_action_log ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.report_action_log ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.report_action_log_id_seq;

-- ── 钱包 ──
ALTER TABLE IF EXISTS public.wallet ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.wallet ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.wallet_id_seq;

ALTER TABLE IF EXISTS public.wallet_transaction ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.wallet_transaction ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.wallet_transaction_id_seq;

-- ── 直播 ──
ALTER TABLE IF EXISTS public.live_room ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.live_room ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.live_room_id_seq;

-- ── 推送 ──
ALTER TABLE IF EXISTS public.push_token ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.push_token ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.push_token_id_seq;

-- ── 公告 ──
ALTER TABLE IF EXISTS public.announcement ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.announcement ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.announcement_id_seq;

-- ── 合规 ──
ALTER TABLE IF EXISTS public.compliance_key ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.compliance_key ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.compliance_key_id_seq;

-- ── 验证码 ──
-- verification_code 的 id 是 VARCHAR(80)，非 BIGSERIAL，跳过

-- ── 管理员 ──
ALTER TABLE IF EXISTS public.adm_user ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.adm_user ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.adm_user_id_seq;

ALTER TABLE IF EXISTS public.adm_role ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.adm_role ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.adm_role_id_seq;

-- ── 应用配置 ──
ALTER TABLE IF EXISTS public.app_version ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.app_version ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.app_version_id_seq;

ALTER TABLE IF EXISTS public.app_ddl ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.app_ddl ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.app_ddl_id_seq;

-- ── 附近的人 ──
-- geo_people_nearby 没有 id 列（PK 是 user_id），跳过

-- ── 全文搜索 ──
-- fts_user 可能没有 BIGSERIAL id，跳过

-- ── 旧表 / 其他 ──
ALTER TABLE IF EXISTS public.user_group ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.user_group ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.user_group_id_seq;

ALTER TABLE IF EXISTS public.system_id_segment ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.system_id_segment ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.system_id_segment_id_seq;

ALTER TABLE IF EXISTS public.system_id_segment_stats ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.system_id_segment_stats ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.system_id_segment_stats_id_seq;

ALTER TABLE IF EXISTS public.system_datacenter_log ALTER COLUMN id DROP DEFAULT;
ALTER TABLE IF EXISTS public.system_datacenter_log ALTER COLUMN id SET DATA TYPE BIGINT;
DROP SEQUENCE IF EXISTS public.system_datacenter_log_id_seq;

-- ── 重建所有被删除的视图 ──

-- v_channel_realtime_stats（原定义来自 00000046_channel.sql）
CREATE OR REPLACE VIEW v_channel_realtime_stats AS
SELECT
    c.id as channel_id,
    c.name as channel_name,
    c.subscriber_count,
    COUNT(DISTINCT cm.id) as total_messages,
    COALESCE(SUM(cm.view_count), 0) as total_views,
    COALESCE(
        (SELECT COUNT(*) FROM public.channel_reaction cr WHERE cr.channel_id = c.id),
        0
    ) as total_reactions,
    MAX(cm.created_at) as last_message_at
FROM public.channel c
LEFT JOIN public.channel_message cm ON c.id = cm.channel_id AND cm.status = 1
WHERE c.status = 1
GROUP BY c.id, c.name, c.subscriber_count;

COMMENT ON VIEW v_channel_realtime_stats IS '频道实时统计视图';

-- v_user_channel_reading_stats（原定义来自 00000046_channel.sql）
CREATE OR REPLACE VIEW v_user_channel_reading_stats AS
SELECT
    cs.user_id,
    cs.channel_id,
    c.name as channel_name,
    cs.unread_count,
    cs.last_read_at,
    COUNT(DISTINCT cmv.message_id) as viewed_messages,
    MAX(cmv.viewed_at) as last_view_at
FROM public.channel_subscription cs
JOIN public.channel c ON cs.channel_id = c.id
LEFT JOIN public.channel_message_view cmv ON cs.channel_id = cmv.channel_id AND cs.user_id = cmv.user_id
WHERE cs.status = 1
GROUP BY cs.user_id, cs.channel_id, c.name, cs.unread_count, cs.last_read_at;

COMMENT ON VIEW v_user_channel_reading_stats IS '用户频道阅读统计视图';

COMMIT;

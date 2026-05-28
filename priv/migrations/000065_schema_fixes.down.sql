-- Rollback: schema_fixes (合并 missing_fk_indexes + varchar_text_timestamps + schema_fixes)
-- 回滚顺序与 up 相反

BEGIN;

-- ─────────────────────────────────────────────────────────────────
-- 15. 删除 fts_user.allow_search CHECK 约束
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.fts_user
    DROP CONSTRAINT IF EXISTS chk_fts_user_allow_search;

-- ─────────────────────────────────────────────────────────────────
-- 14. 恢复类型
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.live_room ALTER COLUMN tag_id TYPE int;
ALTER TABLE public.adm_user
    ALTER COLUMN role_id TYPE integer[]
    USING role_id::integer[];

-- ─────────────────────────────────────────────────────────────────
-- 13. boolean → smallint
-- ─────────────────────────────────────────────────────────────────
DROP INDEX IF EXISTS idx_announcement_pinned_true;
CREATE INDEX IF NOT EXISTS idx_announcement_pinned
    ON public.announcement USING btree (pinned);

ALTER TABLE public.group_member
    ALTER COLUMN is_join TYPE smallint
    USING CASE WHEN is_join THEN 1 ELSE 0 END;
ALTER TABLE public.group_member ALTER COLUMN is_join SET DEFAULT 0;

ALTER TABLE public.announcement
    ALTER COLUMN pinned TYPE smallint
    USING CASE WHEN pinned THEN 1 ELSE 0 END;
ALTER TABLE public.announcement ALTER COLUMN pinned SET DEFAULT 0;

ALTER TABLE public.config
    ALTER COLUMN system TYPE smallint
    USING CASE WHEN system THEN 1 ELSE 0 END;
ALTER TABLE public.config ALTER COLUMN system SET DEFAULT 0;

-- ─────────────────────────────────────────────────────────────────
-- 12. 撤销 msg_c2g_timeline PRIMARY KEY，恢复 UNIQUE INDEX
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.msg_c2g_timeline
    DROP CONSTRAINT IF EXISTS pk_msg_c2g_timeline;
CREATE UNIQUE INDEX IF NOT EXISTS uk_c2g_timeline_ToUid_MsgId
    ON public.msg_c2g_timeline (to_uid, msg_id);

-- ─────────────────────────────────────────────────────────────────
-- 11. 删除外键补充索引
-- ─────────────────────────────────────────────────────────────────
DROP INDEX IF EXISTS idx_app_ddl_admin_user_id;
DROP INDEX IF EXISTS idx_announcement_adm_user_id;
DROP INDEX IF EXISTS idx_adm_role_parent_id;
DROP INDEX IF EXISTS idx_user_friend_category_owner_uid;
DROP INDEX IF EXISTS idx_group_log_group_id;
DROP INDEX IF EXISTS idx_conversation_client_id;

-- ─────────────────────────────────────────────────────────────────
-- 10. 删除补充的时间戳列
-- ─────────────────────────────────────────────────────────────────
DROP TRIGGER IF EXISTS trg_geo_people_nearby_updated_at ON public.geo_people_nearby;
DROP INDEX  IF EXISTS idx_geo_updated_at;
ALTER TABLE public.user_setting         DROP COLUMN IF EXISTS created_at;
ALTER TABLE public.user_friend_category DROP COLUMN IF EXISTS created_at;
ALTER TABLE public.geo_people_nearby    DROP COLUMN IF EXISTS updated_at;
ALTER TABLE public.conversation         DROP COLUMN IF EXISTS created_at;

-- ─────────────────────────────────────────────────────────────────
-- 9. text → 原 varchar(N)
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.attachment ALTER COLUMN path TYPE varchar(255);
ALTER TABLE public.attachment ALTER COLUMN url  TYPE varchar(255);
ALTER TABLE public.attachment ALTER COLUMN name TYPE varchar(160);

ALTER TABLE public."user" ALTER COLUMN "password" TYPE varchar(800);
ALTER TABLE public."user" ALTER COLUMN avatar     TYPE varchar(320);
ALTER TABLE public."user" ALTER COLUMN sign        TYPE varchar(320);
ALTER TABLE public."user" ALTER COLUMN reg_cosv    TYPE varchar(320);

ALTER TABLE public.user_device  ALTER COLUMN device_vsn  TYPE varchar(680);
ALTER TABLE public.user_friend  ALTER COLUMN tag          TYPE varchar(1600);
ALTER TABLE public.user_collect ALTER COLUMN tag          TYPE varchar(1600);
ALTER TABLE public.user_collect ALTER COLUMN attach_md5   TYPE varchar(880);
ALTER TABLE public.user_collect ALTER COLUMN source       TYPE varchar(200);
ALTER TABLE public.user_collect ALTER COLUMN remark       TYPE varchar(200);
ALTER TABLE public.group_member ALTER COLUMN description  TYPE varchar(400);
ALTER TABLE public.group_log    ALTER COLUMN remark       TYPE varchar(200);
ALTER TABLE public.adm_user     ALTER COLUMN "password"   TYPE varchar(800);
ALTER TABLE public.app_version  ALTER COLUMN download_url TYPE varchar(320);

-- ─────────────────────────────────────────────────────────────────
-- 8. 删除 fts 触发器和函数
-- ─────────────────────────────────────────────────────────────────
DROP TRIGGER IF EXISTS trg_user_fts ON public."user";
DROP FUNCTION IF EXISTS public.sync_fts_user();

-- ─────────────────────────────────────────────────────────────────
-- 7. 删除 updated_at 触发器和函数
-- ─────────────────────────────────────────────────────────────────
DO $$
DECLARE
    tbl  text;
    tbls text[] := ARRAY[
        'config','attachment','app_version','app_ddl',
        'push_token','announcement',
        'user_collect','user_tag','adm_user','adm_role',
        'user_friend_category','user_friend','user_denylist',
        '"group"','group_member','group_notice','group_log',
        'group_random_code','group_tag','user_group','user_group_category',
        'group_schedule','group_file','group_album',
        'group_task','group_vote',
        'channel','feedback','feedback_reply','report_ticket',
        'wallet','live_room','compliance_key'
    ];
    safe_name text;
    trg_name  text;
BEGIN
    FOREACH tbl IN ARRAY tbls LOOP
        safe_name := replace(replace(tbl, '"', ''), '.', '_');
        trg_name  := 'trg_' || safe_name || '_updated_at';
        EXECUTE format('DROP TRIGGER IF EXISTS %I ON public.%s', trg_name, tbl);
    END LOOP;
END;
$$;
DROP FUNCTION IF EXISTS public.set_updated_at();

-- ─────────────────────────────────────────────────────────────────
-- 6. 删除 CHECK 约束
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.config         DROP CONSTRAINT IF EXISTS chk_config_status;
ALTER TABLE public.attachment      DROP CONSTRAINT IF EXISTS chk_attachment_status;
ALTER TABLE public.user_collect    DROP CONSTRAINT IF EXISTS chk_user_collect_status;
ALTER TABLE public.user_friend     DROP CONSTRAINT IF EXISTS chk_user_friend_status;
ALTER TABLE public."group"         DROP CONSTRAINT IF EXISTS chk_group_status;
ALTER TABLE public.group_member    DROP CONSTRAINT IF EXISTS chk_group_member_status;
ALTER TABLE public.channel         DROP CONSTRAINT IF EXISTS chk_channel_status;
ALTER TABLE public.feedback        DROP CONSTRAINT IF EXISTS chk_feedback_status;

-- ─────────────────────────────────────────────────────────────────
-- 5. 恢复原有 status 单列索引
-- ─────────────────────────────────────────────────────────────────
DROP INDEX IF EXISTS idx_announcement_status_time;
CREATE INDEX IF NOT EXISTS idx_announcement_status ON public.announcement USING btree (status);

DROP INDEX IF EXISTS idx_group_vote_group_status;
CREATE INDEX IF NOT EXISTS idx_group_vote_status ON group_vote (status);

DROP INDEX IF EXISTS idx_group_schedule_group_status;
CREATE INDEX IF NOT EXISTS idx_group_schedule_status ON group_schedule (status);

DROP INDEX IF EXISTS idx_group_schedule_participant_schedule_status;
CREATE INDEX IF NOT EXISTS idx_group_schedule_participant_status ON group_schedule_participant (status);

CREATE INDEX IF NOT EXISTS idx_group_task_status ON group_task (status);

DROP INDEX IF EXISTS idx_group_task_assignment_task_status;
CREATE INDEX IF NOT EXISTS idx_group_task_assignment_status ON group_task_assignment (status);

DROP INDEX IF EXISTS idx_live_room_status_time;
CREATE INDEX IF NOT EXISTS i_live_room_Status ON live_room (status);

-- ─────────────────────────────────────────────────────────────────
-- 4. 删除新增索引
-- ─────────────────────────────────────────────────────────────────
DROP INDEX IF EXISTS idx_user_device_uid;
DROP INDEX IF EXISTS idx_user_tag_creator_uid;
DROP INDEX IF EXISTS idx_ufc_uid;
DROP INDEX IF EXISTS idx_user_denylist_uid;
DROP INDEX IF EXISTS idx_group_random_code_uid;
DROP INDEX IF EXISTS idx_group_notice_group_id;
DROP INDEX IF EXISTS idx_push_token_uid;
DROP INDEX IF EXISTS idx_feedback_uid;
DROP INDEX IF EXISTS idx_feedback_reply_uid;
DROP INDEX IF EXISTS idx_group_album_uid;

-- ─────────────────────────────────────────────────────────────────
-- 3. jsonb → json
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.user_setting ALTER COLUMN setting TYPE json USING setting::text::json;
ALTER TABLE public.user_friend  ALTER COLUMN setting TYPE json USING setting::text::json;
ALTER TABLE public.feedback     ALTER COLUMN attach  TYPE json USING attach::text::json;

-- ─────────────────────────────────────────────────────────────────
-- 2. 恢复旧 UNIQUE 约束
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.user_device
    DROP CONSTRAINT IF EXISTS uk_userid_deviceid;
ALTER TABLE public.user_device
    ADD CONSTRAINT uk_status_userid_deviceid UNIQUE (status, user_id, device_id);

ALTER TABLE public.user_collect
    DROP CONSTRAINT IF EXISTS uk_user_collect_uid_kind;
ALTER TABLE public.user_collect
    ADD CONSTRAINT "uk_user_collect_UserId_Status_kindId" UNIQUE (user_id, status, kind_id);

-- ─────────────────────────────────────────────────────────────────
-- 1. 恢复 msg_c2c 压缩（去掉 segmentby）
-- ─────────────────────────────────────────────────────────────────
SELECT remove_compression_policy('msg_c2c', if_exists => true);
ALTER TABLE IF EXISTS msg_c2c SET (timescaledb.compress = false);
ALTER TABLE IF EXISTS msg_c2c SET (
    timescaledb.compress,
    timescaledb.compress_orderby = 'created_at DESC'
);
SELECT add_compression_policy('msg_c2c', INTERVAL '3 days', if_not_exists => true);

-- ─────────────────────────────────────────────────────────────────
-- 回滚 section 35: announcement / msg_c2g / app_upgrade_log 修复
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.app_upgrade_log
    DROP CONSTRAINT IF EXISTS chk_app_upgrade_log_upgrade_type,
    DROP CONSTRAINT IF EXISTS chk_app_upgrade_log_event,
    DROP CONSTRAINT IF EXISTS chk_app_upgrade_log_cos;

SELECT remove_compression_policy('msg_c2g', if_exists => true);
SELECT decompress_chunk(c) FROM show_chunks('msg_c2g') c;
ALTER TABLE public.msg_c2g ALTER COLUMN payload TYPE text USING payload::text;
ALTER TABLE public.msg_c2g SET (
    timescaledb.compress,
    timescaledb.compress_orderby   = 'created_at DESC',
    timescaledb.compress_segmentby = 'to_id'
);
SELECT add_compression_policy('msg_c2g', INTERVAL '3 days', if_not_exists => true);
CREATE INDEX IF NOT EXISTS i_c2g_e2ee ON msg_c2g((e2ee IS NOT NULL)) WHERE e2ee IS NOT NULL;

ALTER TABLE public.announcement
    DROP CONSTRAINT IF EXISTS chk_announcement_type,
    DROP CONSTRAINT IF EXISTS chk_announcement_status;

-- ─────────────────────────────────────────────────────────────────
-- 回滚 section 34: 杂项修复
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.verification_code
    ALTER COLUMN code DROP NOT NULL;
ALTER TABLE public.user_tag_relation
    ALTER COLUMN tag_id  DROP NOT NULL,
    ALTER COLUMN user_id DROP NOT NULL,
    ALTER COLUMN scene   DROP NOT NULL;
ALTER TABLE public.app_ddl
    DROP CONSTRAINT IF EXISTS chk_app_ddl_status;
ALTER TABLE public.user_denylist
    ALTER COLUMN denied_user_id SET DEFAULT '0';

-- ─────────────────────────────────────────────────────────────────
-- 回滚 section 33: group_task_assignment + group_notice 修复
-- ─────────────────────────────────────────────────────────────────
UPDATE public.group_notice SET edit_user_id = 0 WHERE edit_user_id IS NULL;
ALTER TABLE public.group_notice
    ALTER COLUMN edit_user_id SET NOT NULL,
    ALTER COLUMN edit_user_id SET DEFAULT 0;
ALTER TABLE public.group_task_assignment
    DROP CONSTRAINT IF EXISTS chk_task_assignment_reviewed;

-- ─────────────────────────────────────────────────────────────────
-- 回滚 section 32: channel_invitation / channel_price 修复
-- ─────────────────────────────────────────────────────────────────
CREATE INDEX IF NOT EXISTS idx_channel_price_channel_id
    ON public.channel_price(channel_id);
DROP INDEX IF EXISTS uk_channel_invitation_pending;
ALTER TABLE public.channel_invitation
    ADD CONSTRAINT uk_channel_invitation_active UNIQUE (channel_id, invitee_uid);

-- ─────────────────────────────────────────────────────────────────
-- 回滚 section 31: e2ee 修复
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.e2ee_local_backups
    DROP CONSTRAINT IF EXISTS uk_e2ee_local_backup_version;
DROP INDEX IF EXISTS idx_e2ee_trusted_contacts_uid_active;
CREATE INDEX IF NOT EXISTS idx_e2ee_trusted_contacts_status
    ON public.e2ee_trusted_contacts(status);
ALTER TABLE public.e2ee_transfer_sessions
    DROP CONSTRAINT IF EXISTS chk_e2ee_transfer_sessions_status;

-- ─────────────────────────────────────────────────────────────────
-- 回滚 section 30: msg_s2c 修复
-- ─────────────────────────────────────────────────────────────────
SELECT remove_compression_policy('msg_s2c', if_exists => true);
SELECT decompress_chunk(c) FROM show_chunks('msg_s2c') c;
ALTER TABLE public.msg_s2c ALTER COLUMN payload TYPE text USING payload::text;
ALTER TABLE public.msg_s2c SET (
    timescaledb.compress,
    timescaledb.compress_orderby   = 'created_at DESC',
    timescaledb.compress_segmentby = 'to_id'
);
SELECT add_compression_policy('msg_s2c', INTERVAL '3 days', if_not_exists => true);
CREATE INDEX IF NOT EXISTS i_s2c_e2ee ON msg_s2c((e2ee IS NOT NULL)) WHERE e2ee IS NOT NULL;

-- ─────────────────────────────────────────────────────────────────
-- 回滚 section 29: adm_role 约束
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.adm_role
    DROP CONSTRAINT IF EXISTS chk_adm_role_sort,
    DROP CONSTRAINT IF EXISTS chk_adm_role_status;
DROP INDEX IF EXISTS uk_adm_role_name_active;

-- ─────────────────────────────────────────────────────────────────
-- 回滚 section 28: user_log 修复
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.user_log DROP CONSTRAINT IF EXISTS chk_user_log_type;
ALTER TABLE public.user_log ALTER COLUMN body TYPE text USING body::text;
ALTER TABLE public.user_log ALTER COLUMN type TYPE int4;

-- ─────────────────────────────────────────────────────────────────
-- 回滚 section 27: 消息类型约束
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.conversation_delete DROP CONSTRAINT IF EXISTS chk_conversation_delete_type;
ALTER TABLE public.conversation_pin    DROP CONSTRAINT IF EXISTS chk_conversation_pin_type;
ALTER TABLE public.msg_topic           DROP CONSTRAINT IF EXISTS chk_msg_topic_type;
ALTER TABLE public.msg_forward
    DROP CONSTRAINT IF EXISTS chk_msg_forward_forward_type,
    DROP CONSTRAINT IF EXISTS chk_msg_forward_original_type;

-- ─────────────────────────────────────────────────────────────────
-- 回滚 section 26: 删除补充索引
-- ─────────────────────────────────────────────────────────────────
DROP INDEX IF EXISTS idx_group_member_category;
DROP INDEX IF EXISTS idx_msg_mention_from_uid;
DROP INDEX IF EXISTS idx_group_album_photo_comment_user;
DROP INDEX IF EXISTS idx_group_album_photo_uploader;

-- ─────────────────────────────────────────────────────────────────
-- 回滚 section 25: wallet 修复
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.wallet_transaction DROP CONSTRAINT IF EXISTS fk_wallet_tx_wallet;
DROP INDEX IF EXISTS idx_wallet_tx_reference_no;
ALTER TABLE public.wallet_transaction
    DROP CONSTRAINT IF EXISTS chk_wallet_tx_status,
    DROP CONSTRAINT IF EXISTS chk_wallet_tx_type;
ALTER TABLE public.wallet
    DROP CONSTRAINT IF EXISTS chk_wallet_status,
    DROP CONSTRAINT IF EXISTS chk_wallet_frozen,
    DROP CONSTRAINT IF EXISTS chk_wallet_balance;
CREATE UNIQUE INDEX IF NOT EXISTS uk_wallet_UserId ON wallet(user_id);

-- ─────────────────────────────────────────────────────────────────
-- 回滚 section 24: feedback / report_ticket / plugin_audit_log
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.plugin_audit_log DROP CONSTRAINT IF EXISTS chk_plugin_audit_log_result;
DROP INDEX IF EXISTS idx_report_ticket_handled_by;
ALTER TABLE public.report_ticket DROP CONSTRAINT IF EXISTS uk_report_ticket_target_reporter;
DROP INDEX IF EXISTS idx_feedback_reply_feedback_id;
ALTER TABLE public.feedback_reply DROP CONSTRAINT IF EXISTS fk_feedback_reply_feedback;
ALTER TABLE public.feedback
    DROP CONSTRAINT IF EXISTS chk_feedback_rating,
    DROP CONSTRAINT IF EXISTS chk_feedback_type;

-- ─────────────────────────────────────────────────────────────────
-- 回滚 section 23: compliance_key NULL 修复
-- ─────────────────────────────────────────────────────────────────
DROP INDEX IF EXISTS idx_compliance_key_revoked_by;
DROP INDEX IF EXISTS idx_compliance_key_created_by;
ALTER TABLE public.compliance_key ALTER COLUMN created_by SET DEFAULT 0;
ALTER TABLE public.compliance_key ALTER COLUMN revoked_by SET DEFAULT 0;
UPDATE public.compliance_key SET created_by = 0 WHERE created_by IS NULL;
UPDATE public.compliance_key SET revoked_by = 0 WHERE revoked_by IS NULL;

-- ─────────────────────────────────────────────────────────────────
-- 回滚 section 22: channel / moment
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.moment_post
    DROP CONSTRAINT IF EXISTS chk_moment_post_comment_count,
    DROP CONSTRAINT IF EXISTS chk_moment_post_like_count;
ALTER TABLE public.channel_message
    ALTER COLUMN reaction_summary TYPE text USING reaction_summary::text,
    ALTER COLUMN payload          TYPE text USING payload::text;
ALTER TABLE public.channel
    DROP CONSTRAINT IF EXISTS chk_channel_subscriber_count;
DROP INDEX IF EXISTS i_channel_tags;
ALTER TABLE public.channel
    ALTER COLUMN tags TYPE varchar(500) USING tags::text;
ALTER TABLE public.channel ALTER COLUMN tags SET DEFAULT '[]';

-- ─────────────────────────────────────────────────────────────────
-- 回滚 section 21: 消息系统约束
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.user_group ALTER COLUMN setting TYPE text USING setting::text;
DROP INDEX IF EXISTS i_msg_store_group_id;
ALTER TABLE public.msg_store DROP CONSTRAINT IF EXISTS chk_msg_store_chat_type;
CREATE INDEX IF NOT EXISTS idx_msg_reaction_msg ON msg_reaction(msg_id, msg_type);
ALTER TABLE msg_reaction DROP CONSTRAINT IF EXISTS chk_msg_reaction_msg_type;
ALTER TABLE public.conversation DROP CONSTRAINT IF EXISTS chk_conversation_type;

-- ─────────────────────────────────────────────────────────────────
-- 回滚 section 20: 业务 CHECK 约束
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE live_room ALTER COLUMN viewer_count DROP NOT NULL;
ALTER TABLE live_room DROP CONSTRAINT IF EXISTS chk_live_room_status;
CREATE INDEX IF NOT EXISTS idx_group_schedule_remind_is_sent
    ON group_schedule_remind (is_sent);
DROP INDEX IF EXISTS idx_group_schedule_remind_pending;
ALTER TABLE group_schedule
    DROP CONSTRAINT IF EXISTS chk_schedule_remind_before,
    DROP CONSTRAINT IF EXISTS chk_schedule_time;
ALTER TABLE public.group_random_code
    ALTER COLUMN validity_at SET NOT NULL,
    ALTER COLUMN validity_at SET DEFAULT CURRENT_TIMESTAMP;
ALTER TABLE public.group_notice ALTER COLUMN body TYPE varchar(2000);
ALTER TABLE group_album_photo
    DROP CONSTRAINT IF EXISTS chk_photo_comment_count,
    DROP CONSTRAINT IF EXISTS chk_photo_like_count;
ALTER TABLE group_album     DROP CONSTRAINT IF EXISTS chk_group_album_photo_count;
ALTER TABLE group_file
    DROP CONSTRAINT IF EXISTS chk_group_file_download_count,
    DROP CONSTRAINT IF EXISTS chk_group_file_status;
ALTER TABLE group_file ALTER COLUMN file_name TYPE varchar(255);
DROP INDEX IF EXISTS idx_group_vote_record_options;
ALTER TABLE group_vote_record ALTER COLUMN option_ids TYPE text USING option_ids::text;
ALTER TABLE public."group"
    DROP CONSTRAINT IF EXISTS chk_group_creator_uid,
    DROP CONSTRAINT IF EXISTS chk_group_owner_uid,
    DROP CONSTRAINT IF EXISTS chk_group_member_count,
    DROP CONSTRAINT IF EXISTS chk_group_member_max;
ALTER TABLE public."user"
    DROP CONSTRAINT IF EXISTS chk_user_status,
    DROP CONSTRAINT IF EXISTS chk_user_gender;
ALTER TABLE public."user" ALTER COLUMN gender TYPE int4 USING gender::int4;
ALTER TABLE public.push_token
    DROP CONSTRAINT IF EXISTS chk_push_token_status,
    DROP CONSTRAINT IF EXISTS chk_push_token_platform,
    DROP CONSTRAINT IF EXISTS chk_push_token_device_type;
ALTER TABLE public.app_version
    DROP CONSTRAINT IF EXISTS chk_app_version_upgrade_type,
    DROP CONSTRAINT IF EXISTS chk_app_version_grayscale;

-- ─────────────────────────────────────────────────────────────────
-- 回滚 section 19: boolean 转换
-- ─────────────────────────────────────────────────────────────────
DROP INDEX IF EXISTS idx_c2g_timeline_to_uid_pending;
CREATE INDEX IF NOT EXISTS idx_c2g_timeline_ToUid_ClientAck
    ON public.msg_c2g_timeline (to_uid, client_ack);
ALTER TABLE public.msg_c2g_timeline
    ALTER COLUMN client_ack TYPE smallint USING CASE WHEN client_ack THEN 1 ELSE 0 END;
ALTER TABLE public.msg_c2g_timeline ALTER COLUMN client_ack SET DEFAULT 0;

ALTER TABLE public.app_version_policy
    ALTER COLUMN grayscale_enabled TYPE smallint USING CASE WHEN grayscale_enabled THEN 1 ELSE 0 END;
ALTER TABLE public.app_version_policy ALTER COLUMN grayscale_enabled SET DEFAULT 0;

ALTER TABLE public.app_version
    ALTER COLUMN force_update TYPE int USING CASE WHEN force_update THEN 1 ELSE 2 END;
ALTER TABLE public.app_version ALTER COLUMN force_update SET DEFAULT 0;

-- ─────────────────────────────────────────────────────────────────
-- 回滚 section 18: fts 触发器
-- ─────────────────────────────────────────────────────────────────
DROP TRIGGER IF EXISTS trg_user_fts ON public."user";
CREATE OR REPLACE FUNCTION public.sync_fts_user()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
    INSERT INTO public.fts_user (user_id, token)
    VALUES (
        NEW.id,
        to_tsvector('jiebacfg',
            COALESCE(NEW.nickname, '') || ' ' ||
            COALESCE(NEW.account, '')  || ' ' ||
            COALESCE(NEW.mobile, '')
        )
    )
    ON CONFLICT (user_id) DO UPDATE
        SET token = to_tsvector('jiebacfg',
            COALESCE(NEW.nickname, '') || ' ' ||
            COALESCE(NEW.account, '')  || ' ' ||
            COALESCE(NEW.mobile, '')
        );
    RETURN NEW;
END;
$$;
CREATE TRIGGER trg_user_fts
    AFTER INSERT OR UPDATE OF nickname, account, mobile
    ON public."user"
    FOR EACH ROW EXECUTE FUNCTION public.sync_fts_user();

-- ─────────────────────────────────────────────────────────────────
-- 回滚 section 17: attachment 类型
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.attachment ALTER COLUMN info TYPE text USING info::text;
ALTER TABLE public.attachment ALTER COLUMN referer_time TYPE int;
ALTER TABLE public.attachment ALTER COLUMN size         TYPE int;

-- ─────────────────────────────────────────────────────────────────
-- 回滚 section 16: adm_user 补丁
-- ─────────────────────────────────────────────────────────────────
DROP INDEX IF EXISTS idx_adm_user_role_id;
ALTER TABLE public.adm_user DROP CONSTRAINT IF EXISTS chk_adm_user_status;
DROP INDEX IF EXISTS uk_adm_email;
DROP INDEX IF EXISTS uk_adm_mobile;
CREATE UNIQUE INDEX IF NOT EXISTS "uk_Adm_Mobile" ON public.adm_user (mobile);
CREATE UNIQUE INDEX IF NOT EXISTS "uk_Adm_Email"  ON public.adm_user (email);
ALTER TABLE public.adm_user DROP COLUMN IF EXISTS updated_at;

-- ─────────────────────────────────────────────────────────────────
-- 回滚 section 15-fix: feedback.status CHECK
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.feedback DROP CONSTRAINT IF EXISTS chk_feedback_status;
ALTER TABLE public.feedback
    ADD CONSTRAINT chk_feedback_status CHECK (status IN (-1, 0, 1, 2));

COMMIT;

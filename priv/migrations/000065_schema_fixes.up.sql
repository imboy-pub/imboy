-- =============================================================================
-- Migration: schema_fixes (合并 schema_fixes + varchar_text_timestamps + missing_fk_indexes)
-- 1.  msg_c2c 补充 compress_segmentby
-- 2.  UNIQUE 约束去除可变 status 列
-- 3.  json → jsonb
-- 4.  补充高频 user_id / FK 索引
-- 5.  删除低区分度 status 单列索引，替换为复合索引
-- 6.  status 列加 CHECK 约束
-- 7.  updated_at 自动维护触发器
-- 8.  fts_user tsvector 自动同步触发器
-- 9.  varchar → text（无业务上限的大字段）
-- 10. 补充缺失的 created_at / updated_at 列
-- 11. 外键列补充缺失索引
-- 12. msg_c2g_timeline UNIQUE INDEX 提升为 PRIMARY KEY
-- =============================================================================

BEGIN;

-- ─────────────────────────────────────────────────────────────────
-- 1. msg_c2c 补充 compress_segmentby（与其他消息表保持一致）
-- ─────────────────────────────────────────────────────────────────
SELECT remove_compression_policy('msg_c2c', if_exists => true);
ALTER TABLE IF EXISTS msg_c2c SET (timescaledb.compress = false);
ALTER TABLE IF EXISTS msg_c2c SET (
    timescaledb.compress,
    timescaledb.compress_orderby  = 'created_at DESC',
    timescaledb.compress_segmentby = 'to_id'
);
SELECT add_compression_policy('msg_c2c', INTERVAL '3 days', if_not_exists => true);

-- ─────────────────────────────────────────────────────────────────
-- 2. 修复含可变 status 的 UNIQUE 约束
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.user_device
    DROP CONSTRAINT IF EXISTS uk_status_userid_deviceid;
ALTER TABLE public.user_device
    ADD CONSTRAINT uk_userid_deviceid UNIQUE (user_id, device_id);

ALTER TABLE public.user_collect
    DROP CONSTRAINT IF EXISTS "uk_user_collect_UserId_Status_kindId";
ALTER TABLE public.user_collect
    ADD CONSTRAINT uk_user_collect_uid_kind UNIQUE (user_id, kind_id);

-- ─────────────────────────────────────────────────────────────────
-- 3. json → jsonb
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.user_setting
    ALTER COLUMN setting TYPE jsonb USING setting::jsonb;
ALTER TABLE public.user_friend
    ALTER COLUMN setting TYPE jsonb USING setting::jsonb;
ALTER TABLE public.feedback
    ALTER COLUMN attach TYPE jsonb USING attach::jsonb;

-- ─────────────────────────────────────────────────────────────────
-- 4. 补充高频 user_id / FK 索引
-- ─────────────────────────────────────────────────────────────────
CREATE INDEX IF NOT EXISTS idx_user_device_uid        ON public.user_device (user_id);
CREATE INDEX IF NOT EXISTS idx_user_tag_creator_uid   ON public.user_tag (creator_user_id);
CREATE INDEX IF NOT EXISTS idx_ufc_uid                ON public.user_friend_category (user_id);
CREATE INDEX IF NOT EXISTS idx_user_denylist_uid      ON public.user_denylist (user_id);
CREATE INDEX IF NOT EXISTS idx_group_random_code_uid  ON public.group_random_code (user_id);
CREATE INDEX IF NOT EXISTS idx_group_notice_group_id  ON public.group_notice (group_id);
CREATE INDEX IF NOT EXISTS idx_push_token_uid         ON public.push_token (user_id);
CREATE INDEX IF NOT EXISTS idx_feedback_uid           ON public.feedback (user_id);
CREATE INDEX IF NOT EXISTS idx_feedback_reply_uid     ON public.feedback_reply (user_id);
CREATE INDEX IF NOT EXISTS idx_group_album_uid        ON public.group_album (user_id);

-- ─────────────────────────────────────────────────────────────────
-- 5. 删除低区分度 status 单列索引，替换为复合索引
-- ─────────────────────────────────────────────────────────────────
DROP INDEX IF EXISTS idx_announcement_status;
CREATE INDEX IF NOT EXISTS idx_announcement_status_time
    ON public.announcement (status, created_at DESC);

DROP INDEX IF EXISTS idx_group_vote_status;
CREATE INDEX IF NOT EXISTS idx_group_vote_group_status
    ON group_vote (group_id, status);

DROP INDEX IF EXISTS idx_group_schedule_status;
CREATE INDEX IF NOT EXISTS idx_group_schedule_group_status
    ON group_schedule (group_id, status);

DROP INDEX IF EXISTS idx_group_schedule_participant_status;
CREATE INDEX IF NOT EXISTS idx_group_schedule_participant_schedule_status
    ON group_schedule_participant (schedule_id, status);

DROP INDEX IF EXISTS idx_group_task_status;

DROP INDEX IF EXISTS idx_group_task_assignment_status;
CREATE INDEX IF NOT EXISTS idx_group_task_assignment_task_status
    ON group_task_assignment (task_id, status);

DROP INDEX IF EXISTS i_live_room_Status;
CREATE INDEX IF NOT EXISTS idx_live_room_status_time
    ON live_room (status, created_at DESC);

-- ─────────────────────────────────────────────────────────────────
-- 6. status 列加 CHECK 约束（核心高频表）
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.config
    ADD CONSTRAINT chk_config_status CHECK (status IN (-1, 0, 1));
ALTER TABLE public.attachment
    ADD CONSTRAINT chk_attachment_status CHECK (status IN (-1, 0, 1));
ALTER TABLE public.user_collect
    ADD CONSTRAINT chk_user_collect_status CHECK (status IN (-1, 0, 1));
ALTER TABLE public.user_friend
    ADD CONSTRAINT chk_user_friend_status CHECK (status IN (-1, 0, 1));
ALTER TABLE public."group"
    ADD CONSTRAINT chk_group_status CHECK (status IN (-1, 0, 1));
ALTER TABLE public.group_member
    ADD CONSTRAINT chk_group_member_status CHECK (status IN (-1, 0, 1, 2));
ALTER TABLE public.channel
    ADD CONSTRAINT chk_channel_status CHECK (status IN (-1, 0, 1));
ALTER TABLE public.feedback
    ADD CONSTRAINT chk_feedback_status CHECK (status IN (-1, 0, 1, 2));

-- ─────────────────────────────────────────────────────────────────
-- 7. updated_at 自动维护触发器
-- ─────────────────────────────────────────────────────────────────
CREATE OR REPLACE FUNCTION public.set_updated_at()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
    NEW.updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$;

DO $$
DECLARE
    tbl text;
    tbls text[] := ARRAY[
        'config', 'attachment', 'app_version', 'app_ddl',
        'push_token', 'announcement',
        'user_collect', 'user_tag', 'adm_user', 'adm_role',
        'user_friend_category', 'user_friend', 'user_denylist',
        '"group"', 'group_member', 'group_notice', 'group_log',
        'group_random_code', 'group_tag', 'user_group', 'user_group_category',
        'group_schedule', 'group_file', 'group_album',
        'group_task', 'group_vote',
        'channel', 'feedback', 'feedback_reply', 'report_ticket',
        'wallet', 'live_room', 'compliance_key'
    ];
    safe_name text;
    trg_name  text;
BEGIN
    FOREACH tbl IN ARRAY tbls LOOP
        safe_name := replace(replace(tbl, '"', ''), '.', '_');
        trg_name  := 'trg_' || safe_name || '_updated_at';
        IF NOT EXISTS (
            SELECT 1 FROM pg_trigger WHERE tgname = trg_name
        ) THEN
            EXECUTE format(
                'CREATE TRIGGER %I BEFORE UPDATE ON public.%s '
                'FOR EACH ROW EXECUTE FUNCTION public.set_updated_at()',
                trg_name, tbl
            );
        END IF;
    END LOOP;
END;
$$;

-- ─────────────────────────────────────────────────────────────────
-- 8. fts_user tsvector 自动同步触发器
-- ─────────────────────────────────────────────────────────────────
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

DROP TRIGGER IF EXISTS trg_user_fts ON public."user";
CREATE TRIGGER trg_user_fts
    AFTER INSERT OR UPDATE OF nickname, account, mobile
    ON public."user"
    FOR EACH ROW EXECUTE FUNCTION public.sync_fts_user();

-- ─────────────────────────────────────────────────────────────────
-- 9. varchar → text（无业务上限的大字段）
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.attachment ALTER COLUMN path TYPE text;
ALTER TABLE public.attachment ALTER COLUMN url  TYPE text;
ALTER TABLE public.attachment ALTER COLUMN name TYPE text;

ALTER TABLE public."user" ALTER COLUMN "password" TYPE text;
ALTER TABLE public."user" ALTER COLUMN avatar     TYPE text;
ALTER TABLE public."user" ALTER COLUMN sign        TYPE text;
ALTER TABLE public."user" ALTER COLUMN reg_cosv    TYPE text;

ALTER TABLE public.user_device  ALTER COLUMN device_vsn  TYPE text;
ALTER TABLE public.user_friend  ALTER COLUMN tag          TYPE text;
ALTER TABLE public.user_collect ALTER COLUMN tag          TYPE text;
ALTER TABLE public.user_collect ALTER COLUMN attach_md5   TYPE text;
ALTER TABLE public.user_collect ALTER COLUMN source       TYPE text;
ALTER TABLE public.user_collect ALTER COLUMN remark       TYPE text;
ALTER TABLE public.group_member ALTER COLUMN description  TYPE text;
ALTER TABLE public.group_log    ALTER COLUMN remark       TYPE text;
ALTER TABLE public.adm_user     ALTER COLUMN "password"   TYPE text;
ALTER TABLE public.app_version  ALTER COLUMN download_url TYPE text;

-- ─────────────────────────────────────────────────────────────────
-- 10. 补充缺失的时间戳列
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.user_setting
    ADD COLUMN IF NOT EXISTS created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE public.user_friend_category
    ADD COLUMN IF NOT EXISTS created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE public.geo_people_nearby
    ADD COLUMN IF NOT EXISTS updated_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL;
CREATE INDEX IF NOT EXISTS idx_geo_updated_at ON public.geo_people_nearby (updated_at);
ALTER TABLE public.conversation
    ADD COLUMN IF NOT EXISTS created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL;

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM pg_trigger WHERE tgname = 'trg_geo_people_nearby_updated_at'
    ) THEN
        CREATE TRIGGER trg_geo_people_nearby_updated_at
            BEFORE UPDATE ON public.geo_people_nearby
            FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();
    END IF;
END;
$$;

-- ─────────────────────────────────────────────────────────────────
-- 11. 外键列补充缺失索引
-- ─────────────────────────────────────────────────────────────────
CREATE INDEX IF NOT EXISTS idx_app_ddl_admin_user_id         ON public.app_ddl (admin_user_id);
CREATE INDEX IF NOT EXISTS idx_announcement_adm_user_id      ON public.announcement (adm_user_id);
CREATE INDEX IF NOT EXISTS idx_adm_role_parent_id             ON public.adm_role (parent_id);
CREATE INDEX IF NOT EXISTS idx_user_friend_category_owner_uid ON public.user_friend_category (owner_user_id);
CREATE INDEX IF NOT EXISTS idx_group_log_group_id             ON public.group_log (group_id);
CREATE INDEX IF NOT EXISTS idx_conversation_client_id         ON public.conversation (client_id);

-- ─────────────────────────────────────────────────────────────────
-- 12. msg_c2g_timeline: UNIQUE INDEX 提升为 PRIMARY KEY（零额外开销）
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.msg_c2g_timeline
    ADD CONSTRAINT pk_msg_c2g_timeline
    PRIMARY KEY USING INDEX uk_c2g_timeline_ToUid_MsgId;

-- ─────────────────────────────────────────────────────────────────
-- 13. smallint → boolean（纯 0/1 布尔标志列）
-- ─────────────────────────────────────────────────────────────────
-- config.system: "是否为系统配置" (0=否, 1=是)
ALTER TABLE public.config
    ALTER COLUMN system TYPE boolean
    USING CASE WHEN system = 1 THEN true ELSE false END;
ALTER TABLE public.config
    ALTER COLUMN system SET DEFAULT false;

-- announcement.pinned: "是否置顶: 0 否, 1 是"
ALTER TABLE public.announcement
    ALTER COLUMN pinned TYPE boolean
    USING CASE WHEN pinned = 1 THEN true ELSE false END;
ALTER TABLE public.announcement
    ALTER COLUMN pinned SET DEFAULT false;

-- 将低效全表偏低基数索引替换为偏索引（仅索引置顶行，体积减小 ~99%）
DROP INDEX IF EXISTS idx_announcement_pinned;
CREATE INDEX IF NOT EXISTS idx_announcement_pinned_true
    ON public.announcement (created_at DESC)
    WHERE pinned = true;

-- group_member.is_join: "是否加入的群: 1 是 0 否"
ALTER TABLE public.group_member
    ALTER COLUMN is_join TYPE boolean
    USING CASE WHEN is_join = 1 THEN true ELSE false END;
ALTER TABLE public.group_member
    ALTER COLUMN is_join SET DEFAULT false;

-- ─────────────────────────────────────────────────────────────────
-- 14. 类型精确修复
-- ─────────────────────────────────────────────────────────────────
-- adm_user.role_id: integer[] → bigint[]（角色 ID 与其他表对齐）
ALTER TABLE public.adm_user
    ALTER COLUMN role_id TYPE bigint[]
    USING role_id::bigint[];

-- live_room.tag_id: INT → bigint（与全局 ID 规范对齐）
ALTER TABLE public.live_room
    ALTER COLUMN tag_id TYPE bigint;

-- ─────────────────────────────────────────────────────────────────
-- 15. fts_user.allow_search 值域约束
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.fts_user
    ADD CONSTRAINT chk_fts_user_allow_search
    CHECK (allow_search IN (1, 2));

-- ─────────────────────────────────────────────────────────────────
-- 15-fix. 修正 feedback.status CHECK（原遗漏值 3=已完结）
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.feedback DROP CONSTRAINT IF EXISTS chk_feedback_status;
ALTER TABLE public.feedback
    ADD CONSTRAINT chk_feedback_status CHECK (status IN (-1, 0, 1, 2, 3));

-- ─────────────────────────────────────────────────────────────────
-- 16. adm_user 关键补丁
-- ─────────────────────────────────────────────────────────────────
-- 16a. updated_at 列（section-7 触发器需要此列，原表缺失）
ALTER TABLE public.adm_user
    ADD COLUMN IF NOT EXISTS updated_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL;

-- 16b. mobile/email 唯一索引 → 部分唯一索引（允许多行 NULL）
DROP INDEX IF EXISTS "uk_Adm_Mobile";
DROP INDEX IF EXISTS "uk_Adm_Email";
CREATE UNIQUE INDEX IF NOT EXISTS uk_adm_mobile
    ON public.adm_user (mobile) WHERE mobile IS NOT NULL AND mobile <> '';
CREATE UNIQUE INDEX IF NOT EXISTS uk_adm_email
    ON public.adm_user (email)  WHERE email  IS NOT NULL AND email  <> '';

-- 16c. status 值域约束
ALTER TABLE public.adm_user
    ADD CONSTRAINT chk_adm_user_status CHECK (status IN (-1, 0, 1));

-- 16d. role_id GIN 索引（支持 @> 查询"拥有某角色的所有管理员"）
CREATE INDEX IF NOT EXISTS idx_adm_user_role_id ON public.adm_user USING GIN(role_id);

-- ─────────────────────────────────────────────────────────────────
-- 17. attachment 数据类型修复
-- ─────────────────────────────────────────────────────────────────
-- size/referer_time int → bigint（文件 > 2 GB 时 int 溢出）
ALTER TABLE public.attachment ALTER COLUMN size         TYPE bigint;
ALTER TABLE public.attachment ALTER COLUMN referer_time TYPE bigint;

-- info: text → jsonb（原注释"附件信息json"，启用 JSON 操作符）
ALTER TABLE public.attachment
    ALTER COLUMN info TYPE jsonb
    USING CASE WHEN info IS NULL OR trim(info) = '' THEN '{}'::jsonb
               ELSE info::jsonb END;
ALTER TABLE public.attachment ALTER COLUMN info SET DEFAULT '{}';

-- ─────────────────────────────────────────────────────────────────
-- 18. fts_user 触发器冲突修复
-- ─────────────────────────────────────────────────────────────────
-- 18a. 删除旧触发器（与 section-8 的 trg_user_fts 冲突，造成双重写入）
DROP TRIGGER IF EXISTS imboy_for_fts_user ON public."user";

-- 18b. 升级 sync_fts_user：补充 DELETE 分支 + 纳入 sign/region 字段
CREATE OR REPLACE FUNCTION public.sync_fts_user()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
    IF (TG_OP = 'DELETE') THEN
        DELETE FROM public.fts_user WHERE user_id = OLD.id;
        RETURN OLD;
    END IF;
    INSERT INTO public.fts_user (user_id, token)
    VALUES (
        NEW.id,
        to_tsvector('jiebacfg',
            COALESCE(NEW.nickname, '') || ' ' ||
            COALESCE(NEW.account,  '') || ' ' ||
            COALESCE(NEW.mobile,   '') || ' ' ||
            COALESCE(NEW.sign,     '') || ' ' ||
            COALESCE(NEW.region,   '')
        )
    )
    ON CONFLICT (user_id) DO UPDATE
        SET token = to_tsvector('jiebacfg',
            COALESCE(NEW.nickname, '') || ' ' ||
            COALESCE(NEW.account,  '') || ' ' ||
            COALESCE(NEW.mobile,   '') || ' ' ||
            COALESCE(NEW.sign,     '') || ' ' ||
            COALESCE(NEW.region,   '')
        );
    RETURN NEW;
END;
$$;

-- 18c. 重建触发器：覆盖 DELETE + sign/region 字段变更
DROP TRIGGER IF EXISTS trg_user_fts ON public."user";
CREATE TRIGGER trg_user_fts
    AFTER INSERT OR UPDATE OF nickname, account, mobile, sign, region OR DELETE
    ON public."user"
    FOR EACH ROW EXECUTE FUNCTION public.sync_fts_user();

-- ─────────────────────────────────────────────────────────────────
-- 19. 补充 boolean 转换
-- ─────────────────────────────────────────────────────────────────
-- app_version.force_update: "1 是  2 否" → boolean
ALTER TABLE public.app_version
    ALTER COLUMN force_update TYPE boolean USING (force_update = 1);
ALTER TABLE public.app_version
    ALTER COLUMN force_update SET DEFAULT false;

-- app_version_policy.grayscale_enabled: "0 关闭  1 启用" → boolean
ALTER TABLE public.app_version_policy
    ALTER COLUMN grayscale_enabled TYPE boolean USING (grayscale_enabled = 1);
ALTER TABLE public.app_version_policy
    ALTER COLUMN grayscale_enabled SET DEFAULT false;

-- msg_c2g_timeline.client_ack（section-12 已将 UNIQUE→PK）
ALTER TABLE public.msg_c2g_timeline
    ALTER COLUMN client_ack TYPE boolean USING (client_ack = 1);
ALTER TABLE public.msg_c2g_timeline
    ALTER COLUMN client_ack SET DEFAULT false;
DROP INDEX IF EXISTS idx_c2g_timeline_ToUid_ClientAck;
CREATE INDEX IF NOT EXISTS idx_c2g_timeline_to_uid_pending
    ON public.msg_c2g_timeline (to_uid) WHERE client_ack = false;

-- ─────────────────────────────────────────────────────────────────
-- 20. 补充业务 CHECK 约束
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.app_version
    ADD CONSTRAINT chk_app_version_grayscale
    CHECK (grayscale_percent BETWEEN 0 AND 100);
ALTER TABLE public.app_version
    ADD CONSTRAINT chk_app_version_upgrade_type
    CHECK (upgrade_type IN ('force', 'recommend', 'silent'));

ALTER TABLE public.push_token
    ADD CONSTRAINT chk_push_token_device_type CHECK (device_type IN ('android', 'ios', 'web')),
    ADD CONSTRAINT chk_push_token_platform    CHECK (platform    IN ('fcm', 'apns', 'web_push')),
    ADD CONSTRAINT chk_push_token_status      CHECK (status      IN (0, 1));

ALTER TABLE public."user"
    ALTER COLUMN gender TYPE smallint USING gender::smallint;
ALTER TABLE public."user"
    ADD CONSTRAINT chk_user_gender CHECK (gender IN (0, 1, 2, 3)),
    ADD CONSTRAINT chk_user_status CHECK (status IN (-1, 0, 1, 2));

ALTER TABLE public."group"
    ADD CONSTRAINT chk_group_member_max   CHECK (member_max   > 0),
    ADD CONSTRAINT chk_group_member_count CHECK (member_count >= 0),
    ADD CONSTRAINT chk_group_owner_uid    CHECK (owner_uid    > 0),
    ADD CONSTRAINT chk_group_creator_uid  CHECK (creator_uid  > 0);

-- group_vote_record.option_ids: TEXT → JSONB（存储 JSON 数组，支持 GIN 查询）
ALTER TABLE group_vote_record
    ALTER COLUMN option_ids TYPE jsonb USING option_ids::jsonb;
ALTER TABLE group_vote_record
    ALTER COLUMN option_ids SET DEFAULT '[]';
CREATE INDEX IF NOT EXISTS idx_group_vote_record_options
    ON group_vote_record USING GIN(option_ids);

-- group_file
ALTER TABLE group_file ALTER COLUMN file_name TYPE text;
ALTER TABLE group_file
    ADD CONSTRAINT chk_group_file_status         CHECK (status         IN (0, 1)),
    ADD CONSTRAINT chk_group_file_download_count CHECK (download_count >= 0);

-- group_album / group_album_photo
ALTER TABLE group_album
    ADD CONSTRAINT chk_group_album_photo_count CHECK (photo_count >= 0);
ALTER TABLE group_album_photo
    ADD CONSTRAINT chk_photo_like_count    CHECK (like_count    >= 0),
    ADD CONSTRAINT chk_photo_comment_count CHECK (comment_count >= 0);

-- group_notice: body varchar → text；修正重复注释
ALTER TABLE public.group_notice ALTER COLUMN body TYPE text;
COMMENT ON COLUMN public.group_notice.updated_at IS '最后更新时间';
COMMENT ON COLUMN public.group_notice.expired_at IS '公告有效期截止时间';

-- group_random_code.validity_at: DEFAULT CURRENT_TIMESTAMP（建表即过期）→ NULL
ALTER TABLE public.group_random_code ALTER COLUMN validity_at DROP DEFAULT;
ALTER TABLE public.group_random_code ALTER COLUMN validity_at DROP NOT NULL;
COMMENT ON COLUMN public.group_random_code.validity_at IS '有效期截止时间，NULL 表示永久有效';

-- group_schedule: 时间合法性约束
ALTER TABLE group_schedule
    ADD CONSTRAINT chk_schedule_time          CHECK (end_at > start_at),
    ADD CONSTRAINT chk_schedule_remind_before CHECK (remind_before IS NULL OR remind_before > 0);

-- group_schedule_remind: 单列 is_sent 索引 → 部分索引（仅未发送行，体积减小 ~90%）
DROP INDEX IF EXISTS idx_group_schedule_remind_is_sent;
CREATE INDEX IF NOT EXISTS idx_group_schedule_remind_pending
    ON group_schedule_remind (remind_at) WHERE is_sent = false;

-- live_room
ALTER TABLE live_room
    ADD CONSTRAINT chk_live_room_status CHECK (status IN (0, 1, 2));
ALTER TABLE live_room ALTER COLUMN viewer_count SET NOT NULL;
ALTER TABLE live_room ALTER COLUMN viewer_count SET DEFAULT 0;

-- ─────────────────────────────────────────────────────────────────
-- 21. 消息系统类型约束 & 冗余索引清理
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.conversation
    ADD CONSTRAINT chk_conversation_type
    CHECK (type IN ('', 'C2C', 'C2G', 'C2S', 'S2C'));

-- msg_reaction: 值域约束 + 删除被 idx_msg_reaction_emoji 完全覆盖的冗余索引
ALTER TABLE msg_reaction
    ADD CONSTRAINT chk_msg_reaction_msg_type CHECK (msg_type IN ('c2c', 'c2g'));
DROP INDEX IF EXISTS idx_msg_reaction_msg;

-- msg_store: chat_type 约束 + C2G 场景缺失的 group_id 索引
ALTER TABLE public.msg_store
    ADD CONSTRAINT chk_msg_store_chat_type CHECK (chat_type IN ('c2c', 'c2g'));
CREATE INDEX IF NOT EXISTS i_msg_store_group_id
    ON public.msg_store (group_id, created_at DESC) WHERE group_id IS NOT NULL;

-- user_group.setting: text → jsonb
ALTER TABLE public.user_group
    ALTER COLUMN setting TYPE jsonb
    USING CASE WHEN setting IS NULL OR trim(setting) = '' THEN '{}'::jsonb
               ELSE setting::jsonb END;
ALTER TABLE public.user_group ALTER COLUMN setting SET DEFAULT '{}';

-- ─────────────────────────────────────────────────────────────────
-- 22. channel / moment 修复
-- ─────────────────────────────────────────────────────────────────
-- channel.tags: varchar → jsonb + GIN 索引（支持标签查询）
ALTER TABLE public.channel
    ALTER COLUMN tags TYPE jsonb
    USING CASE WHEN tags IS NULL OR trim(tags) = '' THEN '[]'::jsonb
               ELSE tags::jsonb END;
ALTER TABLE public.channel ALTER COLUMN tags SET DEFAULT '[]';
CREATE INDEX IF NOT EXISTS i_channel_tags ON public.channel USING GIN(tags);

ALTER TABLE public.channel
    ADD CONSTRAINT chk_channel_subscriber_count CHECK (subscriber_count >= 0);

-- channel_message: payload / reaction_summary text → jsonb
ALTER TABLE public.channel_message
    ALTER COLUMN payload TYPE jsonb
    USING CASE WHEN payload IS NULL OR trim(payload) = '' THEN '{}'::jsonb
               ELSE payload::jsonb END;
ALTER TABLE public.channel_message ALTER COLUMN payload SET DEFAULT '{}';

ALTER TABLE public.channel_message
    ALTER COLUMN reaction_summary TYPE jsonb
    USING CASE WHEN reaction_summary IS NULL OR trim(reaction_summary) = '' THEN '{}'::jsonb
               ELSE reaction_summary::jsonb END;
ALTER TABLE public.channel_message ALTER COLUMN reaction_summary SET DEFAULT '{}';

ALTER TABLE public.moment_post
    ADD CONSTRAINT chk_moment_post_like_count    CHECK (like_count    >= 0),
    ADD CONSTRAINT chk_moment_post_comment_count CHECK (comment_count >= 0);

-- ─────────────────────────────────────────────────────────────────
-- 23. compliance_key NULL 语义修复
-- ─────────────────────────────────────────────────────────────────
-- created_by / revoked_by 用 0 代 NULL → 还原为 NULL（0 不是有效用户 ID）
UPDATE public.compliance_key SET created_by = NULL WHERE created_by = 0;
UPDATE public.compliance_key SET revoked_by = NULL WHERE revoked_by = 0;
ALTER TABLE public.compliance_key ALTER COLUMN created_by DROP DEFAULT;
ALTER TABLE public.compliance_key ALTER COLUMN revoked_by DROP DEFAULT;
CREATE INDEX IF NOT EXISTS idx_compliance_key_created_by
    ON public.compliance_key (created_by) WHERE created_by IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_compliance_key_revoked_by
    ON public.compliance_key (revoked_by) WHERE revoked_by IS NOT NULL;

-- ─────────────────────────────────────────────────────────────────
-- 24. feedback / report_ticket / plugin_audit_log 修复
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.feedback
    ADD CONSTRAINT chk_feedback_type   CHECK (type   IS NULL OR type   IN ('bugReport', 'featureRequest')),
    ADD CONSTRAINT chk_feedback_rating CHECK (rating IS NULL OR rating IN ('bad', 'neutral', 'good'));

-- feedback_reply: 外键 + feedback_id 索引
ALTER TABLE public.feedback_reply
    ADD CONSTRAINT fk_feedback_reply_feedback
    FOREIGN KEY (feedback_id) REFERENCES public.feedback(id) ON DELETE CASCADE;
CREATE INDEX IF NOT EXISTS idx_feedback_reply_feedback_id
    ON public.feedback_reply (feedback_id, created_at DESC);

-- report_ticket: 防止同一用户对同一目标重复举报 + handled_by 索引
ALTER TABLE public.report_ticket
    ADD CONSTRAINT uk_report_ticket_target_reporter
    UNIQUE (target_type, target_id, reporter_uid);
CREATE INDEX IF NOT EXISTS idx_report_ticket_handled_by
    ON public.report_ticket (handled_by) WHERE handled_by IS NOT NULL;

-- plugin_audit_log.result 值域约束
ALTER TABLE public.plugin_audit_log
    ADD CONSTRAINT chk_plugin_audit_log_result
    CHECK (result IS NULL OR result IN ('ok', 'failed', 'cancelled', 'timeout'));

-- ─────────────────────────────────────────────────────────────────
-- 25. wallet 修复
-- ─────────────────────────────────────────────────────────────────
-- 25a. user_id UNIQUE 列约束自带索引，手动 CREATE UNIQUE INDEX 重复，删除节省写入开销
DROP INDEX IF EXISTS uk_wallet_UserId;

-- 25b. 业务 CHECK 约束
ALTER TABLE public.wallet
    ADD CONSTRAINT chk_wallet_balance CHECK (balance >= 0),
    ADD CONSTRAINT chk_wallet_frozen  CHECK (frozen  >= 0),
    ADD CONSTRAINT chk_wallet_status  CHECK (status  IN (0, 1, 2));

ALTER TABLE public.wallet_transaction
    ADD CONSTRAINT chk_wallet_tx_type   CHECK (tx_type IN (1, 2, 3, 4)),
    ADD CONSTRAINT chk_wallet_tx_status CHECK (status  IN (0, 1, 2));

-- 25c. reference_no 索引（按单号查流水高频操作）
CREATE INDEX IF NOT EXISTS idx_wallet_tx_reference_no
    ON public.wallet_transaction (reference_no) WHERE reference_no <> '';

-- 25d. wallet_id 外键约束（流水必须归属有效钱包）
ALTER TABLE public.wallet_transaction
    ADD CONSTRAINT fk_wallet_tx_wallet
    FOREIGN KEY (wallet_id) REFERENCES public.wallet(id);

-- ─────────────────────────────────────────────────────────────────
-- 26. 补充缺失的关键索引
-- ─────────────────────────────────────────────────────────────────
CREATE INDEX IF NOT EXISTS idx_group_album_photo_uploader
    ON group_album_photo (uploader_id);
CREATE INDEX IF NOT EXISTS idx_group_album_photo_comment_user
    ON group_album_photo_comment (user_id);
CREATE INDEX IF NOT EXISTS idx_msg_mention_from_uid
    ON msg_mention (from_uid, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_group_member_category
    ON public.group_member (user_id, category_id) WHERE category_id > 0;

-- ─────────────────────────────────────────────────────────────────
-- 27. 消息转发 / msg_topic / 会话子表：类型字段枚举约束
-- ─────────────────────────────────────────────────────────────────
ALTER TABLE public.msg_forward
    ADD CONSTRAINT chk_msg_forward_original_type CHECK (original_type IN ('c2c', 'c2g')),
    ADD CONSTRAINT chk_msg_forward_forward_type  CHECK (forward_type  IN ('c2c', 'c2g'));

ALTER TABLE public.msg_topic
    ADD CONSTRAINT chk_msg_topic_type CHECK (type IN ('', 'C2S', 'C2G'));

ALTER TABLE public.conversation_pin
    ADD CONSTRAINT chk_conversation_pin_type CHECK (conversation_type IN ('c2c', 'c2g'));

ALTER TABLE public.conversation_delete
    ADD CONSTRAINT chk_conversation_delete_type CHECK (conversation_type IN ('c2c', 'c2g'));

-- ─────────────────────────────────────────────────────────────────
-- 28. user_log 字段修复
-- ─────────────────────────────────────────────────────────────────
-- type: int4 → smallint（用户操作 100/102/110；管理审计 901/902/903）
ALTER TABLE public.user_log
    ALTER COLUMN type TYPE smallint;
ALTER TABLE public.user_log
    ADD CONSTRAINT chk_user_log_type CHECK (type IN (100, 102, 110, 901, 902, 903));

-- body: 存储 JSON 文本，改用 jsonb 支持运算符查询
ALTER TABLE public.user_log
    ALTER COLUMN body TYPE jsonb USING body::jsonb;

-- ─────────────────────────────────────────────────────────────────
-- 29. adm_role 约束补全
-- ─────────────────────────────────────────────────────────────────
-- role_name 部分唯一索引：删除状态(-1)可重名，其他状态不可重名
CREATE UNIQUE INDEX IF NOT EXISTS uk_adm_role_name_active
    ON public.adm_role (role_name) WHERE status <> -1;

ALTER TABLE public.adm_role
    ADD CONSTRAINT chk_adm_role_status CHECK (status IN (-1, 0, 1)),
    ADD CONSTRAINT chk_adm_role_sort   CHECK (sort >= 0);

-- ─────────────────────────────────────────────────────────────────
-- 30. msg_s2c 修复
-- ─────────────────────────────────────────────────────────────────
-- 30a. 删除无实际收益的表达式索引
--   i_s2c_e2ee 在常量表达式 (e2ee IS NOT NULL) 上建索引，
--   WHERE e2ee IS NOT NULL 已过滤，索引列仅存储全为 true 的常量，毫无意义
DROP INDEX IF EXISTS i_s2c_e2ee;

-- 30b. payload text → jsonb（超表：需先解压，改完再压）
SELECT remove_compression_policy('msg_s2c', if_exists => true);
SELECT decompress_chunk(c) FROM show_chunks('msg_s2c') c;
ALTER TABLE public.msg_s2c ALTER COLUMN payload TYPE jsonb USING payload::jsonb;
ALTER TABLE public.msg_s2c SET (
    timescaledb.compress,
    timescaledb.compress_orderby   = 'created_at DESC',
    timescaledb.compress_segmentby = 'to_id'
);
SELECT add_compression_policy('msg_s2c', INTERVAL '3 days', if_not_exists => true);

-- ─────────────────────────────────────────────────────────────────
-- 31. e2ee 表修复
-- ─────────────────────────────────────────────────────────────────
-- 31a. e2ee_transfer_sessions.status 值域约束（注释仅列举，无 CHECK）
ALTER TABLE public.e2ee_transfer_sessions
    ADD CONSTRAINT chk_e2ee_transfer_sessions_status
    CHECK (status IN ('pending', 'accepted', 'confirmed', 'expired', 'cancelled'));

-- 31b. e2ee_trusted_contacts: 低基数单列 status 索引无效
--   替换为 uid 部分索引（活跃联系人），覆盖最常见的查询模式
DROP INDEX IF EXISTS idx_e2ee_trusted_contacts_status;
CREATE INDEX IF NOT EXISTS idx_e2ee_trusted_contacts_uid_active
    ON public.e2ee_trusted_contacts (uid) WHERE status = 'active';

-- 31c. e2ee_local_backups: 补充版本唯一约束（同设备同版本只能有一条）
ALTER TABLE public.e2ee_local_backups
    ADD CONSTRAINT uk_e2ee_local_backup_version UNIQUE (uid, device_id, backup_version);

-- ─────────────────────────────────────────────────────────────────
-- 32. channel_invitation / channel_price 修复
-- ─────────────────────────────────────────────────────────────────
-- 32a. channel_invitation: UNIQUE(channel_id, invitee_uid) 全局唯一会阻止被拒/过期后重新邀请
--   改为部分唯一索引，仅 pending(0) 状态生效
ALTER TABLE public.channel_invitation
    DROP CONSTRAINT IF EXISTS uk_channel_invitation_active;
CREATE UNIQUE INDEX IF NOT EXISTS uk_channel_invitation_pending
    ON public.channel_invitation (channel_id, invitee_uid) WHERE status = 0;

-- 32b. channel_price: channel_id UNIQUE 列约束已内含 B-tree 索引，手动索引重复
DROP INDEX IF EXISTS idx_channel_price_channel_id;

-- ─────────────────────────────────────────────────────────────────
-- 33. group_task_assignment + group_notice 修复
-- ─────────────────────────────────────────────────────────────────
-- 33a. 已批改(status=3)时必须记录批改人，否则数据不完整
ALTER TABLE public.group_task_assignment
    ADD CONSTRAINT chk_task_assignment_reviewed
    CHECK (status <> 3 OR reviewed_by IS NOT NULL);

-- 33b. group_notice.edit_user_id: DEFAULT 0 与用户 ID 0 语义冲突
--   "从未被编辑"应为 NULL，而非 0
ALTER TABLE public.group_notice
    ALTER COLUMN edit_user_id DROP NOT NULL,
    ALTER COLUMN edit_user_id SET DEFAULT NULL;
UPDATE public.group_notice SET edit_user_id = NULL WHERE edit_user_id = 0;

-- ─────────────────────────────────────────────────────────────────
-- 34. 杂项修复
-- ─────────────────────────────────────────────────────────────────
-- 34a. user_denylist.denied_user_id: DEFAULT '0'（字符串字面量）→ DEFAULT 0（整数）
--   bigint 列赋字符串默认值依赖隐式类型转换，存在隐患
ALTER TABLE public.user_denylist
    ALTER COLUMN denied_user_id SET DEFAULT 0;

-- 34b. app_ddl.status 值域约束（注释已标注 -1/0/1，但无 CHECK）
ALTER TABLE public.app_ddl
    ADD CONSTRAINT chk_app_ddl_status CHECK (status IN (-1, 0, 1));

-- 34c. user_tag_relation: scene/user_id/tag_id 可为 NULL
--   触发器直接引用这三列做 UPDATE，NULL 会导致触发器匹配不到行（静默失效）
ALTER TABLE public.user_tag_relation
    ALTER COLUMN scene   SET NOT NULL,
    ALTER COLUMN user_id SET NOT NULL,
    ALTER COLUMN tag_id  SET NOT NULL;

-- 34d. verification_code.code: 验证码字段可为 NULL，逻辑上不合理
ALTER TABLE public.verification_code
    ALTER COLUMN code SET NOT NULL;

-- ─────────────────────────────────────────────────────────────────
-- 35. announcement / msg_c2g / app_upgrade_log 修复
-- ─────────────────────────────────────────────────────────────────
-- 35a. announcement: status/type 值域约束（注释已写明枚举，无 CHECK）
ALTER TABLE public.announcement
    ADD CONSTRAINT chk_announcement_status CHECK (status IN (-1, 0, 1, 2)),
    ADD CONSTRAINT chk_announcement_type   CHECK (type IN ('info', 'warning', 'important'));

-- 35b. msg_c2g: 同 msg_s2c，i_c2g_e2ee 对常量表达式建索引，无实际收益
DROP INDEX IF EXISTS i_c2g_e2ee;

-- 35c. msg_c2g.payload text → jsonb（超表：先解压，改类型，重新压缩）
SELECT remove_compression_policy('msg_c2g', if_exists => true);
SELECT decompress_chunk(c) FROM show_chunks('msg_c2g') c;
ALTER TABLE public.msg_c2g ALTER COLUMN payload TYPE jsonb USING payload::jsonb;
ALTER TABLE public.msg_c2g SET (
    timescaledb.compress,
    timescaledb.compress_orderby   = 'created_at DESC',
    timescaledb.compress_segmentby = 'to_id'
);
SELECT add_compression_policy('msg_c2g', INTERVAL '3 days', if_not_exists => true);

-- 35d. app_upgrade_log: 枚举字段补 CHECK，防止脏数据污染统计报表
ALTER TABLE public.app_upgrade_log
    ADD CONSTRAINT chk_app_upgrade_log_cos
        CHECK (cos IN ('android', 'ios', 'web', 'macos')),
    ADD CONSTRAINT chk_app_upgrade_log_event
        CHECK (event IN ('check','prompted','download_start','download_done',
                         'verify_ok','verify_fail','install','cancel','error')),
    ADD CONSTRAINT chk_app_upgrade_log_upgrade_type
        CHECK (upgrade_type IN ('force', 'recommend', 'silent', 'none'));

COMMIT;




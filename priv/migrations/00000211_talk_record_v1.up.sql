-- 00000211_talk_record_v1.up.sql
-- 面聊记录 talk_record V1 首版表（17 张，不含 voice_profile* 声纹表）。
-- 契约：docs/design/2026-10-20-face-to-face-notes/BACKEND-API.md
--       §2 逻辑表全量 / §8 FK 增强 / §10.2 attachment_ref / §11 V3 收敛补充
--       CONTRACT-FREEZE-T02.md §2（编号 00000211、信封/幂等/错误码惯例）
-- 迁移契约：up=可重复执行（新表 IF NOT EXISTS 语义由 erlang_migrate 单事务保证），
--           down=安全回滚（有业务数据禁止回退）。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策：
--   * 主键 id bigint TSID 由应用层生成（elib_tsid），与 "user"/workspace/organization 同构，
--     不使用 DB 序列/默认值（对齐 00000001 foundation / 00000076 / 00000095 惯例）。
--   * user 外键删除策略跟随仓库现状：
--     - NOT NULL 归属/创建锚点（owner_user_id、created_by、grantee、started_by 等）
--       用 ON DELETE RESTRICT——对齐 00000126 organization.owner_id 所有权不变量；
--       设计 §11 要求创建者注销不得级联删除记录，RESTRICT fail-closed，
--       由账号注销执行器（user_deletion_executor 声明式处置）显式处理，不 CASCADE。
--     - 可空审计/署名列（actor_id、matched_user_id、confirmed_by、edited_by 等）
--       用 ON DELETE SET NULL——对齐 enterprise 域主流（00000114/00000119 等）。
--   * organization / workspace / attachment 引用一律 ON DELETE RESTRICT
--     （对齐 00000168 workspace_attachment_retention、00000095 workspace.organization_id）。
--   * 复合 FK 目标唯一约束：capture/speaker/transcript 建 uq_<t>_record_id (record_id, id)，
--     防止跨记录 ID 混用（设计 §8），与 00000114 uq_obi_org_id 同款模式。
--   * 子表对主表不写 ON DELETE CASCADE：删除走软删（deleted_at + cleanup_status）
--     + 显式清理工作流（设计 §2 "note 子表 FK 删除由明确清理工作流处理"）。
--   * capture 活跃唯一（同设备 / 同记录）为部分唯一索引；活跃集合 =
--     draft/recording/paused/interrupted（未 ended 即占用，配合 §11.1 lease/fencing）。
SET LOCAL lock_timeout = '5s';

-- ============================================================
-- 1. talk_record 主表
-- ============================================================
CREATE TABLE talk_record (
    id                           bigint                       NOT NULL,  -- TSID（应用层生成）
    owner_user_id                bigint                       NOT NULL,  -- 当前管理责任人（§9）
    created_by                   bigint                       NOT NULL,  -- 创建事实主体（§9；注销不级联）
    client_recording_id          uuid                         NOT NULL,  -- 客户端离线 UUID（§11 幂等身份）
    source_type                  text                         DEFAULT 'live_recording' NOT NULL,
    scope                        text                         NOT NULL,
    organization_id              bigint,                                 -- NULL=personal（ck 组合校验）
    workspace_id                 bigint,                                 -- 仅企业记录可选
    title                        character varying(120)       DEFAULT '' NOT NULL,
    language                     character varying(32)        DEFAULT '' NOT NULL,
    active_capture_id            bigint,                                 -- 当前活跃采集投影（复合 FK）
    capture_status               text                         DEFAULT 'draft' NOT NULL,
    media_status                 text                         DEFAULT 'local_only' NOT NULL,
    transcript_status            text                         DEFAULT 'not_requested' NOT NULL,
    summary_status               text                         DEFAULT 'not_requested' NOT NULL,
    processing_mode              text                         DEFAULT 'record_only' NOT NULL,
    duration_ms                  bigint                       DEFAULT 0 NOT NULL,
    audio_manifest_attachment_id bigint,                                 -- 聚合清单附件（§10.1）
    current_transcript_id        bigint,                                 -- 当前发布转写版本（复合 FK）
    content_revision             bigint                       DEFAULT 1 NOT NULL,
    notes                        text                         DEFAULT '' NOT NULL,
    revision                     bigint                       DEFAULT 1 NOT NULL,
    acl_revision                 bigint                       DEFAULT 1 NOT NULL,
    custody_status               text                         DEFAULT 'active' NOT NULL,
    ended_at                     timestamp with time zone,
    retention_until              timestamp with time zone,
    cleanup_status               text                         DEFAULT 'none' NOT NULL,
    deleted_at                   timestamp with time zone,
    created_at                   timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at                   timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_talk_record PRIMARY KEY (id),
    -- 个人/企业归属组合（设计 §1/§2）：personal → org/ws 均 NULL；organization → org 必填。
    CONSTRAINT ck_tr_scope_owner CHECK (
        (scope = 'personal' AND organization_id IS NULL AND workspace_id IS NULL)
        OR (scope = 'organization' AND organization_id IS NOT NULL)),
    CONSTRAINT ck_tr_source_type CHECK (source_type = ANY (ARRAY['live_recording'::text, 'imported_audio'::text, 'remote_call'::text])),
    CONSTRAINT ck_tr_scope CHECK (scope = ANY (ARRAY['personal'::text, 'organization'::text])),
    -- V1 API 仅接受 live_recording（§8），imported_audio/remote_call 为未来扩展占位。
    CONSTRAINT ck_tr_capture_status CHECK (capture_status = ANY (ARRAY['draft'::text, 'recording'::text, 'paused'::text, 'interrupted'::text, 'ended'::text])),
    CONSTRAINT ck_tr_media_status CHECK (media_status = ANY (ARRAY['local_only'::text, 'uploading'::text, 'verifying'::text, 'assembling'::text, 'available'::text, 'retryable_failed'::text])),
    CONSTRAINT ck_tr_transcript_status CHECK (transcript_status = ANY (ARRAY['not_requested'::text, 'unavailable'::text, 'queued'::text, 'running'::text, 'ready'::text, 'failed'::text])),
    CONSTRAINT ck_tr_summary_status CHECK (summary_status = ANY (ARRAY['not_requested'::text, 'unavailable'::text, 'queued'::text, 'running'::text, 'ready'::text, 'failed'::text])),
    CONSTRAINT ck_tr_processing_mode CHECK (processing_mode = ANY (ARRAY['record_only'::text, 'transcribe'::text, 'summarize'::text])),
    CONSTRAINT ck_tr_duration CHECK (duration_ms >= 0),
    CONSTRAINT ck_tr_content_revision CHECK (content_revision >= 1),
    CONSTRAINT ck_tr_revision CHECK (revision >= 1),
    CONSTRAINT ck_tr_acl_revision CHECK (acl_revision >= 1),
    CONSTRAINT ck_tr_custody_status CHECK (custody_status = ANY (ARRAY['active'::text, 'handover_required'::text])),
    CONSTRAINT ck_tr_cleanup_status CHECK (cleanup_status = ANY (ARRAY['none'::text, 'pending'::text, 'blocked_retention'::text, 'blocked_hold'::text, 'retained_by_other_reference'::text, 'failed'::text, 'complete'::text]))
);

COMMENT ON TABLE  talk_record IS '面聊记录主表（个人/企业共用，scope 仅 personal|organization；软删 + cleanup_status）';
COMMENT ON COLUMN talk_record.owner_user_id IS '当前管理责任人（§9 移交只改此列，不改 created_by/scope）';
COMMENT ON COLUMN talk_record.created_by IS '创建事实主体；UNIQUE(created_by,client_recording_id) 为离线重放幂等键（§11）';
COMMENT ON COLUMN talk_record.client_recording_id IS '客户端生成的录音 UUID；服务器不自行生成';
COMMENT ON COLUMN talk_record.active_capture_id IS '当前活跃采集（CAS 投影；复合 FK 保证同记录）';
COMMENT ON COLUMN talk_record.content_revision IS '仅在原文/发言人署名等内容变化时推进（§11.2）';
COMMENT ON COLUMN talk_record.acl_revision IS '授权/撤销/失效/责任移交时推进（§11.2）';
COMMENT ON COLUMN talk_record.custody_status IS '管理责任状态：active | handover_required（§9 原 owner 失权待移交）';
COMMENT ON COLUMN talk_record.cleanup_status IS '删除清理状态（§10.4）；仅协调，不执行存储 DELETE';
COMMENT ON COLUMN talk_record.notes IS '跟随记录 ACL 的共享补充笔记，不是个人私密笔记';

-- 创建幂等唯一键（§11 V3：created_by + client_recording_id，移交 owner 不改变幂等身份）
CREATE UNIQUE INDEX uq_tr_creator_client ON talk_record (created_by, client_recording_id);

-- owner 列表（§2 索引清单）
CREATE INDEX i_tr_owner_list ON talk_record (owner_user_id, created_at DESC, id DESC)
    WHERE deleted_at IS NULL;
-- 企业检索（仅检索不授权；不含 personal 行）
CREATE INDEX i_tr_org_ws ON talk_record (organization_id, workspace_id, created_at DESC, id DESC)
    WHERE organization_id IS NOT NULL;

ALTER TABLE talk_record DROP CONSTRAINT IF EXISTS fk_tr_owner_user;
ALTER TABLE talk_record ADD CONSTRAINT fk_tr_owner_user
    FOREIGN KEY (owner_user_id) REFERENCES "user"(id) ON DELETE RESTRICT;
ALTER TABLE talk_record DROP CONSTRAINT IF EXISTS fk_tr_created_by;
ALTER TABLE talk_record ADD CONSTRAINT fk_tr_created_by
    FOREIGN KEY (created_by) REFERENCES "user"(id) ON DELETE RESTRICT;
ALTER TABLE talk_record DROP CONSTRAINT IF EXISTS fk_tr_organization;
ALTER TABLE talk_record ADD CONSTRAINT fk_tr_organization
    FOREIGN KEY (organization_id) REFERENCES organization(id) ON DELETE RESTRICT;
-- 组合 FK：workspace 必须属于同 organization（目标唯一约束由 00000116 建立）
ALTER TABLE talk_record DROP CONSTRAINT IF EXISTS fk_tr_workspace;
ALTER TABLE talk_record ADD CONSTRAINT fk_tr_workspace
    FOREIGN KEY (organization_id, workspace_id)
    REFERENCES workspace(organization_id, id) ON DELETE RESTRICT;
ALTER TABLE talk_record DROP CONSTRAINT IF EXISTS fk_tr_manifest_attachment;
ALTER TABLE talk_record ADD CONSTRAINT fk_tr_manifest_attachment
    FOREIGN KEY (audio_manifest_attachment_id) REFERENCES attachment(id) ON DELETE RESTRICT;

-- ============================================================
-- 2. talk_record_participant 参与者（≠授权≠同意，仅信息）
-- ============================================================
CREATE TABLE talk_record_participant (
    id                bigint                       NOT NULL,  -- TSID
    record_id         bigint                       NOT NULL,
    user_id           bigint,                                 -- 可空=手动标签 guest
    display_name      character varying(120)       DEFAULT '' NOT NULL,
    attendance_source text                         DEFAULT 'selected' NOT NULL,
    created_at        timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at        timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_talk_record_participant PRIMARY KEY (id),
    CONSTRAINT ck_trp_attendance_source CHECK (attendance_source = ANY (ARRAY['selected'::text, 'manual'::text])),
    CONSTRAINT fk_trp_record FOREIGN KEY (record_id) REFERENCES talk_record(id) ON DELETE RESTRICT,
    CONSTRAINT fk_trp_user FOREIGN KEY (user_id) REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE talk_record_participant IS '面聊参与者名单（仅参与者信息，不等于授权或同意；§2）';

-- 同记录同用户去重（可空 user 不参与）
CREATE UNIQUE INDEX uq_trp_record_user ON talk_record_participant (record_id, user_id)
    WHERE user_id IS NOT NULL;

-- ============================================================
-- 3. talk_record_grant 显式授权
-- ============================================================
CREATE TABLE talk_record_grant (
    id              bigint                       NOT NULL,  -- TSID
    record_id       bigint                       NOT NULL,
    grantee_user_id bigint                       NOT NULL,
    role            text                         NOT NULL,
    allow_export    boolean                      DEFAULT false NOT NULL,
    granted_by      bigint                       NOT NULL,
    revoked_at      timestamp with time zone,
    expires_at      timestamp with time zone,
    created_at      timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at      timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_talk_record_grant PRIMARY KEY (id),
    CONSTRAINT ck_trg_role CHECK (role = ANY (ARRAY['viewer'::text, 'editor'::text])),
    CONSTRAINT fk_trg_record FOREIGN KEY (record_id) REFERENCES talk_record(id) ON DELETE RESTRICT,
    CONSTRAINT fk_trg_grantee FOREIGN KEY (grantee_user_id) REFERENCES "user"(id) ON DELETE RESTRICT,
    CONSTRAINT fk_trg_granted_by FOREIGN KEY (granted_by) REFERENCES "user"(id) ON DELETE RESTRICT
);

COMMENT ON TABLE  talk_record_grant IS '面聊记录显式授权（viewer/editor；不能移除最后 owner——owner 在主表；§2）';
COMMENT ON COLUMN talk_record_grant.allow_export IS 'viewer 导出须单独 allow_export=true；editor 不可改 ACL/删除原记录';

-- 活跃授权唯一（撤销后可重新授权）
CREATE UNIQUE INDEX uq_trg_active ON talk_record_grant (record_id, grantee_user_id)
    WHERE revoked_at IS NULL;
-- grantee 活跃视图（§2 索引清单）
CREATE INDEX i_trg_grantee_active ON talk_record_grant (grantee_user_id, record_id)
    WHERE revoked_at IS NULL;

-- ============================================================
-- 4. talk_record_capture 采集会话
-- ============================================================
CREATE TABLE talk_record_capture (
    id                  bigint                       NOT NULL,  -- TSID
    record_id           bigint                       NOT NULL,
    device_session_id   uuid                         NOT NULL,
    device_id           text                         NOT NULL,
    started_by          bigint                       NOT NULL,
    source_type         text                         DEFAULT 'live_recording' NOT NULL,
    status              text                         DEFAULT 'draft' NOT NULL,
    reported_started_at timestamp with time zone,               -- 设备上报开始时间（仅显示；§3）
    server_started_at   timestamp with time zone,               -- 服务器时间用于审计
    ended_at            timestamp with time zone,
    duration_ms         bigint                       DEFAULT 0 NOT NULL,
    revision            bigint                       DEFAULT 1 NOT NULL,
    last_seen_at        timestamp with time zone,               -- §11.1 失联恢复
    lease_epoch         bigint                       DEFAULT 0 NOT NULL,
    lease_until         timestamp with time zone,               -- 弱网络过期=失联不证明停录
    created_at          timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at          timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_talk_record_capture PRIMARY KEY (id),
    CONSTRAINT ck_trc_source_type CHECK (source_type = ANY (ARRAY['live_recording'::text, 'imported_audio'::text, 'remote_call'::text])),
    CONSTRAINT ck_trc_status CHECK (status = ANY (ARRAY['draft'::text, 'recording'::text, 'paused'::text, 'interrupted'::text, 'ended'::text])),
    CONSTRAINT ck_trc_duration CHECK (duration_ms >= 0),
    CONSTRAINT ck_trc_revision CHECK (revision >= 1),
    CONSTRAINT ck_trc_lease_epoch CHECK (lease_epoch >= 0),
    CONSTRAINT fk_trc_record FOREIGN KEY (record_id) REFERENCES talk_record(id) ON DELETE RESTRICT,
    CONSTRAINT fk_trc_started_by FOREIGN KEY (started_by) REFERENCES "user"(id) ON DELETE RESTRICT,
    -- 复合 FK 目标：子表 (record_id, capture_id) 引用，防跨记录混用（§8）
    CONSTRAINT uq_trc_record_id UNIQUE (record_id, id)
);

COMMENT ON TABLE  talk_record_capture IS '面聊采集会话（每设备单一写入者；V1 每记录单 capture；§2/§11.1）';
COMMENT ON COLUMN talk_record_capture.device_session_id IS '设备录音会话 UUID（恢复/接管判据）';
COMMENT ON COLUMN talk_record_capture.lease_epoch IS '失联 fencing 纪元：用户显式结束后推进，旧设备写被拒（§11.1）';

CREATE UNIQUE INDEX uq_trc_record_device_session ON talk_record_capture (record_id, device_session_id);
-- 活跃集合 = 未 ended（draft/recording/paused/interrupted）
CREATE UNIQUE INDEX uq_trc_device_active ON talk_record_capture (device_id)
    WHERE status IN ('draft', 'recording', 'paused', 'interrupted');
CREATE UNIQUE INDEX uq_trc_record_active ON talk_record_capture (record_id)
    WHERE status IN ('draft', 'recording', 'paused', 'interrupted');

-- ============================================================
-- 5. talk_record_capture_event 采集事件
-- ============================================================
CREATE TABLE talk_record_capture_event (
    id               bigint                       NOT NULL,  -- TSID
    record_id        bigint                       NOT NULL,
    capture_id       bigint                       NOT NULL,
    device_session_id uuid                        NOT NULL,
    client_event_id  uuid                         NOT NULL,  -- 客户端事件 UUID（幂等）
    seq              bigint                       NOT NULL,
    event            text                         NOT NULL,
    position_ms      bigint                       NOT NULL,
    reported_at      timestamp with time zone     NOT NULL,  -- 设备上报时间
    received_at      timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at       timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_talk_record_capture_event PRIMARY KEY (id),
    CONSTRAINT ck_trce_seq CHECK (seq >= 0),
    CONSTRAINT ck_trce_position CHECK (position_ms >= 0),
    CONSTRAINT ck_trce_event CHECK (event = ANY (ARRAY['start'::text, 'pause'::text, 'resume'::text, 'interrupt'::text, 'end'::text])),
    CONSTRAINT fk_trce_record FOREIGN KEY (record_id) REFERENCES talk_record(id) ON DELETE RESTRICT,
    -- 复合 FK：事件必须属于同记录的 capture（§8）
    CONSTRAINT fk_trce_capture FOREIGN KEY (record_id, capture_id)
        REFERENCES talk_record_capture(record_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE talk_record_capture_event IS '面聊采集事件流（暂停/中断/离线重放顺序；§2）';

CREATE UNIQUE INDEX uq_trce_capture_client_event ON talk_record_capture_event (capture_id, client_event_id);
CREATE UNIQUE INDEX uq_trce_capture_seq ON talk_record_capture_event (capture_id, seq);

-- ============================================================
-- 6. talk_record_audio_part 音频分段
-- ============================================================
CREATE TABLE talk_record_audio_part (
    id                 bigint                       NOT NULL,  -- TSID
    record_id          bigint                       NOT NULL,
    capture_id         bigint                       NOT NULL,
    seq                integer                      NOT NULL,
    start_ms           bigint                       NOT NULL,  -- 有效音频时间轴（§11.1）
    end_ms             bigint                       NOT NULL,
    capture_offset_ms  bigint                       DEFAULT 0 NOT NULL,  -- 有效音频位置（§11.1）
    captured_wall_time timestamp with time zone,               -- 仅溯源
    attachment_id      bigint                       NOT NULL,  -- 附件唯一存储事实源（§10.1）
    sha256             character varying(64)        NOT NULL,
    byte_size          bigint                       NOT NULL,
    codec              character varying(32),
    sample_rate        integer,
    channels           smallint,
    upload_status      text                         DEFAULT 'pending' NOT NULL,
    created_at         timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at         timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_talk_record_audio_part PRIMARY KEY (id),
    CONSTRAINT ck_trap_seq CHECK (seq >= 0),
    CONSTRAINT ck_trap_time_axis CHECK (start_ms >= 0 AND end_ms >= start_ms),
    CONSTRAINT ck_trap_capture_offset CHECK (capture_offset_ms >= 0),
    CONSTRAINT ck_trap_sha256 CHECK (sha256 ~ '^[0-9a-f]{64}$'),
    CONSTRAINT ck_trap_byte_size CHECK (byte_size >= 0),
    CONSTRAINT ck_trap_channels CHECK (channels IS NULL OR channels > 0),
    CONSTRAINT ck_trap_sample_rate CHECK (sample_rate IS NULL OR sample_rate > 0),
    CONSTRAINT ck_trap_upload_status CHECK (upload_status = ANY (ARRAY['pending'::text, 'uploading'::text, 'uploaded'::text, 'confirmed'::text, 'failed'::text])),
    CONSTRAINT fk_trap_record FOREIGN KEY (record_id) REFERENCES talk_record(id) ON DELETE RESTRICT,
    -- 复合 FK：分段必须属于同记录的 capture（§8）；相同序号不同 hash 由 UNIQUE+应用层返回冲突
    CONSTRAINT fk_trap_capture FOREIGN KEY (record_id, capture_id)
        REFERENCES talk_record_capture(record_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_trap_attachment FOREIGN KEY (attachment_id) REFERENCES attachment(id) ON DELETE RESTRICT
);

COMMENT ON TABLE  talk_record_audio_part IS '面聊音频分段（纳入既有附件生命周期；§6/§10.1）';
COMMENT ON COLUMN talk_record_audio_part.attachment_id IS '既有 attachment.id；禁止拿其他记录/聊天附件拼接（§6）';
COMMENT ON COLUMN talk_record_audio_part.upload_status IS '分段上传投影：pending→uploading→uploaded→confirmed；failed 可重试';

CREATE UNIQUE INDEX uq_trap_capture_seq ON talk_record_audio_part (capture_id, seq);

-- ============================================================
-- 7. talk_record_mark 重点标记
-- ============================================================
CREATE TABLE talk_record_mark (
    id             bigint                       NOT NULL,  -- TSID
    record_id      bigint                       NOT NULL,
    author_id      bigint                       NOT NULL,
    client_mark_id uuid                         NOT NULL,  -- 离线去重
    position_ms    bigint                       NOT NULL,
    text           character varying(1000)      DEFAULT '' NOT NULL,
    created_at     timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at     timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_talk_record_mark PRIMARY KEY (id),
    CONSTRAINT ck_trm_position CHECK (position_ms >= 0),
    CONSTRAINT fk_trm_record FOREIGN KEY (record_id) REFERENCES talk_record(id) ON DELETE RESTRICT,
    CONSTRAINT fk_trm_author FOREIGN KEY (author_id) REFERENCES "user"(id) ON DELETE RESTRICT
);

COMMENT ON TABLE talk_record_mark IS '面聊重点时间点标记（离线幂等去重；§2）';

-- 离线去重（§2）
CREATE UNIQUE INDEX uq_trm_client_mark ON talk_record_mark (record_id, author_id, client_mark_id);
-- mark 时间轴（§2 索引清单）
CREATE INDEX i_trm_position ON talk_record_mark (record_id, position_ms);

-- ============================================================
-- 8. talk_record_speaker 说话人
-- ============================================================
CREATE TABLE talk_record_speaker (
    id              bigint                       NOT NULL,  -- TSID
    record_id       bigint                       NOT NULL,
    local_label     character varying(64)        NOT NULL,  -- 如 SPEAKER_00 / 说话人1
    display_name    character varying(120)       DEFAULT '' NOT NULL,
    matched_user_id bigint,                                 -- 人工/认领绑定；自动建议不得直接变确认（§2）
    identity_source text                         DEFAULT 'anonymous' NOT NULL,
    confirmed_by    bigint,
    confidence      numeric(5,4),
    revision        bigint                       DEFAULT 1 NOT NULL,
    created_at      timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at      timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_talk_record_speaker PRIMARY KEY (id),
    CONSTRAINT ck_trsp_identity_source CHECK (identity_source = ANY (ARRAY['anonymous'::text, 'self_claim'::text, 'manual'::text, 'voice_suggestion'::text])),
    CONSTRAINT ck_trsp_confidence CHECK (confidence IS NULL OR (confidence >= 0 AND confidence <= 1)),
    CONSTRAINT ck_trsp_revision CHECK (revision >= 1),
    CONSTRAINT fk_trsp_record FOREIGN KEY (record_id) REFERENCES talk_record(id) ON DELETE RESTRICT,
    CONSTRAINT fk_trsp_matched_user FOREIGN KEY (matched_user_id) REFERENCES "user"(id) ON DELETE SET NULL,
    CONSTRAINT fk_trsp_confirmed_by FOREIGN KEY (confirmed_by) REFERENCES "user"(id) ON DELETE SET NULL,
    -- 复合 FK 目标：segment (record_id, speaker_id) 引用（§8 speaker 与 segment 同 record）
    CONSTRAINT uq_trsp_record_id UNIQUE (record_id, id)
);

COMMENT ON TABLE talk_record_speaker IS '面聊说话人标签（匿名标签/人工姓名/绑定来源；不暴露 voice embedding；§2）';

CREATE UNIQUE INDEX uq_trsp_record_label ON talk_record_speaker (record_id, local_label);

-- ============================================================
-- 9. talk_record_transcript 转写版本
-- ============================================================
CREATE TABLE talk_record_transcript (
    id                   bigint                       NOT NULL,  -- TSID
    record_id            bigint                       NOT NULL,
    version              integer                      NOT NULL,
    source_manifest_hash character varying(64)        NOT NULL,
    source_revision      bigint                       NOT NULL,
    language             character varying(32)        DEFAULT '' NOT NULL,
    status               text                         DEFAULT 'queued' NOT NULL,
    model_version        character varying(64),
    created_at           timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at           timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_talk_record_transcript PRIMARY KEY (id),
    CONSTRAINT ck_trt_version CHECK (version >= 1),
    CONSTRAINT ck_trt_source_revision CHECK (source_revision >= 1),
    CONSTRAINT ck_trt_source_manifest_hash CHECK (source_manifest_hash ~ '^[0-9a-f]{64}$'),
    CONSTRAINT ck_trt_status CHECK (status = ANY (ARRAY['queued'::text, 'running'::text, 'ready'::text, 'no_speech'::text, 'failed'::text])),
    CONSTRAINT fk_trt_record FOREIGN KEY (record_id) REFERENCES talk_record(id) ON DELETE RESTRICT,
    -- 复合 FK 目标：record.current_transcript_id 与 segment 引用（§8）
    CONSTRAINT uq_trt_record_id UNIQUE (record_id, id)
);

COMMENT ON TABLE talk_record_transcript IS '面聊转写版本（每次模型重跑创建新版本；发布前 CAS；§2）';

CREATE UNIQUE INDEX uq_trt_record_version ON talk_record_transcript (record_id, version);

-- ============================================================
-- 10. talk_record_segment 转写片段
-- ============================================================
CREATE TABLE talk_record_segment (
    id                bigint                       NOT NULL,  -- TSID
    record_id         bigint                       NOT NULL,
    transcript_version integer                     NOT NULL,
    seq               integer                      NOT NULL,
    speaker_id        bigint,
    start_ms          bigint                       NOT NULL,
    end_ms            bigint                       NOT NULL,
    text              text                         DEFAULT '' NOT NULL,
    confidence        numeric(5,4),
    is_final          boolean                      DEFAULT false NOT NULL,
    origin            text                         NOT NULL,
    revision          bigint                       DEFAULT 1 NOT NULL,
    edited_by         bigint,
    created_at        timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at        timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_talk_record_segment PRIMARY KEY (id),
    CONSTRAINT ck_trseg_seq CHECK (seq >= 0),
    -- §2：CHECK end>=start>=0
    CONSTRAINT ck_trseg_time_axis CHECK (start_ms >= 0 AND end_ms >= start_ms),
    CONSTRAINT ck_trseg_confidence CHECK (confidence IS NULL OR (confidence >= 0 AND confidence <= 1)),
    CONSTRAINT ck_trseg_origin CHECK (origin = ANY (ARRAY['model'::text, 'human'::text])),
    CONSTRAINT ck_trseg_revision CHECK (revision >= 1),
    CONSTRAINT fk_trseg_record FOREIGN KEY (record_id) REFERENCES talk_record(id) ON DELETE RESTRICT,
    -- 复合 FK：segment 必须引用同记录的转写版本（§8）
    CONSTRAINT fk_trseg_transcript FOREIGN KEY (record_id, transcript_version)
        REFERENCES talk_record_transcript(record_id, version) ON DELETE RESTRICT,
    -- 复合 FK：speaker 与 segment 必须同 record（§8；MATCH SIMPLE：speaker_id NULL 放行）
    CONSTRAINT fk_trseg_speaker FOREIGN KEY (record_id, speaker_id)
        REFERENCES talk_record_speaker(record_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_trseg_edited_by FOREIGN KEY (edited_by) REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE talk_record_segment IS '面聊转写片段（人工修订以 origin/revision 追踪，不覆盖模型原版；§2）';

CREATE UNIQUE INDEX uq_trseg_record_version_seq ON talk_record_segment (record_id, transcript_version, seq);
-- segment 时间轴（§2 索引清单）
CREATE INDEX i_trseg_timeline ON talk_record_segment (record_id, transcript_version, start_ms, id);

-- ============================================================
-- 11. talk_record_summary 摘要
-- ============================================================
CREATE TABLE talk_record_summary (
    id                      bigint                       NOT NULL,  -- TSID
    record_id               bigint                       NOT NULL,
    source_transcript_version integer                    NOT NULL,
    source_content_revision bigint                       NOT NULL,
    status                  text                         DEFAULT 'draft' NOT NULL,
    overview                text                         DEFAULT '' NOT NULL,
    decisions               jsonb                        DEFAULT '[]'::jsonb NOT NULL,
    actions                 jsonb                        DEFAULT '[]'::jsonb NOT NULL,
    model_version           character varying(64),
    revision                bigint                       DEFAULT 1 NOT NULL,
    confirmed_by            bigint,
    created_at              timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at              timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_talk_record_summary PRIMARY KEY (id),
    CONSTRAINT ck_trsum_source_content_revision CHECK (source_content_revision >= 1),
    CONSTRAINT ck_trsum_status CHECK (status = ANY (ARRAY['draft'::text, 'confirmed'::text, 'stale'::text])),
    CONSTRAINT ck_trsum_decisions CHECK (jsonb_typeof(decisions) = 'array'),
    CONSTRAINT ck_trsum_actions CHECK (jsonb_typeof(actions) = 'array'),
    CONSTRAINT ck_trsum_revision CHECK (revision >= 1),
    CONSTRAINT fk_trsum_record FOREIGN KEY (record_id) REFERENCES talk_record(id) ON DELETE RESTRICT,
    -- 复合 FK：摘要必须绑定同记录已成功转写版本（§3/§8）
    CONSTRAINT fk_trsum_transcript FOREIGN KEY (record_id, source_transcript_version)
        REFERENCES talk_record_transcript(record_id, version) ON DELETE RESTRICT,
    CONSTRAINT fk_trsum_confirmed_by FOREIGN KEY (confirmed_by) REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE talk_record_summary IS '面聊摘要草稿（decisions/actions 内 segment_ids 由服务端校验同记录；正文校对后标 stale；§2）';

CREATE INDEX i_trsum_record ON talk_record_summary (record_id, created_at DESC);

-- ============================================================
-- 12. talk_record_job 处理任务
-- ============================================================
CREATE TABLE talk_record_job (
    id                      bigint                       NOT NULL,  -- TSID
    record_id               bigint                       NOT NULL,
    kind                    text                         NOT NULL,
    input_version           bigint,
    input_manifest_hash     character varying(64),
    input_transcript_version integer,
    input_content_revision  bigint,
    dedupe_key              character varying(256)       NOT NULL,
    state                   text                         DEFAULT 'queued' NOT NULL,
    attempt                 integer                      DEFAULT 0 NOT NULL,
    lease_owner             character varying(128),
    lease_token             uuid,
    lease_until             timestamp with time zone,
    retry_at                timestamp with time zone,
    error_code              character varying(64),
    provider_request_id     character varying(128),
    finished_at             timestamp with time zone,
    created_at              timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at              timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_talk_record_job PRIMARY KEY (id),
    CONSTRAINT ck_trj_kind CHECK (kind = ANY (ARRAY['assemble'::text, 'transcribe'::text, 'summarize'::text, 'export'::text, 'purge'::text])),
    CONSTRAINT ck_trj_input_transcript_version CHECK (input_transcript_version IS NULL OR input_transcript_version >= 1),
    CONSTRAINT ck_trj_input_content_revision CHECK (input_content_revision IS NULL OR input_content_revision >= 1),
    CONSTRAINT ck_trj_input_manifest_hash CHECK (input_manifest_hash IS NULL OR input_manifest_hash ~ '^[0-9a-f]{64}$'),
    CONSTRAINT ck_trj_state CHECK (state = ANY (ARRAY['queued'::text, 'running'::text, 'retry_wait'::text, 'succeeded'::text, 'failed'::text, 'cancelled'::text])),
    CONSTRAINT ck_trj_attempt CHECK (attempt >= 0),
    -- 终态必须有完成时间（worker 故障不丢任务；失败不假 complete）
    CONSTRAINT ck_trj_finished CHECK (state NOT IN ('succeeded', 'failed', 'cancelled') OR finished_at IS NOT NULL),
    CONSTRAINT fk_trj_record FOREIGN KEY (record_id) REFERENCES talk_record(id) ON DELETE RESTRICT
);

COMMENT ON TABLE  talk_record_job IS '面聊持久处理任务（去重至少覆盖客户端离线补传窗口；§4/§A4 范式）';
COMMENT ON COLUMN talk_record_job.dedupe_key IS '应用层去重键（建议 record:kind:input 组合）；HTTP 超时不得产生重复付费调用';
COMMENT ON COLUMN talk_record_job.lease_token IS 'worker 租约令牌（§11.2）；claim/finish 分开事务仿 moya_ai_worker';

-- 任务去重（§2：dedupe_key UNIQUE）
CREATE UNIQUE INDEX uq_trj_dedupe ON talk_record_job (dedupe_key);
-- 队列扫描（§2 索引清单：job(state,retry_at,lease_until)）
CREATE INDEX i_trj_queue ON talk_record_job (state, retry_at, lease_until);

-- ============================================================
-- 13. talk_record_export 导出
-- ============================================================
CREATE TABLE talk_record_export (
    id             bigint                       NOT NULL,  -- TSID
    record_id      bigint                       NOT NULL,
    requester_id   bigint                       NOT NULL,
    format         text                         NOT NULL,
    include_audio  boolean                      DEFAULT false NOT NULL,  -- V1 恒 false
    attachment_id  bigint,
    job_id         bigint,
    state          text                         DEFAULT 'queued' NOT NULL,
    expires_at     timestamp with time zone,
    created_at     timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at     timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_talk_record_export PRIMARY KEY (id),
    CONSTRAINT ck_tre_format CHECK (format = ANY (ARRAY['txt'::text, 'md'::text])),
    CONSTRAINT ck_tre_state CHECK (state = ANY (ARRAY['queued'::text, 'ready'::text, 'failed'::text, 'expired'::text])),
    CONSTRAINT fk_tre_record FOREIGN KEY (record_id) REFERENCES talk_record(id) ON DELETE RESTRICT,
    CONSTRAINT fk_tre_requester FOREIGN KEY (requester_id) REFERENCES "user"(id) ON DELETE RESTRICT,
    CONSTRAINT fk_tre_attachment FOREIGN KEY (attachment_id) REFERENCES attachment(id) ON DELETE RESTRICT,
    CONSTRAINT fk_tre_job FOREIGN KEY (job_id) REFERENCES talk_record_job(id) ON DELETE RESTRICT
);

COMMENT ON TABLE talk_record_export IS '面聊文字导出（V1 txt/md 不含音频；每次下载重验 ACL；短期自动清理；§2）';

CREATE INDEX i_tre_record ON talk_record_export (record_id, created_at DESC);

-- ============================================================
-- 14. talk_record_consent_event 同意事件
-- ============================================================
CREATE TABLE talk_record_consent_event (
    id                        bigint                       NOT NULL,  -- TSID
    record_id                 bigint,                                 -- 可空=账号级同意
    actor_id                  bigint                       NOT NULL,
    kind                      text                         NOT NULL,
    policy_version            character varying(64)        NOT NULL,
    processing_mode           text                         NOT NULL,
    processor_config_version  character varying(64),
    captured_at               timestamp with time zone     NOT NULL,
    created_at                timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_talk_record_consent_event PRIMARY KEY (id),
    -- voice_* 为后续声纹阶段占位（V1 无入口），DB 层保留值域
    CONSTRAINT ck_trcon_kind CHECK (kind = ANY (ARRAY['recording_notice_ack'::text, 'processing_opt_in'::text, 'voice_enroll'::text, 'voice_withdraw'::text])),
    CONSTRAINT ck_trcon_processing_mode CHECK (processing_mode = ANY (ARRAY['record_only'::text, 'transcribe'::text, 'summarize'::text])),
    CONSTRAINT fk_trcon_record FOREIGN KEY (record_id) REFERENCES talk_record(id) ON DELETE RESTRICT,
    CONSTRAINT fk_trcon_actor FOREIGN KEY (actor_id) REFERENCES "user"(id) ON DELETE RESTRICT
);

COMMENT ON TABLE talk_record_consent_event IS '面聊同意事实记录（记录操作事实，不伪造现场全员同意；§2）';

CREATE INDEX i_trcon_actor ON talk_record_consent_event (actor_id, captured_at DESC);
CREATE INDEX i_trcon_record ON talk_record_consent_event (record_id)
    WHERE record_id IS NOT NULL;

-- ============================================================
-- 15. talk_record_audit 审计
-- ============================================================
CREATE TABLE talk_record_audit (
    id                  bigint                       NOT NULL,  -- TSID
    record_id           bigint,                                 -- 可空=跨记录操作
    actor_id            bigint,                                 -- 可空=系统/已匿名化主体（§11）
    action              character varying(64)        NOT NULL,
    target_id           bigint,
    request_id          character varying(64),
    metadata_allowlist  jsonb,
    created_at          timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_talk_record_audit PRIMARY KEY (id),
    CONSTRAINT fk_trau_record FOREIGN KEY (record_id) REFERENCES talk_record(id) ON DELETE RESTRICT,
    -- 审计主体注销：SET NULL 匿名化（对齐 enterprise_audit_event fk_eae_actor；§11 不级联删记录）
    CONSTRAINT fk_trau_actor FOREIGN KEY (actor_id) REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE talk_record_audit IS '面聊审计（授权/编辑/导出/删除/识别纠正；不写音频、原文、向量或外部授权 URL；§2）';

CREATE INDEX i_trau_record ON talk_record_audit (record_id, created_at DESC)
    WHERE record_id IS NOT NULL;
CREATE INDEX i_trau_actor ON talk_record_audit (actor_id, created_at DESC)
    WHERE actor_id IS NOT NULL;

-- ============================================================
-- 16. talk_record_idempotency 共享幂等键
-- ============================================================
CREATE TABLE talk_record_idempotency (
    id              bigint                       NOT NULL,  -- TSID
    actor_id        bigint                       NOT NULL,
    method          character varying(16)        NOT NULL,
    resource        character varying(256)       NOT NULL,
    idempotency_key character varying(256)       NOT NULL,
    payload_hash    character varying(64)        NOT NULL,
    result_ref      text,
    expires_at      timestamp with time zone     NOT NULL,
    created_at      timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at      timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_talk_record_idempotency PRIMARY KEY (id),
    CONSTRAINT ck_trid_payload_hash CHECK (payload_hash ~ '^[0-9a-f]{64}$'),
    CONSTRAINT fk_trid_actor FOREIGN KEY (actor_id) REFERENCES "user"(id) ON DELETE RESTRICT
);

COMMENT ON TABLE talk_record_idempotency IS '面聊共享幂等键（同键异 payload 409；过期由清理任务删除；不保存录音正文或签名 URL；§2/§4）';

-- 唯一键 actor+method+resource+key（§2）
CREATE UNIQUE INDEX uq_trid_actor_method ON talk_record_idempotency (actor_id, method, resource, idempotency_key);
-- 过期清理扫描
CREATE INDEX i_trid_expires ON talk_record_idempotency (expires_at);

-- ============================================================
-- 17. talk_record_attachment_ref 附件业务引用
-- ============================================================
CREATE TABLE talk_record_attachment_ref (
    id            bigint                       NOT NULL,  -- TSID
    record_id     bigint                       NOT NULL,
    attachment_id bigint                       NOT NULL,
    capture_id    bigint,                                 -- 可空=非分段产物（manifest/export 等）
    role          text                         NOT NULL,
    business_key  character varying(256)       NOT NULL,  -- audio_part/export 以自身 ID 生成，重试不重复绑定
    state         text                         DEFAULT 'bound' NOT NULL,
    released_at   timestamp with time zone,
    created_at    timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_talk_record_attachment_ref PRIMARY KEY (id),
    CONSTRAINT ck_trar_role CHECK (role = ANY (ARRAY['audio_part'::text, 'original_audio'::text, 'manifest'::text, 'transcoded_audio'::text, 'export'::text])),
    CONSTRAINT ck_trar_state CHECK (state = ANY (ARRAY['bound'::text, 'released'::text])),
    CONSTRAINT ck_trar_released CHECK (state <> 'released' OR released_at IS NOT NULL),
    CONSTRAINT fk_trar_record FOREIGN KEY (record_id) REFERENCES talk_record(id) ON DELETE RESTRICT,
    CONSTRAINT fk_trar_attachment FOREIGN KEY (attachment_id) REFERENCES attachment(id) ON DELETE RESTRICT,
    -- 复合 FK：capture 绑定必须同记录（§8/§10.2）
    CONSTRAINT fk_trar_capture FOREIGN KEY (record_id, capture_id)
        REFERENCES talk_record_capture(record_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE talk_record_attachment_ref IS '面聊附件业务引用（§10.2；GC 资格谓词必须识别本表 bound 引用）';

-- 绑定业务唯一（§10.2）
CREATE UNIQUE INDEX uq_trar_record_role_key ON talk_record_attachment_ref (record_id, role, business_key);
-- 附件 GC 资格谓词支撑（按 attachment 反查 bound 引用）
CREATE INDEX i_trar_attachment ON talk_record_attachment_ref (attachment_id)
    WHERE state = 'bound';

-- ============================================================
-- 主表对 capture/transcript 的复合投影引用（表建完后补挂）
-- ============================================================
-- active_capture_id 必须指向同记录的活跃/历史采集（§8）
ALTER TABLE talk_record DROP CONSTRAINT IF EXISTS fk_tr_active_capture;
ALTER TABLE talk_record ADD CONSTRAINT fk_tr_active_capture
    FOREIGN KEY (id, active_capture_id)
    REFERENCES talk_record_capture(record_id, id) ON DELETE RESTRICT;
-- current_transcript_id 必须指向同记录的转写版本（§8）
ALTER TABLE talk_record DROP CONSTRAINT IF EXISTS fk_tr_current_transcript;
ALTER TABLE talk_record ADD CONSTRAINT fk_tr_current_transcript
    FOREIGN KEY (id, current_transcript_id)
    REFERENCES talk_record_transcript(record_id, id) ON DELETE RESTRICT;

-- 00000214_talk_record_job_t12.up.sql
-- 面聊记录 T12：持久处理 job 运行时扩展 + 拼装 manifest 固化表。
-- 契约：BACKEND-API.md §2（talk_record_job/talk_record_assembly）、§3（media
--       状态机 manifest 固化在 complete 事务中）、§4（complete 202+jobs/retry/
--       processing-requests）、§6（manifest/音频产物引用）、§11.2（输入版本绑定）
-- 迁移契约：禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹；down 成对可逆
--       （有业务数据禁止回退，down 仅服务空表/测试库往返）。
--
-- 设计决策：
--   * talk_record_job.suspended：transcribe/summarize 在处理器未选型时的诚实
--     挂起位（state=retry_wait + error_code=waiting_processor + suspended=true，
--     claim 谓词排除）。不用 retry_at=远未来 hack——挂起与退避是两种语义，
--     处理器选型后（T13/T14）可精确 unsuspend 而不必猜 retry_at。
--   * talk_record_assembly：complete 事务内固化的拼装 manifest（§3"manifest
--     固化在 complete 事务中"）。segments 为段清单快照（seq/part/attachment/
--     sha256/byte_size/offset_ms/duration_ms）；manifest_hash 为整体指纹
--     （canonical JSON SHA-256），job.input_manifest_hash 与之绑定（§11.2
--     迟到结果 CAS 判据）。audio_attachment_id 可空——V1 无字节级拼接，
--     回听按 manifest 逐段/Range 授权（§4 playback），留列给集成期真实合成
--     音频产物（经 attachment adapter bind_artifact_tx 登记）。
--   * UNIQUE(record_id)：V1 单 capture 单次 complete；重放/重跑走应用层
--     幂等（同 manifest_hash upsert 刷新），追加分采集聚合是后续版本迁移。
SET LOCAL lock_timeout = '5s';

-- ============================================================
-- 1. talk_record_job 运行时扩展
-- ============================================================
ALTER TABLE talk_record_job
    ADD COLUMN IF NOT EXISTS suspended boolean DEFAULT false NOT NULL;

COMMENT ON COLUMN talk_record_job.suspended IS
    '诚实挂起位：处理器未选型（waiting_processor）时 state=retry_wait 且 suspended=true，claim 谓词排除；选型后置 false 恢复调度';

-- ============================================================
-- 2. talk_record_assembly 拼装 manifest 固化表
-- ============================================================
CREATE TABLE talk_record_assembly (
    id                  bigint                       NOT NULL,  -- TSID（应用层生成）
    record_id           bigint                       NOT NULL,
    capture_id          bigint                       NOT NULL,
    job_id              bigint,                                 -- 产出本版 manifest 的 complete job
    manifest_hash       character varying(64)        NOT NULL,
    segments            jsonb                        NOT NULL,
    part_count          integer                      NOT NULL,
    total_duration_ms   bigint                       NOT NULL,
    total_bytes         bigint                       NOT NULL,
    audio_attachment_id bigint,                                 -- V1 恒 NULL（无字节拼接；集成期合成音频产物）
    created_at          timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at          timestamp with time zone     DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_talk_record_assembly PRIMARY KEY (id),
    CONSTRAINT ck_tra_manifest_hash CHECK (manifest_hash ~ '^[0-9a-f]{64}$'),
    CONSTRAINT ck_tra_segments CHECK (jsonb_typeof(segments) = 'array'),
    CONSTRAINT ck_tra_part_count CHECK (part_count >= 0),
    CONSTRAINT ck_tra_total_duration CHECK (total_duration_ms >= 0),
    CONSTRAINT ck_tra_total_bytes CHECK (total_bytes >= 0),
    CONSTRAINT fk_tra_record FOREIGN KEY (record_id) REFERENCES talk_record(id) ON DELETE RESTRICT,
    -- 复合 FK：manifest 归属同记录的 capture（§8 防跨记录混用）
    CONSTRAINT fk_tra_capture FOREIGN KEY (record_id, capture_id)
        REFERENCES talk_record_capture(record_id, id) ON DELETE RESTRICT,
    -- 合成音频产物（如未来物化）：必须同记录已登记资产
    CONSTRAINT fk_tra_audio_attachment FOREIGN KEY (audio_attachment_id)
        REFERENCES attachment(id) ON DELETE RESTRICT
);

COMMENT ON TABLE  talk_record_assembly IS '面聊拼装 manifest 固化（§3 complete 事务固化；job 输入版本绑定真源）';
COMMENT ON COLUMN talk_record_assembly.manifest_hash IS '整体 manifest 指纹（canonical JSON SHA-256）；job.input_manifest_hash 与之 CAS 比对';
COMMENT ON COLUMN talk_record_assembly.audio_attachment_id IS 'V1 恒 NULL：无字节级拼接，回听按 segments 逐段/Range；集成期合成音频经附件 adapter 登记后回填';

-- V1 单 capture 单 manifest（重放同 hash 幂等刷新；多 capture 聚合留后续迁移）
CREATE UNIQUE INDEX uq_tra_record ON talk_record_assembly (record_id);

-- job 反查其产出 manifest（迟到结果拒绝诊断）
CREATE INDEX i_tra_job ON talk_record_assembly (job_id)
    WHERE job_id IS NOT NULL;

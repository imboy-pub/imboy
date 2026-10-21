-- 00000211_talk_record_v1.down.sql
-- 面聊记录 talk_record V1 首版表回退（与 up 完全互逆）。
-- 有业务数据禁止回退（对齐 00000168/00000201 down 惯例：已填充表要求备份恢复，不静默丢弃）。
-- 禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
SET LOCAL lock_timeout = '5s';

-- 守卫：主表存在业务行时拒绝回退（子表经 FK RESTRICT 间接保护；幂等键为瞬态可随回退消失）。
DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM talk_record) THEN
        RAISE EXCEPTION 'TALK_RECORD_V1_DOWN_BLOCKED: business rows exist; restore a verified backup or run the explicit cleanup workflow first'
            USING ERRCODE = '23514';
    END IF;
END
$$;

-- 主表对 capture/transcript 的复合投影引用（up 末尾补挂，先拆）
ALTER TABLE talk_record DROP CONSTRAINT IF EXISTS fk_tr_current_transcript;
ALTER TABLE talk_record DROP CONSTRAINT IF EXISTS fk_tr_active_capture;

-- 子表按依赖逆序删除（全部无 CASCADE，依赖由建表顺序保证）
DROP TABLE IF EXISTS talk_record_attachment_ref;
DROP TABLE IF EXISTS talk_record_idempotency;
DROP TABLE IF EXISTS talk_record_audit;
DROP TABLE IF EXISTS talk_record_consent_event;
DROP TABLE IF EXISTS talk_record_export;
DROP TABLE IF EXISTS talk_record_job;
DROP TABLE IF EXISTS talk_record_summary;
DROP TABLE IF EXISTS talk_record_segment;
DROP TABLE IF EXISTS talk_record_transcript;
DROP TABLE IF EXISTS talk_record_speaker;
DROP TABLE IF EXISTS talk_record_mark;
DROP TABLE IF EXISTS talk_record_audio_part;
DROP TABLE IF EXISTS talk_record_capture_event;
DROP TABLE IF EXISTS talk_record_capture;
DROP TABLE IF EXISTS talk_record_grant;
DROP TABLE IF EXISTS talk_record_participant;
DROP TABLE IF EXISTS talk_record;

-- 00000213_talk_record_mark_capture.down.sql
-- 与 up 成对可逆：撤除 mark 采集绑定列/约束/索引/注释，恢复 00000211 形态。
-- 有业务数据禁止回退（erlang_migrate 惯例：丢弃 capture_id/capture_offset_ms/
-- revision 数据即丢失 mark 时间轴真源与版本域）。
SET LOCAL lock_timeout = '5s';

DROP INDEX IF EXISTS i_trm_capture;
ALTER TABLE talk_record_mark DROP CONSTRAINT IF EXISTS ck_trm_revision;
ALTER TABLE talk_record_mark DROP CONSTRAINT IF EXISTS ck_trm_capture_offset;
ALTER TABLE talk_record_mark DROP CONSTRAINT IF EXISTS fk_trm_capture;
ALTER TABLE talk_record_mark DROP COLUMN IF EXISTS revision;
ALTER TABLE talk_record_mark DROP COLUMN IF EXISTS capture_offset_ms;
ALTER TABLE talk_record_mark DROP COLUMN IF EXISTS capture_id;

COMMENT ON COLUMN talk_record_mark.position_ms IS NULL;

-- 00000214_talk_record_job_t12.down.sql
-- 成对回滚：talk_record_assembly 表 + talk_record_job.suspended 列。
-- 有业务数据禁止回退（manifest 为 complete 固化证据）；仅服务空表/测试库往返。
SET LOCAL lock_timeout = '5s';

DROP TABLE IF EXISTS talk_record_assembly;

ALTER TABLE talk_record_job DROP COLUMN IF EXISTS suspended;

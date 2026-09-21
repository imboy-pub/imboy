-- 迁移 00000140 down: 对称回滚（只 DROP 本迁移新建的对象/列，不动任何既存对象）。
-- 纪律同 00000139 down：不触碰 136/139 建的表、不触碰 eb 域 retention 对象、
-- 不触碰共享表 attachment / "group" / workspace 本身。
--
-- 命名与 up 一一对应（efapi 前缀；见 up 头部的命名纪律说明）。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- 5) enterprise_message_origin
DROP TRIGGER IF EXISTS trg_efapi_message_origin_no_delete ON enterprise_message_origin;
DROP FUNCTION IF EXISTS fn_efapi_message_origin_no_delete();
DROP TABLE IF EXISTS enterprise_message_origin;

-- 4) enterprise_group_origin
DROP TRIGGER IF EXISTS trg_efapi_group_origin_guard ON enterprise_group_origin;
DROP FUNCTION IF EXISTS fn_efapi_group_origin_guard();
DROP TABLE IF EXISTS enterprise_group_origin;

-- 3) enterprise_application_usage
DROP TRIGGER IF EXISTS trg_efapi_usage_no_delete ON enterprise_application_usage;
DROP FUNCTION IF EXISTS fn_efapi_usage_no_delete();
DROP TABLE IF EXISTS enterprise_application_usage;

-- 2) enterprise_attachment_retention
DROP VIEW IF EXISTS v_efapi_attachment_purgeable;
DROP TRIGGER IF EXISTS trg_efapi_attachment_retention_guard ON enterprise_attachment_retention;
DROP FUNCTION IF EXISTS fn_efapi_attachment_retention_guard();
DROP TABLE IF EXISTS enterprise_attachment_retention;

-- 1) enterprise_application 内容策略列 + 守卫
DROP TRIGGER IF EXISTS trg_efapi_application_mime_guard ON enterprise_application;
DROP FUNCTION IF EXISTS fn_efapi_application_mime_guard();
ALTER TABLE enterprise_application DROP CONSTRAINT IF EXISTS ck_ea_max_file_size;
ALTER TABLE enterprise_application DROP COLUMN IF EXISTS max_file_size_bytes;
ALTER TABLE enterprise_application DROP COLUMN IF EXISTS allowed_mime_types;

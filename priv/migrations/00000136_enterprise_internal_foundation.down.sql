-- 迁移 00000136 down: 回滚 Enterprise Internal API 基础五表。
-- 数据不变量：五表均为一期新表（无前置数据），down 按「子表先于父表」的
-- 依赖顺序整表 DROP，无数据回填需求；触发器先于宿主表撤除。
-- 迁移契约：down=安全回滚、可重复执行。禁止 BEGIN/COMMIT。
--
-- DROP 顺序（依赖倒序）：
--   5 enterprise_oa_sso_code        -> 依赖 application/user/organization
--   4 enterprise_internal_idempotency -> 依赖 application/organization
--   3 enterprise_external_identity  -> 依赖 application/member/organization（含触发器）
--   2 enterprise_application_credential -> 依赖 application/organization
--   1 enterprise_application        -> 依赖 organization/user

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 5 回滚: enterprise_oa_sso_code
-- ============================================================
DROP TABLE IF EXISTS enterprise_oa_sso_code;

-- ============================================================
-- Phase 4 回滚: enterprise_internal_idempotency
-- ============================================================
DROP TABLE IF EXISTS enterprise_internal_idempotency;

-- ============================================================
-- Phase 3 回滚: enterprise_external_identity（触发器随表消亡，函数显式清理）
-- ============================================================
DROP TABLE IF EXISTS enterprise_external_identity;

DROP FUNCTION IF EXISTS fn_enterprise_external_identity_member_guard();

-- ============================================================
-- Phase 2 回滚: enterprise_application_credential
-- ============================================================
DROP TABLE IF EXISTS enterprise_application_credential;

-- ============================================================
-- Phase 1 回滚: enterprise_application
-- ============================================================
DROP TABLE IF EXISTS enterprise_application;

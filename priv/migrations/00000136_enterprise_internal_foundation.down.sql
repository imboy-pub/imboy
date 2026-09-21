-- 迁移 00000136 down: 回滚 Enterprise Internal API 基础五表。
-- 修订 00000136R（EPGZ-01R，同号原子迁移，2026-09-21）：
--   * enterprise_application.allowed_redirect_uris 列与守卫触发器/函数随表
--     整体回滚（触发器随表消亡，函数显式清理）；
--   * push_token.chk_push_token_platform 恢复 00000001 原定义值域
--     （fcm/apns/web_push，无 jpush）——注意 fail-closed：若表中已存在
--     platform='jpush' 行，恢复将被 CHECK 校验拒绝（down 报错），需先
--     清理 jpush token 行再回滚（一期新功能行属可弃数据）。
-- 数据不变量：五表均为一期新表（无前置数据），down 按「子表先于父表」的
-- 依赖顺序整表 DROP，无数据回填需求；触发器先于宿主表撤除。
-- 迁移契约：down=安全回滚、可重复执行。禁止 BEGIN/COMMIT。
--
-- DROP 顺序（依赖倒序）：
--   6 push_token platform CHECK 恢复原值域（既有表，独立于五表）
--   5 enterprise_oa_sso_code        -> 依赖 application/user/organization
--   4 enterprise_internal_idempotency -> 依赖 application/organization
--   3 enterprise_external_identity  -> 依赖 application/member/organization（含触发器）
--   2 enterprise_application_credential -> 依赖 application/organization
--   1 enterprise_application        -> 依赖 organization/user（含 redirect 守卫）

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 6 回滚: push_token platform CHECK 恢复 00000001 原定义
-- ============================================================
ALTER TABLE public.push_token
    DROP CONSTRAINT IF EXISTS chk_push_token_platform;
ALTER TABLE public.push_token
    ADD CONSTRAINT chk_push_token_platform CHECK (((platform)::text = ANY ((ARRAY['fcm'::character varying, 'apns'::character varying, 'web_push'::character varying])::text[])));

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
--（redirect allowlist 列随表消亡；redirect 守卫触发器随表消亡，函数显式清理）
-- ============================================================
DROP TABLE IF EXISTS enterprise_application;

DROP FUNCTION IF EXISTS fn_enterprise_application_redirect_guard();

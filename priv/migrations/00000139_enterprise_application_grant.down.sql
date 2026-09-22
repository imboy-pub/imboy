-- 迁移 00000139 down: 回滚 Enterprise Application Grant（Org/Workspace Grant）。
-- 对称性：up 建 3 表（grant / grant_scope / grant_workspace）+ 2 只读视图 +
--   1 个删除守卫函数（触发器随宿主表消亡），本文件逐项反向清理。
-- 数据不变量：三表均为本迁移新建（无前置数据、无 backfill、无列改写），
--   down 只做整表 DROP；不存在「一边删表一边改既有业务表」的不可逆混做
--   （plan §5 纪律：一个 migration 不混做不可逆删除）。
-- 顺序：先视图（依赖 grant 表），再子表（grant_workspace / grant_scope），
--   最后父表 grant；删除守卫函数在父表 DROP 时随触发器消亡，函数显式清理。
-- 迁移契约：down=安全回滚、可重复执行。禁止 BEGIN/COMMIT。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 4 回滚: 读取面视图
-- ============================================================
DROP VIEW IF EXISTS public.v_enterprise_effective_application_grant_scope;
DROP VIEW IF EXISTS public.v_enterprise_effective_application_grant;

-- ============================================================
-- Phase 3 回滚: 显式 Workspace Grant 行
-- ============================================================
DROP TABLE IF EXISTS enterprise_application_grant_workspace;

-- ============================================================
-- Phase 2 回滚: Grant 固定 scope 集合
-- ============================================================
DROP TABLE IF EXISTS enterprise_application_grant_scope;

-- ============================================================
-- Phase 1 回滚: Grant（删除守卫触发器随表消亡，函数显式清理）
-- ============================================================
DROP TABLE IF EXISTS enterprise_application_grant;

DROP FUNCTION IF EXISTS fn_enterprise_application_grant_no_delete();

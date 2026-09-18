-- 迁移 00000134: Agent Run 基础三表（down，完整对称回滚）。
-- 计划契约：docs/architecture/2026-09-16-imboy-agent-runtime-v3.1.md §9.3-9.4 Frozen Run/Effect Schema Contract。
-- 迁移契约：down=与 up 完全对称、逆序回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 回滚顺序：先卸 append-only 守卫（触发器→函数），再按 FK 依赖逆序 drop 表
-- （agent_effect → agent_run_event → agent_run），最后移除 up 建立的全部索引
-- （随表 drop 已级联移除，此处 IF EXISTS 显式声明对称性）。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 1（逆序）: 卸载 append-only 守卫
-- ============================================================
DROP TRIGGER IF EXISTS trg_agent_run_event_append_only ON agent_run_event;
DROP FUNCTION IF EXISTS fn_agent_run_event_append_only();

-- ============================================================
-- Phase 2（逆序）: drop 三表（FK 依赖逆序）
-- ============================================================
DROP TABLE IF EXISTS agent_effect;
DROP TABLE IF EXISTS agent_run_event;
DROP TABLE IF EXISTS agent_run;

-- ============================================================
-- Phase 3: 显式移除索引（表级联已删，IF EXISTS 保证幂等对称）
-- ============================================================
DROP INDEX IF EXISTS i_ar_status_lease;
DROP INDEX IF EXISTS i_ar_agent_org_created;
DROP INDEX IF EXISTS i_ar_grant_status;

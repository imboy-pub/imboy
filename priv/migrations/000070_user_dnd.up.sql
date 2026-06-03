-- ============================================================
-- Migration 000070: 用户免打扰(DND)偏好
--
-- 执行机制：本文件由 erlang_migrate:up/1 -> erlang_migrate_pg:exec_sql/2
--   整体包裹在单个 BEGIN/COMMIT 事务中执行，任一语句失败则整体 ROLLBACK。
--   因此本文件 **不写** 显式 BEGIN/COMMIT，**不使用** CREATE INDEX CONCURRENTLY
--   （CONCURRENTLY 不能在事务块内运行）。
--   所有语句幂等（IF [NOT] EXISTS / 幂等 WHERE 条件），可安全重跑。
-- ============================================================

-- 段1：新增表（免打扰规则，按用户维度 1:1）
CREATE TABLE IF NOT EXISTS public.user_dnd_rule (
    id          bigserial PRIMARY KEY,           -- 主键 自增长ID
    user_id     int8 NOT NULL,                   -- 用户ID (TSID bigint)
    start_min   int2 NOT NULL DEFAULT 0,         -- 免打扰起始(自0点起分钟数, 0-1439)
    end_min     int2 NOT NULL DEFAULT 0,         -- 免打扰结束(自0点起分钟数, 0-1439)
    status      int2 NOT NULL DEFAULT 1,         -- 1=启用 0=停用
    updated_at  timestamptz,                     -- 更新时间
    created_at  timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP  -- 创建时间
);
CREATE UNIQUE INDEX IF NOT EXISTS uk_dnd_rule_UserId ON public.user_dnd_rule(user_id);
COMMENT ON COLUMN public.user_dnd_rule.user_id IS '用户ID (TSID bigint)';

-- 段2：现有表加字段（小表加 NOT NULL + 常量默认值；PG11+ 仅改 catalog，不重写表）
ALTER TABLE public."user" ADD COLUMN IF NOT EXISTS dnd_enabled boolean NOT NULL DEFAULT false;
COMMENT ON COLUMN public."user".dnd_enabled IS '全局免打扰开关，默认关闭';

-- 段3：新增索引（普通创建；小表无需 CONCURRENTLY）
CREATE INDEX IF NOT EXISTS i_dnd_rule_Status
    ON public.user_dnd_rule (user_id, status) WHERE status = 1;

-- 段4：数据回填（示范：为已禁用用户默认开启免打扰）
--   幂等：WHERE dnd_enabled = false 保证重跑不会反复改写已处理行
UPDATE public."user" SET dnd_enabled = true WHERE status = 0 AND dnd_enabled = false;

-- ============================================================
-- Migration: 00000087_plugin_audit_log
-- Description: 插件操作审计日志
--
-- 设计说明：
--   记录插件生命周期状态转换的审计日志
--   started_at / ended_at 使用 BIGINT 毫秒时间戳（非 TIMESTAMPTZ），
--   因为审计场景需要精确毫秒级对比计算 duration_ms
--   created_at 遵循项目规范使用 TIMESTAMPTZ
--
-- Source of truth: docs/plugin/lifecycle.md §11
-- ============================================================

CREATE TABLE IF NOT EXISTS public.plugin_audit_log (
    id              BIGINT PRIMARY KEY,                          -- TSID
    plugin_name     VARCHAR(64)  NOT NULL,
    operation_id    VARCHAR(64),                                  -- admin 触发的操作 id（NULL = 内部事件）
    event           VARCHAR(64)  NOT NULL,                        -- state_entered / step_done / rollback_started / ...
    from_state      VARCHAR(32),                                  -- 转换前 state
    to_state        VARCHAR(32),                                  -- 转换后 state
    step            VARCHAR(64),                                  -- 当前 step
    operator        VARCHAR(64)  DEFAULT 'system',               -- admin uid 或 'system'
    started_at      BIGINT       NOT NULL,                        -- 毫秒时间戳（审计场景需精确毫秒级对比）
    ended_at        BIGINT,                                       -- 毫秒时间戳，NULL = 仍在进行
    duration_ms     INT,                                          -- = ended_at - started_at
    result          VARCHAR(16),                                  -- ok / failed / cancelled / timeout
    error_code      VARCHAR(64),                                  -- 失败时分类码
    error_detail    TEXT,                                         -- 失败时详细信息（脱敏后）
    metadata        JSONB DEFAULT '{}',                           -- 任意扩展
    created_at      TIMESTAMPTZ   NOT NULL DEFAULT NOW()
);

COMMENT ON TABLE public.plugin_audit_log IS '插件操作审计日志';
COMMENT ON COLUMN public.plugin_audit_log.id IS 'TSID 分布式 ID';
COMMENT ON COLUMN public.plugin_audit_log.plugin_name IS '插件名称';
COMMENT ON COLUMN public.plugin_audit_log.operation_id IS 'admin 触发的操作 id（NULL = 内部事件）';
COMMENT ON COLUMN public.plugin_audit_log.event IS '事件类型：state_entered / step_done / rollback_started 等';
COMMENT ON COLUMN public.plugin_audit_log.from_state IS '转换前状态';
COMMENT ON COLUMN public.plugin_audit_log.to_state IS '转换后状态';
COMMENT ON COLUMN public.plugin_audit_log.step IS '当前执行步骤';
COMMENT ON COLUMN public.plugin_audit_log.operator IS '操作者：admin uid 或 system';
COMMENT ON COLUMN public.plugin_audit_log.started_at IS '开始时间（毫秒时间戳，用于精确 duration 计算）';
COMMENT ON COLUMN public.plugin_audit_log.ended_at IS '结束时间（毫秒时间戳，NULL = 仍在进行）';
COMMENT ON COLUMN public.plugin_audit_log.duration_ms IS '持续毫秒数 = ended_at - started_at';
COMMENT ON COLUMN public.plugin_audit_log.result IS '结果：ok / failed / cancelled / timeout';
COMMENT ON COLUMN public.plugin_audit_log.error_code IS '失败时分类码';
COMMENT ON COLUMN public.plugin_audit_log.error_detail IS '失败时详细信息（脱敏后）';
COMMENT ON COLUMN public.plugin_audit_log.metadata IS 'JSONB 任意扩展字段';
COMMENT ON COLUMN public.plugin_audit_log.created_at IS '记录创建时间';

ALTER TABLE IF EXISTS public.plugin_audit_log OWNER TO imboy_user;

CREATE INDEX IF NOT EXISTS i_plugin_audit_log_plugin_started
    ON public.plugin_audit_log (plugin_name, started_at DESC);
CREATE INDEX IF NOT EXISTS i_plugin_audit_log_operation
    ON public.plugin_audit_log (operation_id);
CREATE INDEX IF NOT EXISTS i_plugin_audit_log_event
    ON public.plugin_audit_log (event, started_at DESC);

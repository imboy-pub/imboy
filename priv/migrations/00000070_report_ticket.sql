-- ================================================================
-- Report Center: unified report tickets and action logs
-- ================================================================

CREATE TABLE IF NOT EXISTS public.report_ticket (
    id BIGSERIAL PRIMARY KEY,
    target_type VARCHAR(16) NOT NULL,
    target_id BIGINT NOT NULL,
    reporter_uid BIGINT NOT NULL,
    reason VARCHAR(64) NOT NULL DEFAULT '',
    description VARCHAR(500) NOT NULL DEFAULT '',
    status SMALLINT NOT NULL DEFAULT 0,      -- 0 pending, 1 rejected, 2 violated
    handled_by BIGINT,
    handled_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    CONSTRAINT chk_report_ticket_target_type CHECK (target_type IN ('moment', 'group', 'channel', 'user')),
    CONSTRAINT chk_report_ticket_status CHECK (status IN (0, 1, 2))
);

CREATE INDEX IF NOT EXISTS idx_report_ticket_status_created
    ON public.report_ticket(status, created_at DESC);

CREATE INDEX IF NOT EXISTS idx_report_ticket_target_status
    ON public.report_ticket(target_type, target_id, status, created_at DESC);

CREATE INDEX IF NOT EXISTS idx_report_ticket_reporter_created
    ON public.report_ticket(reporter_uid, created_at DESC);

COMMENT ON TABLE public.report_ticket IS '举报中心工单表（统一 group/channel/user/moment）';
COMMENT ON COLUMN public.report_ticket.status IS '0待处理 1已驳回 2已确认违规';

CREATE TABLE IF NOT EXISTS public.report_action_log (
    id BIGSERIAL PRIMARY KEY,
    report_id BIGINT NOT NULL,
    operator_uid BIGINT NOT NULL,
    result SMALLINT NOT NULL,                -- 1 rejected, 2 violated
    note VARCHAR(500) NOT NULL DEFAULT '',
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    CONSTRAINT fk_report_action_report_id FOREIGN KEY (report_id)
        REFERENCES public.report_ticket(id) ON DELETE CASCADE,
    CONSTRAINT chk_report_action_result CHECK (result IN (1, 2))
);

CREATE INDEX IF NOT EXISTS idx_report_action_report_created
    ON public.report_action_log(report_id, created_at DESC);

CREATE INDEX IF NOT EXISTS idx_report_action_operator_created
    ON public.report_action_log(operator_uid, created_at DESC);

COMMENT ON TABLE public.report_action_log IS '举报处理操作日志';

CREATE OR REPLACE FUNCTION fn_report_ticket_touch_updated_at()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS tr_report_ticket_updated_at ON public.report_ticket;
CREATE TRIGGER tr_report_ticket_updated_at
    BEFORE UPDATE ON public.report_ticket
    FOR EACH ROW
    EXECUTE FUNCTION fn_report_ticket_touch_updated_at();

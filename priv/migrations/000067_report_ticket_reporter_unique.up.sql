-- report_ticket: 唯一索引，支撑 report_ticket_repo:create/5 的
-- ON CONFLICT (target_type, target_id, reporter_uid) DO NOTHING 语义。
-- 缺失此唯一约束时，INSERT ... ON CONFLICT 会抛 42P10，导致举报功能完全不可用。
-- 业务语义：同一举报人对同一目标只保留一条工单（去重）。
CREATE UNIQUE INDEX IF NOT EXISTS uk_report_ticket_target_reporter
  ON public.report_ticket (target_type, target_id, reporter_uid);

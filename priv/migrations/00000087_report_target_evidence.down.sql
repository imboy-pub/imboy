-- 回滚迁移 00087: report_ticket 一等举报目标与结构化证据（R-01）
-- Down migration for 00087. Drops message-target columns/constraints/indexes
-- and restores the original four-value target_type CHECK.
-- 注意：回滚会丢弃消息举报行携带的 scope/author/evidence 数据。
-- Do not edit manually.

DROP INDEX IF EXISTS public.idx_report_ticket_sub_scope;
DROP INDEX IF EXISTS public.idx_report_ticket_author;

ALTER TABLE public.report_ticket DROP CONSTRAINT IF EXISTS chk_report_ticket_message_shape;
ALTER TABLE public.report_ticket DROP CONSTRAINT IF EXISTS chk_report_ticket_target_sub_type;

ALTER TABLE public.report_ticket DROP CONSTRAINT IF EXISTS chk_report_ticket_target_type;
ALTER TABLE public.report_ticket ADD CONSTRAINT chk_report_ticket_target_type CHECK (
    (target_type)::text = ANY ((ARRAY['moment'::character varying, 'group'::character varying, 'channel'::character varying, 'user'::character varying])::text[])
);

ALTER TABLE public.report_ticket DROP COLUMN IF EXISTS evidence;
ALTER TABLE public.report_ticket DROP COLUMN IF EXISTS target_author_id;
ALTER TABLE public.report_ticket DROP COLUMN IF EXISTS target_scope_id;
ALTER TABLE public.report_ticket DROP COLUMN IF EXISTS target_sub_type;

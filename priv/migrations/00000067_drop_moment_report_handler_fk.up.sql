-- 删除 moment_report.handled_by 对 "user"(id) 的外键。
-- 根因：该列写入的是 adm_user 体系的管理员 id（moment_logic:admin_resolve_report/4
-- 传入 adm_user_id），与 "user" 表 id 空间不同。凡处理者不是「兼具普通用户身份」的
-- 管理员，UPDATE 必违反外键，导致管理后台单条/批量举报处理整体不可用（HTTP 200 +
-- code:400「处理举报失败」）。
-- 对齐 report_ticket.handled_by 的既有模式：同字段同写入方式，无外键。
-- 发布前只读预检（确认存量数据即可，无需修复）：
-- SELECT handled_by FROM public.moment_report WHERE handled_by IS NOT NULL
--   AND handled_by NOT IN (SELECT id FROM public."user") LIMIT 100;
ALTER TABLE public.moment_report
    DROP CONSTRAINT IF EXISTS fk_moment_report_handler;

-- 00000099_teaching_admin_audit.down.sql
-- 安全回滚：审计表整体删除。
-- ⚠️ down 会物理删除审计历史——生产环境执行前必须先导出（§9.3 删除演练的一部分）；
--    空库/开发库回滚无数据负担。

DROP INDEX IF EXISTS i_teaching_admin_audit_operator;
DROP INDEX IF EXISTS i_teaching_admin_audit_learner;
DROP TABLE IF EXISTS teaching_admin_audit;

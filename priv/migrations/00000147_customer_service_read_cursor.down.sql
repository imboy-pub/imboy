-- 迁移 00000147 回滚: 移除客服已读游标表 customer_service_read_cursor。
--
-- 回滚语义：本表是 CS-BE-04 的纯增量事实表（无触发器/无函数/无回填），
-- 游标可随时由客户端重新 ACK 重建——DROP 即完整恢复迁移前形态。
-- 复合 FK（session/seat）随表删除，不影响被引用表。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

DROP INDEX IF EXISTS i_csrc_org_identity;
DROP TABLE IF EXISTS customer_service_read_cursor;

-- 迁移 00000148 回滚: 移除客服坐席 presence 心跳 lease 表。
--
-- 回滚语义：本表是 CS-BE-05 的纯运行态事实输入（无触发器/无函数/无回填），
-- 心跳由客户端周期性重新上报即可重建——DROP 即完整恢复迁移前形态。
-- 派生状态是应用层纯函数（cs_presence），无任何 DB 侧对象残留；
-- FK CASCADE 随表删除，不影响被引用的 customer_service_seat。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

DROP TABLE IF EXISTS customer_service_seat_presence;

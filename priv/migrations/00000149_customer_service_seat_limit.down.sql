-- 迁移 00000149 回滚: 移除客服席位 entitlement 配置表。
--
-- 回滚语义：本表是 CS-BE-06 的人工配置事实（无触发器/无函数/无回填），
-- 删除后所有组织回到 unlimited 默认（与引入前形态一致）；检查路径读不到
-- limit 行即 unlimited，不产生任何悬垂引用。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

DROP TABLE IF EXISTS customer_service_seat_limit;

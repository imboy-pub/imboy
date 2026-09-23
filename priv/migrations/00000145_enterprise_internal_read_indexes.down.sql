-- 迁移 00000145 回滚: enterprise internal 只读面三个热点索引
-- （V2.1 F2 索引裁决）。只移除本迁移新增的 3 个索引，不动任何表/列/约束。
--
-- 纯索引回滚无数据风险：查询计划退回既有访问路径
-- （i_workspace_organization_id + status 过滤 / i_group_scope_ws /
--  group_member Seq Scan），只损失性能，不损失正确性。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

DROP INDEX IF EXISTS i_ws_org_created;
DROP INDEX IF EXISTS i_group_ws_created;
DROP INDEX IF EXISTS i_gm_grp_created;

-- 迁移 00000150 回滚: 移除 INT-23 keyset 分页索引。
--
-- 回滚语义：本迁移只建一个索引（无函数/触发器/无数据回填）；down 精确删除
-- bot_delivery_ewh_keyset_idx，00000141 的 bot_delivery_ewh_owner_idx 与其余
-- bot_delivery 索引保持原样。删索引后读面回落 offset 形态属于应用层回滚
-- （代码随迁移同回滚），无悬垂引用。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

DROP INDEX IF EXISTS bot_delivery_ewh_keyset_idx;

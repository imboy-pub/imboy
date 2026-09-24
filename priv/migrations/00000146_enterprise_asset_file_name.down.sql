-- 迁移 00000146 回滚: 移除 enterprise_asset.file_name 列与本迁移新增的 CHECK。
--
-- 回滚语义：file_name 是纯展示性增量列（无回填、无触发器、无索引、无 FK），
-- DROP 即完整恢复迁移前形态。消息-附件绑定（message_id）与保留期守卫不受影响。
-- 先 DROP CONSTRAINT 再 DROP COLUMN：约束显式移除，不依赖列级联语义。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

ALTER TABLE enterprise_asset
    DROP CONSTRAINT IF EXISTS ck_enterprise_asset_file_name;

ALTER TABLE enterprise_asset
    DROP COLUMN IF EXISTS file_name;

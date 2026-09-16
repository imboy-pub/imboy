-- 迁移 00000118 回滚：企业附件。
-- 迁移契约：禁止 BEGIN/COMMIT（erlang_migrate 外层单事务包裹）。

DROP TRIGGER IF EXISTS trg_enterprise_asset_purge_guard ON enterprise_asset;
DROP FUNCTION IF EXISTS fn_enterprise_asset_purge_guard();

DROP TRIGGER IF EXISTS trg_enterprise_asset_retention_guard ON enterprise_asset;
DROP FUNCTION IF EXISTS fn_enterprise_asset_retention_guard();

DROP TABLE IF EXISTS enterprise_asset;

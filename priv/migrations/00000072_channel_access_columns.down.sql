-- 00000072_channel_access_columns.down.sql
-- 回滚频道正交访问模型迁移
--
-- ⚠️ 回滚前置条件（部署顺序约束）：
--   必须先回滚应用代码到 Step 10 之前版本（无任何 visibility/access_type/join_policy 引用），
--   再执行本回滚。否则所有 channel 相关查询将 ERROR: column does not exist（服务全面 500）。
-- 迁移契约：禁止 BEGIN/COMMIT（erlang_migrate 外层单事务包裹）。

-- 恢复 type 列（临用 DEFAULT -1 作为哨兵，避免 catch-all 误伤 C1）
ALTER TABLE channel ADD COLUMN IF NOT EXISTS type smallint DEFAULT -1;

-- 回填 type：从三正交字段计算 type（反向投影）
-- 单 UPDATE + CASE，WHERE type=-1 确保幂等
UPDATE channel SET type = CASE
    WHEN visibility = 0 AND access_type = 0 AND join_policy = 0 THEN 0     -- C1
    WHEN visibility = 1 AND access_type = 0 AND join_policy = 1 THEN 1     -- C2
    WHEN visibility = 0 AND access_type = 1 AND join_policy = 3 THEN 2     -- C3
    WHEN visibility = 1 AND access_type = 1 AND join_policy = 3 THEN 1     -- C4
    WHEN join_policy = 2 THEN 1                                              -- approval（fail-closed）
    ELSE 1                                                                   -- 未定义组合（fail-closed，不回落 0）
END WHERE type = -1;

-- 恢复原列定义：DEFAULT 0 NOT NULL
ALTER TABLE channel ALTER COLUMN type SET DEFAULT 0;
ALTER TABLE channel ALTER COLUMN type SET NOT NULL;

-- 删除新字段及相关对象
ALTER TABLE channel DROP CONSTRAINT IF EXISTS chk_channel_no_free_purchase;

DROP INDEX IF EXISTS i_channel_visibility_active;

ALTER TABLE channel DROP COLUMN IF EXISTS join_policy;
ALTER TABLE channel DROP COLUMN IF EXISTS access_type;
ALTER TABLE channel DROP COLUMN IF EXISTS visibility;

DROP TABLE IF EXISTS channel_access_type2_audit;
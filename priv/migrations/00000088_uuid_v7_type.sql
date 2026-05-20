-- ================================================================
-- 迁移 e2ee_transfer_sessions.session_id 到原生 uuid 类型 + UUIDv7
-- 版本: 00000088
-- 日期: 2026-05-20
-- 说明:
--   1. 将 VARCHAR(48) 列改为 PostgreSQL 原生 uuid 类型（16 字节存储）
--   2. 应用层改用 UUIDv7（时序 UUID），提升 B-tree 索引顺序插入性能
-- ================================================================

BEGIN;

-- 将 session_id 从 VARCHAR(48) 转换为原生 uuid 类型
-- 已有数据通过 ::uuid 强制转换（要求列值必须是有效 UUID 格式）
ALTER TABLE e2ee_transfer_sessions
    ALTER COLUMN session_id TYPE uuid USING session_id::uuid;

COMMENT ON COLUMN e2ee_transfer_sessions.session_id IS '会话标识（UUIDv7 时序 UUID，顺序插入友好，16 字节存储）';

COMMIT;

-- ================================================================
-- 验证查询
-- ================================================================

-- 检查列类型已变更
SELECT column_name, data_type, character_maximum_length
FROM information_schema.columns
WHERE table_name = 'e2ee_transfer_sessions'
  AND column_name = 'session_id';
-- 预期: data_type = 'uuid', character_maximum_length = null

-- 检查索引仍有效
SELECT indexname, indexdef
FROM pg_indexes
WHERE tablename = 'e2ee_transfer_sessions'
  AND indexname LIKE '%session_id%';

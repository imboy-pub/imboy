-- Migration: 00000083_hypertable_timeseries.sql
-- 将 msg_read、msg_c2g_timeline、user_log 转为 TimescaleDB hypertable
-- 配置压缩和保留策略
--
-- 前提条件：TimescaleDB 扩展已安装（msg_store 迁移中已验证）
-- 注意：hypertable 要求所有唯一索引必须包含分区列（时间列）

-- ============================================================
-- 1. user_log — 无主键、无唯一索引，直接转换
-- ============================================================

SELECT create_hypertable(
    'user_log',
    'created_at',
    chunk_time_interval => INTERVAL '30 days',
    migrate_data => TRUE,
    if_not_exists => TRUE
);

-- 压缩策略：按 uid 分段，按 created_at 降序
ALTER TABLE user_log SET (
    timescaledb.compress,
    timescaledb.compress_orderby = 'created_at DESC',
    timescaledb.compress_segmentby = 'uid'
);

-- 7 天后自动压缩
SELECT add_compression_policy('user_log', INTERVAL '7 days', if_not_exists => TRUE);

-- 90 天保留策略
SELECT add_retention_policy('user_log', INTERVAL '90 days', if_not_exists => TRUE);


-- ============================================================
-- 2. msg_c2g_timeline — 有唯一索引需要调整
-- ============================================================

-- 删除不包含 created_at 的唯一索引（hypertable 要求）
DROP INDEX IF EXISTS uk_c2g_timeline_MsgId;
DROP INDEX IF EXISTS uk_c2g_timeline_ToUid_MsgId;

SELECT create_hypertable(
    'msg_c2g_timeline',
    'created_at',
    chunk_time_interval => INTERVAL '7 days',
    migrate_data => TRUE,
    if_not_exists => TRUE
);

-- 重建唯一索引，包含 created_at
CREATE UNIQUE INDEX IF NOT EXISTS uk_c2g_timeline_ToUid_MsgId_CreatedAt
    ON msg_c2g_timeline (to_uid, msg_id, created_at);

-- 重建普通索引
CREATE INDEX IF NOT EXISTS idx_c2g_timeline_ToUid_ClientAck
    ON msg_c2g_timeline (to_uid, client_ack);

-- 压缩策略：按 to_gid 分段
ALTER TABLE msg_c2g_timeline SET (
    timescaledb.compress,
    timescaledb.compress_orderby = 'created_at DESC',
    timescaledb.compress_segmentby = 'to_gid'
);

-- 7 天后自动压缩（投递队列，ACK 后通常可删）
SELECT add_compression_policy('msg_c2g_timeline', INTERVAL '7 days', if_not_exists => TRUE);


-- ============================================================
-- 3. msg_read — 有主键和唯一索引需要调整
-- ============================================================

-- 删除 BIGSERIAL 主键（hypertable 不兼容不含时间列的 PK）
ALTER TABLE msg_read DROP CONSTRAINT IF EXISTS msg_read_pkey;

-- 删除不包含 created_at 的唯一索引
DROP INDEX IF EXISTS uk_msg_read_msg_to_did;

SELECT create_hypertable(
    'msg_read',
    'created_at',
    chunk_time_interval => INTERVAL '30 days',
    migrate_data => TRUE,
    if_not_exists => TRUE
);

-- 重建唯一索引，包含 created_at
CREATE UNIQUE INDEX IF NOT EXISTS uk_msg_read_msg_to_did_created
    ON msg_read (msg_id, to_uid, to_did, created_at);

-- 保留其他查询索引
CREATE INDEX IF NOT EXISTS idx_msg_read_from_uid ON msg_read (from_uid);
CREATE INDEX IF NOT EXISTS idx_msg_read_to_uid ON msg_read (to_uid);
CREATE INDEX IF NOT EXISTS idx_msg_read_read_at ON msg_read (read_at);

-- 压缩策略：按 from_uid 分段
ALTER TABLE msg_read SET (
    timescaledb.compress,
    timescaledb.compress_orderby = 'created_at DESC',
    timescaledb.compress_segmentby = 'from_uid'
);

-- 30 天后自动压缩
SELECT add_compression_policy('msg_read', INTERVAL '30 days', if_not_exists => TRUE);

-- 180 天保留策略
SELECT add_retention_policy('msg_read', INTERVAL '180 days', if_not_exists => TRUE);

-- ============================================================
-- Migration: 00000075_msg_store
-- Description: 永久消息存储表 + per-conversation 序列号表
--
-- 设计说明：
--   msg_store_staging  → WAL 暂存缓冲区（现有，1h 清理）
--   msg_c2c / msg_c2g  → 投递队列（现有，ACK 后删除）
--   msg_store_seq      → per-conversation 单调递增计数器（新增）
--   msg_store          → 永久消息存储（新增），Worker 写入，不因 ACK 删除
--
-- 同步游标用法（类似 Telegram PTS）：
--   客户端保存 last_conv_seq，拉取时传入
--   SELECT ... WHERE conv_key = $1 AND conv_seq > $2 ORDER BY conv_seq ASC LIMIT $3
--
-- 启用开关：
--   在 sys.config 中设置 {msg_archive_enabled, true} 启用归档
-- ============================================================


-- ============================================================
-- 1. per-conversation 序列号表
-- ============================================================
CREATE TABLE IF NOT EXISTS public.msg_store_seq (
    conv_key  VARCHAR(64) NOT NULL,
    seq       BIGINT      NOT NULL DEFAULT 0,
    PRIMARY KEY (conv_key)
);

ALTER TABLE IF EXISTS public.msg_store_seq OWNER TO imboy_user;

COMMENT ON TABLE  public.msg_store_seq IS 'per-conversation 序列号计数器';
COMMENT ON COLUMN public.msg_store_seq.conv_key IS '会话唯一键：c2c:{min_uid}:{max_uid} 或 c2g:{gid}';
COMMENT ON COLUMN public.msg_store_seq.seq      IS '当前最大序列号，每次写入原子 +1';


-- ============================================================
-- 2. 永久消息存储表
-- ============================================================
CREATE TABLE IF NOT EXISTS public.msg_store (
    id         BIGSERIAL,
    chat_type  VARCHAR(3)   NOT NULL,   -- 'c2c' | 'c2g'
    conv_key   VARCHAR(64)  NOT NULL,   -- 与 msg_store_seq.conv_key 一致
    conv_seq   BIGINT       NOT NULL,   -- per-conversation 单调递增，客户端游标
    msg_id     VARCHAR(50)  NOT NULL,
    msg_type   VARCHAR(50)  NOT NULL,
    from_id    BIGINT       NOT NULL,
    to_id      BIGINT,                  -- C2C: 接收者 user_id；C2G: NULL
    group_id   BIGINT,                  -- C2G: 群 ID；C2C: NULL
    e2ee       JSONB,
    payload    TEXT         NOT NULL,
    created_at TIMESTAMPTZ  NOT NULL DEFAULT NOW(),
    server_ts  TIMESTAMPTZ  NOT NULL DEFAULT NOW(),
    PRIMARY KEY (id, created_at)
);

ALTER TABLE IF EXISTS public.msg_store OWNER TO imboy_user;

-- 3. 幂等唯一索引（防重复归档）
CREATE UNIQUE INDEX IF NOT EXISTS uk_msg_store_msg_id_created_at
    ON public.msg_store (msg_id, created_at);

-- 4. 游标查询核心索引
CREATE INDEX IF NOT EXISTS i_msg_store_conv_seq
    ON public.msg_store (conv_key, conv_seq);

-- 5. 发送者查询索引（管理员审计用）
CREATE INDEX IF NOT EXISTS i_msg_store_from_id
    ON public.msg_store (from_id, created_at DESC);

-- 6. TimescaleDB 超表（每 30 天一个 chunk）
SELECT create_hypertable(
    'msg_store',
    'created_at',
    chunk_time_interval => INTERVAL '30 days',
    if_not_exists => TRUE
);

-- 7. 启用压缩（按 conv_key 分段，减少随机 I/O）
ALTER TABLE msg_store SET (
    timescaledb.compress,
    timescaledb.compress_orderby   = 'created_at DESC',
    timescaledb.compress_segmentby = 'conv_key'
);

-- 8. 压缩策略：90 天后压缩
SELECT add_compression_policy('msg_store', INTERVAL '90 days');

-- 注意：保留策略由运营商按业务需求单独配置，此处不设默认值
-- 示例（保留 2 年）：
-- SELECT add_retention_policy('msg_store', INTERVAL '2 years');

COMMENT ON TABLE  public.msg_store IS '消息永久存储表：Worker 写入，不因客户端 ACK 删除';
COMMENT ON COLUMN public.msg_store.chat_type  IS '聊天类型：c2c | c2g';
COMMENT ON COLUMN public.msg_store.conv_key   IS '会话键：c2c:{min_uid}:{max_uid} 或 c2g:{gid}';
COMMENT ON COLUMN public.msg_store.conv_seq   IS 'per-conversation 单调递增序列号，客户端增量同步游标';
COMMENT ON COLUMN public.msg_store.msg_id     IS '消息全局唯一 ID';
COMMENT ON COLUMN public.msg_store.from_id    IS '发送者 user_id';
COMMENT ON COLUMN public.msg_store.to_id      IS '接收者 user_id（C2C）';
COMMENT ON COLUMN public.msg_store.group_id   IS '群组 ID（C2G）';
COMMENT ON COLUMN public.msg_store.e2ee       IS 'E2EE 元数据（JSONB，加密消息时非 NULL）';
COMMENT ON COLUMN public.msg_store.payload    IS '消息内容（明文 JSON 或加密 base64 密文）';
COMMENT ON COLUMN public.msg_store.created_at IS '消息创建时间（分区键）';
COMMENT ON COLUMN public.msg_store.server_ts  IS '服务端接收时间';

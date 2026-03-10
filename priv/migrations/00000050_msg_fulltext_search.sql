-- 消息全文搜索索引创建
-- 针对私聊消息和群聊消息创建中文分词的全文搜索索引

-- NOTE:
-- pure_migrations executes each SQL file in a transaction, and TimescaleDB
-- hypertables do not support CONCURRENTLY index creation.
CREATE INDEX IF NOT EXISTS idx_msg_c2c_payload_fts
ON msg_c2c USING GIN (to_tsvector('jiebacfg', payload::text));

CREATE INDEX IF NOT EXISTS idx_msg_c2g_payload_fts
ON msg_c2g USING GIN (to_tsvector('jiebacfg', payload::text));

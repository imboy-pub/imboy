-- 消息全文搜索索引创建
-- 针对私聊消息和群聊消息创建中文分词的全文搜索索引

CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_msg_c2c_payload_fts
ON msg_c2c USING GIN (to_tsvector('jiebacfg', payload::text));

CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_msg_c2g_payload_fts
ON msg_c2g USING GIN (to_tsvector('jiebacfg', payload::text));
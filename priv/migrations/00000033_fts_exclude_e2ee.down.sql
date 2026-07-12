-- 回滚：恢复无条件的消息全文搜索索引 / Rollback: restore unconditional message FTS indexes

DROP INDEX IF EXISTS idx_msg_c2c_payload_fts;
CREATE INDEX idx_msg_c2c_payload_fts ON public.msg_c2c
    USING gin (to_tsvector('public.jiebacfg'::regconfig, payload));

DROP INDEX IF EXISTS idx_msg_c2g_payload_fts;
CREATE INDEX idx_msg_c2g_payload_fts ON public.msg_c2g
    USING gin (to_tsvector('public.jiebacfg'::regconfig, (payload)::text));

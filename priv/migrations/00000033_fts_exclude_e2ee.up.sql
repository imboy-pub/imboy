-- 消息全文搜索索引排除 E2EE 密文 / Exclude E2EE ciphertext from message FTS indexes
-- 安全：E2EE 消息 payload 为 base64 密文，不应进入倒排索引（判定信号与 i_c2c_e2ee 一致：e2ee IS NULL 为明文）
-- Security: E2EE payload is base64 ciphertext and must not enter the GIN index
-- (canonical plaintext predicate, consistent with i_c2c_e2ee: e2ee IS NULL)

DROP INDEX IF EXISTS idx_msg_c2c_payload_fts;
CREATE INDEX idx_msg_c2c_payload_fts ON public.msg_c2c
    USING gin (to_tsvector('public.jiebacfg'::regconfig, payload))
    WHERE (e2ee IS NULL);

DROP INDEX IF EXISTS idx_msg_c2g_payload_fts;
CREATE INDEX idx_msg_c2g_payload_fts ON public.msg_c2g
    USING gin (to_tsvector('public.jiebacfg'::regconfig, (payload)::text))
    WHERE (e2ee IS NULL);

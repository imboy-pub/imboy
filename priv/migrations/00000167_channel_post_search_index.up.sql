-- Match the existing channel_message content expression; no second content store.
CREATE INDEX idx_channel_message_content_jieba
    ON public.channel_message
    USING gin (to_tsvector('jiebacfg'::regconfig, coalesce(content, '')));

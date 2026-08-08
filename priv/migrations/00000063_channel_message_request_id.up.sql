-- 频道消息发布幂等键：同一作者、频道和 request_id 只允许一条消息。
ALTER TABLE public.channel_message
    ADD COLUMN IF NOT EXISTS request_id varchar(64);

CREATE UNIQUE INDEX IF NOT EXISTS uq_channel_message_request_id
    ON public.channel_message (author_id, channel_id, request_id)
    WHERE request_id IS NOT NULL;

COMMENT ON COLUMN public.channel_message.request_id IS
    '客户端发布请求幂等键；同一作者和频道内复用时返回原消息';

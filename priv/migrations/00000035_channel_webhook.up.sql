-- P1: 频道 incoming webhook —— token 即凭证的免认证入站消息通道
-- Channel incoming webhook: token-as-credential inbound message channel.
-- 每个 webhook 绑定创建时的 channel（channel locking：token 不携带路由信息，
-- 目标频道只来自本表行），并绑定一个 account_type=2 的 system_bot 用户作为消息作者。

CREATE TABLE IF NOT EXISTS public.channel_webhook (
    id          bigint      NOT NULL,                          -- TSID
    channel_id  bigint      NOT NULL,                          -- 绑定的频道（webhook 只能发到该频道）
    name        varchar(64) NOT NULL DEFAULT '',               -- webhook 名称（同时用作 bot 昵称）
    token       varchar(64) NOT NULL,                          -- 不可猜测随机凭证（strong_rand_bytes hex）
    bot_uid     bigint      NOT NULL,                          -- 绑定的 system_bot 用户（user.account_type=2）
    creator_uid bigint      NOT NULL,                          -- 创建者（频道管理员）
    status      smallint    NOT NULL DEFAULT 1,                -- 1=启用 2=停用
    created_at  timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at  timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (id)
);
CREATE UNIQUE INDEX IF NOT EXISTS uk_channel_webhook_token
    ON public.channel_webhook(token);
CREATE INDEX IF NOT EXISTS idx_channel_webhook_channel_id
    ON public.channel_webhook(channel_id);

COMMENT ON TABLE public.channel_webhook IS '频道 incoming webhook（token 即凭证，绑定 channel + system_bot 作者）';
COMMENT ON COLUMN public.channel_webhook.id IS 'TSID 主键';
COMMENT ON COLUMN public.channel_webhook.channel_id IS '绑定频道 ID（channel locking：只能发到该频道）';
COMMENT ON COLUMN public.channel_webhook.name IS 'webhook 名称（bot 昵称）';
COMMENT ON COLUMN public.channel_webhook.token IS '随机凭证（仅创建时明文返回一次；list 只回前 8 位掩码）';
COMMENT ON COLUMN public.channel_webhook.bot_uid IS '消息作者 bot 用户 ID（user.account_type=2 服务端事实标记，防钓鱼）';
COMMENT ON COLUMN public.channel_webhook.creator_uid IS '创建者用户 ID（须为频道管理员 role>=2）';
COMMENT ON COLUMN public.channel_webhook.status IS '状态：1=启用 2=停用（停用后 incoming 统一 404）';

-- billing_subscription 增加 owner_uid：订阅归属人
--
-- 背景：billing_handler 的 9 个端点此前全部只从请求参数取 tenant_id /
-- subscription_id，从不校验请求者身份 —— 任意持有合法 JWT 的用户都能续费、
-- 取消、支付他人的订阅。tenant_id 由客户端传入，不能作为授权依据。
--
-- 单租户简化：owner_uid = 创建订阅时的 current_uid。
-- 历史数据 owner_uid=0 表示「无主订阅」，用户端一律拒绝操作，只允许管理端处理。

ALTER TABLE public.billing_subscription
    ADD COLUMN IF NOT EXISTS owner_uid bigint DEFAULT 0 NOT NULL;
--;

COMMENT ON COLUMN public.billing_subscription.owner_uid IS
    '订阅归属用户 id；0=历史无主订阅，用户端拒绝操作，仅管理端可处理';
--;

CREATE INDEX IF NOT EXISTS idx_billing_sub_owner
    ON public.billing_subscription USING btree (owner_uid);
--;

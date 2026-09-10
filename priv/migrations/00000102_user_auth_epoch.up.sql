-- Task 10 / LT-04：持久化会话吊销（session epoch）
-- 每用户单调递增 epoch；access/refresh token 签发时携带 ep claim，
-- 共享鉴权入口（HTTP auth_ds / WS websocket_ds / 既有 WS 心跳）比对：
-- token.ep < current epoch ⇒ 已吊销（改密/全端登出/管理端禁用/重置密码均 bump）。
-- 缺行等价 epoch=1（首 bump 即 2）；epoch 只增不减；DB 权威 = 重启后依然生效。

CREATE TABLE IF NOT EXISTS public.user_auth_epoch (
    user_id    bigint PRIMARY KEY,
    epoch      bigint NOT NULL DEFAULT 1,
    updated_at timestamptz NOT NULL DEFAULT now()
);

COMMENT ON TABLE public.user_auth_epoch
    IS 'Task10/LT-04 session epoch; token ep < row epoch = revoked; missing row = epoch 1';

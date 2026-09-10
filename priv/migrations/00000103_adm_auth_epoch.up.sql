-- Task 12 / LT-06：管理后台会话吊销（session epoch）
-- 每管理员单调递增 epoch；登录签发的 cookie sig 内嵌 (epoch, exp) 并整体 HMAC；
-- 中间件校验：sig.epoch < 当势 epoch ⇒ 已吊销（登出/管理端强制下线均 bump）；
-- exp 过期 ⇒ 拒绝。缺行等价 epoch=1（首 bump 即 2）；epoch 只增不减；
-- DB 权威 = 重启后依然生效；epoch 现势不可确认 ⇒ fail-closed 拒绝。

CREATE TABLE IF NOT EXISTS public.adm_auth_epoch (
    admin_id   bigint PRIMARY KEY,
    epoch      bigint NOT NULL DEFAULT 1,
    updated_at timestamptz NOT NULL DEFAULT now()
);

COMMENT ON TABLE public.adm_auth_epoch
    IS 'Task12/LT-06 admin session epoch; cookie sig epoch < row epoch = revoked; missing row = epoch 1';

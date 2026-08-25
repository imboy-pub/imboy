-- 00000073_channel_order_refunding.up.sql
-- B-09 (channel_order)：增加 status=5「退款中」占位态
--
-- 背景：
--   channel_logic_order:do_refund_order/5 旧流程是「调网关退款 → 改订单状态」
--   若调网关后本地落库失败（DB 抖动 / 连接断），订单状态仍停在 1(已支付)，
--   重试会**第二次调用网关退款** —— 网关侧没有幂等键的话就是重复退款。
--
-- 修法：
--   改成「CAS 占位(1→5) → 调网关 → CAS 收尾(5→2)」。
--   第二个请求拿不到 1→5 的 CAS，根本走不到网关调用那一步。
--   网关明确失败时释放占位(5→1)；网关成功但收尾失败则**故意留在 5**，
--   由人工核对收尾 —— 释放回 1 才是危险的那条路。
--
-- 本迁移只做一件事：放开 status 的 CHECK 约束让 5 合法。
-- 不改任何存量行（没有任何行会自动变成 5）。

ALTER TABLE public.channel_order
    DROP CONSTRAINT IF EXISTS chk_channel_order_status;
--;

ALTER TABLE public.channel_order
    ADD CONSTRAINT chk_channel_order_status CHECK ((status = ANY (ARRAY[0, 1, 2, 3, 4, 5])));
--;

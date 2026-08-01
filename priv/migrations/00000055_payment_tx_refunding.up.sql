-- 00000055_payment_tx_refunding.up.sql
-- B-09：payment_transaction 增加 status=5「退款中」占位态
--
-- 背景：
--   finance_adm_logic:refund_payment_by_biz/3 旧流程是「调网关退款 → mark_refunded」。
--   若 mark_refunded 失败（DB 抖动 / 连接断），流水状态仍停在 1(成功)，
--   管理员在后台重试时会**第二次调用网关退款** —— 网关侧没有幂等键的话就是重复退款，
--   真金白银退两次。
--
-- 修法：
--   改成「CAS 占位(1→5) → 调网关 → CAS 收尾(5→3)」。占位是重试的唯一闸门：
--   第二个请求拿不到 1→5 的 CAS，根本走不到网关调用那一步。
--   网关明确失败时释放占位(5→1)；网关成功但收尾失败则**故意留在 5**，
--   由人工核对收尾 —— 释放回 1 才是危险的那条路。
--
-- 本迁移只做一件事：放开 status 的 CHECK 约束让 5 合法。
-- 不改任何存量行（没有任何行会自动变成 5）。
--
-- ⚠️ 约束用 DROP IF EXISTS + ADD 重建，而不是假设当前定义与
--   00000010_payment.up.sql 里写的完全一致 —— foundation 迁移的建表语句可能已 stale
--   （#20 那次就踩过：foundation 写 md5，真实库早已改名 file_hash256）。

ALTER TABLE public.payment_transaction
    DROP CONSTRAINT IF EXISTS chk_payment_tx_status;
--;

ALTER TABLE public.payment_transaction
    ADD CONSTRAINT chk_payment_tx_status CHECK ((status = ANY (ARRAY[0, 1, 2, 3, 4, 5])));
--;

COMMENT ON COLUMN public.payment_transaction.status IS
    '状态: 0待支付 1成功 2失败 3已退款 4部分退款 5退款中(B-09 占位态)';
--;

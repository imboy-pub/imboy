-- G-QR 回滚：删表即彻底清除（含已消费/未消费 token——回滚不得把
-- token 恢复成可再次领取，表删除后无任何残留路径）。
DROP TABLE IF EXISTS public.qr_login_session;

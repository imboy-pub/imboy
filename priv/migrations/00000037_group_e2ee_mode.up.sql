-- 迁移 000037: 群级 E2EE 开关（P0-B B4）
--   "group" 表加 e2ee_mode：0=off（默认，现状不变）1=required（群主开启后
--   服务端 fail-closed 拒收未加密的内容消息）。仅允许 0→1 单向开启（MVP，
--   防"关闭后明文泄漏预期"歧义），应用层约束。
--   Adds "group".e2ee_mode (0=off default, 1=required). Owner-only one-way
--   enable; server rejects plaintext content messages when required.

ALTER TABLE public."group" ADD COLUMN IF NOT EXISTS e2ee_mode smallint NOT NULL DEFAULT 0;
COMMENT ON COLUMN public."group".e2ee_mode IS '群级 E2EE 模式 0=off（默认）1=required（拒收明文，单向开启）';

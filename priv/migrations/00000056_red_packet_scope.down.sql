-- 00000056_red_packet_scope.down.sql
-- 回滚 B-11 的红包作用域绑定。
--
-- ⚠️ 回滚即恢复"任何登录用户拿到 packet_id 就能领"的越权面。
--   回滚前请先把 {red_packet_require_scope, ...} 关掉，否则 send 侧会继续
--   要求一个已经不存在的字段。

ALTER TABLE public.red_packet
    DROP CONSTRAINT IF EXISTS chk_red_packet_scope_pair;
--;

ALTER TABLE public.red_packet
    DROP CONSTRAINT IF EXISTS chk_red_packet_scope_type;
--;

ALTER TABLE public.red_packet DROP COLUMN IF EXISTS scope_id;
--;

ALTER TABLE public.red_packet DROP COLUMN IF EXISTS scope_type;
--;

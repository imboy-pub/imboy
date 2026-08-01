-- 00000056_red_packet_scope.up.sql
-- B-11：red_packet 增加会话作用域，使「非该群成员不得领取」可判定
--
-- ⚠️ 计划判据的前提原本不成立，先说清楚：
--   判据写的是「非该群成员领取返回无权操作」，但 red_packet 表**没有任何群/会话
--   绑定字段**（只有 sender_uid/amount/count/greeting/status/expires_at），
--   客户端 send 也只提交 amount/count/type/greeting —— 服务端根本无从知道
--   这个红包属于哪个群。红包是靠一条 msg_type=redPacket 的聊天消息分享出去的，
--   作用域只存在于"那条消息发去了哪里"，从未落库。
--   因此 open/2 此前是：**任何登录用户拿到 packet_id 就能领**。
--
-- 本迁移补上绑定字段，让判据可实现：
--   scope_type: NULL(未绑定，旧数据/旧客户端) / 'C2C' / 'C2G'
--   scope_id:   C2G 时为 group_id；C2C 时为对端 uid
--
-- 兼容策略（刻意）：
--   两列可空。未绑定的红包**沿用旧行为**（不校验），否则本迁移一上线，所有
--   在途的旧红包和未升级客户端发的红包会立刻全部领不了。
--   等客户端全量升级后，把 {red_packet_require_scope, true} 打开，
--   send 会拒绝不带作用域的请求 —— 与 #94 的 legacy 开关是同一个套路，
--   **开关不打开就等于没修完**，必须登记为待办而不是当作已完成。

ALTER TABLE public.red_packet
    ADD COLUMN IF NOT EXISTS scope_type character varying(8);
--;

ALTER TABLE public.red_packet
    ADD COLUMN IF NOT EXISTS scope_id bigint;
--;

ALTER TABLE public.red_packet
    DROP CONSTRAINT IF EXISTS chk_red_packet_scope_type;
--;

ALTER TABLE public.red_packet
    ADD CONSTRAINT chk_red_packet_scope_type
    CHECK (scope_type IS NULL OR (scope_type)::text = ANY (ARRAY['C2C'::text, 'C2G'::text]));
--;

-- 绑定了作用域就必须有 scope_id，避免出现"声称是群红包但不知道哪个群"的半绑定行
ALTER TABLE public.red_packet
    DROP CONSTRAINT IF EXISTS chk_red_packet_scope_pair;
--;

ALTER TABLE public.red_packet
    ADD CONSTRAINT chk_red_packet_scope_pair
    CHECK ((scope_type IS NULL AND scope_id IS NULL) OR (scope_type IS NOT NULL AND scope_id IS NOT NULL));
--;

COMMENT ON COLUMN public.red_packet.scope_type IS '作用域类型: NULL未绑定(旧数据) C2C单聊 C2G群聊';
--;

COMMENT ON COLUMN public.red_packet.scope_id IS '作用域ID: C2G为group_id，C2C为对端uid';
--;

-- 00000057_channel_order_refunding.down.sql
-- 回退：移出 status=5，回到原来的 0-4 约束

ALTER TABLE public.channel_order
    DROP CONSTRAINT IF EXISTS chk_channel_order_status;
--;

ALTER TABLE public.channel_order
    ADD CONSTRAINT chk_channel_order_status CHECK ((status = ANY (ARRAY[0, 1, 2, 3, 4])));
--;
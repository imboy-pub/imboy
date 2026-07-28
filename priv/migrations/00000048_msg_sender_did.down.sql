-- 00000048_msg_sender_did.down.sql
-- 回滚 A2-a 的 sender_did 列。
--   回滚后离线拉取的 C2C PFv3 消息重新丢失发送者设备标识，
--   接收侧 context binding 第 6 项将重新失配（消息不可读）。
--   已落库的设备标识不可恢复。

ALTER TABLE IF EXISTS public.msg_store_staging
    DROP COLUMN IF EXISTS sender_did;

ALTER TABLE IF EXISTS public.msg_c2c
    DROP COLUMN IF EXISTS sender_did;

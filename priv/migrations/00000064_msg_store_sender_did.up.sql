-- 00000064_msg_store_sender_did.up.sql
-- PFv3 历史解密需要归档时保存服务端认证过的发送设备标识。
ALTER TABLE IF EXISTS public.msg_store
    ADD COLUMN IF NOT EXISTS sender_did character varying(128);

COMMENT ON COLUMN public.msg_store.sender_did IS
    '发送方设备ID（服务端认证态注入，客户端不可伪造）；PFv3 历史 context binding #6，NULL=未提供';

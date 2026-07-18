-- 00000044_device_trust.down.sql
-- 回滚设备信任审计表。所有信任决策历史事件丢失。

DROP TABLE IF EXISTS public.trust_audit CASCADE;

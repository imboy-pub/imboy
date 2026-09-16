-- 00000123 BUG-01：投递 ACK 在 device_id IS NULL 时不幂等
--
-- 背景（RULING-2026-09-15 授权本地修复；冻结计划已要求 device_id 为空时 ACK 仍幂等）：
--   enterprise_message_delivery 的逻辑键唯一约束
--   uq_emd_org_message_recipient_device (organization_id, message_id, recipient_ref, device_id)
--   建于 00000116，是普通 UNIQUE。PostgreSQL 默认 UNIQUE 把 NULL 视为互不相等，
--   因此 device_id IS NULL 的同一逻辑键重放会插入第二行 —— ACK 幂等声明失效。
--
-- 修复：改为 PostgreSQL 18 的 UNIQUE NULLS NOT DISTINCT，NULL 在键比较中视为相等；
--   非 NULL 键语义不变。产品侧 ACK 插入使用列集推断
--   ON CONFLICT (organization_id, message_id, recipient_ref, device_id) DO UPDATE，
--   迁移后该推断自动命中新约束（含 NULL 键），无需改产品 SQL。
--
-- 数据守卫（fail-closed，不修数据）：
--   若迁移前已存在 device_id IS NULL 的重复逻辑键行，本迁移在重建唯一索引时
--   必然失败并整体回滚 —— 按规程登记 BLOCKED_DATA 处置，绝不在此删除任何行。
--
-- down：恢复 00000116 的原始普通 UNIQUE（回退后 NULL 键重放将再次失去幂等，
--   这是旧语义的如实恢复，不是本迁移的缺陷）。

ALTER TABLE enterprise_message_delivery
    DROP CONSTRAINT uq_emd_org_message_recipient_device;

ALTER TABLE enterprise_message_delivery
    ADD CONSTRAINT uq_emd_org_message_recipient_device
    UNIQUE NULLS NOT DISTINCT (organization_id, message_id, recipient_ref, device_id);

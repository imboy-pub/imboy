-- 00000123 down：恢复 00000116 的原始普通 UNIQUE（NULL 互不相等）。

ALTER TABLE enterprise_message_delivery
    DROP CONSTRAINT uq_emd_org_message_recipient_device;

ALTER TABLE enterprise_message_delivery
    ADD CONSTRAINT uq_emd_org_message_recipient_device
    UNIQUE (organization_id, message_id, recipient_ref, device_id);

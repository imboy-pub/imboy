-- 迁移 00000154_customer_service_seat_console_list_index.down.sql: 对称回滚
-- 00000154（索引-查询对齐）。恢复 153 的原始索引形态
-- i_cssc_org_ws_status (organization_id, workspace_id, status)，删除
-- i_cssc_org_ws_id。纯索引操作，无数据变更。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

DROP INDEX IF EXISTS i_cssc_org_ws_id;

CREATE INDEX IF NOT EXISTS i_cssc_org_ws_status ON customer_service_seat_console
    USING btree (organization_id, workspace_id, status);

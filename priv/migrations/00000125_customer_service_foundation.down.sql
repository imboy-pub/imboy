-- 迁移 00000125 down: 安全回滚客服基础五表（与 up 严格逆序）。
-- 事件表先于会话表删除（FK 依赖）；enterprise_conversation 上由本迁移追加的
-- uq_ec_org_id_contact 索引一并移除，116 及更早迁移的对象零改动。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

DROP TRIGGER IF EXISTS trg_customer_service_event_append_only ON customer_service_event;
DROP FUNCTION IF EXISTS fn_customer_service_event_append_only();
DROP TABLE IF EXISTS customer_service_event;

DROP INDEX IF EXISTS i_cse_org_created;
DROP INDEX IF EXISTS i_cse_org_session;

DROP TABLE IF EXISTS customer_service_session;
DROP INDEX IF EXISTS i_csss_org_contact;
DROP INDEX IF EXISTS i_csss_org_identity_active;
DROP INDEX IF EXISTS i_csss_org_status;

DROP TABLE IF EXISTS customer_service_visit_token;
DROP INDEX IF EXISTS i_csvt_org_contact;

DROP TABLE IF EXISTS customer_service_shop_key;

DROP TABLE IF EXISTS customer_service_seat;
DROP INDEX IF EXISTS i_css_org_enabled;

DROP INDEX IF EXISTS uq_ec_org_id_contact;

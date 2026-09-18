-- 迁移 00000132 down: 安全回滚 Widget 持久化基础（与 up 严格逆序）。
-- visit_token 上本迁移追加的三列/FK/索引先删（既有运营侧令牌行零损失），
-- 两个新表后删（FK 依赖）；125 及更早迁移的对象除被追加的列外零改动。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

DROP TABLE IF EXISTS customer_service_widget_nonce;
DROP INDEX IF EXISTS i_cswn_expiry;

DROP INDEX IF EXISTS i_csvt_widget_install;
ALTER TABLE customer_service_visit_token
    DROP CONSTRAINT IF EXISTS fk_csvt_widget_installation;
ALTER TABLE customer_service_visit_token
    DROP COLUMN IF EXISTS widget_installation_id,
    DROP COLUMN IF EXISTS anonymous_subject_hmac,
    DROP COLUMN IF EXISTS last_seen_at;

DROP TABLE IF EXISTS customer_service_widget_identity_key;

DROP TABLE IF EXISTS customer_service_widget_installation;
DROP INDEX IF EXISTS i_cswi_org_status;

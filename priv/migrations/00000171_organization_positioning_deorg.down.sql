-- 00000171 down：撤除实例级唯一约束与注释修订（列与数据不动）。
ALTER TABLE enterprise_external_identity DROP CONSTRAINT IF EXISTS uq_eei_app_external_global;
ALTER TABLE enterprise_application DROP CONSTRAINT IF EXISTS uq_ea_key_global;

COMMENT ON TABLE enterprise_application IS
    '企业集成 Application（/api/internal/v1 调用主体，归 Organization；调用方只能是客户 OA 后端）';
COMMENT ON COLUMN enterprise_application.organization_id IS
    '资源 owner 租户；删除机构一律 RESTRICT（fail-closed）';
COMMENT ON COLUMN enterprise_application.application_key IS
    'Org 内唯一的稳定标识（URL/审计/凭证组合用）';
COMMENT ON TABLE enterprise_external_identity IS NULL;

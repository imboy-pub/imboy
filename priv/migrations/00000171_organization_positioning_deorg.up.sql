-- 00000171: organization 定位修订 —— OA SSO 链去 org 授权边界（M1）
--
-- ADR: docs/architecture/2026-10-12-organization-positioning-amendment.md
--   （owner 裁决：organization 是业务对象，非租户；授权边界 = 部署实例）
--
-- expand-first：两表 organization_id 列与既有 org 维度约束全部保留为
-- 业务归属（business attribution），不删、不收缩。本迁移只新增实例级
-- 唯一约束，并在前置校验失败时 FAIL（人工处置后重跑）。

-- 前置校验 1：application_key 存量跨 org 重复则 FAIL。
-- 处置指引：为冲突应用改 key（API/凭证引用方同步）或合并应用后重跑。
DO $$
DECLARE dup_keys text;
BEGIN
    SELECT string_agg(application_key, ', ' ORDER BY application_key)
      INTO dup_keys
      FROM (SELECT application_key
              FROM enterprise_application
             GROUP BY application_key
            HAVING count(*) > 1) d;
    IF dup_keys IS NOT NULL THEN
        RAISE EXCEPTION 'application_key duplicated across organizations: %', dup_keys;
    END IF;
END $$;

-- 前置校验 2：(application_id, external_user_id) 重复则 FAIL。
-- fk_eei_application 已保证 identity.organization_id = application.organization_id，
-- 理论上不可能重复；守卫保留（fail-closed，防历史数据漂移）。
DO $$
DECLARE dup_count bigint;
BEGIN
    SELECT count(*) INTO dup_count FROM (
        SELECT application_id, external_user_id
          FROM enterprise_external_identity
         GROUP BY application_id, external_user_id
        HAVING count(*) > 1
    ) d;
    IF dup_count > 0 THEN
        RAISE EXCEPTION
            'enterprise_external_identity duplicate (application_id, external_user_id): % group(s)',
            dup_count;
    END IF;
END $$;

ALTER TABLE enterprise_application
    ADD CONSTRAINT uq_ea_key_global UNIQUE (application_key);

ALTER TABLE enterprise_external_identity
    ADD CONSTRAINT uq_eei_app_external_global UNIQUE (application_id, external_user_id);

COMMENT ON TABLE enterprise_application IS
    '企业集成 Application（/api/internal/v1 调用主体，实例级实体；organization_id 为业务归属，非授权边界——2026-10-12 定位修订）';
COMMENT ON COLUMN enterprise_application.organization_id IS
    '业务归属（business attribution）；非授权边界、非租户（2026-10-12 定位修订）';
COMMENT ON COLUMN enterprise_application.application_key IS
    '实例内全局唯一稳定标识（uq_ea_key_global；URL/审计/凭证组合用）';
COMMENT ON TABLE enterprise_external_identity IS
    'IMBoy user ↔ 外部系统员工标识映射（实例级 (application_id, external_user_id) 唯一；mapping 对 OA 免登可选——缺省回退 user_id）；organization_id 为业务归属';

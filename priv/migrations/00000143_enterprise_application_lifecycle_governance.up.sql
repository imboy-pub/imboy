-- 迁移 00000143: enterprise_application 生命周期治理 + Grant 撤销归因双通道
-- （FULL-08 / plan-full §3.1、§3.2、§7）。
--
-- 为什么必须改 schema（不是「接线」能绕过的合同级缺口）：
--   Admin 治理面契约冻结在 imboyadmin 分支 run/full-candidate-admin-20260921T101806Z
--   的 src/modules/enterprise_apps/api/contracts.ts（ENDPOINTS A-01..A-14 逐字字符串
--   被单测钉死）。后端现有 schema 支撑不了其中两条：
--
--   1) enterprise_application 无 version 列，status 的 CHECK 只有
--      ('active','disabled') 二值。而冻结契约要求：
--        * 每个 Application 行必须返回 version（contracts.ts:APPLICATION_SAFE_KEYS）；
--        * A-03（生命周期）/ A-04（scope）的请求体带 expected_version，即 CAS 写入
--          （并发下不得静默覆盖）；
--        * 生命周期枚举是 draft|active|disabled|archived 四值
--          （contracts.ts:APPLICATION_STATUSES，archived 为终态）。
--   2) enterprise_application_grant 的 revoked_by_user_id 是 REFERENCES "user"(id)，
--      且 ck_eag_status_revoked_match 要求 revoked 行该列 NOT NULL。Admin 治理面
--      的撤销者是**平台管理员（adm_user）**，不是租户 user —— 不补列就只能
--      （a）伪造一个租户 user 作执行者，或（b）放弃留痕。两者都不可接受。
--
-- 本迁移做的事（全部幂等）：
--   1) enterprise_application.version —— 乐观锁版本号，治理写入时 version+1 并
--      以 WHERE version = expected_version 做 CAS。DEFAULT 1，既有行自动填 1。
--   2) ck_ea_status 放宽为四值（与前端 APPLICATION_STATUSES 逐字同序）。
--   3) i_ea_org_created —— Admin 列表可按 (organization_id, created_at DESC, id DESC)
--      走索引，避免全表扫（B2 性能口径要求 EXPLAIN 见索引）。
--   4) enterprise_application_grant.revoked_by_adm_user_id —— 平台管理员 ID。
--      **不加 FK**：adm_user 是另一域的对象，加 FK 会让删除管理员牵连租户授权表
--      （对本审计列而言是错误的生命周期耦合）。该列只作留痕，不参与任何授权判定。
--   5) ck_eag_status_revoked_match 放宽为「执行者恰有一个」：
--        * 未撤销 ⇒ 两个 actor 列都为 NULL（语义不变）；
--        * 已撤销 ⇒ revoked_by_user_id / revoked_by_adm_user_id **恰有一个非空**
--          —— 都空 = 无归因（静默撤权不可接受），都非空 = 归因歧义。
--
-- 命名纪律（FULL-02 真库实测：同 schema 函数名全局唯一，CREATE OR REPLACE 会
--   静默改写他域同名守卫函数体）：本迁移**不新建任何函数/触发器**，只加列/索引/
--   约束，故无需域前缀；新增对象名 i_ea_org_created / ck_ea_version /
--   ck_ea_status / revoked_by_adm_user_id / ck_eag_status_revoked_match 均属本表域，
--   且都用 IF NOT EXISTS / DROP-IF-EXISTS-再建 保证幂等。
--
-- 迁移契约：up=可重复执行；down=把对象还原到 142 的形态（见 down 文件对新
--   生命周期状态的数据收敛说明）。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- 1) enterprise_application.version（乐观锁）
-- ============================================================
ALTER TABLE enterprise_application
    ADD COLUMN IF NOT EXISTS version integer NOT NULL DEFAULT 1;

ALTER TABLE enterprise_application
    DROP CONSTRAINT IF EXISTS ck_ea_version;
ALTER TABLE enterprise_application
    ADD CONSTRAINT ck_ea_version CHECK (version >= 1);

COMMENT ON COLUMN enterprise_application.version IS
    '乐观锁版本号（迁移 00000143 / FULL-08）：Admin 治理写入（status/scopes）以 WHERE version = expected_version 做 CAS，成功即 +1；并发写只有赢家生效，其余返回 version_conflict';

-- ============================================================
-- 2) 生命周期枚举放宽为四值（draft / active / disabled / archived）
-- ============================================================
-- 顺序与 imboyadmin contracts.ts:APPLICATION_STATUSES 逐字一致（含 archived 为终态
-- 的语义；迁移顺序点本身在前端/应用层，DB 只保证值域）。
ALTER TABLE enterprise_application
    DROP CONSTRAINT IF EXISTS ck_ea_status;
ALTER TABLE enterprise_application
    ADD CONSTRAINT ck_ea_status CHECK (status = ANY (ARRAY['draft','active','disabled','archived']));

COMMENT ON COLUMN enterprise_application.status IS
    '生命周期（迁移 00000143 放宽为四值）：draft 草稿 / active 启用 / disabled 停用 / archived 归档（终态）。合法迁移由应用层裁决，DB 只保证值域';

-- ============================================================
-- 3) Admin 列表索引
-- ============================================================
CREATE INDEX IF NOT EXISTS i_ea_org_created
    ON enterprise_application (organization_id, created_at DESC, id DESC);

COMMENT ON INDEX i_ea_org_created IS
    'Admin 治理列表（迁移 00000143）：按组织分页浏览 Application，避免全表 Seq Scan';

-- ============================================================
-- 4) Grant 撤销归因：平台管理员通道
-- ============================================================
ALTER TABLE enterprise_application_grant
    ADD COLUMN IF NOT EXISTS revoked_by_adm_user_id bigint;

COMMENT ON COLUMN enterprise_application_grant.revoked_by_adm_user_id IS
    '撤销者：平台管理员 adm_user.id（迁移 00000143）。无 FK（跨平台域，删除管理员不牵连租户授权行）；只作审计留痕，不参与任何授权判定。与 revoked_by_user_id 恰有一个非空';

ALTER TABLE enterprise_application_grant
    DROP CONSTRAINT IF EXISTS ck_eag_status_revoked_match;
ALTER TABLE enterprise_application_grant
    ADD CONSTRAINT ck_eag_status_revoked_match CHECK (
        (status = 'revoked') = (revoked_at IS NOT NULL)
        AND CASE
                WHEN revoked_at IS NULL THEN
                    revoked_by_user_id IS NULL AND revoked_by_adm_user_id IS NULL
                ELSE
                    (revoked_by_user_id IS NOT NULL) <> (revoked_by_adm_user_id IS NOT NULL)
            END
    );

COMMENT ON CONSTRAINT ck_eag_status_revoked_match ON enterprise_application_grant IS
    '撤销一致性（迁移 00000143 放宽执行者来源）：status=revoked 与 revoked_at 严格配对；未撤销时两个 actor 列皆 NULL；已撤销时 revoked_by_user_id（租户 user）与 revoked_by_adm_user_id（平台管理员）恰有一个非空——两者皆空为无归因撤权，两者皆非空为归因歧义';

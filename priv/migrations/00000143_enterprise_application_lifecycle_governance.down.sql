-- 迁移 00000143 回滚: enterprise_application 生命周期治理 + Grant 撤销归因双通道
-- （FULL-08）。把对象还原到 00000142 结束时的形态。
--
-- 回滚范围：只移除本迁移新增的列 / 索引 / 约束，不动
--   * enterprise_application 的既有列（136 的 status / allowed_scopes / 140 的内容策略列）
--   * enterprise_application_grant 的既有列与触发器（139 的授权语义）
--   * 137/138（foreign run 持有，全程未触碰）
--
-- ⚠ 数据收敛（必须显式说明，不做静默丢数据）：
--   回到 142 意味着 ck_ea_status 恢复为二值 ('active','disabled')，而 143 期间
--   可能已产生 draft / archived 行。若不处理，重加窄约束会因脏值直接失败，
--   使整条回滚链卡死（erlang_migrate 的 down 链是串行的）。故 down 先把
--   draft/archived 收敛为 disabled —— 这是**安全方向**的单向收紧：
--   归档/草稿态应用在此后不可被内部面鉴权通过（auth 只认 active），
--   收敛为 disabled 不会扩大任何授权面。
--   本迁移只用于 scratch 验证与回滚到 142；禁止在生产库执行。
--
-- 禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- 1) 收敛新生命周期状态（见文件头说明）
-- ============================================================
UPDATE enterprise_application
SET status = 'disabled'
WHERE status IN ('draft', 'archived');

-- ============================================================
-- 2) 还原 Grant 约束与列
-- ============================================================
-- ⚠ 归因收敛（A0-REV review 发现）：139 的窄约束要求 revoked 行必有租户
--   user 归因（(revoked_at IS NULL) = (revoked_by_user_id IS NULL)），而 143
--   期间可能已产生 **adm 通道撤销**的行（revoked_at NOT NULL + user 列 NULL +
--   revoked_by_adm_user_id 非空）。若不处理，下方 ADD CONSTRAINT 对存量行
--   校验必败，整条回滚链卡死（与 draft/archived 同类问题，此处同样按
--   **安全方向**收敛：归因物化为 organization.owner_id —— 语义是「该组织侧
--   的授权已被撤销」，不删行（审计留痕，D3）、不改 status（不复活授权）、
--   不扩权；真实操作者是平台管理员，139 形态无处安放，故记为组织侧归属。
--   顺序要点：**先拆 143 宽约束再收敛** —— PG 的 CHECK 是逐行即时校验，
--   先填 user 列会瞬间构成「双 actor 非空」而撞 143 的 XOR 约束。
ALTER TABLE enterprise_application_grant
    DROP CONSTRAINT IF EXISTS ck_eag_status_revoked_match;

UPDATE enterprise_application_grant g
SET revoked_by_user_id = o.owner_id
FROM organization o
WHERE g.revoked_by_adm_user_id IS NOT NULL
  AND g.revoked_by_user_id IS NULL
  AND o.id = g.organization_id;

ALTER TABLE enterprise_application_grant
    ADD CONSTRAINT ck_eag_status_revoked_match CHECK (
        (status = 'revoked') = (revoked_at IS NOT NULL)
        AND (revoked_at IS NULL) = (revoked_by_user_id IS NULL)
    );

ALTER TABLE enterprise_application_grant
    DROP COLUMN IF EXISTS revoked_by_adm_user_id;

-- ============================================================
-- 3) 还原 Application 索引 / 状态值域 / version 列
-- ============================================================
DROP INDEX IF EXISTS i_ea_org_created;

ALTER TABLE enterprise_application
    DROP CONSTRAINT IF EXISTS ck_ea_status;
ALTER TABLE enterprise_application
    ADD CONSTRAINT ck_ea_status CHECK (status = ANY (ARRAY['active','disabled']));

COMMENT ON COLUMN enterprise_application.status IS NULL;

ALTER TABLE enterprise_application
    DROP CONSTRAINT IF EXISTS ck_ea_version;
ALTER TABLE enterprise_application
    DROP COLUMN IF EXISTS version;

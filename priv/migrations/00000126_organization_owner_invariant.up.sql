-- 迁移 00000126: Organization Owner Invariant（M01 / ORG_SLOT_OWNER_INVARIANT）
-- Core Contract C04：organization.owner_id 的 FK 改为 ON DELETE RESTRICT，
-- 并建立 active-owner partial unique index，保证每 Org 最多一条
-- role='owner' AND status='active' 的 organization_member 行。
--
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
-- 冻结实现（计划 ORG-01 DATABASE 节 + 任务卡 ORG-01）：
--   (1) fk_organization_owner 由 ON DELETE CASCADE 改为 ON DELETE RESTRICT
--       —— 消灭「删 owner 用户级联删组织」（ORG-09 P11 钉死的现状）；
--   (2) 部分唯一索引 uq_organization_member_single_active_owner
--       —— 消灭「双 owner 行被允许」（ORG-09 P02 钉死的现状）。
-- expand 期兼容：trg_organization_owner_member_sync 同步触发器保留不删
-- （contract switch 属后续 Gate）；本迁移只加静态约束，不改任何触发器。
-- 防御性对账：若存量数据违反不变量（双 owner / 非 Human owner / 投影不一致），
-- fail-closed 报错终止迁移，不做静默修复（对账属 ORG-02/ORG-11 的 Gate）。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 0: 存量数据对账（fail-closed，幂等可重复）
-- ============================================================
DO $$
DECLARE
    v_double_owner bigint;
    v_non_human_owner bigint;
    v_projection_drift bigint;
BEGIN
    -- 同一 Org 两条及以上 active owner 行
    SELECT count(*) INTO v_double_owner
      FROM (
          SELECT organization_id
            FROM organization_member
           WHERE role = 'owner' AND status = 'active'
           GROUP BY organization_id
          HAVING count(*) > 1
      ) s;
    IF v_double_owner > 0 THEN
        RAISE EXCEPTION
            '迁移 00000126 对账失败：% 个 organization 存在多条 active owner 行，请先人工对账',
            v_double_owner
            USING ERRCODE = '23514', CONSTRAINT = 'organization_owner_invariant_reconcile';
    END IF;

    -- active owner 行指向非 Human 账号（account_type <> 0）
    SELECT count(*) INTO v_non_human_owner
      FROM organization_member om
      JOIN "user" u ON u.id = om.user_id
     WHERE om.role = 'owner' AND om.status = 'active' AND u.account_type <> 0;
    IF v_non_human_owner > 0 THEN
        RAISE EXCEPTION
            '迁移 00000126 对账失败：% 个 active owner 行指向非 Human 账号（account_type<>0）',
            v_non_human_owner
            USING ERRCODE = '23514', CONSTRAINT = 'organization_owner_invariant_reconcile';
    END IF;

    -- active owner 行与 organization.owner_id 投影不一致
    SELECT count(*) INTO v_projection_drift
      FROM organization o
      JOIN organization_member om
        ON om.organization_id = o.id AND om.role = 'owner' AND om.status = 'active'
     WHERE om.user_id <> o.owner_id;
    IF v_projection_drift > 0 THEN
        RAISE EXCEPTION
            '迁移 00000126 对账失败：% 个 organization 的 active owner 行与 owner_id 不一致',
            v_projection_drift
            USING ERRCODE = '23514', CONSTRAINT = 'organization_owner_invariant_reconcile';
    END IF;
END
$$;

-- ============================================================
-- Phase 1: fk_organization_owner 改 ON DELETE RESTRICT
--   owner 用户删除在未 transfer 前被稳定拒绝（C04/C17），不再级联删组织。
-- ============================================================
ALTER TABLE organization DROP CONSTRAINT IF EXISTS fk_organization_owner;
ALTER TABLE organization ADD CONSTRAINT fk_organization_owner
    FOREIGN KEY (owner_id) REFERENCES "user"(id) ON DELETE RESTRICT;

COMMENT ON CONSTRAINT fk_organization_owner ON organization IS
    'Owner 用户引用；ON DELETE RESTRICT：未完成 owner transfer 前删除 owner 用户被稳定拒绝（C04/C17）';

-- ============================================================
-- Phase 2: active-owner partial unique index
--   每 Org 最多一条 role='owner' AND status='active' 的成员行；
--   transfer 事务按「先降旧、再升新」顺序写入，语句间瞬态由该索引即时裁决。
-- ============================================================
DROP INDEX IF EXISTS uq_organization_member_single_active_owner;
CREATE UNIQUE INDEX uq_organization_member_single_active_owner
    ON organization_member USING btree (organization_id)
    WHERE role = 'owner' AND status = 'active';

COMMENT ON INDEX uq_organization_member_single_active_owner IS
    '每 Organization 最多一条 active owner 成员行（C04 SOURCE OF TRUTH 唯一性）';

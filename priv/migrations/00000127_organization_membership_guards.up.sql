-- 迁移 00000127: Organization Membership Guards（M02 / ORG_SLOT_MEMBERSHIP_GUARDS）
-- Core Contract C04：organization 与 organization_member 两侧建
-- DEFERRABLE INITIALLY DEFERRED 约束触发器，事务提交时校验
-- 「恰好一个 active Human owner 且 = organization.owner_id」。
--
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 为什么把 00000113 的 trg_organization_primary_owner_member_guard 改为
-- DEFERRABLE INITIALLY DEFERRED（竞态裁决，冻结实现第 3/5 点）：
--   transfer 顺序是「先降旧 owner → 再升新 owner → 最后改 owner_id 投影」。
--   原 BEFORE 即时 guard 在「降旧 owner」一步会读到未更新的 owner_id（仍=旧 owner）
--   而误拒；deferred 化后 guard 在提交时评估，读到 owner_id 的最终值——
--   00000113 的原始语义「先转移 organization.owner_id，再处理旧 Owner 的成员行」
--   完全保留：提交时 owner_id 已指向新 owner，旧 owner 的降级被放行；
--   任何绕过 transfer 的降级/移除/暂停（提交时 owner_id 仍指向该行）依然被拒。
--   旧同步触发器 trg_organization_owner_member_sync 按 C04 TRANSITION 保留不动。
--   两侧 invariant constraint trigger 是与 sync/guard 正交的最终防线（fail closed）。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 1: 主 Owner 成员行 guard → DEFERRABLE INITIALLY DEFERRED
--   复用 00000113 的 fn_organization_primary_owner_member_guard 函数，仅改触发器时点。
-- ============================================================
DROP TRIGGER IF EXISTS trg_organization_primary_owner_member_guard ON organization_member;

CREATE CONSTRAINT TRIGGER trg_organization_primary_owner_member_guard
    AFTER UPDATE OF role, status OR DELETE ON organization_member
    DEFERRABLE INITIALLY DEFERRED
    FOR EACH ROW
    EXECUTE FUNCTION fn_organization_primary_owner_member_guard();

COMMENT ON TRIGGER trg_organization_primary_owner_member_guard ON organization_member IS
    '提交时拒绝「organization.owner_id 仍指向该行」的降级/移除/暂停（C04：先 transfer 再改成员行）';

-- ============================================================
-- Phase 2: organization 侧 invariant（提交时校验）
--   恰好一个 active Human owner 且与 owner_id 一致；org 行被删除时跳过。
-- ============================================================
CREATE OR REPLACE FUNCTION fn_organization_owner_invariant() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_org_id            bigint;
    v_owner_id          bigint;
    v_human_owner_count integer;
    v_owner_user_id     bigint;
BEGIN
    IF TG_OP = 'DELETE' THEN
        -- org 行删除：成员行随 FK CASCADE 一并消失，不存在 owner 不变量约束。
        RETURN NULL;
    END IF;
    v_org_id   := NEW.id;
    v_owner_id := NEW.owner_id;

    SELECT count(*), min(om.user_id)
      INTO v_human_owner_count, v_owner_user_id
      FROM organization_member om
      JOIN "user" u ON u.id = om.user_id
     WHERE om.organization_id = v_org_id
       AND om.role = 'owner'
       AND om.status = 'active'
       AND u.account_type = 0;

    IF v_human_owner_count <> 1 THEN
        RAISE EXCEPTION
            'organization % 提交校验失败：必须恰好一个 active Human owner（实际 % 个）',
            v_org_id, v_human_owner_count
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_organization_owner_invariant',
                  COLUMN = 'owner_id',
                  TABLE = 'organization';
    END IF;
    IF v_owner_user_id <> v_owner_id THEN
        RAISE EXCEPTION
            'organization % 提交校验失败：active Human owner % 与 owner_id % 投影不一致',
            v_org_id, v_owner_user_id, v_owner_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_organization_owner_invariant',
                  COLUMN = 'owner_id',
                  TABLE = 'organization';
    END IF;
    RETURN NULL;
END;
$$;

DROP TRIGGER IF EXISTS trg_organization_owner_invariant ON organization;
CREATE CONSTRAINT TRIGGER trg_organization_owner_invariant
    AFTER INSERT OR UPDATE OF owner_id ON organization
    DEFERRABLE INITIALLY DEFERRED
    FOR EACH ROW
    EXECUTE FUNCTION fn_organization_owner_invariant();

COMMENT ON TRIGGER trg_organization_owner_invariant ON organization IS
    '提交时校验恰好一个 active Human owner 且 = owner_id（C04 SOURCE OF TRUTH 最终防线）';

-- ============================================================
-- Phase 3: organization_member 侧 invariant（提交时校验）
--   org 仍存在时同 org 侧不变量；org 行已删除（CASCADE 链）则放行。
-- ============================================================
CREATE OR REPLACE FUNCTION fn_organization_member_owner_invariant() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_org_id            bigint;
    v_owner_id          bigint;
    v_human_owner_count integer;
    v_owner_user_id     bigint;
BEGIN
    IF TG_OP = 'DELETE' THEN
        v_org_id := OLD.organization_id;
    ELSE
        v_org_id := NEW.organization_id;
    END IF;

    SELECT o.owner_id INTO v_owner_id
      FROM organization o
     WHERE o.id = v_org_id;
    IF v_owner_id IS NULL THEN
        -- organization 行已随本事务删除（CASCADE），整体消失，无需 owner 不变量。
        RETURN NULL;
    END IF;

    SELECT count(*), min(om.user_id)
      INTO v_human_owner_count, v_owner_user_id
      FROM organization_member om
      JOIN "user" u ON u.id = om.user_id
     WHERE om.organization_id = v_org_id
       AND om.role = 'owner'
       AND om.status = 'active'
       AND u.account_type = 0;

    IF v_human_owner_count <> 1 OR v_owner_user_id <> v_owner_id THEN
        RAISE EXCEPTION
            'organization_member 提交校验失败：organization % 必须恰好一个 active Human owner（实际 % 个）且与 owner_id % 一致',
            v_org_id, v_human_owner_count, v_owner_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_organization_member_owner_invariant',
                  COLUMN = 'role',
                  TABLE = 'organization_member';
    END IF;
    RETURN NULL;
END;
$$;

DROP TRIGGER IF EXISTS trg_organization_member_owner_invariant ON organization_member;
CREATE CONSTRAINT TRIGGER trg_organization_member_owner_invariant
    AFTER INSERT OR UPDATE OF role, status OR DELETE ON organization_member
    DEFERRABLE INITIALLY DEFERRED
    FOR EACH ROW
    EXECUTE FUNCTION fn_organization_member_owner_invariant();

COMMENT ON TRIGGER trg_organization_member_owner_invariant ON organization_member IS
    '提交时校验所属 organization 恰好一个 active Human owner 且 = owner_id（C04 成员侧防线）';

-- 迁移 00000128: Organization Invitation（M03 / ORG_SLOT_INVITATION，ledger 已由 ORG-00 分配 128）
-- Core Contract C11：Invite、Membership、Restore、Accept、Reject、Expire、Revoke
-- 是不同 command/state；SOURCE OF TRUTH = organization_invitation（本表）。
--
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 冻结实现（计划 ORG-03 DATABASE 节 + 任务卡 ORG-03 + Core Contract C11）：
--   * 状态机 CHECK：pending/accepted/rejected/expired/revoked；
--     终态 = accepted/rejected/expired/revoked，唯一可进入态 = pending。
--   * token 只存 digest（sha256 小写 hex，64 字符，CHECK 兜底）；明文禁止落库/日志，
--     与仓内既有模式一致（cs_access_app:default_digest/1、customer_service_visit_token）。
--   * V1 target_user 必填（C11 INVARIANTS）；invited_by 必填（审计）；
--     expires_at 必填（expiry）；created_at/updated_at/responded_at 审计时间戳。
--   * 部分唯一索引 uq_organization_invitation_single_pending：
--     同 (organization_id, target_user_id) 最多一条未终结（pending）邀请；
--     终态行不受约束，历史可并存。
--   * 不承载 invited role：邀请只表达「成为成员」这一事实，落位 role 由
--     membership 侧 adapter（ORG-01 之后的 hook，ORG-10 集成）按 C03/C04 裁决，
--     本表不复制 membership 的角色语义（C11：pending 不进 Membership）。
--   * FK 与 00000113 organization_member 同口径：organization / user 删除时 CASCADE
--     （C17：User 删除结果由 deletion orchestrator 的 RESTRICT/guard 裁决，
--     invitation 不是 blocker，也不允许它反向决定业务结果）。
--   * accept 一次性消费的原子语义由应用层单条 CAS UPDATE（WHERE status='pending'）
--     完成，DB 侧以部分唯一索引锁「同 (org,target) 单条未终结」为兜底。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 1: organization_invitation 表
-- ============================================================
CREATE TABLE IF NOT EXISTS organization_invitation (
    id             bigint NOT NULL,
    organization_id bigint NOT NULL,
    target_user_id  bigint NOT NULL,
    invited_by      bigint NOT NULL,
    token_digest    text   NOT NULL,
    status          text   DEFAULT 'pending' NOT NULL,
    expires_at      timestamp with time zone NOT NULL,
    responded_at    timestamp with time zone,
    created_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_organization_invitation PRIMARY KEY (id),
    CONSTRAINT ck_organization_invitation_status
        CHECK (status = ANY (ARRAY['pending'::text, 'accepted'::text, 'rejected'::text,
                                   'expired'::text, 'revoked'::text])),
    CONSTRAINT ck_organization_invitation_token_digest
        CHECK (token_digest ~ '^[0-9a-f]{64}$')
);

ALTER TABLE organization_invitation DROP CONSTRAINT IF EXISTS fk_organization_invitation_organization;
ALTER TABLE organization_invitation ADD CONSTRAINT fk_organization_invitation_organization
    FOREIGN KEY (organization_id) REFERENCES organization(id) ON DELETE CASCADE;

ALTER TABLE organization_invitation DROP CONSTRAINT IF EXISTS fk_organization_invitation_target_user;
ALTER TABLE organization_invitation ADD CONSTRAINT fk_organization_invitation_target_user
    FOREIGN KEY (target_user_id) REFERENCES "user"(id) ON DELETE CASCADE;

ALTER TABLE organization_invitation DROP CONSTRAINT IF EXISTS fk_organization_invitation_invited_by;
ALTER TABLE organization_invitation ADD CONSTRAINT fk_organization_invitation_invited_by
    FOREIGN KEY (invited_by) REFERENCES "user"(id) ON DELETE CASCADE;

-- ============================================================
-- Phase 2: 索引（C11：同 (org,target) 最多一条未终结邀请）
-- ============================================================
CREATE UNIQUE INDEX IF NOT EXISTS uq_organization_invitation_single_pending
    ON organization_invitation (organization_id, target_user_id)
    WHERE status = 'pending';

-- target 视角「我的邀请」查询（跨 Org 列 pending）
CREATE INDEX IF NOT EXISTS i_organization_invitation_target_status
    ON organization_invitation (target_user_id, status, id DESC);

-- Org 治理视角列表
CREATE INDEX IF NOT EXISTS i_organization_invitation_org_created
    ON organization_invitation (organization_id, created_at DESC);

-- ============================================================
-- Phase 3: 注释（C11 契约锚点）
-- ============================================================
COMMENT ON TABLE organization_invitation IS
    'Organization 邀请（C11 SOURCE OF TRUTH）：状态机 pending/accepted/rejected/expired/revoked；token 只存 sha256 hex digest；同 (organization_id,target_user_id) 最多一条 pending（uq_organization_invitation_single_pending）。';
COMMENT ON COLUMN organization_invitation.token_digest IS
    'sha256(token 明文) 小写 hex（64 字符）；明文只在 create 响应返回一次，禁止落库/日志。';
COMMENT ON COLUMN organization_invitation.responded_at IS
    '进入终态（accepted/rejected/expired/revoked）的时刻；pending 行为 NULL，一次性消费由应用层 CAS UPDATE 保证恰写一次。';

-- 扩展依赖：无（纯核心类型，不依赖任何扩展）。

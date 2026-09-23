-- 迁移 00000144 回滚: enterprise scope 枚举扩到 14 值 + internal 幂等 response 快照
-- （V2.1）。把对象还原到 00000143 结束时的形态。
--
-- 回滚范围：只移除本迁移新增的列 / 约束，恢复 10 值 scope CHECK；
--   不动 136/139/143 的任何其他对象。
--
-- ⚠ 数据收敛（必须显式说明，不做静默丢数据）：
--   1) enterprise_application_grant_scope 若在 144 生效期间被授予了 4 个新
--      只读 scope（groups:read / workspaces:read / projects:read /
--      channels:read），回到 143 的 10 值枚举必须先收敛这些行——直接重加
--      窄 CHECK 会因脏值失败并卡死回滚链。收敛方式是 **删除** 这些 scope
--      行：这是 fail-closed 方向的单向收紧（相关授权面收窄为零，绝不放大
--      权限）；10 值枚举本身无法表达这些授权，保留即约束违例。
--      生产库执行本回滚前必须人工盘点受影响 Grant 并另行迁移计划。
--   2) response_body 列整体 DROP：幂等表是 24h TTL 的 scratch 语义表，
--      response_body 不是业务数据（§11：TTL 过后按新请求执行）。down 只
--      允许在 scratch/candidate 验证场景执行；生产回退另立计划（对称于
--      143 down 的口径）。删除不丢任何业务行——幂等行本身保留。
--
-- 禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- 1) 幂等 completion 一致性 + response 快照列
-- ============================================================
ALTER TABLE enterprise_internal_idempotency
    DROP CONSTRAINT IF EXISTS ck_eii_completion;

-- scratch 语义 DROP（见文件头说明 2：生产回退需另立计划）
ALTER TABLE enterprise_internal_idempotency
    DROP COLUMN IF EXISTS response_body;

COMMENT ON COLUMN enterprise_internal_idempotency.response_code IS '已存储响应码（claim 前可空）';
COMMENT ON TABLE enterprise_internal_idempotency IS
    '/api/internal/v1 幂等记录（所有 mutation 必带 Idempotency-Key；同 key 同 body 重放原结果，同 key 异 body 409 idempotency_conflict）';

-- ============================================================
-- 2) Grant scope 固定枚举：14 -> 10（先收敛新值行，见文件头说明 1）
-- ============================================================
ALTER TABLE enterprise_application_grant_scope
    DROP CONSTRAINT IF EXISTS ck_eags_scope_fixed;

DELETE FROM enterprise_application_grant_scope
WHERE scope IN (
    'groups:read'::text,
    'workspaces:read'::text,
    'projects:read'::text,
    'channels:read'::text
);

ALTER TABLE enterprise_application_grant_scope
    ADD CONSTRAINT ck_eags_scope_fixed CHECK (scope = ANY (ARRAY[
        'application:read'::text,
        'identities:read'::text,
        'identities:write'::text,
        'groups:write'::text,
        'files:write'::text,
        'messages:send'::text,
        'messages:send_as_human'::text,
        'friend_requests:create'::text,
        'webhooks:manage'::text,
        'sso:exchange'::text
    ]));

COMMENT ON CONSTRAINT ck_eags_scope_fixed ON enterprise_application_grant_scope IS
    'Grant 授权的固定 scope 集合（plan-gz §4.2 十值枚举；无 wildcard，DB 层 23514 拒绝通配/未登记值）';
